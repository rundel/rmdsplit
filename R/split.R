find_splits = function(rmd, headings) {
  secs = parsermd::rmd_node_sections(rmd, levels = headings, drop_na = TRUE)
  sec_ids = match(secs, unique(secs))
  sec_id_counts = table(sec_ids)

  content = sec_ids %in% which(sec_id_counts > 1) & # Sections with content
    purrr::map_lgl(secs, ~ length(.x) != 0)         # Not preamble

  splits = secs[content]

  splits[!duplicated(splits)]
}




#knitr_hook_chunk = function() {
#  structure(
#    list(
#      engine = "r",
#      name = "knitr-hook-chunk",
#      options = list(include = FALSE),
#      code = deparse( quote(
#        local({
#          hook_error = knitr::knit_hooks$get("error")
#          knitr::knit_hooks$set(error = function(x, options) {
#            if (isTRUE(options$preamble)) x = character()
#            hook_error(x, options)
#          })
#        })
#      ) ),
#      indent = ""
#    ),
#    class = "rmd_chunk"
#  )
#}



#' @export
rmd_split_render = function(
  file, template = file,
  render_func = wkhtml_renderer,
  headings = 1:6, out_dir,
  prefix = "split_", ext = "pdf",
  force_allow_errors = FALSE,
  add_subtitle = NULL
) {
  stopifnot(fs::dir_exists(out_dir))

  file = fs::path_real(file)
  out_dir = fs::path_real(out_dir)
  template = fs::path_real(template)

  dir = fs::path_dir(file)
  file = fs::path_file(file)

  withr::local_dir(dir)


  rmd = parsermd::parse_rmd(file)
  rmd_tmpl = parsermd::parse_rmd(template)

  # FIXME dirty hack for now
  if (!is.null(add_subtitle))
    rmd[[1]]$subtitle = add_subtitle

  secs = parsermd::rmd_node_sections(rmd, levels = headings, drop_na = TRUE)

  splits = find_splits(rmd_tmpl, headings)

  pb = progress::progress_bar$new(
    total = length(splits) + 1,
    format = ":what [:bar] :percent eta: :eta"
  )


  # Create a title page with yaml details

  rmd_file = withr::local_tempfile(tmpdir = getwd(), fileext = ".Rmd")

  rmd %>%
    parsermd::rmd_subset(type_refs = c("rmd_yaml","rmd_yaml_list"))  %>%
    parsermd::as_document() %>%
    paste(collapse="\n") %>%
    readr::write_file(rmd_file)

  output = file.path(
    out_dir,
    paste0(prefix, 0, ".", ext)
  )

  pb$tick(tokens = list(what = fs::path_file(output)))

  render_func(rmd_file,  output)

  # Work through the splits, one output per
  for(i in seq_along(splits)) {
    split = splits[i]

    start = match(split, secs)
    stopifnot(!is.na(start))

    preamble = rmd[seq_len(start-1)] %>%
      structure(class = c("rmd_ast", "list")) %>%
      parsermd::rmd_subset(
        type_refs = c("rmd_yaml", "rmd_yaml_list",
                      "rmd_heading", "rmd_markdown"),
        exclude = TRUE
      )

    preamble = purrr::map(
      preamble,
      function(node) {
        if (inherits(node, "rmd_chunk")) {
          node$options$include = FALSE
        }

        node
      }
    ) %>%
      structure(class = c("rmd_ast", "list"))

    cur_sec = rmd %>%
      parsermd::rmd_subset(sec_refs = split)

    if (force_allow_errors) {
      preamble = purrr::map(
        preamble,
        function(node) {
          if (inherits(node, "rmd_chunk")) {
            node$options$include = FALSE
            node$code = c(
              "try({",
              node$code,
              "})"
            )
          }
          node
        }
      ) %>%
        structure(class = c("rmd_ast", "list"))

      cur_sec = purrr::map(
        cur_sec,
        function(node) {
          if (inherits(node, "rmd_chunk")) {
            node$options$error = TRUE
            node$options$include = TRUE
          }
          node
        }
      ) %>%
        structure(class = c("rmd_ast", "list"))
    }

    split_ast = structure(
      c(preamble, cur_sec),
      class = c("rmd_ast", "list")
    )

    doc = parsermd::as_document(split_ast) %>%
      paste(collapse="\n")

    try({
      rmd_file = withr::local_tempfile(tmpdir = getwd(), fileext = ".Rmd")

      readr::write_file(doc, rmd_file)

      output = file.path(
        out_dir,
        paste0(prefix, i, ".", ext)
      )

      pb$tick(tokens = list(what = fs::path_file(output)))
      render_func(rmd_file,  output)
    })
  }

  invisible()
}

#webshot2::rmdshot(rmd_file,  paste0("split_", i,".png"))

#rmarkdown::render(
#  rmd_file,
#  weasydoc::hpdf_document(),
#  #output_format = rmarkdown::pdf_document(
#  #  latex_engine = "xelatex",
#  #  pandoc_args = paste0(
#  #    "--shift-heading-level-by=", 1-min(headings)
#  #  )
#  #),
#  output_file = paste0("split_", i,".html")
#)

#' @export
wkhtml_renderer = function(file, out, debug = FALSE) {
  html_out = withr::local_tempfile(tmpdir = getwd(), fileext = ".html")

  # This is nonsense but needed to shut pandoc up
  out_fmt = rmarkdown::html_document(keep_md = debug)
  out_fmt$pandoc$args = c(out_fmt$pandoc$args, "--metadata", "pagetitle=wkhtml")

  rmarkdown::render(
    file,
    output_file = html_out,
    output_format = out_fmt,
    quiet = !debug
  )

  processx::run(
    "wkhtmltopdf",
    c("-d", "200", html_out, out),
    echo_cmd = debug
  )
}

#' @export
wkhtml_renderer_debug = function(file, out) {
  wkhtml_renderer(file, out, TRUE)
}


