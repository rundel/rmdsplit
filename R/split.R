#' @export
rmd_split_render = function(
  file, render_func, headings = 1:6, out_dir,
  prefix = "split_", ext = "pdf"
) {
  stopifnot(fs::dir_exists(out_dir))

  file = fs::path_abs(file)

  dir = fs::path_dir(file)
  file = fs::path_file(file)

  withr::local_dir(dir)


  rmd = parsermd::parse_rmd(file)
  rmd_tbl = parsermd::as_tibble(rmd)

  secs = parsermd::rmd_node_sections(rmd, levels = headings, drop_na = TRUE)
  sec_ids = match(secs, unique(secs))
  sec_id_counts = table(sec_ids)

  content = sec_ids %in% which(sec_id_counts > 1) & # Sections with content
    purrr::map_lgl(secs, ~ !all(is.na(.x))) # Not preamble

  splits = secs[content]
  splits = splits[!duplicated(splits)]

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

      render_func(rmd_file,  output)
    })
  }

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
  render_func(rmd_file,  output)

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
wkhtml_renderer = function(file, out) {
  html_out = withr::local_tempfile(tmpdir = getwd(), fileext = ".html")

  # This is nonsense but needed to shut pandoc up
  out_fmt = rmarkdown::html_document()
  out_fmt$pandoc$args = c(out_fmt$pandoc$args, "--metadata", "pagetitle=blah")

  rmarkdown::render(
    file,
    output_file = html_out,
    output_format = out_fmt,
    quiet = TRUE
  )

  processx::run(
    "wkhtmltopdf",
    c("-d", "200", html_out, out)
  )
}


