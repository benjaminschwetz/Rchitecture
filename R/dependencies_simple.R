dependencies_simple <- function(path) {
  script <- readChar(path, file.info(path)$size)
  py <- reticulate::import_main()
  py$ast <- reticulate::import("ast")
  py$tree <- py$ast$parse(script)
  reticulate::source_python("inst/python/python-ast-explorer/parse.py")
  l_ast <- purrr::flatten(jsonify_ast(py$tree)$Module$body) # assumption:there is only one module per ast
  # cleaning up imports
  import <- l_ast[names(l_ast)=="Import"] %>%
    purrr::modify_depth(3, dplyr::bind_cols) %>%
    purrr::map_df("names")
  # cleaning up import froms
  import_from <- l_ast[names(l_ast)=="ImportFrom"] %>% # deps omit points... thank you very much!
    purrr::map(~purrr::modify_at(.x, "names",
                                 ~ purrr::map_df(.x,
                                                 ~.x %>%
                                                   purrr::flatten() %>%
                                                   unlist()))) %>%
    purrr::map_df(dplyr::bind_cols)
  # cleaning up class defs
  class_def <- l_ast[names(l_ast)=="ClassDef"]
  l_out <- list(
    import = import,
    import_from = import_from
  )
  return(l_out)
}
