ast <- function(path) {
  script <- readChar(path, file.info(path)$size)
  py <- reticulate::import_main()
  py$ast <- reticulate::import("ast")
  py$tree <- py$ast$parse(script)
  with(tree, tree$body[1]$value)
  # tree$body is a list of 7 ast objects
  # ast$dump(tree) puts out formatted code
  return(tree$body[1]$value)
}

ast_list <- function(path){
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
  # cleaning up function defs
  function_def <- l_ast[names(l_ast)=="FunctionDef"] %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    purrr::compact() %>%
    tibble::as_tibble()
  # cleaning up import froms
  import_from <- l_ast[names(l_ast)=="ImportFrom"] %>%
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
    function_def = function_def,
    import_from = import_from
  )
  return(l_out)
}
