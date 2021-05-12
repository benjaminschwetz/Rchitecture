ast_list <- function(path){
  script <- readChar(path, file.info(path)$size)
  py <- reticulate::import_main()
  py$ast <- reticulate::import("ast")
  py$tree <- py$ast$parse(script)
  reticulate::source_python(system.file("python/python-ast-explorer/parse.py", package ="Rchitecture"))
  l_ast <- purrr::flatten(jsonify_ast(py$tree)$Module$body) # assumption:there is only one module per ast
  return(l_ast)
}
