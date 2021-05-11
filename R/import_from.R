import_from <- function(path) {
  script <- readChar(path, file.info(path)$size)
  py <- reticulate::import_main()
  py$ast <- reticulate::import("ast")
  py$tree <- py$ast$parse(script)
  reticulate::source_python(system.file("python/python-ast-explorer/parse.py", package ="Rchitecture"))
  l_ast <- purrr::flatten(jsonify_ast(py$tree)$Module$body) # assumption:there is only one module per ast
  # cleaning up import froms to data frame
  tbl_import_from <- l_ast[names(l_ast)=="ImportFrom"] %>% # deps omit points... thank you very much!
    purrr::map(~purrr::modify_at(.x, "names",
                                 ~ purrr::map_df(.x,
                                                 ~.x %>%
                                                   purrr::flatten() %>%
                                                   unlist()))) %>%
    purrr::map_df(dplyr::bind_cols)
  if(!"module" %in% names(tbl_import_from)) { # if all null disapears
    tbl_import_from <- tbl_import_from %>%
      dplyr::mutate(
        module = NA_character_
      )
  }
  # inspecting if anything relative to the file path qualifies as a python module
  folder <- stringr::str_remove(path, "[^/]+$")
  other_files <- list.files(folder, recursive = TRUE, full.names = TRUE)
  other_py_scripts <- other_files %>%
    stringr::str_extract(".*\\.py$") %>%
    na.omit()
  if(nrow(tbl_import_from)>0 & length(other_py_scripts)>0){
    py_paths <- tibble::tibble(
      absolute_paths = other_py_scripts,
      relative_py = absolute_paths %>%
        stringr::str_remove(glue::glue("^{folder}/")) %>%
        stringr::str_remove("\\.py$") %>%
        stringr::str_replace_all("/", "\\.")
    )
    # what is from . import api, db_session
   matches <- dplyr::left_join(
     tbl_import_from,
     py_paths,
     by = c("module"="relative_py")
   )
   matches %>%
     dplyr::filter(!is.na(absolute_paths)) %>%
     dplyr::transmute(
       to = path,
       from = absolute_paths,
       type= "import_from",
       directed = "TRUE"
     )
  } else tibble::tibble(
    source = character(),
    target = character(),
    type = character(),
    directed = logical()
  )
}
