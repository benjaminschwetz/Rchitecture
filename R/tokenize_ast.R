#' Tokenizer for python ast
#'
#' @param script_path absolute paths from scripts to tokenize.
#' @param path_prefix leading folders from absolute path to omit from names.
#'
#' @return a named list, compatible with \code{quanteda::tokenize()}
#' @export
tokenize_ast <- function(script_path, path_prefix = "."){
  possible_ast_list <- purrr::possibly(ast_list, NA_character_)
  purrr::map(
    paste(path_prefix,script_path,sep = "/"),
             ~ possible_ast_list(.x) %>%
               unlist() %>%
               unname) %>%
    magrittr::set_names(script_path)
}
