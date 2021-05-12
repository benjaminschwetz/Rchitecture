tokenize_ast <- function(script_path){
  purrr::map(script_path,
             ~ ast_list(.x) %>%
               unlist() %>%
               unname) %>%
    magrittr::set_names(script_path)
}
