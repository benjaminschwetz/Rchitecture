#' NOT WORKING Visualize internal dependencies
#'
#' @param project_path path to the repository
#' @param path_prefix leading folders from absolute path to omit from visualization.
#' @param dependencies_file file with one dependency per line
generate_dependency_graph <- function(project_path, path_prefix, dependencies_file) {
  # parse external dependencies out from file to table
  ext_dependencies <- readLines(dependencies_file)
  tbl_ext_dependencies <- tibble::tibble(
    id = ext_dependencies,
    version = str_extract(id, "(?<=\\=\\=).*$"),
    dir = str_extract(id, "(?<=git\\+git://).*(?=\\.git)") %>%
      dplyr::coalesce(paste0("pip/",id)) %>%
      paste0("requirements.txt/", .)
  )
  ## generate graph from it
  tree_dep <- data.tree::as.Node(tbl_ext_dependencies,pathName="dir")
  tree_unique_dep <- make_names_unique(tree_dep)
  dep_name_data <- data.tree::ToDataFrameTree(tree_unique_dep, "name", "short_name")
  igraph_dep <- data.tree::as.igraph.Node(tree_unique_dep, directed = TRUE)
  tbl_gr_dep_struc <- tidygraph::as_tbl_graph(igraph_dep) %>%
    tidygraph::activate("nodes") %>%
    tidygraph::left_join(dep_name_data, by = "name") %>%
    tidygraph::mutate(orig_gr = "tbl_gr_dep_struc")
  # py stuff
  folder_contents <- content_table(project_path, include_folders = FALSE, path_prefix) %>%
    mutate(path = stringr::str_replace_all(path,"/+", "/")) %>%
    dplyr::filter(file_type == "py")
  tree_py <- data.tree::as.Node(folder_contents,pathName="path")
  tree_py$Do(function(x) {
    x$content_type <- Aggregate(node = x,
                                attribute = "file_type",
                                aggFun = function(x){
                                  n_vals <- length(unique(x))
                                  ifelse(n_vals == 0, NA_character_,
                                         ifelse(n_vals == 1, unique(x),
                                                "mixed"))
                                })
  },
  traversal = "post-order")
  tree_unique <- make_names_unique(tree_py)
  content_data <- data.tree::ToDataFrameTree(tree_unique, "name", "file_type", "content_type", "short_name", "name_root")
  igraph_py <- data.tree::as.igraph.Node(tree_unique, directed = TRUE)
  tbl_gr_py <- tidygraph::as_tbl_graph(igraph_py) %>%
    tidygraph::activate("nodes") %>%
    dplyr::left_join(folder_contents,
                     by = c("name" = "path")) %>%
    dplyr::left_join(content_data, by = "name") %>%
    dplyr::mutate(
      isdir = tidyr::replace_na(isdir, TRUE)
    ) %>%
    tidygraph::mutate(orig_gr = "tbl_gr_py")
  ## parse all tokens
  l_ast_tokens <- tokenize_ast(folder_contents$path, path_prefix)
  tbl_ast_tokens <- tibble::tibble(names(l_ast_tokens), l_ast_tokens) %>%
    tidyr::unnest(cols = c(l_ast_tokens))
  names(tbl_ast_tokens) <- c("script_path", "token")
  tbl_ext_dep_edges <- dplyr::inner_join(
    tbl_ast_tokens,
    tbl_ext_dependencies,
    by = c(token = "id")
  ) %>%
    dplyr::transmute(
      from = script_path,
      to = dir,
      type = "depends_on"
    ) %>%
    dplyr::count(from, to, type, name = "occurences")
  tbl_int_dep_edges <- dplyr::inner_join(
    tbl_ast_tokens,
    content_data %>% dplyr::filter(is.na(file_type)),
    by = c(token = "name_root")
  ) %>%
    dplyr::transmute(
      from = script_path,
      to = name,
      type = "depends_on"
    ) %>%
    dplyr::count(from, to, type, name = "occurences")
  ## patching the graph together
  tbl_gr_dependencies <- tidygraph::as_tbl_graph(
    dplyr::bind_rows(
      tbl_int_dep_edges,
      tbl_ext_dep_edges
    )
  )
  tbl_gr_py_clean <- tbl_gr_py %>%
    tidygraph::activate("edges") %>%
    tidygraph::mutate(type = "contains",
                      occurences = 1) %>%
    tidygraph::activate("nodes") %>%
    tidygraph::select(
      -size,
      -isdir,
      -levelName,
      -file_name,
      -file_type.y
    )
  tbl_gr_dep_struc_clean <- tbl_gr_dep_struc %>%
    tidygraph::activate("edges") %>%
    tidygraph::mutate(type = "contains",
                      occurences = 1)
  tbl_gr_all <- tidygraph::graph_join(tbl_gr_dependencies,
                                      tbl_gr_dep_struc_clean,
                                      by = "name") %>%
    tidygraph::graph_join(tbl_gr_py_clean,
                          by = "name") %>%
    tidygraph::mutate(
      orig_gr = dplyr::coalesce(orig_gr.x, orig_gr.y),
      short_name = dplyr::coalesce(short_name.x, short_name.y),
      file_type = file_type.x
    ) %>%
    dplyr::select(-tidygraph::ends_with(".x"),
                  -tidygraph::ends_with(".y"),
                  -levelName) %>%
    tidygraph::mutate(
      in_degree_total = tidygraph::centrality_degree( mode = "in"),
      out_degree_total = tidygraph::centrality_degree( mode = "out")
    ) %>%
    tidygraph::morph(
      function(graph){
        graph %>%
          activate("edges") %>%
          filter(type != "depends_on")
      }
    ) %>%
    tidygraph::mutate(
      in_degree_struc = tidygraph::centrality_degree( mode = "in"),
      out_degree_struc = tidygraph::centrality_degree( mode = "out")
    ) %>%
    tidygraph::unmorph()
  # tidygraph::reroute
  # find parents
  parents <- tbl_gr_all %>%
    activate("edges") %>%
    filter(type == "contains") %>%
    as_tibble() %>%
    select(parent = from,
           from = to# node in question
           )
  g <- tbl_gr_all %>%
    activate("edges")  %>%
    tidygraph::left_join(parents, by = "from") %>%
    tidygraph::morph(
      function(graph){
        graph %>%
          filter(type == "depends_on")
      }
    ) %>%
    tidygraph::reroute(to = parent) %>% # doesn't work yet
    tidygraph::unmorph()

  browser()
  ggraph::ggraph(g, layout = "stress", bbox = 40) +
    ggraph::geom_edge_link()+
    ggraph::geom_node_label(aes(label = short_name, filter = out_degree_struc > 0))+ # or maybe here
    ggplot2::coord_fixed() +
    ggplot2::theme_void()
}
