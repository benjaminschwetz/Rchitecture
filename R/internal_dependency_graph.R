#' Visualize internal dependencies
#'
#' @param project_path path to the repository
#' @param path_prefix leading folders from absolute path to omit from visualization.
#' @export
internal_dependency_graph <- function(project_path, path_prefix) {
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
  tbl_gr_dependencies <- tidygraph::as_tbl_graph(
      tbl_int_dep_edges
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
  tbl_gr_all <- tbl_gr_dependencies %>%
    tidygraph::graph_join(tbl_gr_py_clean,
                          by = "name") %>%
    tidygraph::mutate(
      index = row_number(),
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
  rerouted_edges <- tbl_gr_all %>%
    activate("edges") %>%
    as_tibble() %>%
    left_join(parents, by = "from") %>%
    mutate(
      old_from = from,
      from =  dplyr::case_when(
        type == "depends_on" ~ parent,
        TRUE ~ from
      )) %>%
    dplyr::group_by(from, to, type) %>%
    dplyr::summarise(occurences = sum(occurences)) %>%
    dplyr::ungroup()
  tbl_gr_acum <- tidygraph::tbl_graph(
    nodes = tbl_gr_all %>% activate("nodes") %>% as_tibble(),
    edges = rerouted_edges,
    directed = TRUE
  ) %>%
    filter(out_degree_struc != 0)
  # calc layout
  layout_graph <- tbl_gr_acum %>%
    activate("edges") %>%
    tidygraph::convert(
      function(graph){
        graph %>%
          filter(type != "depends_on")
      }
    )
  g_layout <- layout_to_table(layout_as_tree, layout_graph, circular = TRUE) %>%
    select(V1, V2) %>%
    graphlayouts::layout_rotate(90)

  ggraph::ggraph(tbl_gr_acum, layout = "manual", x=g_layout[,1],y=g_layout[,2]) +
    ggraph::geom_edge_link(aes(filter = type == "contains"))+
        ggraph::geom_node_label(aes(label = short_name, filter = out_degree_struc > 0, size = in_degree_total))+ # or maybe here

        ggraph::geom_edge_arc(
      aes(
        filter = type == "depends_on",
        edge_width = occurences,
        alpha= occurences),       color = "deepskyblue3", arrow = arrow(angle = 5, type = "closed"))+
    #ggraph::scale_edge_color_distiller(direction = 1)+
    ggraph::geom_node_text(aes(label = short_name, filter = out_degree_struc > 0, size = in_degree_total))+ # or maybe here
    ggplot2::coord_fixed() +
    ggplot2::theme_void()
}
