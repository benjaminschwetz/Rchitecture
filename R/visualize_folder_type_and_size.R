#' Visualize folder type and size
#'
#' @param folder_path path to the repository
#' @param path_prefix leading folders from absolute path to omit from visualization.
#' @export
visualize_folder_type_and_size <- function(folder_path, path_prefix){
  set.seed(35)
  # browser()
  folder_contents <- content_table(zeeguu_path, include_folders = FALSE, path_prefix) %>%
    mutate(path = stringr::str_replace_all(path,"/+", "/"))
  tree <- data.tree::as.Node(folder_contents,pathName="path")
  tree$Do(function(x) {
    x$size <- Aggregate(node = x,
                              attribute = "size",
                              aggFun = sum)
  },
  traversal = "post-order")
  tree$Do(function(x) {
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
  tree_unique <- make_names_unique(tree)
  size_data <- data.tree::ToDataFrameTree(tree_unique, "name", "size", "content_type", "short_name")
  igraph_graph <- data.tree::as.igraph.Node(tree_unique, directed = TRUE)
  tbl_graph <- tidygraph::as_tbl_graph(igraph_graph) %>%
    tidygraph::activate("nodes") %>%
    dplyr::left_join(folder_contents,
              by = c("name" = "path")) %>%
    dplyr::left_join(size_data, by = "name") %>%
    select(-size.x, size = size.y) %>%
    dplyr::mutate(
       isdir = tidyr::replace_na(isdir, TRUE),
       size = size + 1 # zero weights cannot be plotted
       )

# get all the subgraphs for every directory at depth
# aggregate subfiles
  tbl_graph %>%
    tidygraph::filter(isdir) %>%
  ggraph::ggraph('circlepack', weight = size) +
    ggraph::geom_node_circle(aes(fill = content_type)) +
    ggraph::geom_node_label(aes(label = short_name, filter = depth == 1), nudge_y= -150, repel = FALSE, check_overlap = FALSE)+
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::scale_fill_brewer(type = "qual")
    # ggraph::ggraph('treemap', weight = size) +
    # ggraph::geom_node_tile(aes(fill = content_type))
    # ggraph::ggraph('circlepack', weight = size) +
    # ggraph::geom_edge_link() +
    # ggraph::geom_node_point(aes(colour = content_type)) +
    # ggplot2::coord_fixed()
  # tbl_graph %>%
  #   tidygraph::filter(isdir) %>%
  #   ggraph::ggraph('circlepack', weight = size) +
  #   ggraph::geom_node_voronoi(aes(fill = content_type)) +
  #   ggplot2::coord_fixed() +
  #   ggplot2::theme_void() +
  #   ggplot2::scale_fill_brewer(type = "qual")
  # # ggraph::ggraph('treemap', weight = size) +
  # ggraph::geom_node_tile(aes(fill = content_type))
  # ggraph::ggraph('circlepack', weight = size) +
  # ggraph::geom_edge_link() +
  # ggraph::geom_node_point(aes(colour = content_type)) +
  # ggplot2::coord_fixed()
}
