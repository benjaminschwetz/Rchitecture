try_1 <- function(){
  h <- tbl_gr_all %>%
  activate("edges")  %>%
  tidygraph::left_join(parents, by = "from") %>%
  tidygraph::convert(
    function(graph){
      graph %>%
        filter(type != "depends_on") %>%
        activate("nodes") %>%
        filter(!(out_degree_struc == 0 & orig_gr == "tbl_gr_py"))
    }
  )

g_layout <- layout_to_table(layout_as_tree, h, circular = FALSE) %>%
  select(V1, V2) %>%
  graphlayouts::layout_rotate(90)

ggraph::ggraph(h, layout = "manual", x=g_layout[,1],y=g_layout[,2]) +
  ggraph::geom_edge_link()+
  ggraph::geom_node_point(aes(filter = out_degree_struc > 0))+ # or maybe here
  ggplot2::coord_fixed() +
  ggplot2::theme_void()
}
