#' Failed edge bundling
#'
#' snippets of trying to use the edgebundle package.
#' @export
edge_bundling_failed <- function(){
  browser()
#####
g <- tbl_gr_all %>% tidygraph::as.igraph()
xy <- graphlayouts::layout_with_stress(g, bbox = 50)
fbundle <-edgebundle::edge_bundle_force(g,xy,compatibility_threshold = 0.1)

ggplot(fbundle)+
  geom_path(aes(x,y,group=group,col=as.factor(group)),size = 2,show.legend = FALSE)+
  geom_point(data=as.data.frame(xy),aes(V1,V2),size=5)+
  theme_void()


bb <- graphlayouts::layout_as_backbone(tidygraph::to_undirected(tbl_gr_all), keep = .2)
ggraph(tbl_gr_all,layout="manual",x=bb$xy[,1],y=bb$xy[,2])+
  ggraph::geom_edge_link()+
  ggraph::geom_node_label(aes(label = short_name, filter = out_degree_struc > 0))+
  ggplot2::coord_fixed() +
  ggraph::theme_graph()
}
