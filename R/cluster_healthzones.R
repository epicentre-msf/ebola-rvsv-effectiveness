cluster_hz <- function(subset_adm2_names) {
  
  # Load shapefile, keep NK and Ituri only
  shp_adm2 <- readRDS(here(paths$dir_shp, "sf_adm2_latest.rds")) %>%
    filter(adm1_name %in% c("Nord-Kivu", "Ituri"))
  # Get centroids of given adm2
  shp_adm2_pts <- shp_adm2 %>%
    filter(adm2_name %in% subset_adm2_names) %>%
    sf::st_centroid() %>%
    select(adm1_name, adm2_name, geometry)
  # Make tree, add labels
  tree <- hclust(as.dist(sf::st_distance(shp_adm2_pts$geometry)), method = "complete")
  tree$labels <- shp_adm2_pts$adm2_name
  
  return(tree)
  
}
