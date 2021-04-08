to_multipolygon <- function(temp_sf){
  
  # get geometry types
  geom_types <- st_geometry_type(temp_sf) %>% unique() %>% as.character()
  
  # checks
  if (length(geom_types) > 1 | any(  !( geom_types %like% "MULTIPOLYGON"))) {
    
    # remove linstring
    temp_sf <- subset(temp_sf, st_geometry_type(temp_sf) %>% as.character() != "LINESTRING")
    
    # get polyons
    temp_sf <- st_collection_extract(temp_sf, "POLYGON")
    temp_sf <- sf::st_cast(temp_sf, "MULTIPOLYGON")
    return(temp_sf)
    
  } else {
    return(temp_sf) }
}

###### Simplify temp_sf -----------------

simplify_temp_sf <- function(temp_sf, tolerance=100){
  
  # reproject to utm
  temp_gpkg_simplified <- sf::st_transform(temp_sf, crs=3857)
  
  # simplify with tolerance
  temp_gpkg_simplified <- sf::st_simplify(temp_gpkg_simplified, preserveTopology = T, dTolerance = tolerance)
  
  # reproject to utm
  temp_gpkg_simplified <- sf::st_transform(temp_gpkg_simplified, crs=4674)
  
  # Make any invalid geometry valid # st_is_valid( sf)
  temp_gpkg_simplified <- sf::st_make_valid(temp_gpkg_simplified)
  
  return(temp_gpkg_simplified)
}






dissolve_polygons <- function(mysf, group_column){
  
  
  # a) make sure we have valid geometries
  temp_sf <- sf::st_make_valid(mysf)
  temp_sf <- temp_sf %>% st_buffer(0)
  
  # b) make sure we have sf MULTIPOLYGON
  #temp_sf1 <- temp_sf %>% st_cast("MULTIPOLYGON")
  temp_sf1 <- to_multipolygon(temp_sf)
  
  # c) long but complete dissolve function
  dissolvefun <- function(grp){
    
    # c.1) subset region
    temp_region <- subset(mysf, get(group_column, mysf)== grp )
    
    
    # c.2) create attribute with the number of points each polygon has
    points_in_each_polygon = sapply(1:dim(temp_region)[1], function(i)
      length(st_coordinates(temp_region$geom[i])))
    
    temp_region$points_in_each_polygon <- points_in_each_polygon
    mypols <- subset(temp_region, points_in_each_polygon > 0)
    
    # d) convert to sp
    sf_regiona <- mypols %>% as("Spatial")
    sf_regiona <- rgeos::gBuffer(sf_regiona, byid=TRUE, width=0) # correct eventual topology issues
    
    # c) dissolve borders to create country file
    result <- maptools::unionSpatialPolygons(sf_regiona, rep(TRUE, nrow(sf_regiona@data))) # dissolve
    
    
    # d) get rid of holes
    outerRings = Filter(function(f){f@ringDir==1},result@polygons[[1]]@Polygons)
    outerBounds = sp::SpatialPolygons(list(sp::Polygons(outerRings,ID=1)))
    
    # e) convert back to sf data
    outerBounds <- st_as_sf(outerBounds)
    outerBounds <- st_set_crs(outerBounds, st_crs(mysf))
    st_crs(outerBounds) <- st_crs(mysf)
    
    # retrieve code_region info and reorder columns
    outerBounds <- dplyr::mutate(outerBounds, group_column = grp)
    outerBounds <- dplyr::select(outerBounds, group_column, geometry)
    names(outerBounds)[1] <- group_column
    return(outerBounds)
  }
  
  
  # Apply sub-function
  groups_sf <- pbapply::pblapply(X = unique(get(group_column, mysf)), FUN = dissolvefun )
  
  # rbind results
  temp_sf <- do.call('rbind', groups_sf)
  return(temp_sf)
}