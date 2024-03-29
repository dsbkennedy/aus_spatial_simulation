
pacman::p_load('httr', 'sf', 'furrr', 'tidyverse')

#JVB CODE - THANKS!
Get_ABS_Geography <- function(ASGS, Year, state = NULL){

  
  print(glue::glue("Downloading {Year} {ASGS} boundaries"))
  
  url <- httr::parse_url(glue::glue(
    "https://geo.abs.gov.au/arcgis/rest/services/ASGS{Year}/",
    "{ASGS}/MapServer/0/query"))
  
  if(Year == 2011){
    where_query <- glue::glue("STATE_CODE='{state}'")
  } else {
    where_query <- glue::glue("STATE_CODE_{Year}='{state}'")
  }
  
  url$query <- list(
    where = where_query,
    outFields = '*',
    featureEncoding = "esriDefault",
    returnGeometry = "true",
    f = "geojson")
  
  request <- httr::build_url(url)
  
  out <- sf::st_read(httr::GET(request),
                     quiet = TRUE)
  
  return(out)
  
}

vic_sa2_2016 <- Get_ABS_Geography('SA2',2016,2)
vic_sa1_2016 <- Get_ABS_Geography('SA1',2016,2)
vic_ra_2016 <- Get_ABS_Geography('RA',2016,2)

# Download SSC from https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_ssc_2016_aust_midmif.zip&1270.0.55.003&Data%20Cubes&36A020CE97B00F53CA25802C00147D7C&0&July%202016&13.09.2016&Previous

ssc <- sf::st_read('/Users/dk_work/Desktop/aus_spatial_simulation/data/1270055003_ssc_2016_aust_shape/SSC_2016_AUST.shp')
vic_ssc <- ssc %>% filter(STE_NAME16=='Victoria')

# # Download RA from https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055005_ra_2016_aust_shape.zip&1270.0.55.005&Data%20Cubes&ACAA23F3B41FA7DFCA258251000C8004&0&July%202016&16.03.2018&Latest
# 
# ra <- sf::st_read('/Users/dk_work/Desktop/aus_spatial_simulation/data/1270055005_ra_2016_aust_shape/RA_2016_AUST.shp')
# vic_ra <- ra %>% filter(STE_NAME16=='Victoria')

vic_border <- vic_sa2_2016 %>% dplyr::summarise()

# Number of random points
num_points <- 999

# Generate random points within the polygon
random_points <- sf::st_sample(vic_border, size = num_points, type = "random") %>% 
  st_as_sf(crs=st_crs(vic_border)) %>% 
  dplyr::mutate(id=dplyr::row_number()) 

ggplot() +
  geom_sf(data=random_points) +
  geom_sf(data=vic_sa2_2016,fill=NA)

# Join points to SA2
points_to_sa2 <- random_points %>% sf::st_join(vic_sa2_2016)

# Count points by SA2
points_per_sa2 <- points_to_sa2 %>% count(sa2_name_2016) %>% st_drop_geometry() %>% 
  dplyr::left_join(vic_sa2_2016,.,by='sa2_name_2016') 

ggplot() +
  geom_sf(data=points_per_sa2,aes(fill=n)) 

