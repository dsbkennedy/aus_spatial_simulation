
pacman::p_load('httr', 'sf', 'furrr', 'tidyverse')

Get_ABS_Geography <- function(ASGS, Year, state = 1){

  
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
  
  return(request)
  
}

nsw_sa2_2016 <- Get_ABS_Geography('SA2',2016,1)
nsw_sa1_2016 <- Get_ABS_Geography('SA1',2016,1)

nsw_remoteness_2016 <- Get_ABS_Geography('RA',2016,1)

nsw_border <- nsw_sa2_2016 %>% dplyr::summarise()

# Number of random points
num_points <- 99

# Generate random points within the polygon
random_points <- sf::st_sample(nsw_border, size = num_points, type = "random") %>% 
  st_as_sf(crs=st_crs(nsw_border)) %>% 
  dplyr::mutate(id=dplyr::row_number()) 

ggplot() +
  geom_sf(data=random_points) +
  geom_sf(data=nsw_sa2_2016,fill=NA)

# Join points to SA2
points_to_sa2 <- random_points %>% sf::st_join(nsw_sa2_2016)

# Count points by SA2
points_per_sa2 <- points_to_sa2 %>% count(sa2_name_2016) %>% st_drop_geometry() %>% 
  dplyr::left_join(nsw_sa2_2016,.,by='sa2_name_2016') 

ggplot() +
  geom_sf(data=points_per_sa2,aes(fill=n)) 

