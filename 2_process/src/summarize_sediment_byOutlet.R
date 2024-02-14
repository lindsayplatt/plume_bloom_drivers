
# Convert from raster with sed presence
# to timeseries of % sediment.
create_bbox_sf <- function(bbox_tbl, crs_out) {
  bbox_sf <- tibble(lat = c(bbox_tbl$ymin, bbox_tbl$ymax), 
         lon = c(bbox_tbl$xmin, bbox_tbl$xmax)) %>%
    st_as_sf(coords=c('lon','lat'), crs=4326) %>% 
    st_transform(crs_out) %>% 
    st_bbox() %>% 
    st_as_sfc() %>% 
    st_as_sf() %>% 
    mutate(river = bbox_tbl$river)
  st_geometry(bbox_sf) <- 'geometry'
  # attr(bbox_sf, 'river') <- bbox_tbl$river
  return(bbox_sf)
}

# Get the date and mission name from the filepath 
parse_tif_fn <- function(fn) {
  # Drop the extension and path
  fn_bare <- gsub('.qs', '', basename(fn))
  sed_date <- str_extract(fn_bare, '[0-9]{4}-[0-9]{2}-[0-9]{2}$')
  mission_nm <- str_split(fn_bare, '[0-9]{4}-[0-9]{2}-[0-9]{2}') %>% 
    pluck(1, 2) %>% 
    gsub('^_|_$', '', x=.)
  return(c(date = sed_date, mission = mission_nm))
}

summarize_poly_raster_freq <- function(raster_data, mission_nm, raster_date, poly_sf = NULL, poly_id = NULL) {
  
  # Drop any names attributes leftover from `parse_tif_fn()`
  mission_nm <- unname(mission_nm)
  raster_date <- unname(raster_date)
  
  # First crop to just a specific polygon if specified
  if(!is.null(poly_sf)) {
    raster_data <- crop(raster_data, poly_sf)
  }
  
  # Now count up the total number of cells & the number with different classes present
  n_cells_out <- terra::freq(raster_data, value = 0)$count
  n_cells_cloud <- terra::freq(raster_data, value = 1)$count
  n_cells_water <- terra::freq(raster_data, value = 2)$count
  n_cells_sed <- terra::freq(raster_data, value = 3)$count
  n_cells_total <- ncell(raster_data)
  
  # Summarize the results in a table to be combined with other outlets, 
  # dates, and missions.
  out_data <- tibble(
    mission = mission_nm,
    date = raster_date,
    outsideAOI_pct = round(n_cells_out/n_cells_total*100, digits=3),
    cloud_pct = round(n_cells_cloud/n_cells_total*100, digits=3),
    openwater_pct = round(n_cells_water/n_cells_total*100, digits=3),
    sediment_pct = round(n_cells_sed/n_cells_total*100, digits=3)
  )
  
  if(!is.null(poly_sf)) {
    out_data <- mutate(out_data, poly_nm = poly_id, .after = date)
  }
  
  return(out_data)
}

# Per file, summarize the raster classes for each outlet in a single table.
summarize_class_pct_byOutlet <- function(in_file, outlet_sf_list) {
  
  file_info <- parse_tif_fn(in_file)
  raster_data <- load_terraqs(in_file)
  
  outlet_sf_list %>% 
    map(~summarize_poly_raster_freq(
      raster_data = raster_data, 
      mission_nm = file_info['mission'], 
      raster_date = file_info['date'], 
      poly_sf = .x, 
      poly_id = .x$river)) %>% 
    bind_rows()
  
}

# Per file, summarize the raster classes for each outlet in a single table.
summarize_class_pct_overall <- function(in_file) {
  
  file_info <- parse_tif_fn(in_file)
  raster_data <- load_terraqs(in_file)
  
  summarize_poly_raster_freq(
    raster_data = raster_data, 
    mission_nm = file_info['mission'], 
    raster_date = file_info['date']) 
  
}
