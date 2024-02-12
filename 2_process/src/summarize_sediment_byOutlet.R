
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

summarize_outlet_sed_freq <- function(sed_data, mission_nm, sed_date, outlet_poly, outlet_id) {
  
  # Drop any names attributes leftover from `parse_tif_fn()`
  mission_nm <- unname(mission_nm)
  sed_date <- unname(sed_date)
  
  # First crop to just this outlet's polygon
  sed_data_outlet <- crop(sed_data, outlet_poly)
  
  # Now count up the total number of cells & the number with different classes present
  n_cells_out <- terra::freq(sed_data_outlet, value = 0)$count
  n_cells_cloud <- terra::freq(sed_data_outlet, value = 1)$count
  n_cells_water <- terra::freq(sed_data_outlet, value = 2)$count
  n_cells_sed <- terra::freq(sed_data_outlet, value = 3)$count
  n_cells_total <- ncell(sed_data_outlet)
  
  # Summarize the results in a table to be combined with other outlets, 
  # dates, and missions.
  tibble(
    mission = mission_nm,
    date = sed_date,
    river_outlet = outlet_id,
    outsideAOI_outlet_pct = round(n_cells_out/n_cells_total*100, digits=3),
    cloud_outlet_pct = round(n_cells_cloud/n_cells_total*100, digits=3),
    openwater_outlet_pct = round(n_cells_water/n_cells_total*100, digits=3),
    sediment_outlet_pct = round(n_cells_sed/n_cells_total*100, digits=3)
  )
  
}

# Per file, summarize the info
summarize_sed_pct_byOutlet <- function(in_file, outlet_sf_list) {
  
  file_info <- parse_tif_fn(in_file)
  sed_data <- load_terraqs(in_file)
  
  outlet_sf_list %>% 
    map(~summarize_outlet_sed_freq(
      sed_data, 
      file_info['mission'], 
      file_info['date'], 
      outlet_poly = .x, 
      outlet_id = .x$river)) %>% 
    bind_rows()
  
}
