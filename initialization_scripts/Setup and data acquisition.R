# ============================================
# KELOWNA MULTI-MODAL TRANSPORTATION ANALYSIS
# DATA ACQUISITION SCRIPT 
# ============================================

# Install and load required packages
packages <- c("tidyverse", "igraph", "tidytransit", "osmdata", 
              "sf", "cancensus", "ggplot2", "viridis", "httr")

install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Set working directory to Desktop
desktop_path <- file.path(Sys.getenv("HOME"), "Desktop")
project_dir <- file.path(desktop_path, "kelowna_transport_project")

# Create project directory structure ON DESKTOP
dir.create(project_dir, showWarnings = FALSE)
dir.create(file.path(project_dir, "data"), showWarnings = FALSE)
dir.create(file.path(project_dir, "data/raw"), showWarnings = FALSE)
dir.create(file.path(project_dir, "data/processed"), showWarnings = FALSE)
dir.create(file.path(project_dir, "outputs"), showWarnings = FALSE)
dir.create(file.path(project_dir, "outputs/figures"), showWarnings = FALSE)

# Set working directory
setwd(project_dir)

cat("\n========================================\n")
cat("KELOWNA TRANSPORTATION DATA ACQUISITION\n")
cat("PROPERLY FILTERED TO KELOWNA CITY LIMITS\n")
cat("========================================\n")
cat(paste("Working directory:", getwd(), "\n"))
cat("========================================\n\n")

# ============================================
# STEP 1: GET KELOWNA CITY BOUNDARY FROM CENSUS
# ============================================
cat("1. Downloading Kelowna city boundary from Census...\n")

# Set Census API key
Sys.setenv(CANCENSUS_API_KEY = "CensusMapper_5a436ca7f12dc8d57ba489de27f8a181")
options(cancensus.cache_path = "~/cancensus_cache")
dir.create("~/cancensus_cache", showWarnings = FALSE, recursive = TRUE)

# Get Kelowna CSD boundary (Census Subdivision = City limits)
kelowna_boundary <- get_census(
  dataset = 'CA21', 
  regions = list(CSD = "5935010"),  # Kelowna CSD code
  level = 'CSD',
  geo_format = 'sf',
  quiet = TRUE
) %>%
  st_transform(4326)

cat("   ✓ Kelowna city boundary downloaded\n")

# Get bounding box for OSM queries (slightly expanded for edge cases)
bbox_geom <- st_bbox(kelowna_boundary)
bbox <- c(bbox_geom$xmin - 0.01, bbox_geom$ymin - 0.01, 
          bbox_geom$xmax + 0.01, bbox_geom$ymax + 0.01)

cat(paste("   Bounding box:", 
          round(bbox[1], 4), "to", round(bbox[3], 4), "(lon),",
          round(bbox[2], 4), "to", round(bbox[4], 4), "(lat)\n\n"))

# ============================================
# 2. CENSUS DATA (with proper Kelowna boundary)
# ============================================
cat("2. Downloading census data for Kelowna...\n")

# Download census data for Kelowna Dissemination Areas
census_data <- get_census(
  dataset = 'CA21', 
  regions = list(CSD = "5935010"),  # Kelowna CSD code
  vectors = c(
    "v_CA21_906",   # Median household income
    "v_CA21_1",     # Population
    "v_CA21_6",     # Population density
    "v_CA21_4",     # Total private dwellings
    "v_CA21_7632",  # Transit commuters
    "v_CA21_7656"   # Commute duration
  ),
  level = 'DA',
  geo_format = 'sf',
  quiet = TRUE
)

# Process census data
census_kelowna <- census_data %>%
  st_transform(4326) %>%
  mutate(
    area_id = paste0("N", row_number()),
    median_income = `v_CA21_906: Median total income of household in 2020 ($)`,
    pop_density = `v_CA21_6: Population density per square kilometre`,
    population = `v_CA21_1: Population, 2021`,
    transit_commuters = `v_CA21_7632: Total - Main mode of commuting for the employed labour force aged 15 years and over with a usual place of work or no fixed workplace address`,
    commute_duration = `v_CA21_7656: Total - Commuting duration for the employed labour force aged 15 years and over with a usual place of work or no fixed workplace address`,
    dwellings = `v_CA21_4: Total private dwellings`
  ) %>%
  filter(!is.na(median_income))

saveRDS(census_kelowna, file.path(project_dir, "data/processed/census_data.rds"))
saveRDS(kelowna_boundary, file.path(project_dir, "data/processed/kelowna_boundary.rds"))

cat(paste("   ✓ Census areas:", nrow(census_kelowna), "\n"))
cat(paste("   ✓ Income range: $", 
          format(min(census_kelowna$median_income, na.rm = TRUE), big.mark = ","),
          " - $",
          format(max(census_kelowna$median_income, na.rm = TRUE), big.mark = ","),
          "\n\n"))

# ============================================
# 3. TRANSIT DATA (BC Transit GTFS) 
# ============================================
cat("3. Downloading and filtering BC Transit GTFS data...\n")

gtfs_zip_path <- file.path(project_dir, "data/raw/kelowna_gtfs.zip")

# Download if needed
if (!file.exists(gtfs_zip_path)) {
  tryCatch({
    omd_url <- "https://openmobilitydata.org/p/bc-transit/690/latest/download"
    download.file(omd_url, gtfs_zip_path, mode = "wb")
    cat("   ✓ Downloaded from OpenMobilityData\n")
  }, error = function(e) {
    cat("   ✗ Download failed. Please download manually from:\n")
    cat("     https://openmobilitydata.org/p/bc-transit/690\n")
    stop("Manual download required")
  })
}

# Load GTFS
gtfs <- read_gtfs(gtfs_zip_path)

# Extract ALL stops first
stops_all <- gtfs$stops %>%
  select(stop_id, stop_name, stop_lat, stop_lon) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

cat(paste("   Total stops in GTFS:", nrow(stops_all), "\n"))

# CRITICAL: Filter stops to ONLY those within Kelowna city boundary
stops <- stops_all %>%
  st_filter(kelowna_boundary)

cat(paste("   Stops within Kelowna city limits:", nrow(stops), "\n"))
cat(paste("   Stops removed (West Kelowna, Lake Country, etc.):", 
          nrow(stops_all) - nrow(stops), "\n"))

# Verify no stops west of -119.58 (West Kelowna check)
stops_check <- stops %>%
  mutate(lon = st_coordinates(.)[,1])

west_k_stops <- sum(stops_check$lon < -119.58)
lake_country_stops <- sum(stops_check$lon > -119.30)

cat(paste("   Verification - West Kelowna stops:", west_k_stops, "\n"))
cat(paste("   Verification - Lake Country stops:", lake_country_stops, "\n"))

if(west_k_stops > 0 | lake_country_stops > 0) {
  cat("   WARNING: Still have stops outside Kelowna proper!\n")
  cat("   Applying additional longitude filter...\n")
  
  stops <- stops %>%
    filter(lon >= -119.58 & lon <= -119.30) %>%
    select(-lon)
  
  cat(paste("   Final stops after longitude filter:", nrow(stops), "\n"))
}

# Extract routes
routes <- gtfs$routes %>%
  select(route_id, route_short_name, route_long_name, route_type)

# Save
saveRDS(stops, file.path(project_dir, "data/processed/transit_stops.rds"))
saveRDS(routes, file.path(project_dir, "data/processed/transit_routes.rds"))

cat(paste("   ✓ Transit stops saved:", nrow(stops), "\n"))
cat(paste("   ✓ Transit routes:", nrow(routes), "\n\n"))

# ============================================
# 4. OPENSTREETMAP INFRASTRUCTURE
# ============================================
cat("4. Downloading OpenStreetMap data...\n")
cat("   Note: OSM queries can be slow or timeout. Retrying if needed...\n\n")

# Helper function for OSM queries with retry
osm_query_with_retry <- function(bbox, feature_key, feature_values, description, max_attempts = 3) {
  cat(paste("   -", description, "...\n"))
  
  for(attempt in 1:max_attempts) {
    tryCatch({
      result <- opq(bbox = bbox, timeout = 120) %>%
        add_osm_feature(key = feature_key, value = feature_values) %>%
        osmdata_sf()
      
      cat(paste("     ✓ Success on attempt", attempt, "\n"))
      return(result)
      
    }, error = function(e) {
      if(attempt < max_attempts) {
        cat(paste("     ⚠ Attempt", attempt, "failed. Waiting 5 seconds before retry...\n"))
        Sys.sleep(5)
      } else {
        cat(paste("     ✗ All attempts failed for", description, "\n"))
        cat(paste("     Error:", e$message, "\n"))
        return(NULL)
      }
    })
  }
  return(NULL)
}

# Cycling infrastructure
cycling <- osm_query_with_retry(
  bbox = bbox,
  feature_key = "highway",
  feature_values = c("cycleway", "path"),
  description = "Cycling infrastructure"
)

# If cycling query failed, create empty structure
if(is.null(cycling)) {
  cycling <- list(osm_lines = st_sf(geometry = st_sfc(crs = 4326)))
  cat("     Using empty cycling infrastructure\n")
}

# Filter to Kelowna boundary
if(!is.null(cycling$osm_lines) && nrow(cycling$osm_lines) > 0) {
  cycling$osm_lines <- cycling$osm_lines %>%
    st_transform(4326) %>%
    st_filter(kelowna_boundary)
  cat(paste("     Cycling segments within Kelowna:", nrow(cycling$osm_lines), "\n"))
}

# Walking paths
walking <- osm_query_with_retry(
  bbox = bbox,
  feature_key = "highway",
  feature_values = c("footway", "pedestrian", "path", "steps"),
  description = "Walking paths"
)

if(is.null(walking)) {
  walking <- list(osm_lines = st_sf(geometry = st_sfc(crs = 4326)))
  cat("     Using empty walking infrastructure\n")
}

if(!is.null(walking$osm_lines) && nrow(walking$osm_lines) > 0) {
  walking$osm_lines <- walking$osm_lines %>%
    st_transform(4326) %>%
    st_filter(kelowna_boundary)
  cat(paste("     Walking paths within Kelowna:", nrow(walking$osm_lines), "\n"))
}

# Major roads
roads <- osm_query_with_retry(
  bbox = bbox,
  feature_key = "highway",
  feature_values = c("primary", "secondary", "tertiary"),
  description = "Major roads"
)

if(is.null(roads)) {
  roads <- st_sf(geometry = st_sfc(crs = 4326))
  cat("     Using empty roads layer\n")
} else if(!is.null(roads$osm_lines) && nrow(roads$osm_lines) > 0) {
  roads <- roads$osm_lines %>%
    st_transform(4326) %>%
    st_filter(kelowna_boundary)
  cat(paste("     Road segments within Kelowna:", nrow(roads), "\n"))
} else {
  roads <- st_sf(geometry = st_sfc(crs = 4326))
}

# Save
saveRDS(cycling, file.path(project_dir, "data/processed/cycling_infrastructure.rds"))
saveRDS(walking, file.path(project_dir, "data/processed/walking_infrastructure.rds"))
saveRDS(roads, file.path(project_dir, "data/processed/roads.rds"))

cat("\n   ✓ OSM data saved (some layers may be empty if queries failed)\n\n")

# ============================================
# 5. MAJOR DESTINATIONS (KELOWNA ONLY - within boundary)
# ============================================
cat("5. Defining major destinations...\n")

destinations <- data.frame(
  name = c(
    # Healthcare
    "Kelowna General Hospital",
    
    # Education - Post-Secondary
    "UBCO",
    "Okanagan College",
    
    # Education - High Schools
    "Kelowna Secondary School",
    "Rutland Senior Secondary",
    "Okanagan Mission Secondary",
    
    # Shopping Centers
    "Orchard Park Shopping Centre",
    "Capri Centre Mall",
    
    # Civic/Government
    "Kelowna City Hall",
    "Downtown Kelowna",
    
    # Recreation/Entertainment
    "Prospera Place",
    "Kelowna Waterfront",
    "Mission Recreation Park",
    "H2O Adventure Centre",
    
    # Employment Centers
    "Kelowna International Airport"
  ),
  type = c(
    "Healthcare",
    "Education",
    "Education",
    "Education",
    "Education",
    "Education",
    "Shopping",
    "Shopping",
    "Government",
    "Downtown",
    "Recreation",
    "Recreation",
    "Recreation",
    "Recreation",
    "Transport"
  ),
  lat = c(
    49.8736,   # KGH
    49.9392,   # UBCO
    49.8619,   # Okanagan College
    49.8639,   # Kelowna Secondary
    49.8967,   # Rutland Secondary
    49.8183,   # Okanagan Mission Secondary
    49.8792,   # Orchard Park
    49.8814,   # Capri Centre
    49.8883,   # City Hall
    49.8875,   # Downtown
    49.8925,   # Prospera Place
    49.8922,   # Waterfront
    49.8364,   # Mission Rec
    49.8378,   # H2O
    49.9567    # Airport
  ),
  lon = c(
    -119.4919, # KGH
    -119.3947, # UBCO
    -119.4811, # Okanagan College
    -119.4772, # Kelowna Secondary
    -119.3842, # Rutland Secondary
    -119.4853, # Okanagan Mission Secondary
    -119.4400, # Orchard Park
    -119.4758, # Capri Centre
    -119.4956, # City Hall
    -119.4939, # Downtown
    -119.4953, # Prospera Place
    -119.5008, # Waterfront
    -119.4775, # Mission Rec
    -119.4811, # H2O
    -119.3786  # Airport
  )
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Filter to Kelowna boundary
destinations <- destinations %>%
  st_filter(kelowna_boundary)

saveRDS(destinations, file.path(project_dir, "data/processed/major_destinations.rds"))
cat(paste("   ✓ Major destinations (within city limits):", nrow(destinations), "\n\n"))

# ============================================
# 6. VERIFICATION MAP
# ============================================
cat("6. Creating verification map...\n")

verification_map <- ggplot() +
  # Kelowna boundary
  geom_sf(data = kelowna_boundary, fill = NA, color = "black", size = 2) +
  # Census areas
  geom_sf(data = census_kelowna, aes(fill = median_income), alpha = 0.5) +
  # Transit stops
  geom_sf(data = stops, color = "red", size = 1.5, alpha = 0.8) +
  # Destinations
  geom_sf(data = destinations, color = "blue", size = 3, shape = 17) +
  # Roads (if available)
  {if(nrow(roads) > 0) geom_sf(data = roads, color = "gray40", size = 0.3, alpha = 0.4)} +
  scale_fill_viridis_c(option = "plasma", labels = scales::dollar_format()) +
  labs(
    title = "Kelowna Transportation Network - VERIFIED CITY LIMITS ONLY",
    subtitle = paste0("Transit stops: ", nrow(stops), 
                      " | Destinations: ", nrow(destinations)),
    fill = "Median Income",
    caption = "Black outline = Kelowna city boundary\nNO West Kelowna or Lake Country stops"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 10, color = "red", face = "bold")
  )

ggsave(
  file.path(project_dir, "outputs/figures/verification_map_kelowna_only.png"), 
  verification_map, 
  width = 14, 
  height = 12, 
  dpi = 300
)

cat("   ✓ Verification map saved\n\n")

# ============================================
# 7. SUMMARY REPORT
# ============================================
cat("========================================\n")
cat("DATA ACQUISITION COMPLETE - KELOWNA ONLY\n")
cat("========================================\n\n")

cat("SUMMARY:\n")
cat(sprintf("  Transit stops (Kelowna only):  %d\n", nrow(stops)))
cat(sprintf("  Transit routes:                %d\n", nrow(routes)))
cat(sprintf("  Major destinations:            %d\n", nrow(destinations)))
cat(sprintf("  Census areas (DAs):            %d\n", nrow(census_kelowna)))
cat(sprintf("  Road segments:                 %d\n", nrow(roads)))

cat("\nGEOGRAPHIC VERIFICATION:\n")
stops_coords <- st_coordinates(stops)
cat(sprintf("  Stops longitude range:  %.4f to %.4f\n", 
            min(stops_coords[,1]), max(stops_coords[,1])))
cat(sprintf("  Stops latitude range:   %.4f to %.4f\n", 
            min(stops_coords[,2]), max(stops_coords[,2])))
cat(sprintf("  Expected Kelowna range: -119.58 to -119.30 (lon)\n"))

if(min(stops_coords[,1]) < -119.58) {
  cat("  ⚠ WARNING: Some stops may be in West Kelowna!\n")
}
if(max(stops_coords[,1]) > -119.30) {
  cat("  ⚠ WARNING: Some stops may be in Lake Country!\n")
}
if(min(stops_coords[,1]) >= -119.58 && max(stops_coords[,1]) <= -119.30) {
  cat("  ✓ All stops within expected Kelowna longitude range\n")
}

cat("\nCENSUS DATA:\n")
cat(sprintf("  Income range: $%s - $%s\n",
            format(min(census_kelowna$median_income, na.rm = TRUE), big.mark = ","),
            format(max(census_kelowna$median_income, na.rm = TRUE), big.mark = ",")))
cat(sprintf("  Total population: %s\n", 
            format(sum(census_kelowna$population, na.rm = TRUE), big.mark = ",")))

cat(paste("\nFILES SAVED TO:", file.path(project_dir, "data/processed/"), "\n"))

cat("\nNEXT STEPS:\n")
cat("  1. CHECK verification_map_kelowna_only.png - black line shows city boundary\n")
cat("  2. If map looks correct (no West K or Lake Country), proceed:\n")
cat("  3. Run network_construction.R\n")
cat("  4. Run network_analysis.R\n")
cat("  5. Run visualization.R\n")

cat("\n========================================\n")
cat("ALL DATA PROPERLY FILTERED TO KELOWNA CITY LIMITS\n")
cat("========================================\n")