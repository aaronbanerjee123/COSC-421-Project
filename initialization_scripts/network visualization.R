
# ============================================
# FIX DESTINATIONS - REMOVE WEST KELOWNA
# ============================================

# Reload and filter destinations
destinations_all <- readRDS("data/processed/major_destinations.rds")

# Filter out West Kelowna destinations (longitude > -119.58)
destinations <- destinations_all %>%
  mutate(lon_check = st_coordinates(.)[,1]) %>%
  filter(lon_check > -119.58) %>%
  select(-lon_check)

print(paste("Original destinations:", nrow(destinations_all)))
print(paste("Kelowna only destinations:", nrow(destinations)))

# Show which destinations were removed
removed <- destinations_all %>%
  mutate(lon_check = st_coordinates(.)[,1]) %>%
  filter(lon_check <= -119.58) %>%
  st_drop_geometry()

if(nrow(removed) > 0) {
  print("Removed destinations (West Kelowna):")
  print(removed$name)
}

# Save filtered destinations
saveRDS(destinations, "data/processed/major_destinations.rds")

# ============================================
# VERIFY STOPS ARE FILTERED
# ============================================

stops <- readRDS("data/processed/transit_stops.rds")

# Check if any stops are in West Kelowna
stops_check <- stops %>%
  mutate(lon = st_coordinates(.)[,1]) %>%
  filter(lon <= -119.58)

if(nrow(stops_check) > 0) {
  print(paste("WARNING:", nrow(stops_check), "stops still in West Kelowna!"))
  print("Re-filtering stops...")
  
  stops <- stops %>%
    mutate(lon = st_coordinates(.)[,1]) %>%
    filter(lon > -119.58) %>%
    select(-lon)
  
  saveRDS(stops, "data/processed/transit_stops.rds")
  print(paste("Stops now:", nrow(stops)))
} else {
  print("✓ All stops are in Kelowna proper")
}

print("Data cleaned! Now re-run the visualization script.")
library(tidyverse)
library(igraph)
library(sf)
library(tmap)
library(ggplot2)
library(patchwork)
library(viridis)
library(leaflet)
library(RColorBrewer)

# Set theme
theme_set(theme_minimal(base_size = 12))

# Load all data
centrality_results <- readRDS("data/processed/centrality_results.rds")
network_metrics <- readRDS("data/processed/network_metrics.rds")
rq1_results <- readRDS("data/processed/rq1_accessibility_results.rds")
critical_nodes <- readRDS("data/processed/rq2_critical_nodes.rds")
transit_deserts <- readRDS("data/processed/rq3_transit_deserts.rds")
transit_desert_stats <- readRDS("data/processed/rq3_desert_stats.rds")
resilience_results <- readRDS("data/processed/rq4_resilience_results.rds")
census_data <- readRDS("data/processed/census_data.rds")
stops <- readRDS("data/processed/transit_stops.rds")
destinations <- readRDS("data/processed/major_destinations.rds")
all_edges <- readRDS("data/processed/all_edges.rds")

# Convert census to centroids
neighbourhoods <- census_data %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  select(area_id, population, median_income, geometry)

# Convert centrality results to sf
nodes_sf <- centrality_results %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

# ============================================
# FIGURE 1: NETWORK ON KELOWNA MAP WITH BASEMAP
# ============================================
print("Creating enhanced network map with basemap...")

tmap_mode("plot")

# Create comprehensive network map
tm_network <- tm_shape(census_data) +
  tm_polygons(col = "median_income",
              palette = "YlOrRd",
              title = "Median Income",
              alpha = 0.6,
              border.col = "white",
              border.alpha = 0.3) +
  tm_shape(transit_deserts %>% filter(is_transit_desert)) +
  tm_borders(col = "red", lwd = 2, alpha = 0.3) +
  tm_shape(nodes_sf %>% filter(type == "neighbourhood")) +
  tm_dots(col = "green", size = 0.1, alpha = 0.5) +
  tm_shape(nodes_sf %>% filter(type == "destination")) +
  tm_dots(col = "blue", size = 0.3, alpha = 0.8) +
  tm_shape(stops) +
  tm_dots(col = "red", size = 0.15, alpha = 0.7) +
  tm_layout(
    title = "Kelowna Multi-Modal Transportation Network",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = TRUE
  ) +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(tm_network, "outputs/figures/network_on_map.png", 
          width = 14, height = 10, dpi = 300)

# ============================================
# FIGURE 2: BETWEENNESS CENTRALITY HEATMAP
# ============================================
print("Creating betweenness centrality heatmap...")

tm_betweenness <- tm_shape(census_data) +
  tm_polygons(alpha = 0.2, border.col = "gray80") +
  tm_shape(nodes_sf %>% filter(betweenness > 0)) +
  tm_dots(
    col = "betweenness",
    size = "betweenness",
    palette = "YlOrRd",
    title.col = "Betweenness\nCentrality",
    title.size = "Betweenness",
    alpha = 0.8,
    scale = 2
  ) +
  tm_layout(
    title = "Network Betweenness Centrality",
    legend.outside = TRUE,
    frame = TRUE
  ) +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(tm_betweenness, "outputs/figures/betweenness_heatmap.png",
          width = 12, height = 10, dpi = 300)

# ============================================
# FIGURE 3: CRITICAL NODES ON MAP
# ============================================
print("Creating critical nodes map...")

# Get critical node locations
critical_stops <- stops %>%
  mutate(node_id = paste0("S_", row_number())) %>%
  filter(node_id %in% critical_nodes$node_id[1:10])

tm_critical <- tm_shape(census_data) +
  tm_polygons(alpha = 0.2, border.col = "gray80") +
  tm_shape(stops) +
  tm_dots(col = "gray", size = 0.1, alpha = 0.3) +
  tm_shape(critical_stops) +
  tm_dots(
    col = "red",
    size = 1,
    alpha = 0.9
  ) +
  tm_text("stop_name", size = 0.5, ymod = 0.5) +
  tm_layout(
    title = "Top 10 Critical Transit Stops",
    legend.outside = TRUE,
    frame = TRUE
  )

tmap_save(tm_critical, "outputs/figures/critical_nodes_map.png",
          width = 12, height = 10, dpi = 300)

# ============================================
# FIGURE 4: INCOME VS ACCESSIBILITY MAP
# ============================================
print("Creating income accessibility map...")

# Add centrality to census data
census_with_centrality <- census_data %>%
  mutate(node_number = row_number()) %>%
  left_join(
    centrality_results %>% 
      filter(type == "neighbourhood") %>%
      mutate(node_number = as.numeric(str_extract(node_id, "\\d+"))),
    by = "node_number"
  )

tm_income_access <- tm_shape(census_with_centrality) +
  tm_polygons(
    col = "closeness",
    palette = "-RdYlGn",
    title = "Closeness\nCentrality",
    alpha = 0.8,
    border.col = "white"
  ) +
  tm_shape(stops) +
  tm_dots(col = "black", size = 0.05, alpha = 0.3) +
  tm_layout(
    title = "Transit Accessibility by Area",
    legend.outside = TRUE,
    frame = TRUE
  ) +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(tm_income_access, "outputs/figures/accessibility_map.png",
          width = 12, height = 10, dpi = 300)

# ============================================
# FIGURE 5: ENHANCED TRANSIT DESERT MAP
# ============================================
print("Creating enhanced transit desert map...")

tm_desert_enhanced <- tm_shape(transit_deserts) +
  tm_fill(
    "is_transit_desert",
    palette = c("FALSE" = "#2ECC71", "TRUE" = "#E74C3C"),
    labels = c("Well Served", "Transit Desert"),
    title = "Transit Access",
    alpha = 0.7
  ) +
  tm_borders(alpha = 0.2) +
  tm_shape(stops) +
  tm_dots(col = "black", size = 0.1, alpha = 0.6) +
  tm_shape(destinations) +
  tm_dots(
    col = "blue",
    size = 0.5,
    alpha = 0.8
  ) +
  tm_layout(
    title = "Transit Desert Analysis",
    title.size = 1.2,
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = TRUE,
    main.title.position = "center"
  ) +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(tm_desert_enhanced, "outputs/figures/transit_deserts_enhanced.png",
          width = 14, height = 10, dpi = 300)

# ============================================
# INTERACTIVE LEAFLET MAP
# ============================================
print("Creating interactive Leaflet map...")

# Prepare data for leaflet
stops_leaflet <- stops %>% st_transform(4326)
destinations_leaflet <- destinations %>% st_transform(4326)
census_leaflet <- census_data %>% st_transform(4326)

# Create color palette for income
pal_income <- colorNumeric(
  palette = "YlOrRd",
  domain = census_leaflet$median_income
)

# Create interactive map
leaflet_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Add census areas
  addPolygons(
    data = census_leaflet,
    fillColor = ~pal_income(median_income),
    fillOpacity = 0.6,
    color = "white",
    weight = 1,
    popup = ~paste0(
      "<b>Area:</b> ", area_id, "<br>",
      "<b>Median Income:</b> $", format(median_income, big.mark = ","), "<br>",
      "<b>Population:</b> ", population
    ),
    group = "Income"
  ) %>%
  
  # Add transit stops
  addCircleMarkers(
    data = stops_leaflet,
    radius = 4,
    color = "red",
    fillOpacity = 0.7,
    popup = ~paste0("<b>", stop_name, "</b><br>Stop ID: ", stop_id),
    group = "Transit Stops"
  ) %>%
  
  # Add destinations
  addCircleMarkers(
    data = destinations_leaflet,
    radius = 8,
    color = "blue",
    fillOpacity = 0.9,
    popup = ~paste0("<b>", name, "</b><br>Type: ", type),
    group = "Key Destinations"
  ) %>%
  
  # Add legend
  addLegend(
    "bottomright",
    pal = pal_income,
    values = census_leaflet$median_income,
    title = "Median Income",
    opacity = 0.7
  ) %>%
  
  # Add layer control
  addLayersControl(
    overlayGroups = c("Income", "Transit Stops", "Key Destinations"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Save interactive map
htmlwidgets::saveWidget(
  leaflet_map,
  "outputs/figures/interactive_kelowna_map.html",
  selfcontained = TRUE
)

# ============================================
# BETTER STATISTICAL PLOTS
# ============================================
print("Creating enhanced statistical visualizations...")

# RQ1: Better scatter plots
neighbourhood_for_plot <- neighbourhoods %>%
  st_drop_geometry() %>%
  mutate(node_number = row_number()) %>%
  left_join(
    centrality_results %>% 
      filter(type == "neighbourhood") %>%
      mutate(node_number = as.numeric(str_extract(node_id, "\\d+"))),
    by = "node_number"
  ) %>%
  filter(!is.na(betweenness))

p_income_scatter <- ggplot(neighbourhood_for_plot, 
                           aes(x = median_income/1000, y = closeness)) +
  geom_point(aes(size = population, color = closeness), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  scale_color_viridis_c(option = "plasma") +
  scale_size_continuous(range = c(2, 10)) +
  labs(
    title = "Transit Accessibility vs. Income in Kelowna",
    subtitle = paste0("Correlation: ", round(rq1_results$cor_closeness$estimate, 3),
                      " (p < 0.001)"),
    x = "Median Household Income ($1000s)",
    y = "Closeness Centrality (Accessibility)",
    size = "Population",
    color = "Accessibility"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

ggsave("outputs/figures/income_accessibility_scatter.png", 
       p_income_scatter, width = 12, height = 8, dpi = 300)

print("All enhanced visualizations created!")
print("Check outputs/figures/ for:")
print("  - network_on_map.png (network overlaid on Kelowna)")
print("  - betweenness_heatmap.png (centrality heatmap)")
print("  - critical_nodes_map.png (top 10 critical stops)")
print("  - accessibility_map.png (accessibility by area)")
print("  - transit_deserts_enhanced.png (improved desert map)")
print("  - interactive_kelowna_map.html (INTERACTIVE LEAFLET MAP)")
print("  - income_accessibility_scatter.png (better scatter plot)")