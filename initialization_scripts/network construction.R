library(tidyverse)
library(igraph)
library(sf)
library(tidytransit)

# Load processed data - USE FILTERED STOPS
stops <- readRDS("data/processed/transit_stops.rds")
destinations <- readRDS("data/processed/major_destinations.rds")
census_data <- readRDS("data/processed/census_data.rds")
cycling <- readRDS("data/processed/cycling_infrastructure.rds")
walking <- readRDS("data/processed/walking_infrastructure.rds")

# Load GTFS for trip information
gtfs <- read_gtfs("data/raw/kelowna_gtfs.zip")

kelowna_stop_ids <- stops$stop_id

print(paste("Total stops in GTFS:", nrow(gtfs$stops)))
print(paste("Kelowna-only stops (filtered):", length(kelowna_stop_ids)))

# Filter stop_times to only include Kelowna stops
gtfs$stop_times <- gtfs$stop_times %>%
  filter(stop_id %in% kelowna_stop_ids)

print(paste("Stop times after filtering:", nrow(gtfs$stop_times)))

# Convert census polygons to centroids for network nodes
neighbourhoods <- census_data %>%
  st_centroid() %>%
  select(area_id, population, median_income, geometry)

# ============================================
# PART 1: CREATE TRANSIT NETWORK LAYER (KELOWNA ONLY)
# ============================================
print("Building transit network layer (Kelowna only)...")

# Extract stop times - already filtered above
stop_times <- gtfs$stop_times %>%
  arrange(trip_id, stop_sequence)

print(paste("Processing", nrow(stop_times), "stop times for Kelowna"))

# Create edges between consecutive stops
transit_edges <- stop_times %>%
  group_by(trip_id) %>%
  mutate(
    from_stop = stop_id,
    to_stop = lead(stop_id),
    travel_time = as.numeric(difftime(lead(arrival_time), departure_time, units = "secs"))
  ) %>%
  filter(!is.na(to_stop), !is.na(travel_time)) %>%
  ungroup() %>%
  # CRITICAL: Only keep edges where BOTH stops are in Kelowna
  filter(from_stop %in% kelowna_stop_ids & to_stop %in% kelowna_stop_ids) %>%
  select(from_stop, to_stop, travel_time, trip_id)

print(paste("Transit edges created:", nrow(transit_edges)))

# Calculate average travel time between stops
transit_edges_avg <- transit_edges %>%
  group_by(from_stop, to_stop) %>%
  summarise(
    avg_travel_time = mean(travel_time, na.rm = TRUE),
    frequency = n(),
    .groups = 'drop'
  ) %>%
  filter(!is.na(avg_travel_time), avg_travel_time > 0)

print(paste("Unique transit connections:", nrow(transit_edges_avg)))

# Verify all stops in edges exist in stops dataframe
missing_from <- transit_edges_avg$from_stop[!transit_edges_avg$from_stop %in% kelowna_stop_ids]
missing_to <- transit_edges_avg$to_stop[!transit_edges_avg$to_stop %in% kelowna_stop_ids]

if (length(missing_from) > 0 | length(missing_to) > 0) {
  print("WARNING: Some stops in edges not found in stops dataframe")
  print(paste("Missing from_stop:", length(missing_from)))
  print(paste("Missing to_stop:", length(missing_to)))
  
  # Remove edges with missing stops
  transit_edges_avg <- transit_edges_avg %>%
    filter(from_stop %in% kelowna_stop_ids & to_stop %in% kelowna_stop_ids)
}

# Create transit graph
transit_graph <- graph_from_data_frame(
  transit_edges_avg,
  directed = TRUE,
  vertices = stops %>% st_drop_geometry() %>% select(stop_id, stop_name)
)

# Add edge weights (travel time in seconds)
E(transit_graph)$weight <- transit_edges_avg$avg_travel_time

print(paste("Transit network:", vcount(transit_graph), "nodes,", ecount(transit_graph), "edges"))

# ============================================
# PART 2: CREATE ACTIVE TRANSPORTATION LAYER
# ============================================
print("Building active transportation network...")

# Function to calculate walking time (assuming 5 km/h)
calculate_walk_time <- function(distance_m) {
  return(distance_m / (5000/3600))  # seconds
}

# Function to calculate cycling time (assuming 15 km/h)
calculate_bike_time <- function(distance_m) {
  return(distance_m / (15000/3600))  # seconds
}

# Create a proximity network for walking (max 800m)
all_nodes <- rbind(
  stops %>% select(geometry) %>% mutate(node_id = paste0("S_", row_number()), type = "stop"),
  destinations %>% select(geometry) %>% mutate(node_id = paste0("D_", row_number()), type = "destination"),
  neighbourhoods %>% select(geometry) %>% mutate(node_id = paste0("N_", row_number()), type = "neighbourhood")
)

# Calculate distance matrix
print("Calculating distance matrix (this may take a moment)...")
dist_matrix <- st_distance(all_nodes)
units(dist_matrix) <- NULL  # Remove units for easier manipulation
dist_matrix <- as.numeric(dist_matrix)
dist_matrix <- matrix(dist_matrix, nrow = nrow(all_nodes))

# Create walking edges (max 800m)
print("Creating walking edges...")
walking_edges <- expand.grid(from = 1:nrow(all_nodes), to = 1:nrow(all_nodes)) %>%
  filter(from != to) %>%
  mutate(distance = dist_matrix[cbind(from, to)]) %>%
  filter(distance <= 800) %>%
  mutate(
    from_id = all_nodes$node_id[from],
    to_id = all_nodes$node_id[to],
    walk_time = calculate_walk_time(distance),
    edge_type = "walking"
  ) %>%
  select(from_id, to_id, distance, walk_time, edge_type)

# Check for cycling infrastructure nearby
print("Creating cycling edges...")
if (!is.null(cycling$osm_lines) && nrow(cycling$osm_lines) > 0) {
  cycling_buffer <- st_buffer(cycling$osm_lines, 100)  # 100m buffer
  nodes_near_cycling <- st_intersects(all_nodes, cycling_buffer, sparse = FALSE)
  nodes_near_cycling <- apply(nodes_near_cycling, 1, any)
  
  cycling_edges <- expand.grid(from = 1:nrow(all_nodes), to = 1:nrow(all_nodes)) %>%
    filter(from != to) %>%
    filter(nodes_near_cycling[from] | nodes_near_cycling[to]) %>%
    mutate(distance = dist_matrix[cbind(from, to)]) %>%
    filter(distance <= 2000) %>%
    mutate(
      from_id = all_nodes$node_id[from],
      to_id = all_nodes$node_id[to],
      bike_time = calculate_bike_time(distance),
      edge_type = "cycling"
    ) %>%
    select(from_id, to_id, distance, bike_time, edge_type)
} else {
  cycling_edges <- data.frame()
}

# ============================================
# PART 3: CREATE MULTI-LAYER NETWORK
# ============================================
print("Creating multi-layer network...")

# Combine all nodes
all_nodes_df <- data.frame(
  node_id = all_nodes$node_id,
  type = all_nodes$type,
  x = st_coordinates(all_nodes)[,1],
  y = st_coordinates(all_nodes)[,2]
)

# Combine all edges
all_edges <- data.frame()

# Add transit edges - need to match stop_id to node_id
transit_edges_for_multi <- transit_edges_avg %>%
  mutate(
    from_id = paste0("S_", match(from_stop, stops$stop_id)),
    to_id = paste0("S_", match(to_stop, stops$stop_id)),
    weight = avg_travel_time,
    edge_type = "transit"
  ) %>%
  filter(!is.na(from_id) & !is.na(to_id)) %>%
  select(from_id, to_id, weight, edge_type)

all_edges <- rbind(all_edges, transit_edges_for_multi)

# Add walking edges
if (nrow(walking_edges) > 0) {
  walking_edges_for_multi <- walking_edges %>%
    select(from_id, to_id, weight = walk_time, edge_type)
  all_edges <- rbind(all_edges, walking_edges_for_multi)
}

# Add cycling edges
if (nrow(cycling_edges) > 0) {
  cycling_edges_for_multi <- cycling_edges %>%
    select(from_id, to_id, weight = bike_time, edge_type)
  all_edges <- rbind(all_edges, cycling_edges_for_multi)
}

# Create the multi-layer graph
multi_graph <- graph_from_data_frame(
  all_edges,
  directed = FALSE,
  vertices = all_nodes_df
)

# Save networks
saveRDS(transit_graph, "data/processed/transit_graph.rds")
saveRDS(multi_graph, "data/processed/multi_layer_graph.rds")
saveRDS(all_nodes_df, "data/processed/all_nodes.rds")
saveRDS(all_edges, "data/processed/all_edges.rds")

print("========================================")
print("Network construction complete (Kelowna only)!")
print("========================================")
print(paste("Multi-layer network:", vcount(multi_graph), "nodes,", ecount(multi_graph), "edges"))
print("\nEdge type distribution:")
print(table(all_edges$edge_type))
print("\nNode type distribution:")
print(table(all_nodes_df$type))
print("========================================")
