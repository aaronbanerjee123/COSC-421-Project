# Network Analysis & Metrics Calculation
# This script calculates centrality metrics and answers research questions

library(tidyverse)
library(igraph)
library(sf)
library(broom)

# Load networks and data
multi_graph <- readRDS("data/processed/multi_layer_graph.rds")
transit_graph <- readRDS("data/processed/transit_graph.rds")
all_nodes <- readRDS("data/processed/all_nodes.rds")
census_data <- readRDS("data/processed/census_data.rds")
stops <- readRDS("data/processed/transit_stops.rds")
cycling <- readRDS("data/processed/cycling_infrastructure.rds")

# Convert census data to centroids with area_id
neighbourhoods <- census_data %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  select(area_id, population, median_income, geometry)

# ============================================
# FIX EDGE WEIGHTS
# ============================================
print("Checking and fixing edge weights...")

edge_weights <- E(multi_graph)$weight
print(paste("Min weight:", min(edge_weights, na.rm = TRUE)))
print(paste("Max weight:", max(edge_weights, na.rm = TRUE)))
print(paste("NA weights:", sum(is.na(edge_weights))))
print(paste("Zero or negative weights:", sum(edge_weights <= 0, na.rm = TRUE)))

E(multi_graph)$weight[is.na(E(multi_graph)$weight)] <- 1
E(multi_graph)$weight[E(multi_graph)$weight <= 0] <- 1

print(paste("After fix - Min weight:", min(E(multi_graph)$weight)))
print(paste("After fix - Max weight:", max(E(multi_graph)$weight)))

if (any(is.na(E(transit_graph)$weight) | E(transit_graph)$weight <= 0)) {
  E(transit_graph)$weight[is.na(E(transit_graph)$weight)] <- 1
  E(transit_graph)$weight[E(transit_graph)$weight <= 0] <- 1
}

# ============================================
# CALCULATE CENTRALITY METRICS
# ============================================
print("Calculating centrality metrics...")

print("  - Calculating degree...")
V(multi_graph)$degree <- degree(multi_graph)

print("  - Calculating betweenness (this may take a while)...")
V(multi_graph)$betweenness <- betweenness(multi_graph, normalized = TRUE)

print("  - Calculating closeness...")
V(multi_graph)$closeness <- closeness(multi_graph, normalized = TRUE)

print("  - Calculating eigenvector centrality...")
V(multi_graph)$eigenvector <- eigen_centrality(multi_graph)$vector

print("  - Calculating pagerank...")
V(multi_graph)$pagerank <- page_rank(multi_graph)$vector

# For transit network only
print("Calculating transit network metrics...")
V(transit_graph)$degree <- degree(transit_graph)
V(transit_graph)$betweenness <- betweenness(transit_graph, normalized = TRUE)
V(transit_graph)$closeness <- closeness(transit_graph, normalized = TRUE)

# Create results dataframe
centrality_results <- data.frame(
  node_id = V(multi_graph)$name,
  type = V(multi_graph)$type,
  degree = V(multi_graph)$degree,
  betweenness = V(multi_graph)$betweenness,
  closeness = V(multi_graph)$closeness,
  eigenvector = V(multi_graph)$eigenvector,
  pagerank = V(multi_graph)$pagerank,
  x = V(multi_graph)$x,
  y = V(multi_graph)$y
)

# ============================================
# NETWORK-LEVEL METRICS
# ============================================
print("Calculating network-level metrics...")

network_metrics <- list(
  nodes = vcount(multi_graph),
  edges = ecount(multi_graph),
  density = edge_density(multi_graph),
  diameter = diameter(multi_graph, weights = NA),
  avg_path_length = mean_distance(multi_graph, weights = NULL),
  clustering_coef = transitivity(multi_graph),
  components = components(multi_graph)$no,
  largest_component_size = max(components(multi_graph)$csize),
  assortativity = assortativity_degree(multi_graph)
)

# Calculate network efficiency (handle Inf values properly)
print("Calculating network efficiency...")
distances <- distances(multi_graph, weights = E(multi_graph)$weight)
distances[is.infinite(distances)] <- NA
finite_distances <- distances[!is.na(distances) & distances > 0]
network_metrics$efficiency <- if(length(finite_distances) > 0) {
  mean(1/finite_distances, na.rm = TRUE)
} else {
  NA
}

print("Network metrics calculated:")
print(network_metrics)

# ============================================
# RQ1: ACCESSIBILITY INEQUALITY ANALYSIS
# ============================================
print("Analyzing RQ1: Accessibility Inequality...")

neighbourhood_centrality <- centrality_results %>%
  filter(type == "neighbourhood") %>%
  mutate(node_number = as.numeric(str_extract(node_id, "\\d+")))

neighbourhood_analysis <- neighbourhoods %>%
  st_drop_geometry() %>%
  mutate(node_number = row_number()) %>%
  left_join(neighbourhood_centrality, by = "node_number") %>%
  filter(!is.na(betweenness), !is.na(median_income))

# Check sample size
print(paste("Neighborhoods in analysis:", nrow(neighbourhood_analysis)))

if (nrow(neighbourhood_analysis) > 3) {
  # Correlation analysis
  cor_betweenness <- cor.test(neighbourhood_analysis$median_income, 
                              neighbourhood_analysis$betweenness)
  cor_closeness <- cor.test(neighbourhood_analysis$median_income, 
                            neighbourhood_analysis$closeness)
  
  # Linear regression models
  model_betweenness <- lm(betweenness ~ median_income + population, 
                          data = neighbourhood_analysis)
  model_closeness <- lm(closeness ~ median_income + population, 
                        data = neighbourhood_analysis)
  
  rq1_results <- list(
    cor_betweenness = cor_betweenness,
    cor_closeness = cor_closeness,
    model_betweenness = tidy(model_betweenness),
    model_closeness = tidy(model_closeness),
    r2_betweenness = summary(model_betweenness)$r.squared,
    r2_closeness = summary(model_closeness)$r.squared,
    n_neighborhoods = nrow(neighbourhood_analysis)
  )
  
  print("RQ1 Results:")
  print(paste("Betweenness-Income Correlation:", round(cor_betweenness$estimate, 3),
              "p =", round(cor_betweenness$p.value, 4)))
  print(paste("Closeness-Income Correlation:", round(cor_closeness$estimate, 3),
              "p =", round(cor_closeness$p.value, 4)))
} else {
  print("WARNING: Not enough neighborhoods for statistical analysis")
  rq1_results <- list(error = "Insufficient data")
}

# ============================================
# RQ2: CRITICAL INFRASTRUCTURE IDENTIFICATION
# ============================================
print("Analyzing RQ2: Critical Infrastructure...")

transit_centrality <- centrality_results %>%
  filter(type == "stop") %>%
  arrange(desc(betweenness)) %>%
  head(20)

articulation_point_ids <- articulation_points(multi_graph)
articulation_node_names <- V(multi_graph)$name[articulation_point_ids + 1]

bridge_ids <- bridges(multi_graph)

critical_nodes <- transit_centrality %>%
  mutate(
    is_articulation = node_id %in% articulation_node_names,
    rank = row_number()
  )

print("Top 10 Critical Transit Stops:")
print(critical_nodes %>% select(node_id, betweenness, is_articulation) %>% head(10))
print(paste("Total articulation points:", sum(critical_nodes$is_articulation)))
print(paste("Total bridges:", length(bridge_ids)))

# ============================================
# RQ3: TRANSIT DESERT QUANTIFICATION
# ============================================
print("Analyzing RQ3: Transit Deserts...")

bbox <- st_bbox(stops)
grid <- st_make_grid(
  st_as_sfc(bbox),
  cellsize = 0.005,  # ~500m cells
  square = TRUE
) %>%
  st_sf() %>%
  mutate(grid_id = row_number())

print("Calculating distances to transit stops...")
grid_centers <- st_centroid(grid)
dist_to_transit <- st_distance(grid_centers, stops)
min_dist_to_transit <- apply(dist_to_transit, 1, min)

if (!is.null(cycling$osm_lines) && nrow(cycling$osm_lines) > 0) {
  print("Calculating distances to cycling infrastructure...")
  dist_to_cycling <- st_distance(grid_centers, cycling$osm_lines)
  min_dist_to_cycling <- apply(dist_to_cycling, 1, min)
} else {
  print("No cycling infrastructure found, using Inf")
  min_dist_to_cycling <- rep(Inf, nrow(grid))
}

grid$transit_distance <- as.numeric(min_dist_to_transit)
grid$cycling_distance <- as.numeric(min_dist_to_cycling)
grid$is_transit_desert <- (grid$transit_distance > 800) & (grid$cycling_distance > 400)

transit_desert_stats <- list(
  total_cells = nrow(grid),
  desert_cells = sum(grid$is_transit_desert),
  desert_percentage = mean(grid$is_transit_desert) * 100,
  avg_distance_to_transit = mean(grid$transit_distance),
  median_distance_to_transit = median(grid$transit_distance),
  max_distance_to_transit = max(grid$transit_distance),
  # Add population-weighted metrics if available
  accessible_area = sum(!grid$is_transit_desert) * 0.005 * 0.005  # km²
)

print("Transit Desert Statistics:")
print(paste("Transit deserts:", round(transit_desert_stats$desert_percentage, 1), "% of area"))
print(paste("Average distance to transit:", round(transit_desert_stats$avg_distance_to_transit, 0), "m"))
print(paste("Maximum distance to transit:", round(transit_desert_stats$max_distance_to_transit, 0), "m"))

# ============================================
# RQ4: NETWORK RESILIENCE ANALYSIS
# ============================================
print("Analyzing RQ4: Network Resilience...")

calculate_efficiency <- function(g) {
  if (vcount(g) < 2) return(0)
  distances <- distances(g, weights = E(g)$weight)
  distances[is.infinite(distances)] <- NA
  finite_dists <- distances[!is.na(distances) & distances > 0]
  if(length(finite_dists) > 0) {
    return(mean(1/finite_dists, na.rm = TRUE))
  } else {
    return(0)
  }
}

print("Calculating baseline metrics...")
baseline_efficiency <- calculate_efficiency(multi_graph)
baseline_components <- components(multi_graph)$no
baseline_giant_size <- max(components(multi_graph)$csize)

top_nodes <- centrality_results %>%
  arrange(desc(betweenness)) %>%
  head(10) %>%
  pull(node_id)

print("Simulating node removals...")
resilience_results <- data.frame()

for (i in 1:length(top_nodes)) {
  nodes_to_remove <- top_nodes[1:i]
  g_temp <- delete_vertices(multi_graph, nodes_to_remove)
  
  efficiency <- calculate_efficiency(g_temp)
  components_count <- components(g_temp)$no
  giant_size <- if(vcount(g_temp) > 0) max(components(g_temp)$csize) else 0
  
  resilience_results <- rbind(resilience_results, data.frame(
    nodes_removed = i,
    efficiency = efficiency,
    efficiency_ratio = if(baseline_efficiency > 0) efficiency / baseline_efficiency else NA,
    components = components_count,
    giant_component_size = giant_size,
    giant_size_ratio = giant_size / baseline_giant_size
  ))
  
  print(paste("Removed", i, "nodes..."))
}

print("Resilience Analysis Results:")
print(resilience_results)

# Calculate resilience score (how well network maintains connectivity)
resilience_score <- mean(resilience_results$giant_size_ratio) * 100
print(paste("Overall resilience score:", round(resilience_score, 1), "%"))

# ============================================
# SAVE ALL RESULTS
# ============================================
print("Saving results...")
saveRDS(centrality_results, "data/processed/centrality_results.rds")
saveRDS(network_metrics, "data/processed/network_metrics.rds")
saveRDS(rq1_results, "data/processed/rq1_accessibility_results.rds")
saveRDS(critical_nodes, "data/processed/rq2_critical_nodes.rds")
saveRDS(grid, "data/processed/rq3_transit_deserts.rds")
saveRDS(transit_desert_stats, "data/processed/rq3_desert_stats.rds")
saveRDS(resilience_results, "data/processed/rq4_resilience_results.rds")
saveRDS(neighbourhood_analysis, "data/processed/neighbourhood_analysis.rds")

print("All analyses complete! Results saved.")