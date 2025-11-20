# Kelowna Transportation Network Analysis

Network analysis of Kelowna's multi-modal transportation system examining accessibility, equity, and resilience.

## Research Questions

1. **RQ1**: Does median household income correlate with transportation accessibility?
2. **RQ2**: Which transit stops are critical infrastructure?
3. **RQ3**: Where are Kelowna's "transit deserts" (underserved areas)?
4. **RQ4**: How resilient is the network to critical node failures?

## Project Structure
```
├── initialization_scripts/
│   ├── 01_data_acquisition.R       # Downloads and filters data
│   ├── 02_network_construction.R   # Builds network graphs
│   ├── 03_network_analysis.R       # Calculates metrics, answers RQs
│   └── 04_network_visualization.R  # Creates maps and charts
│
└── output_data/
    ├── data/
    │   ├── raw/                    # Original GTFS data
    │   └── processed/              # 20 .rds files (networks + analysis results)
    └── outputs/
        └── figures/                # 4+ visualization images
```

## Quick Start

### Install Required Packages
```r
install.packages(c("tidyverse", "igraph", "sf", "tidytransit", 
                   "osmdata", "cancensus", "ggplot2", "viridis", "broom"))
```

### Option 1: View Pre-Processed Results (Fastest)

All analysis results are already in `output_data/`. Load and explore:
```r
# Load centrality results
centrality <- readRDS("output_data/data/processed/centrality_results.rds")

# View top 10 most critical stops
library(dplyr)
centrality %>%
  filter(type == "stop") %>%
  arrange(desc(betweenness)) %>%
  head(10)

# Load RQ results
rq1 <- readRDS("output_data/data/processed/rq1_accessibility_results.rds")
rq2 <- readRDS("output_data/data/processed/rq2_critical_nodes.rds")
rq3_stats <- readRDS("output_data/data/processed/rq3_desert_stats.rds")
rq4 <- readRDS("output_data/data/processed/rq4_resilience_results.rds")

# View maps
# Open files in output_data/outputs/figures/
```

### Option 2: Reproduce Full Analysis

Run scripts in sequence (takes ~5 minutes total):
```r
# 1. Download data (2 min)
source("initialization_scripts/01_data_acquisition.R")

# 2. Build networks (1 min)
source("initialization_scripts/02_network_construction.R")

# 3. Run analysis (1 min)
source("initialization_scripts/03_network_analysis.R")

# 4. Create visualizations (1 min)
source("initialization_scripts/04_network_visualization.R")
```

## Key Files

### Network Data
- `multi_layer_graph.rds` - Complete network (765 nodes, 52,501 edges)
- `transit_graph.rds` - Transit-only network (583 stops, 578 connections)

### Analysis Results
- `centrality_results.rds` - Importance scores for all nodes
- `rq1_accessibility_results.rds` - Income vs accessibility correlation
- `rq2_critical_nodes.rds` - Top 20 critical stops
- `rq3_transit_deserts.rds` - Grid showing underserved areas
- `rq4_resilience_results.rds` - Network failure simulation

### Visualizations
- `verification_map_kelowna_only.png` - Data verification map
- Network centrality maps
- Income vs accessibility plots
- Transit desert heat maps

## Data Sources

- **Census**: Statistics Canada 2021 (167 neighborhoods)
- **Transit**: BC Transit GTFS (583 stops, 28 routes - Kelowna only)
- **Infrastructure**: OpenStreetMap (cycling/walking paths)
- **Destinations**: 15 major activity centers

## Key Findings

- Network: 765 nodes, 52,501 connections across transit/walking/cycling
- 583 transit stops analyzed (313 non-Kelowna stops removed)
- Critical infrastructure identified through betweenness centrality
- Transit deserts mapped using 800m accessibility threshold
- Network resilience tested through node removal simulation

## Requirements

- R >= 4.5.0
- Internet connection (for data downloads in scripts)
- ~500 MB disk space



## License

MIT
