# macroecology-bycatch-decapods-Brazil
Data, phylogenies and R workflows for analyzing species richness, phylogenetic diversity and conservation gaps of bycatch decapods in the Brazilian EEZ.

# Biogeography and Conservation of Bycatch Decapods  
**Data and R code accompanying the paper "Decapod Biodiversity Hotspots and Environmental Drivers: A Macroecological Approach About Bycatch Species in Brazil" published on Journal of Biogeography (https://doi.org/10.1111/jbi.70076) 

---

## 🧬 **Data Description**

### **CCDB_OBIS_GeoMar_data.xlsx**  
Raw occurrences of decapod bycatch from:
- CCDB  
- OBIS  
- GeoMar  
- Peer-reviewed studies  

Includes species name, coordinates, sampling year, and data source.

---

### **clados_info.xlsx**  
Metadata for phylogenetic clade grouping and tree visualization.

---

### **Tree_genoma_OK.treefile**  
Maximum likelihood mitogenomic phylogeny generated using IQ-TREE 2.

---

### **tree_pruned.treefile**  
Pruned tree used for calculating PD, PE, ED, WE, and related metrics.

---

### **VariablesDataOK_pix1x1.xlsx**  
Environmental predictors and diversity indices aggregated at **1° × 1° resolution**, including:

- SR, PD, PE, ED, WE  
- bottom temperature  
- salinity  
- primary productivity  
- light availability  
- current velocity  
- trawling effort  

---

### **eez.shp**  
Brazilian Exclusive Economic Zone shapefile for spatial analyses.

---

## **R Workflow Description**

### **1) Editing_mitogenomic_tree.R**  
Prepares and prunes the phylogeny for downstream biodiversity analyses.

### **2) Get_merge_predictors_and_calculate_PD__PE__SR.R**  
Computes SR, PD, PE, ED, WE using:
- `picante`  
- `phylobase`  
- `phyloraster`  

### **3) Indices_correlations.R**  
Correlation matrices among biodiversity indices.

### **4) Analisys_Models_LMER__GAM_and_SPAMM.R**  
Environmental models using:
- Linear Mixed-Effects (LMER)  
- Generalized Additive Models (GAM)  
- Spatial models (`spaMM`)  

### **5) RDA_models.R**  
Redundancy Analysis (RDA): multivariate relationships between environment and diversity.

### **6) Spatial_random_forest.R**  
Spatially explicit Random Forest models (`spatialRF`) identifying key predictors.

### **7) Maps.R**  
Geospatial visualization of biodiversity indices across the EEZ.

### **8) MPAs_hotspot_and_coldspot.R**  
Hotspot/coldspot identification and overlap with MPAs → **conservation gap analysis**.

---

## Software Requirements

### Core Software  
- **R ≥ 4.4**  
- **IQ-TREE 2** (phylogenetic inference)  
- **Geneious Prime** (sequence alignment)  

### Key R Packages  
tidyverse, dplyr, readr, CoordinateCleaner
sf, raster, terra, rnaturalearth, rnaturalearthdata
picante, ape, phylobase, phyloraster, LetsR, SESraster
sdmpredictors, robis
vegan, adespatial, spaMM
randomForest, spatialRF
ggplot2, ggpubr, gridExtra

🌐 Data Sources Used
OBIS — https://obis.org/
Global Fishing Watch — https://globalfishingwatch.org/
GeoMar — https://www.geomar.de/
Bio-ORACLE — https://bio-oracle.org/
GenBank — https://www.ncbi.nlm.nih.gov/genbank/
Protected Planet — https://www.protectedplanet.net/
Canva (visual editing) — https://www.canva.com

📑 Citation
If using this dataset or workflow, please cite:

Teles, J. N. & Mantelatto, F. L. (2025). Decapod Biodiversity Hotspots and Environmental Drivers: A Macroecological Approach About Bycatch Species in Brazil. Journal of Biogeography.
https://doi.org/10.1111/jbi.70076

For data and code, please cite this repository:
Teles, J. N. (2025). macroecology-bycatch-decapods-Brazil: Data and R workflows for biodiversity and phylogenetic analyses of bycatch decapods in the Brazilian EEZ. GitHub Repository.
