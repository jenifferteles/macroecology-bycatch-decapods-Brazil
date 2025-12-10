# HOT/COLDSPOT MAPS FOR SR, PD.SES & PE.SES WITH MARINE MPAs

# 1) Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(lwgeom)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggpubr)

# 2) Load and combine WDPA shapefiles (marine polygons only)
pasta_mpas <- "MPAs_Brazil"
arquivos_wdpa <- file.path(pasta_mpas, c(
  "WDPA_WDOECM_Aug2025_Public_BRA_shp-polygons_0.shp",
  "WDPA_WDOECM_Aug2025_Public_BRA_shp-polygons_1.shp",
  "WDPA_WDOECM_Aug2025_Public_BRA_shp-polygons_2.shp"
))

mpas_poligonos <- do.call(rbind, lapply(arquivos_wdpa, st_read)) %>%
  st_transform(4326) %>%
  st_make_valid()

# 3) Filter for marine areas only
mpas_mar <- mpas_poligonos %>%
  filter(MARINE %in% c(1, 2)) %>%
  st_make_valid() %>%
  filter(st_is_valid(.))

# 4) Load Brazil outline and coastal state boundaries
brazil_sf         <- ne_countries(country = "Brazil", scale = "medium", returnclass = "sf")
states_sf         <- ne_states(country = "Brazil", returnclass = "sf")
coastal_states_sf <- states_sf %>%
  filter(postal %in% c("AP","PA","MA","PI","CE","RN","PB","PE",
                       "AL","SE","BA","ES","RJ","SP","PR","SC","RS"))

# 5) Load grid data and rename columns
data <- read_excel("VariablesDataOK_pix1x1.xlsx") %>%
  as.data.frame() %>%
  na.omit() %>%
  select(1, 2, 33, 38, 40)
names(data) <- c("Longitude", "Latitude", "SR", "PD.SES", "PE.SES")

# 6) Compute 10th and 90th percentiles for each metric
quantiles_10_90 <- function(x) quantile(x, c(0.10, 0.90), na.rm = TRUE)
SR_q <- quantiles_10_90(data$SR)
PD_q <- quantiles_10_90(data$PD.SES)
PE_q <- quantiles_10_90(data$PE.SES)

# 7) Categorize each cell as Hotspot, Coldspot, or Other
data_cat <- data %>%
  mutate(
    SR_cat = case_when(
      SR     >= SR_q[2] ~ "Hotspot",
      SR     <= SR_q[1] ~ "Coldspot",
      TRUE              ~ "Other"
    ),
    PD_cat = case_when(
      PD.SES >= PD_q[2] ~ "Hotspot",
      PD.SES <= PD_q[1] ~ "Coldspot",
      TRUE              ~ "Other"
    ),
    PE_cat = case_when(
      PE.SES >= PE_q[2] ~ "Hotspot",
      PE.SES <= PE_q[1] ~ "Coldspot",
      TRUE              ~ "Other"
    )
  ) %>%
  mutate(across(ends_with("_cat"), factor, levels = c("Hotspot","Coldspot","Other")))

# 8) Define map extent
xlims <- range(data_cat$Longitude) + c(-0.5, +0.5)
ylims <- range(data_cat$Latitude)  + c(-0.5, +0.5)

# 9) Plotting function
plot_metric <- function(cat_col, title_text, show_legend = TRUE) {
  ggplot() +
    geom_sf(data = brazil_sf, fill = "grey94", color = NA) +
    geom_tile(data = data_cat,
              aes_string("Longitude", "Latitude", fill = cat_col)) +
    geom_sf(data = mpas_mar, fill = "black", color = NA) +  # MPAs diretas, sem cortar
    geom_sf(data = coastal_states_sf, fill = NA, color = "lightgrey", size = 0.4) +
    geom_sf_text(data = coastal_states_sf,
                 aes(label = postal),
                 color = "grey40", size = 3, check_overlap = TRUE) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "br", width_hint = 0.3,
                     bar_cols = c("black", "white")) +
    scale_fill_manual(
      values = c(Hotspot = "tomato",
                 Coldspot = "deepskyblue3",
                 Other = "transparent"),
      breaks = c("Hotspot", "Coldspot"),
      labels = c("Hotspot (≥90%)", "Coldspot (≤10%)"),
      na.translate = FALSE,
      name = if (show_legend) "" else NULL
    ) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    labs(title = title_text, x = NULL, y = "Latitude") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.title.y = element_text(color = "black", size = 16, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = if (show_legend) "right" else "none"
    )
}

# 10) Generate individual maps
map_SR <- plot_metric("SR_cat", "a) Species Richness (SR)", show_legend = FALSE)
map_PD <- plot_metric("PD_cat", "b) Phylogenetic Diversity (PD.SES)", show_legend = FALSE)
map_PE <- plot_metric("PE_cat", "c) Phylogenetic Endemism (PE.SES)", show_legend = TRUE)

# 11) Combine and display maps
combined <- ggarrange(map_SR, map_PD, map_PE,
                      ncol = 3, align = "hv",
                      widths = rep(1, 3),
                      font.label = list(size = 18, face = "bold"))
print(combined)

# 12) Save maps
ggsave("hotcold_with_MPA_WDPA_full.tiff", plot = combined,
       width = 18, height = 8, units = "in", dpi = 900, device = "tiff", bg = "white")
ggsave("hotcold_with_MPA_WDPA_full.pdf", plot = combined,
       width = 18, height = 8, units = "in", dpi = 900)
