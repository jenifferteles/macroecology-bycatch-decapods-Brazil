######################         MAPS        #####################################
library("readxl")
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggpubr)  # needed for ggarrange

## Load Brazil and state geometries
brazil_sf <- ne_countries(country = "Brazil",
                          scale   = "medium",
                          returnclass = "sf")
states_sf <- ne_states(country = "Brazil",
                       returnclass = "sf")

## Filter only coastal states by postal code
coastal_states_sf <- states_sf %>%
  filter(postal %in% c("AP","PA","MA","PI","CE","RN","PB","PE",
                       "AL","SE","BA","ES","RJ","SP","PR","SC","RS"))

## Load your data
dados_OK <- read_excel("VariablesDataOK_pix1x1.xlsx") %>%
  as.data.frame() %>%
  na.omit() %>%
  .[, c(1,2,33,38,40)]
colnames(dados_OK) <- c("Longitude", "Latitude", "SR", "PD.SES", "PE.SES")

################################################################################
############# FINAL GGPLOT MAP - SPECIES RICHNESS (SR) #########################
################################################################################
map_SR <- ggplot() +
  # 1) Fill Brazil in light gray, no borders
  geom_sf(data = brazil_sf, fill = "grey94", color = NA) +
  # 2) Species richness tiles
  geom_tile(data = dados_OK, aes(x = Longitude, y = Latitude, fill = SR)) +
  # 3) Coastal state borders in dark gray
  geom_sf(data = coastal_states_sf, fill = NA, color = "lightgrey", size = 0.4) +
  # 4) Coastal state labels in gray
  geom_sf_text(data = coastal_states_sf, aes(label = name),
               color = "grey40", size = 3, check_overlap = TRUE) +
  # 5) North arrow and scale bar
  annotation_north_arrow(location = "tr",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.3) +
  # 6) Color scale and limits
  scale_fill_viridis_c(option = "C", name = NULL,
                       na.value = "transparent", limits = c(0, 80)) +
  # 7) Coordinate system
  coord_sf(
    xlim   = c(min(dados_OK$Longitude) - 1, max(dados_OK$Longitude) + 1),
    ylim   = c(min(dados_OK$Latitude)  - 1, max(dados_OK$Latitude)  + 1),
    expand = TRUE
  ) +
  labs(title = "a) Species Richness (SR)",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid.major     = element_blank(),
    panel.grid.minor     = element_blank(),
    panel.background     = element_rect(fill = "white", colour = NA),
    axis.text.x          = element_text(color = "black", size = 9),
    axis.text.y          = element_text(color = "black", size = 9),
    axis.title.x         = element_blank(),
    axis.title.y         = element_text(color = "black", size = 18, face = "bold"),
    plot.title           = element_text(hjust = 0.5, size = 18, face = "bold")
  )

print(map_SR)

################################################################################
############# FINAL GGPLOT MAP - PHYLOGENETIC DIVERSITY (PD.SES) ##############
################################################################################
map_PD <- ggplot() +
  geom_sf(data = brazil_sf, fill = "grey94", color = NA) +
  geom_tile(data = dados_OK, aes(x = Longitude, y = Latitude, fill = PD.SES)) +
  geom_sf(data = coastal_states_sf, fill = NA, color = "lightgrey", size = 0.4) +
  geom_sf_text(data = coastal_states_sf, aes(label = name),
               color = "grey40", size = 3, check_overlap = TRUE) +
  annotation_north_arrow(location = "tr",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.3) +
  scale_fill_viridis_c(option = "C", name = NULL, na.value = "transparent",
                       limits = c(-2, 4), oob = scales::squish) +
  coord_sf(
    xlim   = c(min(dados_OK$Longitude) - 1, max(dados_OK$Longitude) + 1),
    ylim   = c(min(dados_OK$Latitude)  - 1, max(dados_OK$Latitude)  + 1),
    expand = TRUE
  ) +
  labs(title = "b) Phylogenetic Diversity (PD.SES)",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    axis.text.x      = element_text(color = "black", size = 9),
    axis.text.y      = element_text(color = "black", size = 9),
    axis.title.x     = element_blank(),
    axis.title.y     = element_blank(),
    plot.title       = element_text(hjust = 0.5, size = 18, face = "bold")
  )

print(map_PD)

################################################################################
############# FINAL GGPLOT MAP - PHYLOGENETIC ENDEMISM (PE.SES) ###############
################################################################################
map_PE <- ggplot() +
  geom_sf(data = brazil_sf, fill = "grey94", color = NA) +
  geom_tile(data = dados_OK, aes(x = Longitude, y = Latitude, fill = PE.SES)) +
  geom_sf(data = coastal_states_sf, fill = NA, color = "lightgrey", size = 0.4) +
  geom_sf_text(data = coastal_states_sf, aes(label = name),
               color = "grey40", size = 3, check_overlap = TRUE) +
  annotation_north_arrow(location = "tr",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.3) +
  scale_fill_viridis_c(option = "C", name = NULL, na.value = "transparent",
                       limits = c(-2, 4), oob = scales::squish) +
  coord_sf(
    xlim   = c(min(dados_OK$Longitude) - 1, max(dados_OK$Longitude) + 1),
    ylim   = c(min(dados_OK$Latitude)  - 1, max(dados_OK$Latitude)  + 1),
    expand = TRUE
  ) +
  labs(title = "c) Phylogenetic Endemism (PE.SES)",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    axis.text.x      = element_text(color = "black", size = 9),
    axis.text.y      = element_text(color = "black", size = 9),
    axis.title.x     = element_blank(),
    axis.title.y     = element_blank(),
    plot.title       = element_text(hjust = 0.5, size = 18, face = "bold")
  )

print(map_PE)

################################################################################
#############################     Combine & Save      ##########################
################################################################################
combined_map <- ggarrange(map_SR, map_PD, map_PE,
                          ncol = 3, nrow = 1,
                          font.label = list(size = 18, face = "bold", color = "black"))
print(combined_map)

# Save as PDF
ggsave("combined_maps.pdf", plot = combined_map,
       width = 18, height = 8, units = "in", dpi = 900)

# Save as TIFF with white background
ggsave("combined_maps.tiff", plot = combined_map,
       width = 18, height = 8, units = "in", dpi = 900, device = "tiff", bg = "white")
