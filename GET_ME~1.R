library(phyloraster)
library(terra)
library(ape)
library(phylobase)
library(SESraster)

####### Reading obis ocurrence data and X.dincao of CCDB
library("readxl")
dataOK <- as.data.frame(read_excel ("CCDB_OBIS_GeoMar_data.xlsx"))
dataOK 
summary(dataOK)

###### Cleaning data ###
library(CoordinateCleaner)
flags <- clean_coordinates(x = dataOK,lon = "x",
                           lat = "y",
                           species = "sp",tests = c("centroids",                                  "duplicates","zeros"))
summary(flags)

#Exclude problematic records
data_cleaned <- dataOK[flags$.summary,]
data_cleaned
summary(data_cleaned)
#Separate columns for letsR package
species_name <- data_cleaned [, 'sp']
coordinates <- data_cleaned [, c('x', 'y')]

############# Plot ocurrence and data #####
library(letsR)
all_species <- lets.presab.points(coordinates, species_name, xmn = -53.35, xmx = -25.95, ymn = -33.85, ymx = 5.2 , remove.cells= T,res = 1,crs= "+proj=longlat +datum=WGS84")
all_species
plot(all_species)
###### Crop EEZ ###### 
library(rgdal)
library(sf)
eez_bra <- readOGR("C:/Users/Jenif/Desktop/Artigo Trawling impact in Diversity/Analises estatisticas R/Novas an?lises paper/eezBrazil/eez.shp")
Bra_species <- lets.pamcrop(all_species, eez_bra, remove.sp = TRUE)
Bra_species 

#values(res$Richness_Raster)[values(res$Richness_Raster) == 0] <- NA
plot(Bra_species)
Bra_species$Richness_Raster
data <- Bra_species$Presence_and_Absence_Matrix[]
data <- as.data.frame(data)
colnames(data)[1] <- "Longitude"
colnames(data)[2] <- "Latitude"

library(ape)
tree <- read.tree("Tree_genoma_OK.treefile")
tree
plot.phylo(tree, type = "fan", edge.color = "gray", cex = 0.1)

# pruned tree
library(picante)
tree_pruned <- prune.sample(samp = data, phylo = tree)
plot.phylo(tree_pruned, type = "fan", edge.color = "gray", cex = 0.5)
summary(tree_pruned)

raster <- df2rast(x = data, CRS = "+proj=longlat +datum=WGS84")
class(raster)
plot(raster)

names(raster) == tree_pruned$tip.label

dataprep <- phylo.pres(x = raster, tree = tree_pruned)

names(dataprep$x) == tree_pruned$tip.label

######### Species richness
ras <- dataprep$x
sr <- rast.sr(x = ras)
sr$SR
plot(sr, main = "Species richness")
SR_raster <- raster(sr$SR)
SR_data <- lets.addvar(Bra_species, SR_raster, fun = mean)
SR_data <- as.data.frame(SR_data)
SR_data$SR_mean

####################### Phylogenetic diversity #################################
pdr <- rast.pd(x = dataprep$x, edge.path = dataprep$edge.path, 
               branch.length = dataprep$branch.length)
plot(pdr$PD, main = "Phylogenetic diversity")
PD_raster <- raster(pdr$PD)
PD_data <- lets.addvar(Bra_species, PD_raster, fun = mean)
PD_data <- as.data.frame(PD_data)
#standardized effect size (SES)
PD_SES <- rast.pd.ses(x = dataprep$x, edge.path = dataprep$edge.path, 
                      branch.length = dataprep$branch.length, aleats = 999, 
                      random = "spat")
plot(PD_SES)
PD.SES_raster <- raster(PD_SES$SES.PD)
PD.SES_data <- lets.addvar(Bra_species, PD.SES_raster, fun = mean)
PD.SES_data <- as.data.frame(PD.SES_data)
PD.SES_data

###################### Phylogenetic Endemism  ##################################
per <- rast.pe(x = dataprep$x, dataprep$tree)
per
plot(per$PE, main = "Phylogenetic Endemism")
PE_raster <- raster(per$PE)
PE_data <- lets.addvar(Bra_species, PE_raster, fun = mean)
PE_data <-as.data.frame(PE_data)
#standardized effect size (SES)
PE_SES <- rast.pe.ses(x = dataprep$x, dataprep$tree, aleats = 999, 
                      random = "spat")
plot(PE_SES)
PE.SES_raster <- raster(PE_SES$SES.PE)
PE.SES_data <- lets.addvar(Bra_species, PE.SES_raster , fun = mean)
PE.SES_data <- as.data.frame(PE.SES_data)

####################### Weigthed Endemism ######################################
wer <- rast.we(x = ras)
wer
wer$WE
plot(wer$WE, main ="Weigthed Endemism")
WE_raster <- raster(wer$WE)
WE_data <- lets.addvar(Bra_species, WE_raster, fun = mean)
WE_data <- as.data.frame(WE_data)
#standardized effect size (SES)
WE_SES <- rast.we.ses(ras,  aleats = 999)
plot(WE_SES)
WE.SES_raster <- raster(WE_SES$SES.WE)
WE.SES_data <- lets.addvar(Bra_species, WE.SES_raster, fun = mean)
WE.SES_data <- as.data.frame(WE.SES_data)

#################### Evolutionary Distinctiveness ##############################
ed <- rast.ed(dataprep$x, dataprep$tree)
ed$ED
plot(ed$ED, main ="Evolutionary Distinctiveness")
ED_raster <- raster(ed$ED)
ED_data <- lets.addvar(Bra_species, ED_raster, fun = mean)
ED_data <- as.data.frame(ED_data)
#standardized effect size (SES)
ED_SES <- rast.ed.ses(x = dataprep$x, dataprep$tree, aleats = 999, 
                      random = "spat")
plot(ED_SES)
ED.SES_raster <- raster(ED_SES$SES.ED)
ED.SES_data <- lets.addvar(Bra_species, ED.SES_raster, fun = mean)
ED.SES_data <- as.data.frame(ED.SES_data)

################################################################################
######################### Environmental variables###############################
################################################################################
library(sdmpredictors)
# Inspect the available datasets and layers
datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)
datasets
layers <- list_layers(c("Bio-ORACLE"))
layers

#Quantity of raster available 
list_layers(c("Bio-ORACLE"))$layer_code
### Rodar antes para n?o limitar o tempo de encontrar os dados
options(timeout = max(300, getOption("timeout"))) 

#Environmental variables of the bottom water
bathy <- load_layers(c("BO22_ph","BO22_chlomean_bdmax"
                       ,"BO22_chlorange_bdmax","BO22_chlomean_ss","BO22_curvelmean_bdmax","BO22_dissoxmean_bdmax","BO22_dissoxrange_bdmax","BO22_dissoxltmax_bdmax","BO22_nitratemean_bdmax","BO22_phosphatemean_bdmax","BO22_salinitymean_bdmax","BO22_salinityrange_bdmax","BO22_salinityltmax_bdmax","BO22_tempmean_bdmax","BO22_temprange_bdmax","BO_bathymean","BO22_ironmean_bdmax","BO22_ppmean_bdmax","BO22_silicatemean_bdmax","BO22_tempmean_ss","BO22_salinitymean_ss","BO22_lightbotmean_bdmax","BO22_carbonphytomean_bdmax","BO22_carbonphytoltmax_bdmax","BO22_carbonphytorange_bdmax","BO22_calcite","BO22_carbonphytomean_ss","BO22_ppmean_ss","BO22_pprange_bdmax","BO22_templtmax_bdmax"))
bathy

#Cuts the raster according to the richness map
library(sf)
corta_ext <- extent(Bra_species$Richness_Raster)
bathy_ext <- crop(bathy, corta_ext)
#plot(bathy_ext)

#Checking biological and climate data projections
# projections of both files
Bra_species$Richness_Raster
bathy_ext$BO22_ph
plot(bathy_ext$BO22_carbonphytomean_bdmax)

# Put the projections with the same name
projection(Bra_species$Richness_Raster) <- projection(bathy_ext)

#Extracting climate media per pixel
BraECO_VarAmb <- lets.addvar(Bra_species, bathy_ext, fun = mean)
BraECO_VarAmb <- as.data.frame(BraECO_VarAmb)

#############################################################################
################ Global Fishing Watch data (2018-2019-2020-2021-2022) #######
#############################################################################
library("dplyr")                                                 
library("plyr")                                                  
library("readr")

# Merge csv
effort_df <- list.files(path = "C:/Users/Jenif/Desktop/Artigo Trawling impact in Diversity/Analises estatisticas R/Novas an?lises paper/Data GFW in csv",     
                        pattern = "*.csv", full.names = TRUE) %>%  
  lapply(read_csv) %>%                                            
  bind_rows                                                       

effort_df 

# Re-aggregate the data to 1 degrees
Fishing_data <- effort_df %>% 
  group_by(Lon,Lat) %>% 
  dplyr::summarize(fishing_hours = sum(`Apparent Fishing Hours`, na.rm = T),
                   log_fishing_hours = log10(sum(`Apparent Fishing Hours`, na.rm = T))) %>%   ungroup() %>% 
  mutate(log_fishing_hours = ifelse(log_fishing_hours <= 1, 1, log_fishing_hours),
         log_fishing_hours = ifelse(log_fishing_hours >= 5, 5, log_fishing_hours))
colnames(Fishing_data)[1:2] <- c("x","y")

# Extracting coordinates from Bra_species$Presence_and_Absence_Matrix
Coord <- Bra_species$Presence_and_Absence_Matrix[, c("Longitude(x)", "Latitude(y)")]
# Renaming the columns to 'x' and 'y'
colnames(Coord) <- c("x", "y")
# Converting Coord to a data frame
Coord <- as.data.frame(Coord)

## 1- 12 Krigging fishing data to match the spatial resolution of the 
library (gstat)
Fishing_impact_kri <- krige(fishing_hours~1,location= ~ x + y, data=data.frame(Fishing_data), newdata=data.frame(Coord),maxdist=3)
Fishing_impact_kri_log <- krige(log_fishing_hours~1,location= ~ x + y, data=data.frame(Fishing_data), newdata=data.frame(Coord),maxdist=3)

# Select the coordinates and the variable we want to rasterize
effort_all_raster <- Fishing_impact_kri %>% 
  dplyr::select(x, y, var1.pred)
library(raster)
effort_all_raster <- rasterFromXYZ(effort_all_raster, 
                                crs = "+proj=longlat +datum=WGS84")
plot(effort_all_raster)
# see spatial resolution
res(effort_all_raster)

#extract and merge variables 
Bra_fishing_effort <- lets.addvar(Bra_species, effort_all_raster, fun = mean)
Bra_fishing_effort <- as.data.frame(Bra_fishing_effort)
Bra_fishing_effort$var1.pred_mean
############### DATA FRAME with all VARIABLES #######################
Longitude <- Bra_species$Presence_and_Absence_Matrix[,1]
Latitude <- Bra_species$Presence_and_Absence_Matrix[,2]
######### environmental variables #######

ph  <- BraECO_VarAmb[,107]
chlomean  <- BraECO_VarAmb[,108]
chloRange  <- BraECO_VarAmb[,109]
chloSS <- BraECO_VarAmb[,110]
curvel <- BraECO_VarAmb[,111]
O2 <- BraECO_VarAmb[,112]
O2range <- BraECO_VarAmb[,113]
O2Lmax <- BraECO_VarAmb[,114]
nit <- BraECO_VarAmb[,115]
phosp <- BraECO_VarAmb[,116]
sal <- BraECO_VarAmb[,117]
salrange <- BraECO_VarAmb[,118]
salLmax <- BraECO_VarAmb[,119]
tempmean <- BraECO_VarAmb[,120]
temprange <- BraECO_VarAmb[,121]
bathym <- BraECO_VarAmb[,122]
iron <- BraECO_VarAmb[,123]
pp <- BraECO_VarAmb[,124]
sil <- BraECO_VarAmb[,125]
tempSS <- BraECO_VarAmb[,126]
SalSS <- BraECO_VarAmb[,127]
light <- BraECO_VarAmb[,128]
carbophyto <- BraECO_VarAmb[,129]
carbophytoLmax <- BraECO_VarAmb[,130]
carbophytorange <- BraECO_VarAmb[,131]
calcite <- BraECO_VarAmb[,132]
carbophytoSS <- BraECO_VarAmb[,133]
ppSS <- BraECO_VarAmb[,134]
pprange <- BraECO_VarAmb[,135]
tempLmax <- BraECO_VarAmb[,136]

############ indices variables
SR <- SR_data$SR_mean
PD <- PD_data$PD_mean
WE <- WE_data$WE_mean
PE <- PE_data$PE_mean
ED <- ED_data$ED_mean
PD.SES <- PD.SES_data$SES.PD_mean
PE.SES <- PE.SES_data$SES.PE_mean
WE.SES <- WE.SES_data$SES.WE_mean
ED.SES <- ED.SES_data$SES.ED_mean
fishing_effort <- Bra_fishing_effort$var1.pred_mean

dataCombineOK <- data.frame (Longitude,Latitude, 
                             ph,	chlomean ,	chloRange ,	chloSS,	curvel,	O2,	O2range,	O2Lmax,	nit,	phosp,	sal,	salrange,	salLmax,	tempmean,	temprange,	bathym,	iron,	pp,	sil,	tempSS,	SalSS,	light,	carbophyto,	carbophytoLmax,	carbophytorange,	calcite,	carbophytoSS,	ppSS,	pprange,	tempLmax,	
                             SR, PD, WE, PE, ED,
                             PD.SES, WE.SES, PE.SES, ED.SES, fishing_effort)

dataCombineOK 

# write spreedsheet with final data
library("writexl")
write_xlsx(dataCombineOK,"C:/Users/Jenif/Desktop/Artigo Trawling impact in Diversity/Analises estatisticas R/Novas an?lises paper/VariablesDataOK_pix_1x1.xlsx")
