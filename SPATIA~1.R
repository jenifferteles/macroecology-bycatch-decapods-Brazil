#################### SPATIAL RANDOM FOREST #####################################
library("readxl")
dados_OK <- as.data.frame(read_excel ("VariablesDataOK_pix1x1.xlsx"))
dados_OK <- na.omit(dados_OK)
dados_OK[, 42] <- log10(dados_OK[, 42])
hist(dados_OK[, 42])
shapiro.test(dados_OK[, 42])
head(dados_OK)
colnames(dados_OK)
sum(apply(dados_OK, 2, is.na)) #check if there are NA records 
apply(dados_OK, 2, var) == 0 #columns without zero variance #Columns yielding TRUE should be removed
sum(apply(scale(dados_OK), 2, is.nan))
sum(apply(scale(dados_OK), 2, is.infinite))
dados_OK$SR_log <- log10(dados_OK$SR)

#names of the response variable and the predictors
library(spatialRF)
library(ggplot2)
library(dplyr)

dependent.variable.name <- "PE.SES"
#predictor.variable.names <- c("pp", "sal","curvel","bathym",
#                             "temprange","fishing_effort")
#predictor.variable.names <- c("pp", "sal","curvel","temprange","tempSS","light","fishing_effort")
predictor.variable.names <- c("curvel", "tempmean","sal", "tempSS", "light", "pp","fishing_effort")
#"curvel", "tempmean","sal", "tempSS", "bathym", "pp","fishing_effort"
#predictor.variable.names <- colnames(dados_OK)[c(3:32,44)]


# distance matrix
names(dados_OK)[names(dados_OK) == "Longitude"] <- "x"
names(dados_OK)[names(dados_OK) == "Latitude"] <- "y"
xy <- dados_OK[, c("x", "y")]

library(raster)
dist_data <- pointDistance(dados_OK[, c("x", "y")], lonlat = FALSE)
distance.matrix <- dist_data
head(distance.matrix)

#distance thresholds (same units as distance_matrix)
distance.thresholds <- c(0, 10, 20, 30, 38.2)

#random seed for reproducibility
random.seed <- 1
################################################################################
# PLOT MAP 
world <- rnaturalearth::ne_countries(
  scale = "medium", 
  returnclass = "sf")

ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = world, 
    fill = "white") +
  ggplot2::geom_point(
    data = dados_OK,
    ggplot2::aes(
      x = x,y = y,
      color = PE.SES),
    size = 2.5) +
  ggplot2::scale_color_viridis_c(
    direction = -1, 
    option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "PE.SES") +
  ggplot2::scale_x_continuous(limits = c(-60, -30)) +
  ggplot2::scale_y_continuous(limits = c(-35, 5))  +
  ggplot2::ggtitle("PE.SES") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")

########### Dispersion diagrams of the response variable (y-axis) in relation to each predictor (x-axis)   #######
spatialRF::plot_training_df(
  data = dados_OK,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  ncol = 3,
  point.color = viridis::viridis(100, option = "F"),
  line.color = "gray30")

##### Evaluating the spatial autocorrelation of response variables and predictors at different distance limits. Moran’s I low and p values equal to or greater than 0.05 indicate that there is no spatial autocorrelation for the variable and distance limit provided
spatialRF::plot_training_df_moran(
  data = dados_OK,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  fill.color = viridis::viridis(100,option = "F", direction = -1),point.color = "gray40")

##### Reducing multicollinearity in predictors
###### define a preference order
##### does not need to include the names of all predictors, only those that you would like to keep in analysis
preference.order <- c("pp", "tempmean", "fishing_effort")

predictor.variable.names <- spatialRF::auto_cor(
  x = dados_OK[, predictor.variable.names],
  cor.threshold = 0.7,
  preference.order = preference.order) %>% 
  spatialRF::auto_vif(
    vif.threshold = 7,
    preference.order = preference.order)
###############################################################################
#####   Adjusting a non-spatial random forest model with rf()    #####
###############################################################################
model.non.spatial <- spatialRF::rf(
  data = dados_OK,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random.seed,
  verbose = FALSE)

### Slot residuals: stores the values of the residuals and the results of the spatial normality and autocorrelation tests
spatialRF::plot_residuals_diagnostics(model.non.spatial,verbose = FALSE)

## ###############       The importance of the global variable    #######################
## The importance of the variable represents the increase in the mean error (computed on out-of-bag data) across the trees when a predictor is permuted. Values less than zero would indicate that the variable performs worse than a random one
spatialRF::plot_importance(model.non.spatial, verbose = FALSE)

#measure_importance(), It analyzes the forest to determine the average minimum depth of the tree at which each variable can be found (mean_min_depth), the number of nodes where a variable was selected to make a split (no_of_nodes), the number of times the variable was chosen as the first to start a tree (times_a_root), and the probability that a variable is present in more nodes than would be expected by chance (p_value).
importance.df <- randomForestExplainer::measure_importance( model.non.spatial,
  measures = c("mean_min_depth", "no_of_nodes", "times_a_root", "p_value"))

kableExtra::kbl(importance.df %>% 
    dplyr::arrange(mean_min_depth) %>% 
    dplyr::mutate(p_value = round(p_value, 4)),
  format = "html") %>%
  kableExtra::kable_paper("hover", full_width = F)

### Contribution of Predictors to Model Transferability
###This is done by comparing the performance of the full model with models fitted without each of the predictors. The difference in performance between the full model and a model without a specific predictor represents the contribution of that predictor to the model's transferability.

model.non.spatial <- spatialRF::rf_importance(model = model.non.spatial)

names(model.non.spatial$importance)

model.non.spatial$importance$per.variable %>% 
  ggplot2::ggplot() +
  ggplot2::aes(x = importance.oob,y = importance.cv) + 
  ggplot2::geom_point(size = 3) + 
  ggplot2::theme_bw() +
  ggplot2::xlab("Importance (out-of-bag)") + 
  ggplot2::ylab("Contribution to transferability") + 
  ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "red4")

######################       Repeat         ###################
model.non.spatial.repeat <- spatialRF::rf_repeat(
  model = model.non.spatial, 
  repetitions = 30,
  seed = random.seed,
  verbose = FALSE)

spatialRF::plot_residuals_diagnostics(model.non.spatial.repeat,verbose = FALSE)

spatialRF::print_performance(model.non.spatial.repeat)

plot_importance(model.non.spatial.repeat, verbose = FALSE)

###### Response curves and surfaces ######
spatialRF::plot_response_curves(
  model.non.spatial.repeat,
  quantiles = c(0.1, 0.5, 0.9),
  line.color = viridis::viridis(3, #same number of colors as quantiles
    option = "F", end = 0.9), ncol = 3, show.data = TRUE)

### Setting the quantiles argument to 0.5 and defining show.data as FALSE (the default option) emphasizes the shape of the response curves
spatialRF::plot_response_curves(model.non.spatial.repeat,quantiles = 0.5,ncol = 3)

###########     Model Performance     ###############
#R squared (oob) and RMSE (oob) are the model's R-squared and its root mean squared error when predicting out-of-bag data (the portion of data not used to train individual trees). Among all the values available in the performance slot, these are likely the most honest, as they are the closest to attempting to estimate performance on independent data.
#R squared and pseudo R squared are computed from the observations and predictions, and indicate the extent to which the model's results represent the input data.

spatialRF::print_performance(model.non.spatial.repeat)

#######    Spatial Cross-Validation    #########

####### This overcomes the limitations of the performance scores explained above by providing honest performance based on spatial cross-validation. The function splits the data into several spatially independent training and testing folds. It then fits a model on each training fold, predicts on each testing fold, and calculates performance metrics across the folds.

names(model.non.spatial.repeat$evaluation)

pr <- dados_OK[, c("x", "y")]
pr$group.2 <- pr$group.1 <- "Training"
pr[model.non.spatial.repeat$evaluation$spatial.folds[[1]]$testing, "group.1"] <- "Testing"
pr[model.non.spatial.repeat$evaluation$spatial.folds[[20]]$testing, "group.2"] <- "Testing"

p1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "white") +
  ggplot2::geom_point(data = pr,ggplot2::aes(x = x, y = y, color = group.1), size = 2 ) +
  ggplot2::scale_color_viridis_d(direction = -1, end = 0.5, alpha = 0.8, option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Group") +
  ggplot2::scale_x_continuous(limits = c(-55, -30)) +
  ggplot2::scale_y_continuous(limits = c(-35, 5)) +
  ggplot2::ggtitle("Spatial fold 1") + 
  ggplot2::theme(legend.position = "none",plot.title = ggplot2::element_text(hjust = 0.5)) + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")

p2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "white") +
  ggplot2::geom_point(data = pr,ggplot2::aes( x = x, y = y, color = group.2), size = 2) +
  ggplot2::scale_color_viridis_d( direction = -1, end = 0.5, alpha = 0.8, option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Group") +
  ggplot2::scale_x_continuous(limits = c(-55, -30)) +
  ggplot2::scale_y_continuous(limits = c(-35, 5)) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + 
  ggplot2::ggtitle("Spatial fold 25") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("")

p1 | p2

#Full represents the R-squared of the model trained on the full dataset. Training refers to the R-squared values of the models fitted on the spatial folds (labeled as Training in the maps above), and Testing refers to the R-squared values of the same models on "unseen" data (data not used to train the model, labeled as Testing in the maps above).
spatialRF::plot_evaluation(model.non.spatial.repeat)
spatialRF::print_evaluation(model.non.spatial.repeat)


################################################################################
################################################################################
##########      Fitting a spatial model with rf_spatial()     ##################
################################################################################
################################################################################

spatialRF::plot_moran(model.non.spatial,  verbose = FALSE)

model.spatial <- spatialRF::rf_spatial(
  model = model.non.spatial,
  method = "mem.moran.sequential", #default method
  verbose = FALSE,
  seed = random.seed)

spatialRF::plot_moran(model.spatial, verbose = FALSE)

spatialRF::plot_residuals_diagnostics(model.spatial,verbose = FALSE)

########## Repeat
model.spatial.repeat <- spatialRF::rf_repeat(
  model = model.spatial, 
  repetitions = 30,
  seed = random.seed,
  verbose = FALSE)

spatialRF::plot_residuals_diagnostics(model.spatial.repeat,verbose = FALSE)

spatialRF::plot_importance(model.spatial.repeat, verbose = FALSE)

spatialRF::plot_response_curves(model.spatial.repeat,
  quantiles = c(0.1, 0.5, 0.9),
  line.color = viridis::viridis(3, #same number of colors as quantiles
    option = "F", end = 0.9), ncol = 3, show.data = TRUE)

spatialRF::plot_response_curves(model.spatial.repeat,quantiles = 0.5,ncol = 3)

spatialRF::print_performance(model.spatial.repeat)

########### Cross-validation ##############
names(model.spatial.repeat$evaluation)

pr <- dados_OK[, c("x", "y")]
pr$group.2 <- pr$group.1 <- "Training"
pr[model.spatial.repeat$evaluation$spatial.folds[[1]]$testing, "group.1"] <- "Testing"
pr[model.spatial.repeat$evaluation$spatial.folds[[20]]$testing, "group.2"] <- "Testing"

p1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "white") +
  ggplot2::geom_point(data = pr,ggplot2::aes(x = x, y = y, color = group.1), size = 2 ) +
  ggplot2::scale_color_viridis_d(direction = -1, end = 0.5, alpha = 0.8, option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Group") +
  ggplot2::scale_x_continuous(limits = c(-55, -30)) +
  ggplot2::scale_y_continuous(limits = c(-35, 5)) +
  ggplot2::ggtitle("Spatial fold 1") + 
  ggplot2::theme(legend.position = "none",plot.title = ggplot2::element_text(hjust = 0.5)) + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")

p2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "white") +
  ggplot2::geom_point(data = pr,ggplot2::aes( x = x, y = y, color = group.2), size = 2) +
  ggplot2::scale_color_viridis_d( direction = -1, end = 0.5, alpha = 0.8, option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Group") +
  ggplot2::scale_x_continuous(limits = c(-55, -30)) +
  ggplot2::scale_y_continuous(limits = c(-35, 5)) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + 
  ggplot2::ggtitle("Spatial fold 25") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("")

p1 | p2

spatialRF::plot_evaluation(model.spatial.repeat)
