#################### GLM, GAM and GLMM Models #################################
library("readxl")
library(mgcv)
dados_OK <- as.data.frame(read_excel ("VariablesDataOK_pix1x1.xlsx"))
dados_OK
dados_OK <- na.omit(dados_OK)
dados_OK[, 42] <- log10(dados_OK[, 42])
dados_OK$SR_log <- log10(dados_OK$SR)
dados_OK[, c(3:33, 42)] <- scale(dados_OK[, c(3:33, 42)])
summary(dados_OK)
head(dados_OK)

library(cluster)
# Defining grouping by latitude and longitude(k-means clustering)
set.seed(123)  #reproducibility
coords <- dados_OK[, c("Longitude", "Latitude")]
clusters_geo <- kmeans(coords, centers = 5)  # 5 clusters 
# Adding the cluster to the dataset
dados_OK$cluster_geo <- as.factor(clusters_geo$cluster)
head(dados_OK)

##########################    SR log    ####################################### 
##################   GLMM (Generalized Linear Mixed Models)    #################
library(lme4)
modelo_lmm <- lmer(SR_log ~ curvel  + tempmean + sal + tempSS +	
                     light + pp +	fishing_effort  + (1 | cluster_geo), 
                   data = dados_OK)
summary(modelo_lmm)

######### GAM Models: Smoothed Functions of Latitude and Longitude  #############
library(mgcv)
modelo_gam_esp <- gam(SR_log ~ s(Latitude, Longitude) + 
                        curvel  + tempmean + sal + tempSS +	
                        light + pp +	fishing_effort , 
                  data = dados_OK)
summary(modelo_gam_esp)

############ spatial models(Gaussian Process Models) #######################
library(spaMM)
modelo_spaMM <- fitme(SR_log ~ curvel  + tempmean + sal + tempSS +	
                        light + pp +	fishing_effort  + 
                        Matern(1 | Longitude + Latitude), 
                      data = dados_OK)
summary(modelo_spaMM)
pseudoR2(modelo_spaMM)

# AIC 3 models
aic_lmm <- AIC(modelo_lmm)
aic_gam <- AIC(modelo_gam_esp)
aic_spaMM <- AIC(modelo_spaMM)

# AIC values
cat("AIC LMM:", aic_lmm, "\n")
cat("AIC GAM:", aic_gam, "\n")
cat("AIC spaMM:", aic_spaMM, "\n")

###############################################################################
##########################     PD.SES    ####################################### 
##################   GLMM (Generalized Linear Mixed Models)    #################
library(lme4)
modelo_lmm <- lmer(PD.SES ~ curvel  + tempmean + sal + tempSS +	
                     light + pp +	fishing_effort  + (1 | cluster_geo), 
                   data = dados_OK)
summary(modelo_lmm)

######### GAM Models: Smoothed Functions of Latitude and Longitude  #############
modelo_gam_esp <- gam(PD.SES ~ s(Latitude, Longitude) + 
                        curvel  + tempmean + sal + tempSS +	
                        light + pp +	fishing_effort , 
                      data = dados_OK)
summary(modelo_gam_esp)

############ spatial models (Gaussian Process Models) #######################
library(spaMM)
modelo_spaMM <- fitme(PD.SES ~ curvel  + tempmean + sal + tempSS +	
                        light + pp +	fishing_effort  + 
                        Matern(1 | Longitude + Latitude), 
                      data = dados_OK)
summary(modelo_spaMM)
pseudoR2(modelo_spaMM)

# AIC 3 models
aic_lmm <- AIC(modelo_lmm)
aic_gam <- AIC(modelo_gam_esp)
aic_spaMM <- AIC(modelo_spaMM)

# AIC values
cat("AIC LMM:", aic_lmm, "\n")
cat("AIC GAM:", aic_gam, "\n")
cat("AIC spaMM:", aic_spaMM, "\n")

################################################################################

##########################     PE.SES    ####################################### 
##################   GLMM (Generalized Linear Mixed Models)    #################
library(lme4)
modelo_lmm <- lmer(PE.SES ~ curvel  + tempmean + sal + tempSS +	
                     light + pp +	fishing_effort  + (1 | cluster_geo), 
                   data = dados_OK)
summary(modelo_lmm)

######### GAM Models: Smoothed Functions of Latitude and Longitude #############
modelo_gam_esp <- gam(PE.SES ~ s(Latitude, Longitude) + 
                        curvel  + tempmean + sal + tempSS +	
                        light + pp +	fishing_effort , 
                      data = dados_OK)
summary(modelo_gam_esp)

############ spatial models (Gaussian Process Models)  #######################
library(spaMM)
modelo_spaMM <- fitme(PE.SES ~ curvel  + tempmean + sal + tempSS +	
                        light + pp +	fishing_effort  + 
                        Matern(1 | Longitude + Latitude), 
                      data = dados_OK)
summary(modelo_spaMM)
pseudoR2(modelo_spaMM)
# AIC 3 models
aic_lmm <- AIC(modelo_lmm)
aic_gam <- AIC(modelo_gam_esp)
aic_spaMM <- AIC(modelo_spaMM)
pseudoR2(modelo_spaMM)

# AIC values
cat("AIC LMM:", aic_lmm, "\n")
cat("AIC GAM:", aic_gam, "\n")
cat("AIC spaMM:", aic_spaMM, "\n")
################################################################################

