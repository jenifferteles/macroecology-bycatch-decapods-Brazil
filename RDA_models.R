#########################        RDA       #####################################
set.seed(1)  # Ensures reproducibility
library("readxl")
dados_OK <- as.data.frame(read_excel ("VariablesDataOK_pix1x1.xlsx"))
dados_OK 
dados_OK <- na.omit(dados_OK)
dados_OK[, 42] <- log10(dados_OK[, 42])
dados_OK$SR_log <- log10(dados_OK$SR)
dados_OK[, c(3:33, 42)] <- scale(dados_OK[, c(3:33, 42)]) 
summary(dados_OK)

## Multicolinearidade
library (PerformanceAnalytics)
#chart.Correlation(dados_OK[,c(3:33)], histogram = TRUE)##  TODAS VAR

#all environmental variables 
#ph,	chlomean,	chloRange,	chloSS,	curvel,	O2,	O2range,	O2Lmax,	nit,		phosp,	sal,	salrange,	salLmax,	tempmean,	temprange,	bathym,	iron,	pp,	sil,	tempSS,	SalSS,	light,	carbophyto,	carbophytoLmax,	carbophytorange,	calcite,	carbophytoSS,	ppSS,	pprange,	tempLmax,fishing_effort
library(adespatial)
#sel.vars <- forward.sel(data_total[,c(33:36)], data_total[,c(3:32)])
#sel.vars$variables
pred2 <- subset(dados_OK, select=c(curvel,tempmean, sal, tempSS, light,	pp,	fishing_effort))

chart.Correlation(pred2, histogram = TRUE)##  all variables

library("car")
vif (lm(SR_log ~  curvel  + tempmean + sal + tempSS +	
          light + pp +	fishing_effort ,
        data= dados_OK))

indices_data <- dados_OK[,c("SR_log","PD.SES","PE.SES")]


library(vegan)
rda.bycatch <- rda(indices_data  ~  curvel  + tempmean + sal + tempSS +	
                     light + pp +	fishing_effort,
               data= dados_OK)

## significance of the axes to represent the relationship between the variable predictors and the composition of species 
res.axis <- anova.cca(rda.bycatch, by = "axis") 
res.axis
## variations that contribute the most to variation in species composition 
res.var <- anova.cca(rda.bycatch, by = "term") ## which variable
res.var
##  R2 value of the model
r_quadr <- RsquareAdj(rda.bycatch)
r_quadr

#################################################################
## Multi-scale ordering (MSO) to understand the results of ordering in relation to geographical distance
dadoslatlong <- dados_OK[,c(1,2)]
bird.rda <- mso(rda.bycatch, dadoslatlong, grain = 1, permutations = 999)
#Reset the legend size
par(cex = 0.5)
msoplot(bird.rda,legend = "topright")

## RDA triplot
library(ggord)
library(ggplot2)
ggord(rda.bycatch, size = 1.5, ptslab = TRUE, addsize = 3, 
      axes = c("1", "2"),repel = TRUE)+
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2)

########################################
############ partial RDA
## Generate a LIST W file: neighbor binary list
names(dados_OK)[names(dados_OK) == "Longitude"] <- "x"
names(dados_OK)[names(dados_OK) == "Latitude"] <- "y"

xy <- dados_OK[, c("x", "y")]
indices_data <- dados_OK[,c("SR_log","PD.SES","PE.SES")]

## Generate a LIST W file: neighbor binary list
library(spdep)
mat_knn <- knearneigh(as.matrix(xy), k = 2, longlat = TRUE)
mat_nb <- knn2nb(mat_knn, sym = TRUE)
mat_listw <- nb2listw(mat_nb, style = "W")
mat_listw

##  List the "candidate" methods to obtain the SWM matrix
MEM_mat <- scores.listw(mat_listw, MEM.autocor = "positive")
candidates <- listw.candidates(xy, nb = c("gab", "mst", "dnear"), 
                               weights = c("binary", "flin"))
## Select the best SWM matrix and run the MEM
W_sel_mat <- listw.select(indices_data, candidates, MEM.autocor = "positive",
                          p.adjust = TRUE, method = "FWD")
## Matrix of chosen spatial predictors (MEMs)
spatial.pred <- as.data.frame(W_sel_mat$best$MEM.select)

## assign names to lines
rownames(spatial.pred) <- rownames(xy) 

## Combine environmental and spatial variables in a single data.frame
pred.vars <- data.frame(dados_OK, spatial.pred)
colnames(pred.vars)
## RDA parcial
rda.p <- rda(indices_data ~   curvel  + tempmean + sal + tempSS +	
        light + pp +	fishing_effort + # Environmental predictors
        Condition(MEM11 + MEM2 + MEM8 + MEM54 + MEM3 + MEM49 + MEM32 + 
                  MEM30 + MEM63 + MEM10 + MEM56 + MEM4 + MEM58 + MEM13 + 
                  MEM15),  #spatial predictors
             data = pred.vars)
summary(rda.p)

#  significance of the axes to represent the relationship between the various predictors and the species composition
res.p.axis <- anova.cca(rda.p, by = "axis") 
res.p.axis
# identify which are the variables that contribute or that most contribute to the variation in the composition of species
res.p.var <- anova.cca(rda.p, by = "term") ## which variable
res.p.var
RsquareAdj(rda.p)
## Multi-scale ordering (MSO)
bird.rda <- mso(rda.p, xy, grain = 1, permutations = 999)
#Reset the legend size
par(cex = 0.5)
msoplot(bird.rda,legend = "topright")

## RDA Triplot 
library(ggord)
library(ggplot2)
ggord(rda.p, size = 1.5, ptslab = TRUE, addsize = 3, 
      axes = c("1", "2"),repel = TRUE)+
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2)

ggord(rda.p, size = 3, ptslab = TRUE, addsize = 4, 
      axes = c("1", "2"), repel = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  theme(text = element_text(family = "sans", color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
    panel.grid.minor = element_blank()) +
  labs(title = "",x = "Axis 1 (X%)",y = "Axis 2 (Y%)") +
  coord_cartesian(xlim = c(-1, 0.85), ylim = c(-0.9, 0.9))

# Output session info for reproducibility
sessionInfo()
