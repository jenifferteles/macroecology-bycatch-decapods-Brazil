library("readxl")
dados_OK <- as.data.frame(read_excel ("VariablesDataOK_pix1x1.xlsx"))
dados_OK 
dados_OK <- na.omit(dados_OK)
dados_OK[, 42] <- log10(dados_OK[, 42])
dados_OK$SR_log <- log10(dados_OK$SR)
dados_OK[, c(3:33, 42)] <- scale(dados_OK[, c(3:33, 42)]) 
summary(dados_OK)
head(dados_OK)

metrics <- data.frame(SR = dados_OK$SR,
                       PD = dados_OK$PD,
                       PE = dados_OK$PE,
                       WE = dados_OK$WE,
                       ED = dados_OK$ED,
                       PD.SES = dados_OK$PD.SES,
                       PE.SES = dados_OK$PE.SES,
                       wE.SES = dados_OK$WE.SES,
                       ED.SES = dados_OK$ED.SES)
library(GGally)
ggpairs(metrics, upper = list(continuous = wrap("cor", size = 4))) 


