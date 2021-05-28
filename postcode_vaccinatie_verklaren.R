# auteur: Leonard Vanbrabant (GGD WB) en Arne Meeldink (GGD BZO)
# datum aangemaakt: 24-05-2021
# datum laatst gewijzigd: 25-05-2021 door Leonard Vanbrabant. 

## Verklarend model
library(sf)            # simpel features
library(robustbase)    # robust linaer models
library(lavaan)        # structural equation model

# specifeer data paths
data_path <- "data"


source("Imputeer kenmerkenbestand.R")

## bestanden inlezen
# in het leaflet script staat waar onderstaande tabellen vandaan komen, dit zijn de sf_vac_kwb-bestanden
PC6_tabel <- fread(file.path(data_path, "pc6_sf_vac_kwb.csv"))
PC4_tabel <- fread(file.path(data_path, "pc4_sf_vac_kwb_cleaned.csv"))

vnames <- c("diffLM_inv", "pc6_bins", "INWONER","P_NW_MIG_A","P_WE_MIG_A","P_VROUW",
            "WOZWONING", "P_KOOPWON", "P_UITKMINAOW", "P_ZORG")

## PC6
# de ..vnames is data.table syntax!
tmp <- PC6_tabel[, ..vnames]
cor1 <- cor(tmp, use = "pairwise.complete.obs")
cor1
#knitr::kable(cor1, format = "simple")

## NOTE: WONING en INWONER correleert zeer sterk, daarom wordt
# WONING niet meegenomen in het model

# sommige varianties zijn 100x groter dan de overige varianties, hier fixen we dat.
PC6_tabel$WOZWONING <- PC6_tabel$WOZWONING / 100 

## fit model
model_pc6 <- 'diffLM_inv ~ INWONER + P_NW_MIG_A + P_WE_MIG_A + P_VROUW + WOZWONING + 
                           P_KOOPWON + P_UITKMINAOW + P_ZORG'

fit.lav_pc6 <- sem(model_pc6, data = PC6_tabel, missing = "ml", estimator = "mlr",
                   fixed.x = FALSE)

summary(fit.lav_pc6, standardized = TRUE)
inspect(fit.lav_pc6, "R2")

# 95% van de observaties wordt ivm missings verwijderd. 
# Niet gebruiken.
# summary(lm(diffLM_inv ~ INWONER + P_NW_MIG_A + P_WE_MIG_A + WOZWONING +
#                         P_KOOPWON + UITKMINAOW + P_ZORG,
#            data = PC6_tabel))

## PC4
vnames <- c("diffLM_inv", "pc4_bins", "INWONER","P_NW_MIG_A","P_WE_MIG_A","P_VROUW",
            "WOZWONING", "P_KOOPWON", "P_UITKMINAOW", "P_ZORG")

tmp <- PC4_tabel[, ..vnames]
cor2 <- cor(tmp, use = "pairwise.complete.obs")
cor2

chart.Correlation(tmp, histogram = TRUE, pch = 19)

#tmp %>% sapply(FUN = is.na) %>% colSums %>% sort
#tmp %>% sapply(FUN = is.na) %>% colMeans %>% multiply_by(100) %>% sort %>% round(digits = 2)

#knitr::kable(cor2, format = "simple")

## NOTE: WONING en n INWONER correleren zeer sterk (r > .90), 
## WONING wordt niet in het model meegenomen.

# rescale varianties
PC4_tabel$INWONER   <- PC4_tabel$INWONER / 1000
PC4_tabel$WOZWONING <- PC4_tabel$WOZWONING / 10

# # specifeer model
# model_pc4 <- 'diffLM_inv ~ INWONER + P_NW_MIG_A + P_WE_MIG_A + P_VROUW + WOZWONING +
#                            P_KOOPWON + P_UITKMINAOW + P_ZORG'
# 
# # fit model
# fit.lav_pc4 <- sem(model_pc4, data = PC4_tabel, missing = "ml", estimator = "mlr",
#                    fixed.x = FALSE)
# 
# summary(fit.lav_pc4, standardized = TRUE)
# inspect(fit.lav_pc4, "R2")


summary(lmrob('diffLM_inv ~ INWONER + P_NW_MIG_A + P_WE_MIG_A + P_VROUW + WOZWONING + 
                            P_KOOPWON + P_UITKMINAOW + P_ZORG', data = PC4_tabel,
              setting = "KS2014", method = "MM"))




