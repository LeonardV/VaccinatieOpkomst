library(randomForest)
library(magrittr)
source("impute.R")
source("rfparallel.R")

data_path  <- "data"

# kenmerken op pc4 niveau
cbs_pc4_df <- fread(file.path(data_path, "pc4_sf_vac_kwb.csv"))


## cbs pc4 kenmerken
#vnames <- c("diffLM_inv", "pc4_bins", "INWONER","P_NW_MIG_A","P_WE_MIG_A","P_VROUW",
#            "WOZWONING", "P_KOOPWON", "P_UITKMINAOW", "P_ZORG")

## PC4
# de ..vnames is data.table syntax!
#cbs_pc4_df <- cbs_pc4_df[, ..vnames]

# Welk aantal en percentage records mist er? 
cbs_pc4_df %>% sapply(FUN = is.na) %>% colSums %>% sort
cbs_pc4_df %>% sapply(FUN = is.na) %>% colMeans %>% multiply_by(100) %>% sort %>% round(digits = 2)


#idx <- which(is.na(cbs_pc4_df$P_UITKMINAOW))
#cbs_pc4_df[idx, ]

# Vrouwen
cbs_pc4_df <- impute(
  formula = P_VROUW ~ diffLM_inv + INWONER + P_ZORG,
  data = cbs_pc4_df %>% na.omit,
  newdata = cbs_pc4_df)

# WOZWONING    
cbs_pc4_df <- impute(
  formula = WOZWONING ~ diffLM_inv + INWONER + P_ZORG + P_VROUW,
  data = cbs_pc4_df %>% na.omit,
  newdata = cbs_pc4_df)

# P_KOOPWON   
cbs_pc4_df <- impute(
  formula = P_KOOPWON ~ diffLM_inv + INWONER + P_ZORG + P_VROUW + WOZWONING,
  data = cbs_pc4_df %>% na.omit,
  newdata = cbs_pc4_df)

# P_WE_MIG_A
cbs_pc4_df <- impute(
  formula = P_WE_MIG_A ~ P_KOOPWON + diffLM_inv + INWONER + P_ZORG + P_VROUW + WOZWONING,
  data = cbs_pc4_df %>% na.omit,
  newdata = cbs_pc4_df)

# P_WE_MIG_A
cbs_pc4_df <- impute(
  formula = P_WE_MIG_A ~ P_KOOPWON + diffLM_inv + INWONER + P_ZORG + P_VROUW + WOZWONING,
  data = cbs_pc4_df %>% na.omit,
  newdata = cbs_pc4_df)

# P_UITKMINAOW   
cbs_pc4_df <- impute(
  formula = P_UITKMINAOW ~ P_WE_MIG_A + P_KOOPWON + diffLM_inv + INWONER + P_ZORG + P_VROUW + WOZWONING,
  data = cbs_pc4_df %>% na.omit,
  newdata = cbs_pc4_df)

# P_NW_MIG_A 
cbs_pc4_df <- impute(
  formula = P_NW_MIG_A  ~ P_WE_MIG_A + P_KOOPWON + diffLM_inv + INWONER + P_ZORG + P_VROUW + WOZWONING,
  data = cbs_pc4_df %>% na.omit,
  newdata = cbs_pc4_df)


fwrite(cbs_pc4_df, file = "data/pc4_sf_vac_kwb_cleaned.csv")


