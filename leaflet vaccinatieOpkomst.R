# auteur: Leonard Vanbrabant (GGD WB) en Arne Meeldijk (GGD BZO)
# datum aangemaakt: 10-05-2021
# datum laatst gewijzigd: 25-05-2021 door LV


## door gebruiker aan te passen
# ggd regio
ggd_regio <- "GGD West-Brabant"

# selecteer de gemeentenamen uit je eigen regio
GM_GGD_regio <- c("Alphen-Chaam", "Altena", "Baarle-Nassau", "Bergen op Zoom",
                  "Breda", "Drimmelen", "Etten-Leur", "Geertruidenberg",
                  "Halderberge", "Moerdijk", "Oosterhout", "Roosendaal",
                  "Rucphen", "Steenbergen", "Woensdrecht", "Zundert")


# laad paketten
library(sf)            # simple features
library(data.table)    # fread(), om grote data bestanden snel in te laden
library(robustbase)    # robust linaer models
library(leaflet)       # kaartjes
library(magrittr)      # %>%
library(ggplot2)       # plots
library(htmlwidgets)   # save leaflet kaart


## source files
# haal de laatste vaccinatie data uit coronit
source("get_vaccinatieData.R")


# helper functie: verwijder letters van string en maak er een numerieke variabele van.
to_number <- function(codes) {
  return(as.numeric(sub("[a-zA-Z]+", "", codes)))
}

# set paths: aanpassing naar eigen lokaties
path_shape <- "C:/Users/l.vanbrabant/stack/ShapeFiles" # e.g., .shp of .gpkg
path_data  <- "data" 


## laad gebiedspolygonen: dit zijn polygonen van heel NL!
# PC6
# https://hub.arcgis.com/datasets/esrinl-content::postcodevlakken-pc-6/about
pc6_sf <- read_sf(file.path(path_shape, "2021/PC6/PC6.shp"))

# PC4
# https://hub.arcgis.com/datasets/esrinl-content::postcodevlakken-pc-4/about
pc4_sf <- read_sf(file.path(path_shape, "2021/PC4/PC4.shp"))

# buurt
# https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/wijk-en-buurtkaart-2020
bu_sf <- read_sf(file.path(path_shape, "2020/buurt/buurt_2020_v1.shp"))
bu_sf$BU_CODE <- to_number(bu_sf$BU_CODE)

# wijk
wk_sf <- read_sf(file.path(path_shape, "2020/wijk/wijk_2020_v1.shp"))
wk_sf$WK_CODE <- to_number(wk_sf$WK_CODE)

# gemeente
gm_sf <- read_sf(file.path(path_shape, "2020/gemeente/gemeente_2020_v1.shp"))
gm_sf$GM_CODE <- to_number(gm_sf$GM_CODE)


# dit bestand bevat pc6 incl buurt-, wijk- en gemeentcode. Deze data koppelen
# we later aan de bu, wk en gm polygoon data. 
dfpchn <- fread(file.path(path_data, "pc6hnr20200801_gwb.csv"))
#dfpchn <- dfpchn[, c("PC6", "Buurt2020", "Wijk2020", "Gemeente2020")]
dfpchn <- unique(dfpchn)
dfpchn$PC4 <- to_number(dfpchn$PC6)
names(dfpchn) <- c("PC6", "Huisnummer", "BU_CODE", "WK_CODE", "GM_CODE", "PC4")

# data met de gemeente code en gemeente naam
gm <- fread(file.path(path_data, "gem2020.csv"))
names(gm) <- c("GM_CODE", "GM_NAAM")
# koppel gemeente naam aan pc6
dfpchn_gmnaam <- merge(dfpchn, gm, by = "GM_CODE", all.x = TRUE)


## Wijken, buurten gemeenten toevoegen
dfpchn_gmnaam <- subset(dfpchn_gmnaam, GM_NAAM %in% GM_GGD_regio)
# check: klopt het aantal, anders mogelijk een typefout
unique(dfpchn_gmnaam$GM_NAAM)

## vaccinatie data inladen
vac <- fread(file.path(path_data, paste0(Sys.Date(), "_vaccinaties.csv")))
# opschonen data
vac_df <- subset(vac, !is.na(nm_batchnummer) & !is.na(dt_vaccinatie) &
                   vaccinatiestatus == "Vaccinatie gezet" &
                   ggd_regio == ggd_regio & nr_vaccinatieronde == 1)
# verwijder laatste dubbele cases op basis van bsn
vac_df <- vac_df[!duplicated(vac_df$bsn), ]
vac_df <- vac_df[ , c("postcode", "huisnummer")]
names(vac_df) <- c("PC6", "Huisnummer")

# voeg bu-, wk-, pc4, en pc6-code toe aan vaccinatie data
buwkgm_vac_df <- merge(vac_df, dfpchn_gmnaam, by = c("PC6", "Huisnummer"), all.x = TRUE)

## de postcodes zijn niet altijd correct ingevuld of afwezig, dit levert problemen
## op bij het koppelen. 
sum(is.na(buwkgm_vac_df$GM_NAAM))

## aggregeer aantal vaccinaties naar gebied
pc6_vac <- aggregate(rep(1, nrow(buwkgm_vac_df)), by = list(buwkgm_vac_df$PC6), sum)
  names(pc6_vac) <- c("PC6", "A_VAC")
pc4_vac <- aggregate(rep(1, nrow(buwkgm_vac_df)), by = list(buwkgm_vac_df$PC4), sum)
  names(pc4_vac) <- c("PC4", "A_VAC")
bu_vac <- aggregate(rep(1, nrow(buwkgm_vac_df)), by = list(buwkgm_vac_df$BU_CODE), sum)
  names(bu_vac) <- c("BU_CODE", "A_VAC")
wk_vac <- aggregate(rep(1, nrow(buwkgm_vac_df)), by = list(buwkgm_vac_df$WK_CODE), sum)
  names(wk_vac) <- c("WK_CODE", "A_VAC")
gm_vac <- aggregate(rep(1, nrow(buwkgm_vac_df)), by = list(buwkgm_vac_df$GM_CODE), sum)
  names(gm_vac) <- c("GM_CODE", "A_VAC")


# subset postcodes eigen regio
pc6_sf <- pc6_sf[pc6_sf$PC6 %in% unique(dfpchn_gmnaam$PC6), ]
pc4_sf <- pc4_sf[pc4_sf$PC4 %in% unique(dfpchn_gmnaam$PC4), ]
# subset buurten regio
bu_sf <- subset(bu_sf, GM_NAAM %in% GM_GGD_regio)
bu_sf <- bu_sf[ , c("BU_CODE", "WK_CODE", "GM_CODE",
                    "BU_NAAM", "WK_NAAM", "GM_NAAM")]
# subset wijken regio 
wk_sf <- subset(wk_sf, GM_NAAM %in% GM_GGD_regio)
wk_sf <- wk_sf[ , c("WK_CODE", "GM_CODE",
                    "WK_NAAM", "GM_NAAM")]
# subset gemeenten regio
gm_sf <- subset(gm_sf, GM_NAAM %in% GM_GGD_regio)
gm_sf <- gm_sf[ , c("GM_CODE", "GM_NAAM")]


# voeg gemeente namen toe aan polygoon data pc4 en pc6
dfpc6hn_gmnaam_sub <- unique(dfpchn_gmnaam[, c("PC6", "GM_NAAM")])
pc6_sf <- merge(pc6_sf, dfpc6hn_gmnaam_sub, by = "PC6", all.x = TRUE)
dfpc4hn_gmnaam_sub <- unique(dfpchn_gmnaam[, c("PC4", "GM_NAAM")])
pc4_sf <- merge(pc4_sf, dfpc4hn_gmnaam_sub, by = "PC4", all.x = TRUE)


# voeg vaccinatie aantallen aan polygoon data toe
pc6_sf_vac <- merge(pc6_sf, pc6_vac, by = "PC6", all.x = TRUE)
pc4_sf_vac <- merge(pc4_sf, pc4_vac, by = "PC4", all.x = TRUE)
bu_sf_vac  <- merge(bu_sf, bu_vac  , by = "BU_CODE", all.x = TRUE)
wk_sf_vac  <- merge(wk_sf, wk_vac  , by = "WK_CODE", all.x = TRUE)
gm_sf_vac  <- merge(gm_sf, gm_vac  , by = "GM_CODE", all.x = TRUE)

# transformeer crs (coordinaten reference systeem) naar epsg:4326
# Dit is nodig om later de kaartjes te maken met leaflet(). 
pc6_sf_vac <- st_transform(pc6_sf_vac, 4326)
pc4_sf_vac <- st_transform(pc4_sf_vac, 4326)
bu_sf_vac  <- st_transform(bu_sf_vac, 4326)
wk_sf_vac  <- st_transform(wk_sf_vac, 4326)
gm_sf_vac  <- st_transform(gm_sf_vac, 4326)


# dit zijn alle gezondheidsinstellingen in NL met een woonfunctie.
# bron: BAG
BAG <- fread(file.path(path_data, "gezondheidsInstellingen_met_woonfunctie.csv"))
# selecteer enkel panden die in gebruik zijn
BAG <- BAG[BAG$pandstatus %in% c("Pand in gebruik", "Pand in gebruik (niet ingemeten)"), ]
#table(BAG$pandstatus)
# Aggregeren aantal zorginstellingen naar PC6 
BAG_pc6 <- aggregate(rep(1, nrow(BAG)), by = list(BAG$postcode), sum)
  names(BAG_pc6) <- c("PC6", "A_ZORG")
# selecteer enkel de postcodes uit de eigen regio
BAG_pc6 <- subset(BAG_pc6, PC6 %in% dfpc6hn_gmnaam_sub$PC6)


## kenmerken op pc6 niveau: let op dit is data uit 2017! Geen nieuwere data beschikbaar
## De originele data is een shapefile en is al omgezet naar een csv. Deze data
## kan indien nodig opgevraagd worden bij Leonard Vanbrabant (l.vanbrabant@ggdwestbrabant.nl)
# bron: https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/gegevens-per-postcode
cbs_pc6_df <- fread(file.path(path_data, "PC6_kenmerken.csv"))
# hercodeer missings naar NA: "0 tot en met 4 / geheim = -99997"
cbs_pc6_df[cbs_pc6_df == -99997] <- NA
# hernoem postcode 6
names(cbs_pc6_df)[1] <- "PC6"
# selecteer enkel de postcodes uit de eigen regio
cbs_pc6_df <- subset(cbs_pc6_df, PC6 %in% dfpc6hn_gmnaam_sub$PC6)


# koppel aantal zorginstellingen aan pc6 kerncijfers
cbs_pc6_df <- merge(cbs_pc6_df, BAG_pc6, by = "PC6", all.x = TRUE)
# als er geen koppeling bestaat gaan we er vanuit dat er geen zorginstelling met
# woonfunctie in het gebied aanwezig is.
cbs_pc6_df$A_ZORG[is.na(cbs_pc6_df$A_ZORG)] <- 0

# als missing dan nemen we aan dat het gelijk is aan nul.
cbs_pc6_df$INW_65PL[is.na(cbs_pc6_df$INW_65PL)] <- 0L
cbs_pc6_df$INW_4564[is.na(cbs_pc6_df$INW_4564)] <- 0L
cbs_pc6_df$INW_2544[is.na(cbs_pc6_df$INW_2544)] <- 0L
cbs_pc6_df$INW_1524[is.na(cbs_pc6_df$INW_1524)] <- 0L

# kerncijfers op pc4 niveau
cbs_pc4_df <- fread(file.path(path_data, "cbs_pc4_2020_v1.csv"))
cbs_pc4_df$INW_65PL[is.na(cbs_pc4_df$INW_65PL)] <- 0L
cbs_pc4_df$INW_4564[is.na(cbs_pc4_df$INW_4564)] <- 0L
cbs_pc4_df$INW_2544[is.na(cbs_pc4_df$INW_2544)] <- 0L
cbs_pc4_df$INW_1524[is.na(cbs_pc4_df$INW_1524)] <- 0L
cbs_pc4_df[cbs_pc4_df == -99997] <- NA

# aantal inwoners in een PC4 dat in een PC6 woont met zorginstelling
PC4_zorg <- cbs_pc6_df
PC4_zorg$PC4 <- to_number(PC4_zorg$PC6) 
idx <- PC4_zorg$A_ZORG == 1
PC4_zorg <- aggregate(PC4_zorg$INWONER[idx], list(PC4_zorg$PC4[idx], PC4_zorg$A_ZORG[idx]), 
                      sum, na.rm = TRUE)
names(PC4_zorg) <- c("PC4", "ZORGINSTELLING", "INW_ZORG")

# toevoegen aan PC4 df
cbs_pc4_df <- merge(cbs_pc4_df, PC4_zorg, by = "PC4", all.x = TRUE)
# hercodeer missings 
cbs_pc4_df$INW_ZORG[is.na(cbs_pc4_df$INW_ZORG)] <- 0
cbs_pc4_df$ZORGINSTELLING[is.na(cbs_pc4_df$ZORGINSTELLING)] <- 0

#hist(cbs_pc4_df$INW_ZORG[cbs_pc4_df$INW_ZORG != 0], 30)
#table(cbs_pc4_df$ZORGINSTELLING)

# kerncijfers buurt, wijk, gemeente
kwb_df <- fread(file.path(path_data, "kwb-2020.csv"))

# koppel kerncijfers aan polygoon en vaccinatie aantallen
pc6_sf_vac_kwb <- merge(pc6_sf_vac, cbs_pc6_df, by = "PC6", all.x = TRUE)
pc4_sf_vac_kwb <- merge(pc4_sf_vac, cbs_pc4_df, by = "PC4", all.x = TRUE)
bu_sf_vac_kwb  <- merge(bu_sf_vac, kwb_df, by.x = "BU_CODE", by.y = "gwb_code_8", all.x = TRUE)
wk_sf_vac_kwb  <- merge(wk_sf_vac, kwb_df, by.x = "WK_CODE", by.y = "gwb_code_8", all.x = TRUE)
gm_sf_vac_kwb  <- merge(gm_sf_vac, kwb_df, by.x = "GM_CODE", by.y = "gwb_code_8", all.x = TRUE)

# wat doen we met PC6 gebieden waarbij het aantal gevaccineerden groter is dan het
# aantal inwoners. Dit komt waarschijnlijk, omdat de kerncijfers uit 2017 zijn.
# Voor nu zetten we het aantal inwoners gelijk aan het aantal vaccinaties.

# Note: er zitten ook grote verschillen tussen, bv, 55 inwoners 121 gevaccineerden. 
# wellicht komt dit door grote nieuwe bouwprojectenj
vac_inw.idx <- which(pc6_sf_vac_kwb$A_VAC > pc6_sf_vac_kwb$INWONER)
pc6_sf_vac_kwb$INWONER[vac_inw.idx] <- pc6_sf_vac_kwb$A_VAC[vac_inw.idx]

# het aantal pc4's met en zorginstelling
#table(pc4_sf_vac_kwb$ZORGINSTELLING)

# percentage gevaccineerden berekenen
pc6_sf_vac_kwb$P_VAC <- pc6_sf_vac_kwb$A_VAC / pc6_sf_vac_kwb$INWONER * 100
pc4_sf_vac_kwb$P_VAC <- pc4_sf_vac_kwb$A_VAC / pc4_sf_vac_kwb$INWONER * 100
bu_sf_vac_kwb$P_VAC  <- bu_sf_vac_kwb$A_VAC  / bu_sf_vac_kwb$a_inw * 100
wk_sf_vac_kwb$P_VAC  <- wk_sf_vac_kwb$A_VAC  / wk_sf_vac_kwb$a_inw * 100
gm_sf_vac_kwb$P_VAC  <- gm_sf_vac_kwb$A_VAC  / gm_sf_vac_kwb$a_inw * 100

# pc6 - percentage per leeftijdscat.
pc6_sf_vac_kwb$P_ZORG  <- pc6_sf_vac_kwb$A_ZORG   / pc6_sf_vac_kwb$INWONER * 100
pc6_sf_vac_kwb$P_65PL  <- pc6_sf_vac_kwb$INW_65PL / pc6_sf_vac_kwb$INWONER * 100
pc6_sf_vac_kwb$P_4564  <- pc6_sf_vac_kwb$INW_4564 / pc6_sf_vac_kwb$INWONER * 100
pc6_sf_vac_kwb$P_2544  <- pc6_sf_vac_kwb$INW_2544 / pc6_sf_vac_kwb$INWONER * 100
pc6_sf_vac_kwb$P_1524  <- pc6_sf_vac_kwb$INW_1524 / pc6_sf_vac_kwb$INWONER * 100
pc6_sf_vac_kwb$P_VROUW <- pc6_sf_vac_kwb$VROUW    / pc6_sf_vac_kwb$INWONER * 100
pc6_sf_vac_kwb$P_UITKMINAOW <- pc6_sf_vac_kwb$UITKMINAOW / pc6_sf_vac_kwb$INWONER * 100

# pc4
pc4_sf_vac_kwb$P_ZORG  <- pc4_sf_vac_kwb$INW_ZORG / pc4_sf_vac_kwb$INWONER * 100
pc4_sf_vac_kwb$P_65PL  <- pc4_sf_vac_kwb$INW_65PL / pc4_sf_vac_kwb$INWONER * 100
pc4_sf_vac_kwb$P_4564  <- pc4_sf_vac_kwb$INW_4564 / pc4_sf_vac_kwb$INWONER * 100
pc4_sf_vac_kwb$P_2544  <- pc4_sf_vac_kwb$INW_2544 / pc4_sf_vac_kwb$INWONER * 100
pc4_sf_vac_kwb$P_1524  <- pc4_sf_vac_kwb$INW_1524 / pc4_sf_vac_kwb$INWONER * 100
pc4_sf_vac_kwb$P_VROUW <- pc4_sf_vac_kwb$VROUW    / pc4_sf_vac_kwb$INWONER * 100
pc4_sf_vac_kwb$P_UITKMINAOW <- pc4_sf_vac_kwb$UITKMINAOW / pc4_sf_vac_kwb$INWONER * 100

# buurt
bu_sf_vac_kwb$P_65PL <- bu_sf_vac_kwb$a_65_oo / bu_sf_vac_kwb$a_inw * 100
bu_sf_vac_kwb$P_4564 <- bu_sf_vac_kwb$a_45_64 / bu_sf_vac_kwb$a_inw * 100
bu_sf_vac_kwb$P_2544 <- bu_sf_vac_kwb$a_25_44 / bu_sf_vac_kwb$a_inw * 100
bu_sf_vac_kwb$P_1524 <- bu_sf_vac_kwb$a_15_24 / bu_sf_vac_kwb$a_inw * 100

# wijk
wk_sf_vac_kwb$P_65PL <- wk_sf_vac_kwb$a_65_oo / wk_sf_vac_kwb$a_inw * 100
wk_sf_vac_kwb$P_4564 <- wk_sf_vac_kwb$a_45_64 / wk_sf_vac_kwb$a_inw * 100
wk_sf_vac_kwb$P_2544 <- wk_sf_vac_kwb$a_25_44 / wk_sf_vac_kwb$a_inw * 100
wk_sf_vac_kwb$P_1524 <- wk_sf_vac_kwb$a_15_24 / wk_sf_vac_kwb$a_inw * 100

# gemeene
gm_sf_vac_kwb$P_65PL <- gm_sf_vac_kwb$a_65_oo / gm_sf_vac_kwb$a_inw * 100
gm_sf_vac_kwb$P_4564 <- gm_sf_vac_kwb$a_45_64 / gm_sf_vac_kwb$a_inw * 100
gm_sf_vac_kwb$P_2544 <- gm_sf_vac_kwb$a_25_44 / gm_sf_vac_kwb$a_inw * 100
gm_sf_vac_kwb$P_1524 <- gm_sf_vac_kwb$a_15_24 / gm_sf_vac_kwb$a_inw * 100



## hierond wat plots om de data inzichtelijk te maken
# er lijkt geen duidelijk verschil tussen pc4's met en zonder zorginstelling.j
idx2 <- pc4_sf_vac_kwb$ZORGINSTELLING == 1
plot(y = pc4_sf_vac_kwb$P_VAC[idx2], x = pc4_sf_vac_kwb$P_65PL[idx2], pch = 16)
points(y = pc4_sf_vac_kwb$P_VAC[!idx2], x = pc4_sf_vac_kwb$P_65PL[!idx2],
       pch = 16, col = "red")


#plotten van samenhang tussen percentage 65+ en gevaccineerd
# pc6
pc6_xyplot <- ggplot(data = pc6_sf_vac_kwb, aes(x = P_65PL, y = P_VAC)) +
  geom_point(color = 'black')  +
  labs(title = '65+ers en gevaccineerden per buurt') +
  scale_y_continuous("percentage gevaccineerd") +
  scale_x_continuous("percentage 65+")
pc6_xyplot

# pc4
pc4_xyplot <- ggplot(data = pc4_sf_vac_kwb, aes(x = P_65PL, y = P_VAC)) +
  geom_point(color = 'black')  +
  labs(title = '65+ers en gevaccineerden per buurt') +
  scale_y_continuous("percentage gevaccineerd") +
  scale_x_continuous("percentage 65+")
pc4_xyplot

# buurt
bu_xyplot <- ggplot(data = bu_sf_vac_kwb, aes(x = P_65PL, y = P_VAC)) +
  geom_point(color = 'black')  +
  labs(title = '65+ers en gevaccineerden per buurt') +
  scale_y_continuous("percentage gevaccineerd") +
  scale_x_continuous("percentage 65+")
bu_xyplot


# wijk
wk_xyplot <- ggplot(data = wk_sf_vac_kwb, aes(x = P_65PL, y = P_VAC)) +
  geom_point(color = 'black')  +
  labs(title = '65+ers en gevaccineerden per buurt') +
  scale_y_continuous("percentage gevaccineerd") +
  scale_x_continuous("percentage 65+")
wk_xyplot

# gemeente
gm_xyplot <- ggplot(data = gm_sf_vac_kwb, aes(x = P_65PL, y = P_VAC)) +
  geom_point(color = 'black')  +
  labs(title = '65+ers en gevaccineerden per buurt') +
  scale_y_continuous("percentage gevaccineerd") +
  scale_x_continuous("percentage 65+")
gm_xyplot


## Hier gebeurt de magic
## predictie maken op basis van leeftijd. 
# pc6
tmp <- pc6_sf_vac_kwb[, c("P_VAC", "P_65PL", "P_4564", "P_2544", "P_1524")]
tmp$geometry <- NULL
cor(tmp, use = "pairwise.complete.obs")

fit1_pc6 <- lmrob(P_VAC ~ P_65PL + P_4564 + P_2544 + P_1524,  
                  data = pc6_sf_vac_kwb, method = "MM", setting = "KS2014")
summary(fit1_pc6)$r.squared
pc6_sf_vac_kwb$pred <- predict(object = fit1_pc6, pc6_sf_vac_kwb)

# pc4
tmp <- pc4_sf_vac_kwb[, c("P_VAC", "P_65PL", "P_4564", "P_2544", "P_1524")]
tmp$geometry <- NULL
cor(tmp, use = "pairwise.complete.obs")

fit1_pc4 <- lmrob(P_VAC ~ P_65PL + P_4564 + P_2544 + P_1524,  
                  data = pc4_sf_vac_kwb, method = "MM", setting = "KS2014")
summary(fit1_pc4)$r.squared
pc4_sf_vac_kwb$pred <- predict(object = fit1_pc4, pc4_sf_vac_kwb)


# buurt
tmp <- bu_sf_vac_kwb[, c("P_65PL", "P_4564", "P_2544", "P_1524")]
tmp$geometry <- NULL
cor(tmp, use = "pairwise.complete.obs")

fit1_bu <- lmrob(P_VAC ~ P_65PL + P_4564 + P_2544 + P_1524, 
                 data = bu_sf_vac_kwb, method = "MM", setting = "KS2014")
summary(fit1_bu)$r.squared
bu_sf_vac_kwb$pred <- predict(object = fit1_bu, bu_sf_vac_kwb)

# wijk
tmp <- wk_sf_vac_kwb[, c("P_65PL", "P_4564", "P_2544", "P_1524")]
tmp$geometry <- NULL
cor(tmp, use = "pairwise.complete.obs")

fit1_wk <- lmrob(P_VAC ~ P_65PL + P_4564 + P_2544 + P_1524, 
                 data = wk_sf_vac_kwb, method = "MM", setting = "KS2014")
summary(fit1_wk)$r.squared
wk_sf_vac_kwb$pred <- predict(object = fit1_wk, wk_sf_vac_kwb)

# gemeente
tmp <- gm_sf_vac_kwb[, c("P_65PL", "P_4564", "P_2544", "P_1524")]
tmp$geometry <- NULL
cor(tmp, use = "pairwise.complete.obs")

fit1_gm <- lmrob(P_VAC ~ P_65PL + P_4564 + P_2544 + P_1524,
                 data = gm_sf_vac_kwb, method = "MM", setting = "KS2014")
summary(fit1_gm)$r.squared
gm_sf_vac_kwb$pred <- predict(object = fit1_gm, gm_sf_vac_kwb)



## Verschil tussen predictie en werkelijk berekenen. Dit zijn de residuen
# pc6
pc6_sf_vac_kwb$diffLM     <- round((pc6_sf_vac_kwb$pred  - pc6_sf_vac_kwb$P_VAC), 2)
# een negatief residu betekent dat het gebied achter loopt tov wat verwacht wordt obv leeftijd.
pc6_sf_vac_kwb$diffLM_inv <- -1 * pc6_sf_vac_kwb$diffLM
plot(pc6_sf_vac_kwb$diffLM_inv)
hist(pc6_sf_vac_kwb$diffLM_inv, 30)

# pc4
pc4_sf_vac_kwb$diffLM     <- round((pc4_sf_vac_kwb$pred  - pc4_sf_vac_kwb$P_VAC), 2)
pc4_sf_vac_kwb$diffLM_inv <- -1 * pc4_sf_vac_kwb$diffLM
plot(pc4_sf_vac_kwb$diffLM_inv)
hist(pc4_sf_vac_kwb$diffLM_inv, 30)

# buurt
bu_sf_vac_kwb$diffLM     <- round((bu_sf_vac_kwb$pred  - bu_sf_vac_kwb$P_VAC), 2)
bu_sf_vac_kwb$diffLM_inv <- -1 * bu_sf_vac_kwb$diffLM
plot(bu_sf_vac_kwb$diffLM_inv)
hist(bu_sf_vac_kwb$diffLM_inv, 30)

# wijk
wk_sf_vac_kwb$diffLM     <- round((wk_sf_vac_kwb$pred  - wk_sf_vac_kwb$P_VAC), 2)
wk_sf_vac_kwb$diffLM_inv <- -1 * wk_sf_vac_kwb$diffLM
plot(wk_sf_vac_kwb$diffLM_inv)
hist(wk_sf_vac_kwb$diffLM_inv, 30)

# gemeente
gm_sf_vac_kwb$diffLM     <- round((gm_sf_vac_kwb$pred  - gm_sf_vac_kwb$P_VAC), 2)
gm_sf_vac_kwb$diffLM_inv <- -1 * gm_sf_vac_kwb$diffLM
plot(gm_sf_vac_kwb$diffLM_inv)
hist(gm_sf_vac_kwb$diffLM_inv, 30)



# Hier maken we de kaarten ------------------------------------------------

# add layer id
#pc6_sf_vac_kwb$Id <- 1:nrow(pc6_sf_vac_kwb)
pc4_sf_vac_kwb$Id <- 1:nrow(pc4_sf_vac_kwb)
bu_sf_vac_kwb$Id  <- 1:nrow(bu_sf_vac_kwb)
wk_sf_vac_kwb$Id  <- 1:nrow(wk_sf_vac_kwb)
gm_sf_vac_kwb$Id  <- 1:nrow(gm_sf_vac_kwb)

## bereken z-scores en hak je vervolgens weer in stukjes op basis van de normaal verdeling
pc6_sf_vac_kwb$pc6_z <- (pc6_sf_vac_kwb$diffLM - mean(pc6_sf_vac_kwb$diffLM, na.rm = TRUE)) / 
  sd(pc6_sf_vac_kwb$diffLM, na.rm = TRUE)
pc6_q <- qnorm(c(.025, .05, .25, .50, .75, .95, .975))
pc6_sf_vac_kwb$pc6_bins <- cut(pc6_sf_vac_kwb$pc6_z, breaks = c(-Inf, pc6_q, Inf), 
                               include.lowest = TRUE, right = TRUE,
                               labels = 1:8)

pc4_sf_vac_kwb$pc4_z <- (pc4_sf_vac_kwb$diffLM - mean(pc4_sf_vac_kwb$diffLM, na.rm = TRUE)) / 
  sd(pc4_sf_vac_kwb$diffLM, na.rm = TRUE)
pc4_q <- qnorm(c(.025, .05, .25, .50, .75, .95, .975))
pc4_sf_vac_kwb$pc4_bins <- cut(pc4_sf_vac_kwb$pc4_z, breaks = c(-Inf, pc4_q, Inf), 
                               include.lowest = TRUE, right = TRUE,
                               labels = 1:8)

bu_sf_vac_kwb$bu_z <- (bu_sf_vac_kwb$diffLM - mean(bu_sf_vac_kwb$diffLM, na.rm = TRUE)) / 
  sd(bu_sf_vac_kwb$diffLM, na.rm = TRUE)
bu_q <- qnorm(c(.025, .05, .25, .50, .75, .95, .975))
bu_sf_vac_kwb$bu_bins <- cut(bu_sf_vac_kwb$bu_z, breaks = c(-Inf, bu_q, Inf), 
                             include.lowest = TRUE, right = TRUE)

wk_sf_vac_kwb$wk_z <- (wk_sf_vac_kwb$diffLM - mean(wk_sf_vac_kwb$diffLM, na.rm = TRUE)) / 
  sd(wk_sf_vac_kwb$diffLM, na.rm = TRUE)
wk_q <- qnorm(c(.025, .05, .25, .50, .75, .95, .975))
wk_sf_vac_kwb$wk_bins <- cut(wk_sf_vac_kwb$wk_z, breaks = c(-Inf, wk_q, Inf), 
                             include.lowest = TRUE, right = TRUE,
                             labels = 1:8)

gm_sf_vac_kwb$gm_z <- (gm_sf_vac_kwb$diffLM - mean(gm_sf_vac_kwb$diffLM, na.rm = TRUE)) / 
  sd(gm_sf_vac_kwb$diffLM, na.rm = TRUE)
gm_q <- qnorm(c(.025, .05, .25, .50, .75, .95, .975))
gm_sf_vac_kwb$gm_bins <- cut(gm_sf_vac_kwb$gm_z, breaks = c(-Inf, gm_q, Inf), 
                             include.lowest = TRUE, right = TRUE,
                             labels = 1:8)


# deze data hebben we later nodig voor het verklarende model. De geometery data
# hebben we niet meer nodig.
pc6_sf_vac_kwb_df <- pc6_sf_vac_kwb
pc4_sf_vac_kwb_df <- pc4_sf_vac_kwb
pc6_sf_vac_kwb_df$geometry <- NULL
pc4_sf_vac_kwb_df$geometry <- NULL
fwrite(pc6_sf_vac_kwb_df, file = file.path(path_data, "pc6_sf_vac_kwb.csv"))
fwrite(pc4_sf_vac_kwb_df, file = file.path(path_data, "pc4_sf_vac_kwb.csv"))



pal_pc6 <- colorFactor(colorRamp(c("green", "red")), na.color = "#FFFFFF", domain = pc6_sf_vac_kwb$pc6_bins)
pal_pc4 <- colorFactor(colorRamp(c("green", "red")), na.color = "#FFFFFF", domain = pc4_sf_vac_kwb$pc4_bins)
pal_bu  <- colorFactor(colorRamp(c("green", "red")), na.color = "#FFFFFF", domain = bu_sf_vac_kwb$bu_bins)
pal_wk  <- colorFactor(colorRamp(c("green", "red")), na.color = "#FFFFFF", domain = wk_sf_vac_kwb$wk_bins)
pal_gm  <- colorFactor(colorRamp(c("green", "red")), na.color = "#FFFFFF", domain = gm_sf_vac_kwb$gm_bins)


## maak label
# pc6
labels_pc6 <-
  paste(sprintf("<h3><b>%s</b></h5>", pc6_sf_vac_kwb$PC6),
        sprintf("<h4>Percentage gevaccineerd door GGD: %s</h6>", round(pc6_sf_vac_kwb$P_VAC)),
        sprintf("<h4>Voorspeld percentage: %s</h6>", round(pc6_sf_vac_kwb$pred)),
        sprintf("<h4>Aantal inwoners: %s</h6>", round(pc6_sf_vac_kwb$INWONER)),
        sprintf("<h4>Aantal gevaccineerd door GGD: %s</h6>", round(pc6_sf_vac_kwb$A_VAC))
  )%>%
  lapply(htmltools::HTML)

# pc4
labels_pc4 <-
  paste(sprintf("<h3><b>%s</b></h5>", pc4_sf_vac_kwb$PC4),
        sprintf("<h4>Percentage gevaccineerd door GGD: %s</h6>", sprintf("%.1f", pc4_sf_vac_kwb$P_VAC)),
        sprintf("<h4>Voorspeld percentage: %s</h6>"            , sprintf("%.1f", pc4_sf_vac_kwb$pred)),
        sprintf("<h4>Aantal inwoners: %s</h6>"                 , sprintf("%s"  , pc4_sf_vac_kwb$INWONER)),
        sprintf("<h4>Aantal gevaccineerd door GGD: %s</h6>"    , sprintf("%s"  , pc4_sf_vac_kwb$A_VAC))
  )%>%
  lapply(htmltools::HTML)

# buurt
labels_bu <-
  paste(sprintf("<h3><b>%s</b></h5>", bu_sf_vac_kwb$GM_NAAM),
        sprintf("<h4>Wijk: %s</h6>" , bu_sf_vac_kwb$WK_NAAM),
        sprintf("<h4>Buurt %s</h6>" , bu_sf_vac_kwb$BU_NAAM),
        sprintf("<h4>Percentage gevaccineerd door GGD: %s</h6>", sprintf("%.1f", bu_sf_vac_kwb$P_VAC)),
        sprintf("<h4>Voorspeld percentage: %s</h6>"            , sprintf("%.1f", bu_sf_vac_kwb$pred)),
        sprintf("<h4>Aantal inwoners: %s</h6>"                 , sprintf("%s"  , bu_sf_vac_kwb$a_inw)),
        sprintf("<h4>Aantal gevaccineerd door GGD: %s</h6>"    , sprintf("%s"  , bu_sf_vac_kwb$A_VAC))
       )%>%
  lapply(htmltools::HTML)

# wijk
labels_wk <-
  paste(sprintf("<h3><b>%s</b></h5>", wk_sf_vac_kwb$GM_NAAM),
        sprintf("<h4>Wijk: %s</h6>" , wk_sf_vac_kwb$WK_NAAM),
        sprintf("<h4>Percentage gevaccineerd door GGD: %s</h6>", sprintf("%.1f", wk_sf_vac_kwb$P_VAC)),
        sprintf("<h4>Voorspeld percentage: %s</h6>"            , sprintf("%.1f", wk_sf_vac_kwb$pred)),
        sprintf("<h4>Aantal inwoners: %s</h6>"                 , sprintf("%s"  , wk_sf_vac_kwb$a_inw)),
        sprintf("<h4>Aantal gevaccineerd door GGD: %s</h6>"    , sprintf("%s"  , wk_sf_vac_kwb$A_VAC))
  )%>%
  lapply(htmltools::HTML)

# gemeente
labels_gm <-
  paste(sprintf("<h3><b>%s</b></h5>", gm_sf_vac_kwb$GM_NAAM),
        sprintf("<h4>Percentage gevaccineerd door GGD: %s</h6>", sprintf("%.1f", gm_sf_vac_kwb$P_VAC)),
        sprintf("<h4>Voorspeld percentage: %s</h6>"            , sprintf("%.1f", gm_sf_vac_kwb$pred)),
        sprintf("<h4>Aantal inwoners: %s</h6>"                 , sprintf("%s"  , gm_sf_vac_kwb$a_inw)),
        sprintf("<h4>Aantal gevaccineerd door GGD: %s</h6>"    , sprintf("%s"  , gm_sf_vac_kwb$A_VAC))
  )%>%
  lapply(htmltools::HTML)



kaart <- leaflet() %>% 
  #setView(zoom = zoom) %>% 
  addMapPane(name = "ames_polygons", zIndex = 200) %>% 
  addTiles() %>%
  # addPolygons(data        = pc6_sf_vac_kwb,
  #             color       = "black",
  #             fillColor   = ~pal_pc6(pc6_sf_vac_kwb$pc6_bins),
  #             opacity     = 1,
  #             weight      = 1,
  #             fillOpacity = 0.5,
  #             layerId     = pc6_sf_vac_kwb$Id,
  #             group       = "PC6",
  #             popup       = labels_pc6,
  #             highlightOptions = highlightOptions(color = "black", weight = 3,
  #                                                 bringToFront = TRUE),
  #             options = pathOptions(pane = "ames_polygons")) %>%
  addPolygons(data        = pc4_sf_vac_kwb, 
              color       = "black", 
              fillColor   = ~pal_pc4(pc4_sf_vac_kwb$pc4_bins),
              opacity     = 1,
              weight      = 1,
              fillOpacity = 0.5,
              layerId     = pc4_sf_vac_kwb$Id,
              group       = "PC4",
              popup       = labels_pc4,
              highlightOptions = highlightOptions(color = "black", weight = 3,
                                                  bringToFront = TRUE),
              options = pathOptions(pane = "ames_polygons")) %>%
  addPolygons(data        = bu_sf_vac_kwb, 
              color       = "black", 
              fillColor   = ~pal_bu(bu_sf_vac_kwb$bu_bins),
              opacity     = 1,
              weight      = 1,
              fillOpacity = 0.5,
              layerId     = bu_sf_vac_kwb$Id,
              group       = "Buurt",
              popup       = labels_bu,
              highlightOptions = highlightOptions(color = "black", weight = 3,
                                                  bringToFront = TRUE),
              options = pathOptions(pane = "ames_polygons")) %>%
  addPolygons(data        = wk_sf_vac_kwb,
              color       = "black",
              fillColor   = ~pal_wk(wk_sf_vac_kwb$wk_bins),
              opacity     = 1,
              weight      = 1,
              fillOpacity = 0.5,
              layerId     = wk_sf_vac_kwb$Id,
              group       = "Wijk",
              popup       = labels_wk,
              highlightOptions = highlightOptions(color = "black", weight = 3,
                                                  bringToFront = TRUE),
              options = pathOptions(pane = "ames_polygons")) %>%
  addPolygons(data        = gm_sf_vac_kwb,
              color       = "black",
              fillColor   = ~pal_gm(gm_sf_vac_kwb$gm_bins),
              opacity     = 1, # stroke opacity
              weight      = 1, # stroke width in pixels
              fillOpacity = 0.50,
              layerId     = gm_sf_vac_kwb$Id,
              group       = "Gemeente",
              popup       = labels_gm,
              highlightOptions = highlightOptions(color = "black", weight = 3,
                                                  bringToFront = TRUE),
              options = pathOptions(pane = "ames_polygons")) %>%
  # addLegend("bottomright", group = "Buurt", pal = pal_bu, 
  #           values = bu_sf_vac_kwb$diffLM, opacity = 1,
  #           title = "Verschil voorspeld en \n werkelijk gevaccineerden", 
  #           na.label = "Geen informatie") %>%
  # addLegend("bottomright", group = "Wijk", pal = pal_wk, 
  #           values = wk_sf_vac_kwb$diffLM, opacity = 1,
  #           title = "Verschil voorspeld en \n werkelijk gevaccineerden", 
  #           na.label = "Geen informatie") %>%
  # addLegend("bottomright", group = "Gemeente", pal = pal_gm, 
  #           values = gm_sf_vac_kwb$diffLM, opacity = 1,
  #           title = "Verschil voorspeld en \n werkelijk gevaccineerden", 
  #           na.label = "Geen informatie") %>%
  addLayersControl(
    baseGroups = c("PC6", 
                   "PC4", "Buurt", "Wijk", "Gemeente"),
    options    = layersControlOptions(collapsed = FALSE))


saveWidget(kaart, file = file.path(path_data, "kaart_vaccinatie_gebieden.html"), selfcontained = FALSE)
