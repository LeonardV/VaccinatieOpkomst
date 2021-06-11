# auteur: Leonard Vanbrabant (GGD WB) en Arne Meeldijk (GGD BZO)
# datum aangemaakt: 10-05-2021
# datum laatst gewijzigd: 11-06-2021 door LV

## door gebruiker aan te passen

# selecteer de gemeentenamen uit je eigen regio
GM_GGD_regio <- c("Alphen-Chaam", "Altena", "Baarle-Nassau", "Bergen op Zoom",
                  "Breda", "Drimmelen", "Etten-Leur", "Geertruidenberg",
                  "Halderberge", "Moerdijk", "Oosterhout", "Roosendaal",
                  "Rucphen", "Steenbergen", "Woensdrecht", "Zundert")

# laatst beschikbare datum vaccinatiedata. Haal je nieuwe data op obv de
# get_vaccinatieData() dan gebruik je de data van vandaag.
vac_datum <- "2021-06-09"


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
#source("get_vaccinatieData.R")


# helper functie: verwijder letters van string en maak er een numerieke variabele van.
to_number <- function(codes) {
  return(as.numeric(sub("[a-zA-Z]+", "", codes)))
}

# set paths: aanpassing naar eigen lokaties
#path_shape <- "/home/leonardv/stack/ShapeFiles"
path_shape  <- "C:/Users/l.vanbrabant/stack/ShapeFiles" # e.g., .shp of .gpkg
path_data   <- "data" 
path_result <- "resultaten" 

# wijk
wk_sf <- read_sf(file.path(path_shape, "2020/wijk/wijk_2020_v1.shp"))
wk_sf$WK_CODE <- to_number(wk_sf$WK_CODE)

# subset wijken regio 
wk_sf <- subset(wk_sf, GM_NAAM %in% GM_GGD_regio)
wk_sf <- wk_sf[ , c("WK_CODE", "GM_CODE",
                    "WK_NAAM", "GM_NAAM")]


# dit bestand bevat pc6 incl buurt-, wijk- en gemeentcode. Deze data koppelen
# we later aan de bu, wk en gm polygoon data. 
# https://www.cbs.nl/nl-nl/maatwerk/2020/39/buurt-wijk-en-gemeente-2020-voor-postcode-huisnummer
dfpchn <- fread(file.path(path_data, "pc6hnr20200801_gwb.csv"))
dfpchn <- unique(dfpchn)
dfpchn$PC4 <- to_number(dfpchn$PC6)
names(dfpchn) <- c("PC6", "Huisnummer", "BU_CODE", "WK_CODE", "GM_CODE", "PC4")

# data met de gemeente code en gemeente naam
gm <- fread(file.path(path_data, "gem2020.csv"))
names(gm) <- c("GM_CODE", "GM_NAAM")

# koppel gemeente naam aan pc6
dfpchn_gmnaam <- merge(dfpchn, gm, by = "GM_CODE", all.x = TRUE)

dfpc6hn_gmnaam_sub <- unique(dfpchn_gmnaam[, c("PC6", "GM_NAAM")])
dfpc6hn_gmnaam_sub <- subset(dfpc6hn_gmnaam_sub, GM_NAAM %in% GM_GGD_regio)

## vaccinatie data inladen
vac <- fread(file.path(path_data, paste0(vac_datum, "_vaccinaties.csv")))
# opschonen data
# vac_df <- subset(vac, !is.na(nm_batchnummer) & !is.na(dt_vaccinatie) &
#                       vaccinatiestatus == "Vaccinatie gezet" &
#                       ggd_regio == "GGD West-Brabant" & nr_vaccinatieronde == 1)

## nu worden ook de geplande afsraken meegenomen. Deze data bevat wel meer ruis.

## enkel 1ste vaccinaties eruit filteren!
vac_df <- subset(vac, ((status_afspraak == "GEPLAND" & status_deelname == "GEPLAND") | 
                         vaccinatiestatus == "Vaccinatie gezet") &
                   (is.na(nr_vaccinatieronde) | nr_vaccinatieronde == 1) &
                   ggd_regio == "GGD West-Brabant")

# verwijder dubbele bsn, zo houden we iedereen over die 1 vaccinatie heeft gehad
# of ingepland staat. De tweede afspraak is dus verwijderd. 
vac_df <- vac_df[!duplicated(vac_df$bsn), ]

# ruwe methode, maar goed genoeg
vac_df$leeftijd <- 2021 - year(vac_df$geboortedatum)

# maak subsets op basis van de cbs leeftijdscat.
vac_1524_df <- subset(vac_df, leeftijd >= 15 & leeftijd <= 24)
vac_2544_df <- subset(vac_df, leeftijd >= 25 & leeftijd <= 44)
vac_4564_df <- subset(vac_df, leeftijd >= 45 & leeftijd <= 64)
vac_65PL_df <- subset(vac_df, leeftijd >= 65)

## checks
# vaccinatie status is wel gezet: batchnr bestaat
# vac_df[vac_df$status_afspraak == "VERVALLEN", ]
# vac_df[vac_df$status_deelname == "GEANNULEERD", ]

# neem subset
vac_df      <- vac_df[      , c("postcode", "huisnummer")]
vac_1524_df <- vac_1524_df[ , c("postcode", "huisnummer")]
vac_2544_df <- vac_2544_df[ , c("postcode", "huisnummer")]
vac_4564_df <- vac_4564_df[ , c("postcode", "huisnummer")]
vac_65PL_df <- vac_65PL_df[ , c("postcode", "huisnummer")]
names(vac_df)      <- c("PC6", "Huisnummer")
names(vac_1524_df) <- c("PC6", "Huisnummer")
names(vac_2544_df) <- c("PC6", "Huisnummer")
names(vac_4564_df) <- c("PC6", "Huisnummer")
names(vac_65PL_df) <- c("PC6", "Huisnummer")


# voeg bu-, wk-, wk, en pc6-code toe aan vaccinatie data
buwkgm_vac_df      <- merge(vac_df     , dfpchn_gmnaam, by = c("PC6", "Huisnummer"), all.x = TRUE)
buwkgm_vac_1524_df <- merge(vac_1524_df, dfpchn_gmnaam, by = c("PC6", "Huisnummer"), all.x = TRUE)
buwkgm_vac_2544_df <- merge(vac_2544_df, dfpchn_gmnaam, by = c("PC6", "Huisnummer"), all.x = TRUE)
buwkgm_vac_4564_df <- merge(vac_4564_df, dfpchn_gmnaam, by = c("PC6", "Huisnummer"), all.x = TRUE)
buwkgm_vac_65PL_df <- merge(vac_65PL_df, dfpchn_gmnaam, by = c("PC6", "Huisnummer"), all.x = TRUE)


# aggregeer naar gebied
wk_vac      <- aggregate(rep(1, nrow(buwkgm_vac_df))     , by = list(buwkgm_vac_df$WK_CODE), sum)
wk_vac_1524 <- aggregate(rep(1, nrow(buwkgm_vac_1524_df)), by = list(buwkgm_vac_1524_df$WK_CODE), sum)
wk_vac_2544 <- aggregate(rep(1, nrow(buwkgm_vac_2544_df)), by = list(buwkgm_vac_2544_df$WK_CODE), sum)
wk_vac_4564 <- aggregate(rep(1, nrow(buwkgm_vac_4564_df)), by = list(buwkgm_vac_4564_df$WK_CODE), sum)
wk_vac_65PL <- aggregate(rep(1, nrow(buwkgm_vac_65PL_df)), by = list(buwkgm_vac_65PL_df$WK_CODE), sum)
names(wk_vac)      <- c("WK_CODE", "A_VAC")
names(wk_vac_1524) <- c("WK_CODE", "A_VAC")
names(wk_vac_2544) <- c("WK_CODE", "A_VAC")
names(wk_vac_4564) <- c("WK_CODE", "A_VAC")
names(wk_vac_65PL) <- c("WK_CODE", "A_VAC")


wk_sf_vac      <- merge(wk_sf, wk_vac     , by = "WK_CODE", all.x = TRUE)
wk_sf_vac_1524 <- merge(wk_sf, wk_vac_1524, by = "WK_CODE", all.x = TRUE)
wk_sf_vac_2544 <- merge(wk_sf, wk_vac_2544, by = "WK_CODE", all.x = TRUE)
wk_sf_vac_4564 <- merge(wk_sf, wk_vac_4564, by = "WK_CODE", all.x = TRUE)
wk_sf_vac_65PL <- merge(wk_sf, wk_vac_65PL, by = "WK_CODE", all.x = TRUE)

# transformeer crs (coordinaten reference systeem) naar epsg:4326
# Dit is nodig om later de kaartjes te maken met leaflet(). 
wk_sf_vac      <- st_transform(wk_sf_vac     , 4326)
wk_sf_vac_1524 <- st_transform(wk_sf_vac_1524, 4326)
wk_sf_vac_2544 <- st_transform(wk_sf_vac_2544, 4326)
wk_sf_vac_4564 <- st_transform(wk_sf_vac_4564, 4326)
wk_sf_vac_65PL <- st_transform(wk_sf_vac_65PL, 4326)


# kerncijfers buurt, wijk, gemeente
kwb_df <- fread(file.path(path_data, "kwb-2020.csv"))
wk_sf_vac_kwb       <- merge(wk_sf_vac, kwb_df, by.x = "WK_CODE", by.y = "gwb_code_8", all.x = TRUE)
wk_sf_vac_kwb_1524  <- merge(wk_sf_vac_1524, kwb_df, by.x = "WK_CODE", by.y = "gwb_code_8", all.x = TRUE)
wk_sf_vac_kwb_2544  <- merge(wk_sf_vac_2544, kwb_df, by.x = "WK_CODE", by.y = "gwb_code_8", all.x = TRUE)
wk_sf_vac_kwb_4564  <- merge(wk_sf_vac_4564, kwb_df, by.x = "WK_CODE", by.y = "gwb_code_8", all.x = TRUE)
wk_sf_vac_kwb_65PL  <- merge(wk_sf_vac_65PL, kwb_df, by.x = "WK_CODE", by.y = "gwb_code_8", all.x = TRUE)

# bereken percentage gevaccineerden per wijk
wk_sf_vac_kwb$P_VAC       <- wk_sf_vac_kwb$A_VAC  / wk_sf_vac_kwb$a_inw * 100
wk_sf_vac_kwb_1524$P_VAC  <- wk_sf_vac_kwb_1524$A_VAC  / wk_sf_vac_kwb_1524$a_inw * 100
wk_sf_vac_kwb_2544$P_VAC  <- wk_sf_vac_kwb_2544$A_VAC  / wk_sf_vac_kwb_2544$a_inw * 100
wk_sf_vac_kwb_4564$P_VAC  <- wk_sf_vac_kwb_4564$A_VAC  / wk_sf_vac_kwb_4564$a_inw * 100
wk_sf_vac_kwb_65PL$P_VAC  <- wk_sf_vac_kwb_65PL$A_VAC  / wk_sf_vac_kwb_65PL$a_inw * 100

# wijk
wk_sf_vac_kwb$P_65PL  <- wk_sf_vac_kwb$a_65_oo / wk_sf_vac_kwb$a_inw * 100
wk_sf_vac_kwb$P_4564  <- wk_sf_vac_kwb$a_45_64 / wk_sf_vac_kwb$a_inw * 100
wk_sf_vac_kwb$P_2544  <- wk_sf_vac_kwb$a_25_44 / wk_sf_vac_kwb$a_inw * 100
wk_sf_vac_kwb$P_1524  <- wk_sf_vac_kwb$a_15_24 / wk_sf_vac_kwb$a_inw * 100

wk_sf_vac_kwb_1524$P_65PL  <- wk_sf_vac_kwb_1524$a_65_oo / wk_sf_vac_kwb_1524$a_inw * 100
wk_sf_vac_kwb_1524$P_4564  <- wk_sf_vac_kwb_1524$a_45_64 / wk_sf_vac_kwb_1524$a_inw * 100
wk_sf_vac_kwb_1524$P_2544  <- wk_sf_vac_kwb_1524$a_25_44 / wk_sf_vac_kwb_1524$a_inw * 100
wk_sf_vac_kwb_1524$P_1524  <- wk_sf_vac_kwb_1524$a_15_24 / wk_sf_vac_kwb_1524$a_inw * 100

wk_sf_vac_kwb_2544$P_65PL  <- wk_sf_vac_kwb_2544$a_65_oo / wk_sf_vac_kwb_2544$a_inw * 100
wk_sf_vac_kwb_2544$P_4564  <- wk_sf_vac_kwb_2544$a_45_64 / wk_sf_vac_kwb_2544$a_inw * 100
wk_sf_vac_kwb_2544$P_2544  <- wk_sf_vac_kwb_2544$a_25_44 / wk_sf_vac_kwb_2544$a_inw * 100
wk_sf_vac_kwb_2544$P_1524  <- wk_sf_vac_kwb_2544$a_15_24 / wk_sf_vac_kwb_2544$a_inw * 100

wk_sf_vac_kwb_4564$P_65PL  <- wk_sf_vac_kwb_4564$a_65_oo / wk_sf_vac_kwb_4564$a_inw * 100
wk_sf_vac_kwb_4564$P_4564  <- wk_sf_vac_kwb_4564$a_45_64 / wk_sf_vac_kwb_4564$a_inw * 100
wk_sf_vac_kwb_4564$P_2544  <- wk_sf_vac_kwb_4564$a_25_44 / wk_sf_vac_kwb_4564$a_inw * 100
wk_sf_vac_kwb_4564$P_1524  <- wk_sf_vac_kwb_4564$a_15_24 / wk_sf_vac_kwb_4564$a_inw * 100

wk_sf_vac_kwb_65PL$P_65PL  <- wk_sf_vac_kwb_65PL$a_65_oo / wk_sf_vac_kwb_65PL$a_inw * 100
wk_sf_vac_kwb_65PL$P_4564  <- wk_sf_vac_kwb_65PL$a_45_64 / wk_sf_vac_kwb_65PL$a_inw * 100
wk_sf_vac_kwb_65PL$P_2544  <- wk_sf_vac_kwb_65PL$a_25_44 / wk_sf_vac_kwb_65PL$a_inw * 100
wk_sf_vac_kwb_65PL$P_1524  <- wk_sf_vac_kwb_65PL$a_15_24 / wk_sf_vac_kwb_65PL$a_inw * 100

# wk
ggplot(data = wk_sf_vac_kwb, aes(x = P_65PL, y = P_VAC)) +
  geom_point(color = 'black')  +
  labs(title = '65PL en gevaccineerden per wk') +
  scale_y_continuous("percentage gevaccineerd") +
  scale_x_continuous("percentage 65+") + geom_smooth(method='lmrob')

ggplot(data = wk_sf_vac_kwb_1524, aes(x = P_1524, y = P_VAC)) +
  geom_point(color = 'black')  +
  labs(title = '15-24 en gevaccineerden per wk') +
  scale_y_continuous("percentage gevaccineerd") +
  scale_x_continuous("percentage 15-24") + geom_smooth(method='lmrob')

ggplot(data = wk_sf_vac_kwb_2544, aes(x = P_2544, y = P_VAC)) +
  geom_point(color = 'black')  +
  labs(title = '25-44 en gevaccineerden per wk') +
  scale_y_continuous("percentage gevaccineerd") +
  scale_x_continuous("percentage 25-44") + geom_smooth(method='lmrob')

ggplot(data = wk_sf_vac_kwb_4564, aes(x = P_4564, y = P_VAC)) +
  geom_point(color = 'black')  +
  labs(title = '45-64 en gevaccineerden per wk') +
  scale_y_continuous("percentage gevaccineerd") +
  scale_x_continuous("percentage 45-64") + geom_smooth(method='lmrob')

ggplot(data = wk_sf_vac_kwb_65PL, aes(x = P_65PL, y = P_VAC)) +
  geom_point(color = 'black')  +
  labs(title = '65+ en gevaccineerden per wk') +
  scale_y_continuous("percentage gevaccineerd") +
  scale_x_continuous("percentage 65+") + geom_smooth(method='lmrob')


# wk
tmp      <- wk_sf_vac_kwb[     , c("P_VAC", "P_65PL", "P_4564", "P_2544", "P_1524")]
tmp_1524 <- wk_sf_vac_kwb_1524[, c("P_VAC", "P_65PL", "P_4564", "P_2544", "P_1524")]
tmp_2544 <- wk_sf_vac_kwb_2544[, c("P_VAC", "P_65PL", "P_4564", "P_2544", "P_1524")]
tmp_4564 <- wk_sf_vac_kwb_4564[, c("P_VAC", "P_65PL", "P_4564", "P_2544", "P_1524")]
tmp_65PL <- wk_sf_vac_kwb_65PL[, c("P_VAC", "P_65PL", "P_4564", "P_2544", "P_1524")]

tmp$geometry <- NULL
tmp_1524$geometry <- NULL
tmp_2544$geometry <- NULL
tmp_4564$geometry <- NULL
tmp_65PL$geometry <- NULL

cor(tmp, use = "pairwise.complete.obs")
cor(tmp_1524, use = "pairwise.complete.obs")
cor(tmp_2544, use = "pairwise.complete.obs")
cor(tmp_4564, use = "pairwise.complete.obs")
cor(tmp_65PL, use = "pairwise.complete.obs")

fit_wk      <- lmrob(P_VAC ~ P_65PL + P_4564 + P_2544 + P_1524,  
                      data = wk_sf_vac_kwb, method = "MM", setting = "KS2014")
fit_wk_1524 <- lmrob(P_VAC ~ P_65PL + P_4564 + P_2544 + P_1524,  
                      data = wk_sf_vac_kwb_1524, method = "MM", setting = "KS2014")
fit_wk_2544 <- lmrob(P_VAC ~ P_65PL + P_4564 + P_2544 + P_1524,  
                      data = wk_sf_vac_kwb_2544, method = "MM", setting = "KS2014")
fit_wk_4564 <- lmrob(P_VAC ~ P_65PL + P_4564 + P_2544 + P_1524,  
                      data = wk_sf_vac_kwb_4564, method = "MM", setting = "KS2014")
fit_wk_65PL <- lmrob(P_VAC ~ P_65PL + P_4564 + P_2544 + P_1524,  
                      data = wk_sf_vac_kwb_65PL, method = "MM", setting = "KS2014")

summary(fit_wk)$r.squared
summary(fit_wk_1524)$r.squared
summary(fit_wk_2544)$r.squared
summary(fit_wk_4564)$r.squared
summary(fit_wk_65PL)$r.squared

wk_sf_vac_kwb$pred      <- predict(object = fit_wk     , wk_sf_vac_kwb)
wk_sf_vac_kwb_1524$pred <- predict(object = fit_wk_1524, wk_sf_vac_kwb_1524)
wk_sf_vac_kwb_2544$pred <- predict(object = fit_wk_2544, wk_sf_vac_kwb_2544)
wk_sf_vac_kwb_4564$pred <- predict(object = fit_wk_4564, wk_sf_vac_kwb_4564)
wk_sf_vac_kwb_65PL$pred <- predict(object = fit_wk_65PL, wk_sf_vac_kwb_65PL)

# wk
wk_sf_vac_kwb$diffLM     <- round((wk_sf_vac_kwb$pred  - wk_sf_vac_kwb$P_VAC), 2)
wk_sf_vac_kwb$diffLM_inv <- -1 * wk_sf_vac_kwb$diffLM
hist(wk_sf_vac_kwb$diffLM_inv, 30)

wk_sf_vac_kwb_1524$diffLM     <- round((wk_sf_vac_kwb_1524$pred  - wk_sf_vac_kwb_1524$P_VAC), 2)
wk_sf_vac_kwb_1524$diffLM_inv <- -1 * wk_sf_vac_kwb_1524$diffLM
hist(wk_sf_vac_kwb_1524$diffLM_inv, 30)

wk_sf_vac_kwb_2544$diffLM     <- round((wk_sf_vac_kwb_2544$pred  - wk_sf_vac_kwb_2544$P_VAC), 2)
wk_sf_vac_kwb_2544$diffLM_inv <- -1 * wk_sf_vac_kwb_2544$diffLM
hist(wk_sf_vac_kwb_2544$diffLM_inv, 30)

wk_sf_vac_kwb_4564$diffLM     <- round((wk_sf_vac_kwb_4564$pred  - wk_sf_vac_kwb_4564$P_VAC), 2)
wk_sf_vac_kwb_4564$diffLM_inv <- -1 * wk_sf_vac_kwb_4564$diffLM
hist(wk_sf_vac_kwb_4564$diffLM_inv, 30)

wk_sf_vac_kwb_65PL$diffLM     <- round((wk_sf_vac_kwb_65PL$pred  - wk_sf_vac_kwb_65PL$P_VAC), 2)
wk_sf_vac_kwb_65PL$diffLM_inv <- -1 * wk_sf_vac_kwb_65PL$diffLM
hist(wk_sf_vac_kwb_65PL$diffLM_inv, 30)


# dit zijn alle gezondheidsinstellingen in NL met een woonfunctie.
# bron: BAG
BAG <- st_read(file.path(path_data, "gezondheidsInstellingen_met_woonfunctie.gpkg"))
# selecteer enkel panden die in gebruik zijn
BAG <- subset(BAG, status %in% c("Verblijfsobject in gebruik", 
                                 "Verblijfsobject in gebruik (niet ingemeten)"))
#
# Aggregeren aantal zorginstellingen naar PC6 
#BAG_pc6 <- aggregate(rep(1, nrow(BAG)), by = list(BAG$postcode), sum)
#names(BAG_pc6) <- c("PC6", "A_ZORG")
# selecteer enkel de postcodes uit de eigen regio
BAG <- subset(BAG, postcode %in% dfpc6hn_gmnaam_sub$PC6)



# Hier maken we de kaarten ------------------------------------------------

# add layer id
#pc6_sf_vac_kwb$Id <- 1:nrow(pc6_sf_vac_kwb)
wk_sf_vac_kwb$Id      <- 1:nrow(wk_sf_vac_kwb)
wk_sf_vac_kwb_1524$Id <- 1:nrow(wk_sf_vac_kwb_1524)
wk_sf_vac_kwb_2544$Id <- 1:nrow(wk_sf_vac_kwb_2544)
wk_sf_vac_kwb_4564$Id <- 1:nrow(wk_sf_vac_kwb_4564)
wk_sf_vac_kwb_65PL$Id <- 1:nrow(wk_sf_vac_kwb_65PL)

wk_sf_vac_kwb$wk_z <- (wk_sf_vac_kwb$diffLM - mean(wk_sf_vac_kwb$diffLM, na.rm = TRUE)) / 
  sd(wk_sf_vac_kwb$diffLM, na.rm = TRUE)
wk_q <- qnorm(c(.025, .05, .25, .50, .75, .95, .975))
wk_sf_vac_kwb$wk_bins <- cut(wk_sf_vac_kwb$wk_z, breaks = c(-Inf, wk_q, Inf), 
                               include.lowest = TRUE, right = TRUE,
                               labels = 1:8)


wk_sf_vac_kwb_1524$wk_z <- (wk_sf_vac_kwb_1524$diffLM - mean(wk_sf_vac_kwb_1524$diffLM, 
                                                                na.rm = TRUE)) / 
  sd(wk_sf_vac_kwb_1524$diffLM, na.rm = TRUE)
wk_q <- qnorm(c(.025, .05, .25, .50, .75, .95, .975))
wk_sf_vac_kwb_1524$wk_bins <- cut(wk_sf_vac_kwb_1524$wk_z, breaks = c(-Inf, wk_q, Inf), 
                                    include.lowest = TRUE, right = TRUE,
                                    labels = 1:8)


wk_sf_vac_kwb_2544$wk_z <- (wk_sf_vac_kwb_2544$diffLM - mean(wk_sf_vac_kwb_2544$diffLM, 
                                                                na.rm = TRUE)) / 
  sd(wk_sf_vac_kwb_2544$diffLM, na.rm = TRUE)
wk_q <- qnorm(c(.025, .05, .25, .50, .75, .95, .975))
wk_sf_vac_kwb_2544$wk_bins <- cut(wk_sf_vac_kwb_2544$wk_z, breaks = c(-Inf, wk_q, Inf), 
                                    include.lowest = TRUE, right = TRUE,
                                    labels = 1:8)


wk_sf_vac_kwb_4564$wk_z <- (wk_sf_vac_kwb_4564$diffLM - mean(wk_sf_vac_kwb_4564$diffLM, 
                                                                na.rm = TRUE)) / 
  sd(wk_sf_vac_kwb_4564$diffLM, na.rm = TRUE)
wk_q <- qnorm(c(.025, .05, .25, .50, .75, .95, .975))
wk_sf_vac_kwb_4564$wk_bins <- cut(wk_sf_vac_kwb_4564$wk_z, breaks = c(-Inf, wk_q, Inf), 
                                    include.lowest = TRUE, right = TRUE,
                                    labels = 1:8)


wk_sf_vac_kwb_65PL$wk_z <- (wk_sf_vac_kwb_65PL$diffLM - mean(wk_sf_vac_kwb_65PL$diffLM, 
                                                                na.rm = TRUE)) / 
  sd(wk_sf_vac_kwb_65PL$diffLM, na.rm = TRUE)
wk_q <- qnorm(c(.025, .05, .25, .50, .75, .95, .975))
wk_sf_vac_kwb_65PL$wk_bins <- cut(wk_sf_vac_kwb_65PL$wk_z, breaks = c(-Inf, wk_q, Inf), 
                                    include.lowest = TRUE, right = TRUE,
                                    labels = 1:8)



pal_wk      <- colorFactor(colorRamp(c("green", "red")), na.color = "#FFFFFF", domain = wk_sf_vac_kwb$wk_bins)
pal_wk_1524 <- colorFactor(colorRamp(c("green", "red")), na.color = "#FFFFFF", domain = wk_sf_vac_kwb_1524$wk_bins)
pal_wk_2544 <- colorFactor(colorRamp(c("green", "red")), na.color = "#FFFFFF", domain = wk_sf_vac_kwb_2544$wk_bins)
pal_wk_4464 <- colorFactor(colorRamp(c("green", "red")), na.color = "#FFFFFF", domain = wk_sf_vac_kwb_4564$wk_bins)
pal_wk_65PL <- colorFactor(colorRamp(c("green", "red")), na.color = "#FFFFFF", domain = wk_sf_vac_kwb_65PL$wk_bins)
## maak label

# wk
labels_wk <-
  paste(sprintf("<h3><b>%s</b></h5>", wk_sf_vac_kwb$WK_NAAM),
        sprintf("<h4>Percentage gevaccineerd door GGD: %s</h6>", sprintf("%.1f", wk_sf_vac_kwb$P_VAC)),
        sprintf("<h4>Voorspeld percentage: %s</h6>"            , sprintf("%.1f", wk_sf_vac_kwb$pred)),
        sprintf("<h4>Aantal inwoners: %s</h6>"                 , sprintf("%s"  , wk_sf_vac_kwb$a_inw)),
        sprintf("<h4>Aantal gevaccineerd door GGD: %s</h6>"    , sprintf("%s"  , wk_sf_vac_kwb$A_VAC))
  )%>%
  lapply(htmltools::HTML)

labels_wk_1524 <-
  paste(sprintf("<h3><b>%s</b></h5>", wk_sf_vac_kwb_1524$WK_NAAM),
        sprintf("<h4>Percentage gevaccineerd door GGD: %s</h6>", sprintf("%.1f", wk_sf_vac_kwb_1524$P_VAC)),
        sprintf("<h4>Voorspeld percentage: %s</h6>"            , sprintf("%.1f", wk_sf_vac_kwb_1524$pred)),
        sprintf("<h4>Aantal inwoners: %s</h6>"                 , sprintf("%s"  , wk_sf_vac_kwb_1524$a_inw)),
        sprintf("<h4>Aantal gevaccineerd door GGD: %s</h6>"    , sprintf("%s"  , wk_sf_vac_kwb_1524$A_VAC))
  )%>%
  lapply(htmltools::HTML)

labels_wk_2544 <-
  paste(sprintf("<h3><b>%s</b></h5>", wk_sf_vac_kwb_2544$WK_NAAM),
        sprintf("<h4>Percentage gevaccineerd door GGD: %s</h6>", sprintf("%.1f", wk_sf_vac_kwb_2544$P_VAC)),
        sprintf("<h4>Voorspeld percentage: %s</h6>"            , sprintf("%.1f", wk_sf_vac_kwb_2544$pred)),
        sprintf("<h4>Aantal inwoners: %s</h6>"                 , sprintf("%s"  , wk_sf_vac_kwb_2544$a_inw)),
        sprintf("<h4>Aantal gevaccineerd door GGD: %s</h6>"    , sprintf("%s"  , wk_sf_vac_kwb_2544$A_VAC))
  )%>%
  lapply(htmltools::HTML)

labels_wk_4564 <-
  paste(sprintf("<h3><b>%s</b></h5>", wk_sf_vac_kwb_4564$WK_NAAM),
        sprintf("<h4>Percentage gevaccineerd door GGD: %s</h6>", sprintf("%.1f", wk_sf_vac_kwb_4564$P_VAC)),
        sprintf("<h4>Voorspeld percentage: %s</h6>"            , sprintf("%.1f", wk_sf_vac_kwb_4564$pred)),
        sprintf("<h4>Aantal inwoners: %s</h6>"                 , sprintf("%s"  , wk_sf_vac_kwb_4564$a_inw)),
        sprintf("<h4>Aantal gevaccineerd door GGD: %s</h6>"    , sprintf("%s"  , wk_sf_vac_kwb_4564$A_VAC))
  )%>%
  lapply(htmltools::HTML)

labels_wk_65PL <-
  paste(sprintf("<h3><b>%s</b></h5>", wk_sf_vac_kwb_65PL$WK_NAAM),
        sprintf("<h4>Percentage gevaccineerd door GGD: %s</h6>", sprintf("%.1f", wk_sf_vac_kwb_65PL$P_VAC)),
        sprintf("<h4>Voorspeld percentage: %s</h6>"            , sprintf("%.1f", wk_sf_vac_kwb_65PL$pred)),
        sprintf("<h4>Aantal inwoners: %s</h6>"                 , sprintf("%s"  , wk_sf_vac_kwb_65PL$a_inw)),
        sprintf("<h4>Aantal gevaccineerd door GGD: %s</h6>"    , sprintf("%s"  , wk_sf_vac_kwb_65PL$A_VAC))
  )%>%
  lapply(htmltools::HTML)



kaart <- leaflet() %>% 
  #setView(zoom = zoom) %>% 
  addMapPane(name = "ames_polygons", zIndex = 200) %>% 
  addTiles() %>%
  addPolygons(data        = wk_sf_vac_kwb, 
              color       = "black", 
              fillColor   = ~pal_wk(wk_sf_vac_kwb$wk_bins),
              opacity     = 1,
              weight      = 1,
              fillOpacity = 0.5,
              layerId     = wk_sf_vac_kwb$Id,
              group       = "alle leeftijden",
              popup       = labels_wk,
              highlightOptions = highlightOptions(color = "black", weight = 3,
                                                  bringToFront = TRUE),
              options = pathOptions(pane = "ames_polygons")) %>%
  # addPolygons(data        = wk_sf_vac_kwb_1524,
  #             color       = "black",
  #             fillColor   = ~pal_wk(wk_sf_vac_kwb_1524$wk_bins),
  #             opacity     = 1,
  #             weight      = 1,
  #             fillOpacity = 0.5,
  #             layerId     = wk_sf_vac_kwb_1524$Id,
  #             group       = "leeftijd 15-24",
  #             popup       = labels_wk_1524,
  #             highlightOptions = highlightOptions(color = "black", weight = 3,
  #                                                 bringToFront = TRUE),
  #             options = pathOptions(pane = "ames_polygons")) %>%
  # addPolygons(data        = wk_sf_vac_kwb_2544,
  #             color       = "black",
  #             fillColor   = ~pal_wk(wk_sf_vac_kwb_2544$wk_bins),
  #             opacity     = 1,
  #             weight      = 1,
  #             fillOpacity = 0.5,
  #             layerId     = wk_sf_vac_kwb_2544$Id,
  #             group       = "leeftijd 25-44",
  #             popup       = labels_wk_2544,
  #             highlightOptions = highlightOptions(color = "black", weight = 3,
  #                                                 bringToFront = TRUE),
  #             options = pathOptions(pane = "ames_polygons")) %>%
  addPolygons(data        = wk_sf_vac_kwb_4564,
              color       = "black",
              fillColor   = ~pal_wk(wk_sf_vac_kwb_4564$wk_bins),
              opacity     = 1,
              weight      = 1,
              fillOpacity = 0.5,
              layerId     = wk_sf_vac_kwb_4564$Id,
              group       = "leeftijd 45-64",
              popup       = labels_wk_4564,
              highlightOptions = highlightOptions(color = "black", weight = 3,
                                                  bringToFront = TRUE),
              options = pathOptions(pane = "ames_polygons")) %>%
  addPolygons(data        = wk_sf_vac_kwb_65PL,
              color       = "black",
              fillColor   = ~pal_wk(wk_sf_vac_kwb_65PL$wk_bins),
              opacity     = 1,
              weight      = 1,
              fillOpacity = 0.5,
              layerId     = wk_sf_vac_kwb_65PL$Id,
              group       = "leeftijd 65PL",
              popup       = labels_wk_65PL,
              highlightOptions = highlightOptions(color = "black", weight = 3,
                                                  bringToFront = TRUE),
              options = pathOptions(pane = "ames_polygons")) %>%
  addMarkers(data = BAG, group = "Gezondheidsinstelling met woonfunctie",
             clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("alle leeftijden", #"leeftijd 15-24", "leeftijd 25-44", 
                   "leeftijd 45-64", "leeftijd 65PL"),
    overlayGroups = "Gezondheidsinstelling met woonfunctie",
    options    = layersControlOptions(collapsed = FALSE))


saveWidget(kaart, file = file.path(path_result, paste0(Sys.Date(), "_wb_kaart_vaccinatie_opkomst_wijk.html")), 
           selfcontained = TRUE)
