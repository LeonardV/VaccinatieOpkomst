# auteur: Leonard Vanbrabant (GGD WB) en Arne Meeldijk (GGD BZO)
# datum aangemaakt: 10-05-2021
# datum laatst gewijzigd: 28-06-2021 door LV

## door gebruiker aan te passen

# selecteer de gemeentenamen uit je eigen regio
GM_GGD_regio <- c("Alphen-Chaam", "Altena", "Baarle-Nassau", "Bergen op Zoom",
                  "Breda", "Drimmelen", "Etten-Leur", "Geertruidenberg",
                  "Halderberge", "Moerdijk", "Oosterhout", "Roosendaal",
                  "Rucphen", "Steenbergen", "Woensdrecht", "Zundert",
                  "Dongen", "Gilze en Rijen", "Goirle", "Loon op Zand",
                  "Oisterwijk", "Tilburg", "Waalwijk", "Hilvarenbeek")

# laatst beschikbare datum vaccinatiedata. Haal je nieuwe data op obv de
# get_vaccinatieData() dan gebruik je de data van vandaag.
vac_datum <- "2021-06-29"


# laad paketten
library(sf)            # simple features
library(data.table)    # fread(), om grote data bestanden snel in te laden
library(robustbase)    # robust linaer models
library(leaflet)       # kaartjes
library(magrittr)      # %>%
library(ggplot2)       # plots
library(htmlwidgets)   # save leaflet kaart
library(RColorBrewer)  # voor mooie kleurtjes

## source files
# haal de laatste vaccinatie data uit coronit
#source("get_vaccinatieData.R")

# check of de data goed gevuld zijn
#tmp <- vac
#tmp$datum <- as.Date(tmp$dt_vaccinatie)
#aggregate(rep(1, nrow(tmp)), by = list(tmp$datum), sum)


# helper functie: verwijder letters van string en maak er een numerieke variabele van.
to_number <- function(codes) {
  return(as.numeric(sub("[a-zA-Z]+", "", codes)))
}

# set paths: aanpassing naar eigen lokaties
#path_shape <- "/home/leonardv/stack/ShapeFiles"
path_shape    <- "C:/Users/l.vanbrabant/stack/ShapeFiles" # e.g., .shp of .gpkg
path_data     <- "data" 
path_data_hvb <- "C:/Users/l.vanbrabant/Het Servicecentrum/Jajou, Rana - Bestanden voor Leonard"
path_result   <- "resultaten" 


# bronbestand: https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/wijk-en-buurtkaart-2020
st_layers(file.path(path_shape, "2020/WijkBuurtkaart_2020_v1.gpkg"))
bu_sf <- read_sf(file.path(path_shape, "2020/WijkBuurtkaart_2020_v1.gpkg"), layer = "cbs_buurten_2020")
wk_sf <- read_sf(file.path(path_shape, "2020/WijkBuurtkaart_2020_v1.gpkg"), layer = "cbs_wijken_2020")
gm_sf <- read_sf(file.path(path_shape, "2020/WijkBuurtkaart_2020_v1.gpkg"), layer = "gemeenten2020")



# -------------------------------------------------------------------------
## Correctie voor de nieuwe gebiedsindeling sinds 2021. 
# het aantal inwoners voor de wijk biezenmortel en haren moeten bij de gemeenten 
# tilburg en Oisterwijk respectievelijk opgeteld worden.
wk_code_biezen <- to_number(wk_sf[wk_sf$wijknaam %like% "Biezen", ]$wijkcode)
# aantal inwoners
a_inw_biezen      <- wk_sf[wk_sf$wijknaam %like% "Biezen", ]$aantal_inwoners
# aantal 0-15 jarigen
a_inw_biezen_0014 <- floor(a_inw_biezen * wk_sf[wk_sf$wijknaam %like% "Biezen", ]$percentage_personen_0_tot_15_jaar / 100)

wk_code_haaren <- to_number(wk_sf[wk_sf$wijknaam %like% "Haaren", ]$wijkcode)
a_inw_haaren      <- wk_sf[wk_sf$wijknaam %like% "Haaren", ]$aantal_inwoners
a_inw_haaren_0014 <- floor(a_inw_haaren * wk_sf[wk_sf$wijknaam %like% "Haaren", ]$percentage_personen_0_tot_15_jaar / 100)
# -------------------------------------------------------------------------

# wijk
# rename code en maak er een numerieke waarden van
bu_sf$BU_CODE <- to_number(bu_sf$buurtcode)
bu_sf$WK_CODE <- to_number(bu_sf$wijkcode)
bu_sf$GM_CODE <- to_number(bu_sf$gemeentecode)
wk_sf$WK_CODE <- to_number(wk_sf$wijkcode)
wk_sf$GM_CODE <- to_number(wk_sf$gemeentecode)
gm_sf$GM_CODE <- to_number(gm_sf$gemeentecode)

# rename naam
bu_sf$BU_NAAM <- bu_sf$buurtnaam
bu_sf$WK_NAAM <- bu_sf$wijknaam
bu_sf$GM_NAAM <- bu_sf$gemeentenaam
wk_sf$WK_NAAM <- wk_sf$wijknaam
wk_sf$GM_NAAM <- wk_sf$gemeentenaam
gm_sf$GM_NAAM <- gm_sf$gemeentenaam

# subset wijken ggd regio 
bu_sf <- subset(bu_sf, GM_NAAM %in% GM_GGD_regio)
bu_sf <- bu_sf[ , c("BU_CODE", "WK_CODE", "GM_CODE", "BU_NAAM", "WK_NAAM", "GM_NAAM")]

wk_sf <- subset(wk_sf, GM_NAAM %in% GM_GGD_regio)
wk_sf <- wk_sf[ , c("WK_CODE", "GM_CODE", "WK_NAAM", "GM_NAAM")]

gm_sf <- subset(gm_sf, GM_NAAM %in% GM_GGD_regio)
gm_sf <- gm_sf[ , c("GM_CODE", "GM_NAAM")]


# dit bestand bevat pc6 incl buurt-, wijk- en gemeentcode. Deze data koppelen
# we later aan de bu, wk en gm polygoon data. 
# bronbestand: https://www.cbs.nl/nl-nl/maatwerk/2020/39/buurt-wijk-en-gemeente-2020-voor-postcode-huisnummer
dfpchn <- fread(file.path(path_shape, "pc6hnr20200801_gwb.csv"))
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
# nu worden ook de geplande afsraken meegenomen. Deze data bevat wel meer ruis.
vac_df <- subset(vac, (status_afspraak == "GEPLAND" & status_deelname == "GEPLAND" & 
                         as.Date(dt_start_afspraak) >= Sys.Date() & ggd_regio == "GGD West-Brabant")|
                   (!is.na(dt_vaccinatie) & !is.na(nm_batchnummer) & vaccinatiestatus == "Vaccinatie gezet" &
                      (is.na(nr_vaccinatieronde) | nr_vaccinatieronde == 1)) &
                   ggd_regio == "GGD West-Brabant")

# verwijder dubbele bsn, zo houden we iedereen over die 1 vaccinatie heeft gehad
# of ingepland staat. De tweede afspraak is dus verwijderd. 
vac_df <- vac_df[!duplicated(vac_df$bsn), ]

## checks
# vaccinatie status is wel gezet: batchnr bestaat
# vac_df[vac_df$status_afspraak == "VERVALLEN", ]
# vac_df[vac_df$status_deelname == "GEANNULEERD", ]

# neem subset
vac_df <- vac_df[ , c("postcode", "huisnummer")]
names(vac_df) <- c("PC6", "Huisnummer")

# verwijder incomplete cases
vac_df <- vac_df[complete.cases(vac_df), ]

# voeg bu-, wk-, wk, en pc6-code toe aan vaccinatie data
buwkgm_vac_df <- merge(vac_df, dfpchn_gmnaam, by = c("PC6", "Huisnummer"), all.x = TRUE)

# aggregeer naar gebied
bu_vac <- aggregate(rep(1, nrow(buwkgm_vac_df)), by = list(buwkgm_vac_df$BU_CODE), sum)
names(bu_vac) <- c("BU_CODE", "A_VAC")

wk_vac <- aggregate(rep(1, nrow(buwkgm_vac_df)), by = list(buwkgm_vac_df$WK_CODE), sum)
  names(wk_vac) <- c("WK_CODE", "A_VAC")

gm_vac <- aggregate(rep(1, nrow(buwkgm_vac_df)), by = list(buwkgm_vac_df$GM_CODE), sum)
  names(gm_vac) <- c("GM_CODE", "A_VAC")
  


# laad HvB vaccinatie data ------------------------------------------------
# onze VR is groter dan onze GGD regio. Vanuit GGD HvB ontvangen we 8 gemeenten
# behorende bij onze VR.
# laad HvB data
bu_vac_hvb <- fread(file.path(path_data_hvb, "bu_HVB_MWB_Leonard_2021-06-29.csv"))
wk_vac_hvb <- fread(file.path(path_data_hvb, "wk_HVB_MWB_Leonard_2021-06-29.csv"))
gm_vac_hvb <- fread(file.path(path_data_hvb, "gm_HVB_MWB_Leonard_2021-06-29.csv"))

# rbind vaccinatiedata ggd WB en ggd HvB
bu_vac <- rbind(bu_vac, bu_vac_hvb)
wk_vac <- rbind(wk_vac, wk_vac_hvb)
gm_vac <- rbind(gm_vac, gm_vac_hvb)
# -------------------------------------------------------------------------

# add vaccinatiedata toe aan buurt, wijk en gemeente code
bu_sf_vac <- merge(bu_sf, bu_vac, by = "BU_CODE", all.x = TRUE)
wk_sf_vac <- merge(wk_sf, wk_vac, by = "WK_CODE", all.x = TRUE)
gm_sf_vac <- merge(gm_sf, gm_vac, by = "GM_CODE", all.x = TRUE)

# transformeer crs (coordinaten reference systeem) naar epsg:4326
# Dit is nodig om later de kaartjes te maken met leaflet(). 
bu_sf_vac <- st_transform(bu_sf_vac, 4326)
wk_sf_vac <- st_transform(wk_sf_vac, 4326)
gm_sf_vac <- st_transform(gm_sf_vac, 4326)

# kerncijfers buurt, wijk, gemeente
kwb_df <- fread(file.path(path_data, "kwb-2020.csv"))
bu_sf_vac_kwb <- merge(bu_sf_vac, kwb_df, by.x = "BU_CODE", by.y = "gwb_code_8", all.x = TRUE)
wk_sf_vac_kwb <- merge(wk_sf_vac, kwb_df, by.x = "WK_CODE", by.y = "gwb_code_8", all.x = TRUE)
gm_sf_vac_kwb <- merge(gm_sf_vac, kwb_df, by.x = "GM_CODE", by.y = "gwb_code_8", all.x = TRUE)


# moerdijk en steenberger zitten er twee keer in, shapefile 2020
#table(gm_sf_vac_kwb$GM_NAAM)
gm_sf_vac_kwb <- gm_sf_vac_kwb[-c(14, 22), ]

# bereken percentage gevaccineerden per bwg: we halen de 0-14 jarigen er eerst uit
bu_sf_vac_kwb$a_inw_corrected <- bu_sf_vac_kwb$a_inw - bu_sf_vac_kwb$a_00_14
wk_sf_vac_kwb$a_inw_corrected <- wk_sf_vac_kwb$a_inw - wk_sf_vac_kwb$a_00_14



# corrigeer voor biezenmortel en haren ------------------------------------
## biezenmortel en Haren horen bij tilburg en Oisterwijk, respectievelijk
# aantal 0-14 jarigen optellen
gm_sf_vac_kwb[gm_sf_vac_kwb$GM_NAAM == "Tilburg", ]$a_00_14 <- 
  gm_sf_vac_kwb[gm_sf_vac_kwb$GM_NAAM == "Tilburg", ]$a_00_14 + a_inw_biezen_0014 
gm_sf_vac_kwb[gm_sf_vac_kwb$GM_NAAM == "Oisterwijk", ]$a_00_14 <- 
  gm_sf_vac_kwb[gm_sf_vac_kwb$GM_NAAM == "Oisterwijk", ]$a_00_14 + a_inw_haaren_0014 

# totaal aantal optellen
gm_sf_vac_kwb[gm_sf_vac_kwb$GM_NAAM == "Tilburg", ]$a_inw <- 
  gm_sf_vac_kwb[gm_sf_vac_kwb$GM_NAAM == "Tilburg", ]$a_inw + a_inw_biezen
gm_sf_vac_kwb[gm_sf_vac_kwb$GM_NAAM == "Oisterwijk", ]$a_inw <- 
  gm_sf_vac_kwb[gm_sf_vac_kwb$GM_NAAM == "Oisterwijk", ]$a_inw + a_inw_haaren
# -------------------------------------------------------------------------

gm_sf_vac_kwb$a_inw_corrected <- gm_sf_vac_kwb$a_inw - gm_sf_vac_kwb$a_00_14

# alle aantallen inwoners voor bu, wk, gm met 0 worden op NA gezet.
bu_sf_vac_kwb$a_inw_corrected[bu_sf_vac_kwb$a_inw_corrected == 0] <- NA
wk_sf_vac_kwb$a_inw_corrected[wk_sf_vac_kwb$a_inw_corrected == 0] <- NA
gm_sf_vac_kwb$a_inw_corrected[gm_sf_vac_kwb$a_inw_corrected == 0] <- NA

# subtract aantal 0-14 jarigen van totaal aantal inwoners
bu_sf_vac_kwb$P_VAC <- bu_sf_vac_kwb$A_VAC / bu_sf_vac_kwb$a_inw_corrected * 100
wk_sf_vac_kwb$P_VAC <- wk_sf_vac_kwb$A_VAC / wk_sf_vac_kwb$a_inw_corrected * 100
gm_sf_vac_kwb$P_VAC <- gm_sf_vac_kwb$A_VAC / gm_sf_vac_kwb$a_inw_corrected * 100

# leeftijdspercentages bwg 
bu_sf_vac_kwb$P_65PL  <- bu_sf_vac_kwb$a_65_oo / bu_sf_vac_kwb$a_inw * 100
bu_sf_vac_kwb$P_4564  <- bu_sf_vac_kwb$a_45_64 / bu_sf_vac_kwb$a_inw * 100
bu_sf_vac_kwb$P_2544  <- bu_sf_vac_kwb$a_25_44 / bu_sf_vac_kwb$a_inw * 100
bu_sf_vac_kwb$P_1524  <- bu_sf_vac_kwb$a_15_24 / bu_sf_vac_kwb$a_inw * 100

wk_sf_vac_kwb$P_65PL  <- wk_sf_vac_kwb$a_65_oo / wk_sf_vac_kwb$a_inw * 100
wk_sf_vac_kwb$P_4564  <- wk_sf_vac_kwb$a_45_64 / wk_sf_vac_kwb$a_inw * 100
wk_sf_vac_kwb$P_2544  <- wk_sf_vac_kwb$a_25_44 / wk_sf_vac_kwb$a_inw * 100
wk_sf_vac_kwb$P_1524  <- wk_sf_vac_kwb$a_15_24 / wk_sf_vac_kwb$a_inw * 100

gm_sf_vac_kwb$P_65PL  <- gm_sf_vac_kwb$a_65_oo / gm_sf_vac_kwb$a_inw * 100
gm_sf_vac_kwb$P_4564  <- gm_sf_vac_kwb$a_45_64 / gm_sf_vac_kwb$a_inw * 100
gm_sf_vac_kwb$P_2544  <- gm_sf_vac_kwb$a_25_44 / gm_sf_vac_kwb$a_inw * 100
gm_sf_vac_kwb$P_1524  <- gm_sf_vac_kwb$a_15_24 / gm_sf_vac_kwb$a_inw * 100


# grandmean percentage gevaccineerden veiligheidsregio mwb, zonder 0-14 jarigen
grandMean <- sum(gm_sf_vac_kwb$A_VAC) / sum(gm_sf_vac_kwb$a_inw - gm_sf_vac_kwb$a_00_14) * 100
# 15-24
# grandMean_1524 <- sum(gm_sf_vac_kwb$A_VAC) / sum(gm_sf_vac_kwb$a_15_24) * 100
# # 25-44
# grandMean_2544 <- sum(gm_sf_vac_kwb$A_VAC) / sum(gm_sf_vac_kwb$a_25_44) * 100
# # 45-64
# grandMean_4564 <- sum(gm_sf_vac_kwb$A_VAC) / sum(gm_sf_vac_kwb$a_45_64) * 100
# # 65PL
# grandMean_65PL <- sum(gm_sf_vac_kwb$A_VAC) / sum(gm_sf_vac_kwb$a_65_oo) * 100


bu_sf_vac_kwb$diffLM     <- -1 * round((grandMean - bu_sf_vac_kwb$P_VAC), 2)
#bu_sf_vac_kwb$diffLM_inv <- -1 * bu_sf_vac_kwb$diffLM

wk_sf_vac_kwb$diffLM     <- -1 * round((grandMean - wk_sf_vac_kwb$P_VAC), 2)
#wk_sf_vac_kwb$diffLM_inv <- -1 * wk_sf_vac_kwb$diffLM

gm_sf_vac_kwb$diffLM     <- -1 * round((grandMean - gm_sf_vac_kwb$P_VAC), 2)
#gm_sf_vac_kwb$diffLM_inv <- -1 * gm_sf_vac_kwb$diffLM



# -------------------------------------------------------------------------
#tmp <- wk_sf_vac_kwb[ , c("diffLM_inv", "P_65PL", "P_4564", "P_2544", "P_1524")]
#tmp$geometry <- NULL
#cor(tmp, use = "pairwise.complete.obs")

# fit_bu <- lmrob(diffLM_inv ~ P_65PL + P_4564 + P_2544 + P_1524,  
#                 data = bu_sf_vac_kwb, method = "MM", setting = "KS2014")
# summary(fit_bu)
# 
# 
# fit_wk <- lmrob(diffLM_inv ~ P_65PL + P_4564 + P_2544 + P_1524,  
#                 data = wk_sf_vac_kwb, method = "MM", setting = "KS2014")
# 
# summary(fit_wk)
# 
# 
# fit_gm <- lmrob(diffLM_inv ~ P_65PL + P_4564 + P_2544 + P_1524,  
#                 data = gm_sf_vac_kwb, method = "MM", setting = "KS2014")
# summary(fit_gm)

# -------------------------------------------------------------------------


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


# AZC locaties in NL: lat/lon
AZC <- read.csv2("data/azc_geocoded.csv")
AZC$lon <- as.numeric(AZC$lon)
AZC$lat <- as.numeric(AZC$lat)

# point <- data.frame(lon = AZC$lon, lat = AZC$lat)
# azc_pnts_sf <- st_as_sf(point, coords = c('lon', 'lat'), crs = st_crs(gm_sf_vac_kwb))
# tmp <- st_sf(st_geometry(gm_sf_vac_kwb))
# str(st_join(azc_pnts_sf, tmp, join = st_intersects))



# Hier maken we de kaarten ------------------------------------------------

# add layer id
bu_sf_vac_kwb$Id  <- 1:nrow(bu_sf_vac_kwb)
wk_sf_vac_kwb$Id  <- 1:nrow(wk_sf_vac_kwb)
gm_sf_vac_kwb$Id  <- 1:nrow(gm_sf_vac_kwb)

quant <- qnorm(c(.025, .05, .25, .50, .75, .95, .975))

bu_sf_vac_kwb$bu_z <- (bu_sf_vac_kwb$diffLM) / 
  sd(bu_sf_vac_kwb$diffLM, na.rm = TRUE)
bu_sf_vac_kwb$bins <- cut(bu_sf_vac_kwb$bu_z, breaks = c(-Inf, quant, Inf), 
                          include.lowest = TRUE, right = TRUE,
                          labels = 1:8)

wk_sf_vac_kwb$wk_z <- (wk_sf_vac_kwb$diffLM) / 
  sd(wk_sf_vac_kwb$diffLM, na.rm = TRUE)
wk_q <- qnorm(c(.025, .05, .25, .50, .75, .95, .975))
wk_sf_vac_kwb$bins <- cut(wk_sf_vac_kwb$wk_z, breaks = c(-Inf, quant, Inf), 
                          include.lowest = TRUE, right = TRUE,
                          labels = 1:8)

gm_sf_vac_kwb$gm_z <- (gm_sf_vac_kwb$diffLM) / 
  sd(gm_sf_vac_kwb$diffLM, na.rm = TRUE)
gm_q <- qnorm(c(.025, .05, .25, .50, .75, .95, .975))
gm_sf_vac_kwb$bins <- cut(gm_sf_vac_kwb$gm_z, breaks = c(-Inf, quant, Inf), 
                          include.lowest = TRUE, right = TRUE,
                          labels = 1:8)


colorPal <- c(rev(brewer.pal(8, "Reds")[3:6]), brewer.pal(8, "Greens")[5:8])
pal <- colorNumeric(colorPal, na.color = "#FFFFFF", domain = 1:8)
#show_col(pal(1:8))
#show_col(colorPal)


# pal_bu  <- colorFactor(colorRamp(c("green", "red")), na.color = "#FFFFFF", 
#                        domain = bu_sf_vac_kwb$bins, ordered = TRUE)
# 
# pal_wk  <- colorFactor(colorRamp(c("green", "red")), na.color = "#FFFFFF", 
#                        domain = wk_sf_vac_kwb$bins, ordered = TRUE)
# 
# pal_gm  <- colorFactor(colorRamp(c("green", "red")), na.color = "#FFFFFF", 
#                        domain = gm_sf_vac_kwb$bins, ordered = TRUE)


bu_sf_vac_kwb$P_1524[bu_sf_vac_kwb$P_1524 == 0] <- NA
bu_sf_vac_kwb$P_2544[bu_sf_vac_kwb$P_2544 == 0] <- NA
bu_sf_vac_kwb$P_4564[bu_sf_vac_kwb$P_4564 == 0] <- NA
bu_sf_vac_kwb$P_65PL[bu_sf_vac_kwb$P_65PL == 0] <- NA

wk_sf_vac_kwb$P_1524[wk_sf_vac_kwb$P_1524 == 0] <- NA
wk_sf_vac_kwb$P_2544[wk_sf_vac_kwb$P_2544 == 0] <- NA
wk_sf_vac_kwb$P_4564[wk_sf_vac_kwb$P_4564 == 0] <- NA
wk_sf_vac_kwb$P_65PL[wk_sf_vac_kwb$P_65PL == 0] <- NA

gm_sf_vac_kwb$P_1524[gm_sf_vac_kwb$P_1524 == 0] <- NA
gm_sf_vac_kwb$P_2544[gm_sf_vac_kwb$P_2544 == 0] <- NA
gm_sf_vac_kwb$P_4564[gm_sf_vac_kwb$P_4564 == 0] <- NA
gm_sf_vac_kwb$P_65PL[gm_sf_vac_kwb$P_65PL == 0] <- NA


## maak labels
labels_bu <-
  paste(sprintf("<h3><b>%s</b></h5>", bu_sf_vac_kwb$BU_NAAM),
        sprintf("<h4>Percentage gevaccineerd door/afspraak gepland bij GGD: %s</h6>", sprintf("%.1f", bu_sf_vac_kwb$P_VAC)),
        sprintf("<h4>Gemiddeld percentage gevaccineerd VRMWB: %s</h6>"              , sprintf("%.1f", grandMean)),
        sprintf("<h4>Aantal inwoners 15+: %s</h6>"                                  , sprintf("%s"  , bu_sf_vac_kwb$a_inw_corrected)),
        sprintf("<h4>Aantal gevaccineerd door/afspraak gepland bij GGD: %s</h6>"    , sprintf("%s"  , bu_sf_vac_kwb$A_VAC)),
        sprintf("<h5>Percentage inwoners 15-24 jarigen: %s</h6>"                    , sprintf("%.1f", bu_sf_vac_kwb$P_1524)),
        sprintf("<h5>Percentage inwoners 25-44 jarigen: %s</h6>"                    , sprintf("%.1f", bu_sf_vac_kwb$P_2544)),
        sprintf("<h5>Percentage inwoners 45-64 jarigen: %s</h6>"                    , sprintf("%.1f", bu_sf_vac_kwb$P_4564)),
        sprintf("<h5>Percentage inwoners 65+: %s</h6>"                              , sprintf("%.1f", bu_sf_vac_kwb$P_65PL))
  ) %>%
  lapply(htmltools::HTML)

labels_wk <-
  paste(sprintf("<h3><b>%s</b></h5>", wk_sf_vac_kwb$WK_NAAM),
        sprintf("<h4>Percentage gevaccineerd door/afspraak gepland bij GGD: %s</h6>", sprintf("%.1f", wk_sf_vac_kwb$P_VAC)),
        sprintf("<h4>Gemiddeld percentage gevaccineerd VRMWB: %s</h6>"               , sprintf("%.1f", grandMean)),
        sprintf("<h4>Aantal inwoners 15+: %s</h6>"                                   , sprintf("%s"  , wk_sf_vac_kwb$a_inw_corrected)),
        sprintf("<h4>Aantal gevaccineerd door/afspraak gepland bij GGD: %s</h6>"     , sprintf("%s"  , wk_sf_vac_kwb$A_VAC)),
        sprintf("<h5>Percentage inwoners 15-24 jarigen: %s</h6>"                     , sprintf("%.1f", wk_sf_vac_kwb$P_1524)),
        sprintf("<h5>Percentage inwoners 25-44 jarigen: %s</h6>"                     , sprintf("%.1f", wk_sf_vac_kwb$P_2544)),
        sprintf("<h5>Percentage inwoners 45-64 jarigen: %s</h6>"                     , sprintf("%.1f", wk_sf_vac_kwb$P_4564)),
        sprintf("<h5>Percentage inwoners 65+: %s</h6>"                               , sprintf("%.1f", wk_sf_vac_kwb$P_65PL))
  ) %>%
  lapply(htmltools::HTML)

labels_gm <-
  paste(sprintf("<h3><b>%s</b></h5>", gm_sf_vac_kwb$GM_NAAM),
        sprintf("<h4>Percentage gevaccineerd door/afspraak gepland bij GGD: %s</h6>", sprintf("%.1f", gm_sf_vac_kwb$P_VAC)),
        sprintf("<h4>Gemiddeld percentage gevaccineerd VRMWB: %s</h6>"              , sprintf("%.1f", grandMean)),
        sprintf("<h4>Aantal inwoners 15+: %s</h6>"                                  , sprintf("%s"  , gm_sf_vac_kwb$a_inw_corrected)),
        sprintf("<h4>Aantal gevaccineerd door/afspraak gepland bij GGD: %s</h6>"    , sprintf("%s"  , gm_sf_vac_kwb$A_VAC)),
        sprintf("<h5>Percentage inwoners 15-24 jarigen: %s</h6>"                    , sprintf("%.1f", gm_sf_vac_kwb$P_1524)),
        sprintf("<h5>Percentage inwoners 25-44 jarigen: %s</h6>"                    , sprintf("%.1f", gm_sf_vac_kwb$P_2544)),
        sprintf("<h5>Percentage inwoners 45-64 jarigen: %s</h6>"                    , sprintf("%.1f", gm_sf_vac_kwb$P_4564)),
        sprintf("<h5>Percentage inwoners 65+: %s</h6>"                              , sprintf("%.1f", gm_sf_vac_kwb$P_65PL))
  ) %>%
  lapply(htmltools::HTML)



# bepaal het midden van de kaart. Dit gebruiken we om de kaart te centeren
centroid <- st_centroid(st_union(gm_sf_vac_kwb$geometry))

kaart <- leaflet() %>% 
  setView(lng = centroid[[1]][1], lat = centroid[[1]][2], zoom = 10) %>% 
  addMapPane(name = "ames_polygons", zIndex = 200) %>% 
  addTiles() %>%
  addPolygons(data        = bu_sf_vac_kwb,
              color       = "black",
              fillColor   = ~pal(as.numeric(bu_sf_vac_kwb$bins)),
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
              fillColor   = ~pal(as.numeric(wk_sf_vac_kwb$bins)),
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
              fillColor   = ~pal(as.numeric(gm_sf_vac_kwb$bins)),
              opacity     = 1,
              weight      = 1,
              fillOpacity = 0.5,
              layerId     = gm_sf_vac_kwb$Id,
              group       = "Gemeente",
              popup       = labels_gm,
              highlightOptions = highlightOptions(color = "black", weight = 3,
                                                  bringToFront = TRUE),
              options = pathOptions(pane = "ames_polygons")) %>%
  
  addMarkers(data = BAG, group = "Gezondheidsinstelling met woonfunctie",
             clusterOptions = markerClusterOptions()) %>%
  addMarkers(lat = AZC$lat, lng = AZC$lon, group = "AZC") %>%
  addLayersControl(
    baseGroups = c("Buurt", "Wijk", "Gemeente"),
    overlayGroups = c("Gezondheidsinstelling met woonfunctie", "AZC"), 
    options    = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Gezondheidsinstelling met woonfunctie", "AZC"))


kaart

# saveWidget(kaart, file = file.path(path_result, paste0(Sys.Date(), "_wb_kaart_vaccinatie_opkomst_bwg.html")),
#            selfcontained = TRUE)
