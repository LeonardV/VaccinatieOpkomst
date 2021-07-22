BU_df  <- read.csv2(file.path("data", paste0(Sys.Date(), "_buurt.csv")))
WK_df  <- read.csv2(file.path("data", paste0(Sys.Date(), "_wijk.csv")))
GM_df  <- read.csv2(file.path("data", paste0(Sys.Date(), "_gemeente.csv")))
#PC4_df <- read.csv2(file.path("data", paste0(Sys.Date(), "_pc4.csv")))

# selecteer de gemeentenamen uit je eigen regio
GM_GGD_WB <- c("Alphen-Chaam", "Altena", "Baarle-Nassau", "Bergen op Zoom",
               "Breda", "Drimmelen", "Etten-Leur", "Geertruidenberg",
               "Halderberge", "Moerdijk", "Oosterhout", "Roosendaal",
               "Rucphen", "Steenbergen", "Woensdrecht", "Zundert")

# filter op bu, wk, gm uit eigen regio
BU_df <- subset(BU_df, GM_NAAM %in% GM_GGD_WB)
WK_df <- subset(WK_df, GM_NAAM %in% GM_GGD_WB)
GM_df <- subset(GM_df, GM_NAAM %in% GM_GGD_WB)

grandMean <- round(sum(GM_df$A_VAC) / sum(GM_df$a_inw_corrected) * 100, 2)

# names(BU_df)
# names(WK_df)
# names(GM_df)
#names(PC4_df)

vnames_bu  <- c("BU_NAAM", "WK_NAAM", "gm_naam", "a_inw_corrected", "A_VAC", "P_VAC", "diffLM")
vnames_wk  <- c("WK_NAAM", "GM_NAAM", "a_inw_corrected", "A_VAC", "P_VAC", "diffLM")
vnames_gm  <- c("GM_NAAM", "a_inw_corrected", "A_VAC", "P_VAC", "diffLM")
#vnames_pc4 <- c("BU_NAAM", "WK_NAAM", "gm_naam", "a_inw", "A_VAC", "P_VAC", "diffLM")

bu <- BU_df[, vnames_bu]
wk <- WK_df[, vnames_wk]
gm <- GM_df[, vnames_gm]
#bu <- bu_sf_vac_kwb[, vnames_bu]


#bu[order(bu$diffLM), ]
wk <- wk[order(wk$diffLM), ]
gm <- gm[order(gm$diffLM), ]


vnames_wk_new <- c("Wijknaam", "Gemeentenaam", "Aantal inwoners 15+",
                   "Aantal gevaccineerden", "Percentage gevaccineerden", 
                   "Percentueelverschil met GGD WB")

vnames_gm_new <- c("Gemeentenaam", "Aantal inwoners 15+",
                   "Aantal gevaccineerden", "Percentage gevaccineerden", 
                   "Percentueelverschil met GGD WB")


names(wk) <- vnames_wk_new
names(gm) <- vnames_gm_new

wk$`Percentage gevaccineerden` <- round(wk$`Percentage gevaccineerden`, 2)
gm$`Percentage gevaccineerden` <- round(gm$`Percentage gevaccineerden`, 2)

rownames(wk) <- NULL
rownames(gm) <- NULL


sink('resultaten/wijken.html')
cat("Gemiddelde percentage gevaccineerden GGD WB = ", grandMean)
knitr::kable(wk, format = "html", caption = "Wijken GGD WB")
sink()


sink('resultaten/gemeenten.html') 
cat("Gemiddelde percentage gevaccineerden GGD WB = ", grandMean)
knitr::kable(gm, format = "html", caption = "Gemeenten GGD WB")
sink()

