# auteur: Arne Meeldink (GGD BZO)
# datum aangemaakt: 24-05-2021
# datum laatst gewijzigd: 25-05-2021 door Leonard Vanbrabant. 

#### EXCEL per GEMEENTE
library(openxlsx)
library(data.table)

# imputeer missings voor het kerncijfer bestand pc4. 
# De warnings kunnen genegeerd worden. 
#source("Imputeer kenmerkenbestand.R")

# specifeer data paths
data_path <- "data"
tabel <- fread(file.path(data_path, "pc4_sf_vac_kwb_cleaned.csv"))

vnames <- c("GM_NAAM","PC4","INWONER","P_VAC","pred","diffLM","P_65PL",
            "P_4564","P_2544","P_1524","P_ZORG","P_VROUW",
            "P_NW_MIG_A","P_WE_MIG_A","WOZWONING","P_KOOPWON","P_UITKMINAOW")

tabel <-  tabel[, ..vnames]

names(tabel) <- c("Gemeente", "Postcode", "Aantal_inwoners", "Gevaccineerd_percentage",
                  "Gevaccineerd_voorspeld", "Gevaccineerd_afwijking", 
                  "Procent_65_plus", "Procent_45_64jaar", "Procent_25_44jaar",
                  "Procent_15_24jaar", "Procent_zorg", "Procent_vrouw", 
                  "Niet_westers_migratie_percentage", "Westers_migratie_percentage",
                  "WOZwaarde", "Procent_koopwoning", "Percentage_met_uitkering")


vnames2 <- c("Gevaccineerd_percentage",
             "Gevaccineerd_voorspeld", "Gevaccineerd_afwijking", 
             "Procent_65_plus", "Procent_45_64jaar", "Procent_25_44jaar",
             "Procent_15_24jaar", "Procent_zorg", "Procent_vrouw", 
             "Niet_westers_migratie_percentage", "Westers_migratie_percentage",
             "Procent_koopwoning", "Percentage_met_uitkering")

# rond percentages af op 1 decimaal
tmp <- lapply(tabel[, ..vnames2], FUN = function(x) { sprintf("%.1f", x) } ) 

# hier plakken we de data weer aan elkaar
tabel <- data.frame(tabel[, c("Gemeente", "Postcode", "Aantal_inwoners", "WOZwaarde")], tmp)
 

# hier worden de tabellen boeken per gemeente gemaakt uitgesplitst naar pc4
output <- split(tabel, tabel$Gemeente)
wb <- createWorkbook()

for (i in 1:length(output)) {
  addWorksheet(wb, sheetName = names(output[i]))
  writeData(wb, sheet = names(output[i]), x = output[[i]]) 
}

saveWorkbook(wb, file.path(data_path, "Tabellenboek_vaccinatie_gemeente.xlsx"), overwrite = TRUE)



