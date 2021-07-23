origAddress <- read.csv2("data/vaccinatie_locaties_geocode.csv")

path_shape    <- "C:/Users/l.vanbrabant/stack/ShapeFiles" # e.g., .shp of .gpkg
bu_sf <- read_sf(file.path(path_shape, "2020/WijkBuurtkaart_2020_v1.gpkg"), layer = "cbs_buurten_2020")

GM_GGD_regio <- c("Alphen-Chaam", "Altena", "Baarle-Nassau", "Bergen op Zoom",
                  "Breda", "Drimmelen", "Etten-Leur", "Geertruidenberg",
                  "Halderberge", "Moerdijk", "Oosterhout", "Roosendaal",
                  "Rucphen", "Steenbergen", "Woensdrecht", "Zundert")

df <- subset(bu_sf, gemeentenaam %in% GM_GGD_regio)
df$buurtcode <- to_number(df$buurtcode)

bu_centroids <- st_centroid(df$geom)
bu_centroids <- st_transform(bu_centroids, 4326)

df_dist <- df[c("buurtcode")] 
for (i in 1:nrow(origAddress)) {
  cat("iteration = ", i, "\n") 
  lon <- origAddress$lon[i]
  lat <- origAddress$lat[i]
  vac.sfg <- st_point(c(lon, lat), dim = "XY")
  vac.sfc <- st_sfc(vac.sfg, crs = 4326)
  
  dist <- data.frame(c(st_distance(bu_centroids, vac.sfc) / 1000))
  distance <- c(dist$c.st_distance.bu_centroids..vac.sfc..1000.)
  
  x <- distance[]
  vac_km <- data.frame(stringr::str_extract(x, "[0-9]*.[0-9]."))
  # naam vaccinatielocatie
  names(vac_km) <- origAddress$address[i]
  
  #df2 <- df[c(6,4,2)] 
  df_dist <- cbind(df_dist, vac_km)   
}


df_dist$geom <- NULL
df_dist <- lapply(df_dist, as.numeric)
df_dist <- as.data.frame(df_dist)

min_distance <- apply(df_dist[, -1], 1, min)
result <- cbind(df, min_distance)
result <- result[, c("buurtcode", "min_distance")]


