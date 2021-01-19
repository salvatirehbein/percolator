#' Filtra SCMs que interagiram em alguma regiao de interesse. Por exemplo: bacia Amazonica.
#' Tambem separa oceanicos e continentais.
#'
#' @description This function reads the output from read_family.R to filter MCSs
#' over a region of interest
#'
#' @param arqi Character. Input filename. Ex: a_YYYY.csv
#' @param arqo Character. Output filename. Ex: b_YYYY.csv
#' @param focean Logical. Should I indicate the oceanic MCSs? TRUE by default.
#' @param pregion Character. Set the name of the shapefile of the 
#' region to filter the MCSs. Amazon basin by default.
#' @param psa Character. Set the name of the shapefile for filtering 
#' oceanic MCSs. South America contours by default
#' @return data.frame
#' @importFrom data.table fread fwrite `:=` fifelse as.data.table 
#' @importFrom sf st_read st_intersects st_as_sf st_set_geometry st_crs
#' @export
#' @examples \dontrun{
#' # Do not run
#' filtra_b(arqi = "../a_200101.csv",
#'          arqo = "../b_200101.csv")
#' }
filtra_b <- function(arqi,
                     arqo,
                     focean = TRUE,
                     pregion = system.file("extdata", 
                                          "BACIA/bacia_amazonica_contorno.shp", 
                                          package = "percolator"),
                     psa = system.file("extdata", 
                                       "SA/sa.shp", 
                                       package = "percolator")) {
  cat("Starting filter b... \n")
  ff <- data.table::fread(arqi, fill = TRUE)
  # I added the below because after filtering the original columns disappear:
  ff$LATY <- ff$XLAT
  ff$LONX <- ff$XLON
  ff <- sf::st_as_sf(ff, coords = c("XLON", "XLAT"), crs = 4326)
  
  # Read maps:
  region <- sf::st_read(pregion)
  sa <- sf::st_read(psa)
  
  sf::st_crs(ff) == sf::st_crs(region) # TRUE
  
  # Passa ou nao pela region of interest:
  x <- sf::st_intersects(ff, region, sparse = FALSE)
  
  ff$is_in_region <- x[, 1]
  ft <- data.table::as.data.table(sf::st_set_geometry(ff, NULL))
  # Obtem os IDs dos scms que em algum momento estiveram na region
  cat("ID's that were over the area of interest in any moment of the MCS lifespan \n")
  ids <- unique(ft[ft$is_in_region]$ID)
  print(paste0("MCSs over the Amazon basin: ", length(ids)))
  tt <- ft[ID %in% ids, ]
  tt$origem <- NA
  
  rm(ff, x, region, ft, ids)
  # plot(ff[ff$ID == tt$ID[1], "SIZE"], axes = T,  pch = 16, reset = F, lwd = 5)
  # plot(ff[ff$ID == tt$ID[1], ]$geometry, add = T, type = "l", col = "red")
  # plot(region$geometry, add = T)
  
  # OCEANICOS ####
  if (focean == TRUE) {
    cat("Starting filtering Oceanic MCSs \n")
  } else {
    data.table::fwrite(tt,file = arqo, row.names = FALSE)
  }
  
  tt$XLAT <- tt$LATY
  tt$XLON <- tt$LONX
  ff <- sf::st_as_sf(tt, coords = c("XLON", "XLAT"), crs = 4326)
  x <- sf::st_intersects(ff, sa, sparse = FALSE)
  tt$is_in_sa <- x[, 1]
  # selecionar os SCMs que nasceram fora da America do Sul
  ids <- unique(tt[is_in_sa == FALSE & TIME == min(TIME)]$ID)
  print(paste0("MCSs with genesis over the ocean: ", length(ids)))
  # to <- tt[ID %in% ids, ]
  # Testar plotando:
  # tof <- sf::st_as_sf(to, coords = c("XLON", "XLAT"), crs = 4326)
  # mapview::mapview(tof[tof$ID %in% unique(tof$ID), "ID"])
  
  
  tt[, origem := data.table::fifelse(ID %in% ids, "oceano", "continente")]
  tt$is_in_region <- tt$is_in_sa <- NULL
  
  if(!missing(arqo)){
    data.table::fwrite(tt,
                       file = arqo,
                       row.names = FALSE)
  } else {
    return(tt)
  }
}