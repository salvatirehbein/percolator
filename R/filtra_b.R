#' Filter MCSs by area of interest
#'
#' @description This function reads the output from filtra_a function, filter MCSs
#' over an area of interest (for instance Amazon basin), classifies the MCSs by
#' their origin (for instance oceanic or continental), and then returns the output 
#' in data.table format or saved according to the ofile argument.
#'
#' @param ifile Character. Input filename. Ex: a_YYYY.csv
#' @param ofile Character. Output filename. Ex: b_YYYY.csv
#' @param focean Logical. Should I indicate the oceanic MCSs? TRUE by default.
#' @param aoi Character. Set the name of the shapefile of the 
#' region to filter the MCSs.
#' @param continental_shp Character. Set the name of the shapefile for filtering 
#' oceanic MCSs. South America contours by default.
#' @return data.table
#' @importFrom data.table fread fwrite `:=` fifelse as.data.table 
#' @importFrom sf st_read st_intersects st_as_sf st_set_geometry st_crs
#' @export
#' @examples \dontrun{
#' # Do not run
#' filtra_b(ifile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/a_200101010000.csv", 
#'          aoi = system.file("extdata",  
#'                            "BACIA/bacia_amazonica_contorno.shp",
#'                            package = "percolator"), 
#'          focean = TRUE)
#' }
filtra_b <- function(ifile,
                     ofile,
                     aoi,
                     focean = TRUE,
                     continental_shp = system.file("extdata", 
                                                   "SA_SEM_FOZ/sa.shp", 
                                                   package = "percolator")) {
  message("Starting filter b...")
  ff <- data.table::fread(ifile, fill = TRUE)
  # I added the below because after filtering the original columns disappear:
  ff$LATY <- ff$YLAT
  ff$LONX <- ff$XLON
  ff <- sf::st_as_sf(as.data.frame(ff), 
                     coords = c("LONX", "LATY"), 
                     crs = 4326)
  
  # Read maps:
  region <- sf::st_read(aoi)
  sa <- sf::st_read(continental_shp)
  
  # sf::st_crs(ff) == sf::st_crs(region) # TRUE
  
  cat(paste0("\n Total initial families: ", 
             length(unique(ff$ID))),"\n")
  
  # Passa ou nao pela region of interest:
  x <- sf::st_intersects(ff, region, sparse = FALSE)
  
  ff$is_in_region <- x[, 1]
  
  ft <- data.table::as.data.table(sf::st_set_geometry(ff, NULL))
  
  # Obtem os IDs dos scms que em algum momento estiveram na region
  cat("ID's over the area of interest: \n")
  # is_in_region <- NULL EXCEPTO SE FOR LOGICAL!!!!!!!!!!!!!!!!
  ids <- unique(ft[ft$is_in_region]$ID)
  print(length(ids))
  tt <- ft[ft$ID %in% ids, ]
  # tt$is_in_region <- NULL
  dim(ft)
  dim(tt) #menos id
  
  # plot(ff[ff$ID == tt$ID[1], "SIZE"], axes = T,  pch = 16, reset = F, lwd = 5)
  # plot(ff[ff$ID == tt$ID[1], ]$geometry, add = T, type = "l", col = "red")
  # plot(region$geometry, add = T)
  rm(ff, x, region, ft, ids)
  invisible(gc())
  
  # OCEANICOS ####
  if(focean == TRUE){
    message("Starting filtering Oceanic MCSs...")
    
    # tt$LATY <- tt$YLAT
    # tt$LONX <- tt$XLON
    phase <- NULL
    ff <- sf::st_as_sf(as.data.frame(tt[phase == "genesis"]),
                       coords = c("XLON", "YLAT"), 
                       crs = 4326)
    
    # selecionar os SCMs que nasceram fora da America do Sul
    x <- sf::st_intersects(ff, sa, sparse = FALSE)
    ff$is_in_sa <- x[, 1]   # Se TRUE, nasceu no continente. Se FALSE, nasceu no oceano.
    ids <- unique(ff[ff$is_in_sa,]$ID) # Seleciona somente os que nasceram no continente.
    tt[, origin := data.table::fifelse(ID %in% ids, "continental", "oceanic")]
    
    message("Done!")
  } else {
    tt$origin <- NA
  }
  
  
  
  if(!missing(ofile)){
    data.table::fwrite(tt,
                       file = ofile,
                       row.names = FALSE)
  } else {
    return(tt)
  }
}
