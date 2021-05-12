#' Filter by maximum distance and minimum size (ForTraCC correction type filter)
#' 
#' @description ForTraCC is an automated method for tracking cloud systems. Two little deficiencies
#' were found for tracking with very large datasets for long periods. This function
#' is designed to eliminated the families that were erroneously included in the fam file.
#' In this sense, this filter calculates MCSs displacement between consecutive timestamps, 
#' filter out familieswith larger surrealistic displacements between two consecutive 
#' timesteps, and filter out families with size lower than the threshold 
#' predefined in fortracc_input.
#'
#' @param ifile Character. Input filename. Generally as c_YYYYMM.txt
#' @param ofile Character. Output filename. Sugested: d_YYYYMM.txt
#' @param min_size Numeric. This is the size threshold in pixels to exclude MCSs 
#' that ForTraCC erroneously inserted in the fam_ final files.
#' @param dist_max Numeric. Maximum distance in km allowed between two consecutive timesteps.
#' ForTraCC sometimes connects two no related cloud systems. This filter remove the 
#' entire family when the distance between two consecutive timesteps are larger than 
#' the threshold defined. By default it is 300 km (recommended for 30 min timesteps)
#' @param coords Vector of strings with the coordinates (longitude, latitude) field names.
#' By default it is c("XLON", "YLAT").
#' @return data.frame
#' @importFrom data.table fread fwrite
#' @importFrom geosphere distHaversine
#' @export
#' @examples \dontrun{
#' filtra_d(ifile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/c_200101010000.txt", 
#'          ofile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/d_200101010000.txt")
#' }
filtra_d <- function(ifile,
                     ofile,
                     min_size,
                     dist_max = 300,
                     coords = c("XLON", "YLAT")) {
  cat("Starting filtra_d... \n")
  
  x <- data.table::fread(ifile)
  
  
  if (nrow(x) > 0) {
    
  message("Calculating distances... (This may take some minutes)")
  x_ini <- x[[coords[1]]]
  y_ini <- x[[coords[2]]]
  x_fim <- x_ini[c(2:length(x_ini), NA)]
  y_fim <- y_ini[c(2:length(y_ini), NA)]
    
  df2 <- data.frame(ID = x$ID,
                    phase = x$phase,
                    x_ini,
                    y_ini,
                    x_fim,
                    y_fim)
  
  # Calculates the distance between coordinates in each timestep
  for (i in 1:nrow(df2)){
    df2$distkm[i] <- geosphere::distHaversine(c(df2$x_ini[i], df2$y_ini[i]),
                                             c(df2$x_fim[i], df2$y_fim[i]))/1000
  }
  # Set the last timestep of each ID to NA manually, because
  # there is no displacement in the last timestep.
  df2$distkm <- ifelse(df2$phase == "dissipation" & !is.na(df2$phase),
                       NA,
                       df2$distkm)
  x$dist_km <- df2$distkm
  
  # filter out IDs that with dist_km >  dist_max in at least one timestep of the MCSs
  x$criteria_length <- ifelse(as.numeric(x$dist_km) > dist_max,
                              "BAD", "GOOD")
  IDS <- x[x$criteria_length %in% "BAD", ]$ID
  
  if (length(IDS) == 0) {
    df <- x
  } else {
    df <- x[!x$ID%in%IDS,]
  }
  
  df$criteria_length <- NULL
  message("Finished.")
  
  # Filter by size
  message("Now, let's filter by size...")
  
  SIZE <- NULL
  IDs_out <- unique(df[SIZE < min_size]$ID)
  
  ID <- NULL 
  dff <- df[!ID%in%IDs_out, ]
  
  
  # Output:
  if(!missing(ofile)){
    data.table::fwrite(x = dff, file = ofile, quote = T, row.names = F)
  } else {
    return(dff)
  }
  
  } else {
    message("Este arquivo esta vazio. Nao ha nada o que fazer")
  }
}