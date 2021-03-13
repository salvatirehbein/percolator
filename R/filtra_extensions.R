#' Filter by minimum extention (precipitation Houze) 
#' 
#' @description Houze --> MCSs with area greater than 100 km in at least one direction.
#' To be used only with precipitation data.
#' @param ifile Character. Input filename. Generally as c_YYYYMM.txt
#' @param ofile Character. Output filename. Sugested: d_YYYYMM.txt
#' @param re Numeric. Minimum effective radio for filter MCSs. 
#' @param pixels_size Numeric. Pixels size in km2. Change from date to date.
#' @return data.frame
#' @importFrom data.table fread fwrite
#' @export
#' @examples \dontrun{
#' filtra_extent(ifile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/d_200101010000.csv", 
#'          ofile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/APAGAR.txt",
#'          pixels_size = 11*11)
#' }
filtra_extent <- function(ifile,
                     ofile,
                     re = 50,
                     pixels_size) {
  cat("Starting filtra_e... \n")
  
  x <- data.table::fread(ifile)
  x$SIZE_km <- (x$SIZE)*pixels_size
  
  # Filter by size
  SIZE_km <- NULL
  Ae <- pi*re^2
  IDs <- unique(x[SIZE_km >= Ae]$ID)
  
  ID <- NULL 
  df <- x[ID%in%IDs, ]
  
  
  # Output:
  if(!missing(ofile)){
    data.table::fwrite(x = df, file = ofile, quote = T, row.names = F)
  } else {
    return(dff)
  }
}