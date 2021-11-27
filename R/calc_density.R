#' Calculate density maps
#' 
#' @description Calculate density maps along all the lifecycle by YEAR and MONTH
#'
#' @param ifile Character. Input filename. Generally as c_YYYYMM.txt
#' @param h Numeric. This is the bandwith. By default it is 1.5 
#' @param nx Integer. Bandwidth for x direction.
#' @param ny Integer. Bandwidth for x direction.
#' @param lims Vector. The limits of the rectangle covered by the grid.
#' @param islist Logical. If FALSE (default), returns a RasterBrick.
#' If TRUE, returns a list of RasterBricks. 
#' @return RasterBrick. If islist = TRUE, return list of RasterBricks 
#' @importFrom data.table fread
#' @importFrom MASS kde2d
#' @importFrom raster brick raster mean
#' @export
#' @examples \dontrun{
#' calc_density(ifile = "d_2000_2010.csv",
#'              nx = 800,                         # Rehbein (2021) 
#'              ny = 800,                         # Rehbein (2021)
#'              h = 2,                            # Rehbein (2021)
#'              lims = c(c(-82, -40), c(-23, 7))) # para a bacia Amazonica
#' }
calc_density <- function(ifile,
                         nx,
                         ny,
                         h,
                         lims,
                         islist = FALSE
) {
  
  lista_de_anos <- list()
  df <- data.table::fread(ifile)
  
  MONTHS <- unique(df$MONTH)[order(unique(df$MONTH))]
  
  for(i in seq_along(MONTHS)) {
    
    df2 <- df[MONTH == MONTHS[i]]
    
    YEARS <- unique(df$YEAR)[order(unique(df$YEAR))]
    
    raster::brick(lapply(seq_along(YEARS), function(j) {
      
      k <- MASS::kde2d(
        x = df2[YEAR == YEARS[j]]$XLON,
        y = df2[YEAR == YEARS[j]]$YLAT,
        h = h,                          
        n = c(nx, ny),                 
        lims = lims 
      )
      if(nrow(k$z) == 0) k$z <- matrix(rep(0, length(k$x)*length(k$y)), nx, ny)
      dx <- (k$x[2]-k$x[1])
      dy <- (k$y[2]-k$y[1])
      
      k$z <- k$z * nrow(df2[YEAR == YEARS[j]]) * dx*dy
      raster::raster(k)
    })) -> lista_de_anos[[i]]
  }
  
  if(islist){return(lista_de_anos)} 
  return(raster::brick(lapply(lista_de_anos, raster::mean, na.rm = TRUE)))
}
