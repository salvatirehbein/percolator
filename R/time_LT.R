#' Calculate the Local Time for each timestep of the MCSs
#'
#' @description Calculate the Local Time for each timestep of the MCSs, 
#' using (by default) Natural Earth timezones.
#'
#' @param x data.table or data.frame with lat and long coordinates.
#' @param y String. Input file with the timezones in shapefile format. By default 
#' y is Natural Earth timezones 
#' @param coords vector of strings. Coordinates names (longitude first).
#' \url{www.naturalearthdata.com/downloads/10m-cultural-vectors/timezones/}
#' @return data.frame
#' @importFrom sf sf_use_s2
#' @export
#' @examples \dontrun{
#' }
LT <- function(x,
               y = system.file("extdata",  
                               "ne_10m_time_zones/ne_10m_time_zones.shp",
                               package = "percolator"),
               coords = c("XLON", "YLAT")){
    # Para resolver o problema:
    # "Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
    # Loop 0 is not valid: Edge 672 crosses edge 707"
    # Usei a solucao: https://gis.stackexchange.com/questions/404385/r-sf-some-edges-are-crossing-in-a-multipolygon-how-to-make-it-valid-when-using
    sf::sf_use_s2(FALSE) 
  
    d_SCM <- x
    timezones <- sf::st_read(y)
    
    d_sf_SCM <- sf::st_as_sf(d_SCM, 
                             coords = coords, 
                             crs = 4326)
    intersect_SCM_timez <- sf::st_intersects(d_sf_SCM, 
                                             timezones, 
                                             sparse = F)
    
    # Temos uma matriz de interseccao com 1 milhao de linhas x 120 colunas
    df <- data.frame(colsums = colSums(intersect_SCM_timez, na.rm = TRUE))
    df$id <- 1:length(df$colsums)
    colunas <- df[df$colsums > 0,]$id # estas sao as colunas que precisamos
    
    tz <- lapply(seq_along(colunas), function (i){
      data.frame(tz = ifelse(intersect_SCM_timez[,colunas[i]] == TRUE, 
                             timezones[colunas[i],]$zone,
                             9999))
    })
    tt <- do.call("cbind", tz)
    tt$soma <- rowSums(tt)
    tt$soma2 <- tt$soma - ((length(colunas)-1)*9999)
    tt$soma3 <- ifelse(tt$soma2 > 100, NA, tt$soma2)
    summary(tt$soma3)
    
    # d_SCM$tz <- tt$soma3
    # d_SCM$timeLT <- d_SCM$timeUTC + d_SCM$tz*60*60
    # d_sf_SCM$tz <- tt$soma3
    # d_sf_SCM$timeLT <- d_sf_SCM$timeUTC + d_sf_SCM$tz*60*60
    return(tt$soma3)
}
