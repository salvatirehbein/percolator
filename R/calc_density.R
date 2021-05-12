#' Calculate density maps
#' 
#' @description Calculate density maps according to the lifecycle or for a all 
#' lifecycle.
#'
#' @param ifile Character. Input filename. Generally as c_YYYYMM.txt
#' @param ofile Character. Output filename. Sugested: d_YYYYMM.txt
#' @param h Numeric. This is the bandwith. By default it is 1.5 
#' @return data.frame
#' @importFrom data.table fread fwrite
#' @importFrom geosphere distHaversine
#' @export
#' @examples \dontrun{
#' filtra_d(ifile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/d_", 
#'          ofile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/d_200101010000.txt")
#' }
#' 
# ifile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/d_20"
# x <- data.table::fread(ifile)$XLON
# y <- data.table::fread(ifile)$YLAT
# 
# filtra_d <- function(x,
#                      y,
#                      n = 80*80,
#                      ofile,
#                      h = 1.5) {
#   cat("Starting density maps calc function \n")
#   
#   df <- data.table::fread(ifile)
#   
#   MASS::kde2d(x = x, y = y, n = n, h = h)
#   
#   # Output:
#   if(!missing(ofile)){
#     data.table::fwrite(x = dff, file = ofile, quote = T, row.names = F)
#   } else {
#     return(dff)
#   }
# }
