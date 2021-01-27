#' Filter MCSs by lifespan and area of genesis
#'
#' @description It is strongly recommended using filtra_a function before the 
#' other filters. This function reads the files ouputed by fortran routine/function
#' (YYYYMM.txt), creates IDs for each family of cloud cluster, creates lifespan, timeUTC,
#' fonte, and period fields, filter data by lifespan above a given threshold, 
#' classifies each family by its total lifespan, and then save the final dataset.
#'
#' @param ifile Character. Input filename. Generally as YYYYMM.txt
#' @param ofile Character. Output filename. Sugested: a_YYYYMM.csv
#' @param variable Character. Tells the filter which data was used for 
#' tracking the cloud clusters by ForTraCC. Choose among: "Tb", "PCP", 
#' or "OLR". It is important to set this properly, since each one of 
#' these variables provide files with different fields.
#' @param fonte Character. Source of the data. For instance: "Tb" for the
#' MERG product, "IMERG", "CMORPH", "NICAM AMIP", "NICAM HighResMIP".
#' It will be added as a field in the final data.frame. It is not 
#' used for filtering.
#' @param period Character. Choose between "present" or "future". 
#' It will be added as a field in the final data.frame. 
#' It is not used for filtering.
#' @param filtra_time Numeric. Tells the minimum lifespan the remaining MCSs 
#' should have. In general, they live 3 hours or more. This is the default value.
#' @param classifica_lifespan Numeric. Allows to classify between long- or 
#' short-lived MCS. Require a threshold to identify the lifespans. 
#' Default is 5 hours and 30 minutes.
#' @return data.table
#' @importFrom data.table fread setcolorder
#' @importFrom stats na.omit
#' @export
#' @examples \dontrun{
#' filtra_a(ifile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/p_200101010000.txt", 
#'          ofile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/a_200101010000.txt")
#' }
filtra_a <- function(ifile,
                     ofile,
                     classif = "N",
                     filtra_time = 3.0,
                     classifica_lifespan = 5.7) {
  message("Starting filtra_a... \n")
  cat(paste0("\n Filtrando pelo ciclo de vida!!!!!!! No final teremos familias de
             sistemas com ciclo de vida acima de ", filtra_time, " horas \n"))
  
  dt <- data.table::fread(ifile, fill = TRUE)
  
  nomes <- names(dt)
  
  cat(paste0("\n Total initial families: ", 
             length(unique(dt$ID))),"\n")
  
  # Filter by Classification:
  if(!is.null(classif)){
    message("Filter by classification")
    dt <- dt[CLASSIF == classif,]
  } 
  
  # Filter lifespans:
  if(!is.null(filtra_time)){
    message("Filter by lifespan")
    dt <- dt[total_time >= filtra_time,]
  }
  
  # Classify lifespans:
  ## Let's assume that there is no classification, i.e.:
  dt$lifespan <- NA
  ## Than it can classify or not:
  if(!is.null(classifica_lifespan)){
    message("Classify lifespans")
    dt$lifespan <- ifelse(dt$total_time < classifica_lifespan, 
                          "Short-lived",
                          "Long-lived")
  }
  
  cat(paste0("\n Families after filtra_a: ",
             length(unique(dt$ID))),"\n ")
  
  # Output:
  if(!missing(ofile)){
    data.table::fwrite(x = dt,
                       file = ofile,
                       quote = T,
                       row.names = F)
  } else {
    return(dt)
  }
}

