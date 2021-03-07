#' Performs a pre-filter for filtering fortracc output
#'
#' @description This function performs a pre-filter after adding essentials fields
#' to the data. It should be used after read_pcp.R or read_tb.R and before filtra_a.R.
#' It reads the files outputed by fortran routine/function
#' (YYYYMM.txt); give the fonte and period for each family; creates IDs for each family,
#' obtain their lifespan, timeUTC, phases, distances, and total displacement; 
#' filter data eliminating families in which the genesis, maturation, and/or dissipation 
#' happened at the same time; and then save the final dataset.
#'
#' @param ifile Character. Input filename. Generally as YYYYMM.txt
#' @param ofile Character. Output filename. Sugested: a_YYYYMM.csv
#' @param fonte Character. Source of the data. For instance: "Tb" for the
#' MERG product, "IMERG", "CMORPH", "NICAM AMIP", "NICAM HighResMIP".
#' It will be added as a field in the final data.frame. It is not 
#' used for filtering.
#' @param period Character. Choose between "present" or "future". 
#' It will be added as a field in the final data.frame. 
#' It is not used for filtering.
#' @return data.table
#' @importFrom data.table fread fwrite setcolorder fifelse
#' @importFrom utils menu
#' @importFrom geosphere distHaversine
#' @export
#' @examples \dontrun{
#' pre_filter(ifile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/200101010000.txt", 
#'            ofile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/p_200101010000.txt",
#'            fonte = "IMERG",
#'            period = "present")
#' }
pre_filter <- function(ifile,
                       ofile,
                       fonte,
                       period) {

  periods <- c("present", "future")
  if(missing(period)){
    choice <- utils::menu(periods, title="Choose var")
    period <- periods[choice]
  }
  
  dt <- data.table::fread(ifile, fill = TRUE)
  
  nomes <- names(dt)
  
  # Add ID's ####
  message("Obtaining ID...")
  dt$ID <- paste0(ifelse(nchar(dt$FAMILY) == 1, paste0("0000",dt$FAMILY),
                         ifelse(nchar(dt$FAMILY) == 2, paste0("000",dt$FAMILY),
                                ifelse(nchar(dt$FAMILY) == 3, paste0("00",dt$FAMILY),
                                       ifelse(nchar(dt$FAMILY) == 4, paste0("0",dt$FAMILY),
                                              dt$FAMILY)))),
                  dt$YEAR, 
                  ifelse(dt$MONTH > 9, 
                         as.character(dt$MONTH), 
                         paste0("0", dt$MONTH)),
                  ifelse(dt$DAY > 9, 
                         dt$DAY, 
                         paste0("0", dt$DAY)))
  
  # Reorder columns so ID come first ####
  dt <- data.table::setcolorder(x = dt, 
                                neworder = c("ID",
                                             nomes))
  message("Done!")
  
  # Obtain lifespan ####
  message("Obtaining lifespans...")
  dt <- dt[,total_time := max(TIME), by = ID]
  
  message("Done!")
  # Add Time UTC ####
  message("Obtaining time in UTC...")
  dt$timeUTC <- UTC(YEAR = dt$YEAR,
                     MONTH = dt$MONTH,
                     DAY = dt$DAY,
                     HOUR = dt$HOUR,
                     TIME = dt$TIME)
  message("Done!")
  
  # Add fonte ####
  message("Writing fonte...")
  dt$fonte <- fonte
  message("Done!")
  
  # Add period ####
  message("Writing period...")
  dt$period <- period
  message("Done!")
  
  # Obtain phases ####
  message("Obtaining phases...")
  # add three fields, one for each phase
  # maturation (max. extension) can occurs more than 1 time
  dt[,  `:=` (genesis = data.table::fifelse(TIME == min(TIME), TRUE, FALSE), 
              maturation = data.table::fifelse(SIZE == max(SIZE), TRUE, FALSE), 
              dissipation = data.table::fifelse(TIME == max(TIME), TRUE, FALSE)),
     by = ID]
  message("Done!")
  
  # Obtain the position/timestep in which maturation happens 
  message("Pre-filtering curves...")
  dt[, mat2 := (1:.N)*maturation, by = ID]
  # Remakes the maturation field with TRUE in the last time
  # where the maximum extension happens
  dt[, maturation := data.table::fifelse(mat2 == max(mat2), TRUE, FALSE),
     by = ID]
  # Select IDs in which the genesis, maturation, and
  # dissipation happened at the same time
  # Looking for the situation below:
  # ==========================================================
  #  TIME      SIZE    genesis     maturation     dissipation
  # ==========================================================
  #  tmin       -         1            0               0
  #   -        max        0            1               0
  #  tmax       -         0            0               1
  # ==========================================================
  # Isso eliminaria a necessidade de usar as derivadas.
  ids <- unique(dt[genesis+maturation+dissipation  > 1]$ID)
  dtt <- dt[!ID %in% ids]
  
  dtt$phase <- ifelse(
    dtt$genesis == TRUE, "genesis",
    ifelse(
      dtt$maturation == TRUE, "maturation",
      ifelse(
        dtt$dissipation == TRUE, "dissipation",
        NA)))  
  dtt$genesis <- dtt$maturation <- dtt$dissipation <- dtt$mat2 <- NULL
  message("Done!")
  
  # Obtain distances ####
  message("Calculating distances... (This will take some minutes)")
  
  x <-dtt[phase %in% c("genesis",  "dissipation")]
  lx <- split(x, x$ID)
  dx <- lapply(1:length(lx), function(i){
    data.frame(distkm = geosphere::distHaversine(
      p1 = cbind(lx[[i]]$XLON,lx[[i]]$YLAT)[1,], 
      p2 = cbind(lx[[i]]$XLON,lx[[i]]$YLAT)[2,]
    )/1000,
    ID = lx[[i]]$ID)
    
  })
  dx <- data.table::rbindlist(dx)
  dtt2 <- merge(dtt, unique(dx), by = "ID", all.x = T)
  message("Done!")
  
  # # Obtain spatial features ####
  # dtt$LATY <- dtt$YLAT
  # dtt$LONX <- dtt$XLON
  # dtt <- sf::st_as_sf(dtt, coords = c("LONX", "LATY"), crs = 4326)
  
  
  # Output:
  if(!missing(ofile)){
    message(paste0("Saving final dataset in: \n", ofile))
    data.table::fwrite(x = dtt2, 
                       file = ofile, 
                       quote = T, 
                       row.names = F)
    message("Done!")
  } else {
    message(paste0("Final dataset returned as ", class(dtt2)[1]))
    return(dtt2)
  }
}

