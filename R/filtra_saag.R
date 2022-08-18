#' Obtain SAAG Deep Convection Working Group MCSs from ForTraCC outputs
#'
#' @description This function filter the ForTraCC outputs to select MCSs that match 
#' the criterias defined by the SAAG Deep Convection Working Group at April 13, 2022 meeting 
#' (https://ral.ucar.edu/projects/south-america-affinity-group-saag/deep-convection-working-group).  
#' It also creates unique ID's for each family, obtain their lifespan, timeUTC, timeLT, 
#' phases, distances, and total displacement. There is an option for filter 
#' (if growing_curve is TRUE) data eliminating families in which the
#' genesis, maturation, and/or dissipation happened at the same time. 
#' The criterias are: 
#' - Minimum Tb <= 241 K area must be at least 40,000 km2 for at least 4 continuous hours;
#' - Minimum peak precipitation - 10 mm/h at least one pixel (0.1 x 0.1 deg for 1 h)
#'   inside the whole MCS for 4 continuous hours;
#' - 20,000 km mm/h (e.g., 100 km x 100 km x 2 mm/h) minimum rainfall volume at 
#'   least once in the lifetime of the MCS;
#' - Overshoot threshold - Tb < 225 K exists during the cloud lifetime.

#'
#' @param ifile Character. Input filename. Generally as fam_WRF_PCP_WRF_YYYYMMDDHHMM.txt
#' @param ofile Character. Output filename. Sugested: WRF_PCP_WRF_YYYY.csv
#' @param dx Numeric. Used to determine the area of the pixel. E.g.: 11.132 (default)
#' @param dy Numeric. Used to determine the area of the pixel. E.g.: 11.132 (default)
#' @param min_volume Numeric. Used for filter as determined in the SAAG DCWG Meeting 
#' at April 13, 2022.
#' @param variable Character. "PCP" (default) or "Tb".
#' @param experiment Character. Sugested: "WRF SAAG 3yr" (default) or "WRF SAAG 20yr"
#' @param period Character. Choose between "PRESENT" (default) or "FUTURE".
#' @param growing_curve Boolean. If TRUE (default) families in which the
#' genesis, maturation, and/or dissipation happened at the same time are removed.
#' It is a filter.
#' @return data.table
#' @importFrom data.table fread fwrite setcolorder fifelse
#' @importFrom utils menu
#' @importFrom geosphere distHaversine
#' @export
#' @examples \dontrun{
#' filtra_saag(ifile <- "/home/amanda/Documents/pos_doc/FAPESP/SAAG/diag.txt/fam_SAAG_PCP_WRF_201006052300.txt", 
#'            ofile = "/home/amanda/Documents/pos_doc/FAPESP/SAAG/diag.txt/SAAG_PCP_WRF_2010.txt")
#'}
filtra_saag <- function(ifile,
                        ofile,
                        dx = 0.1*111.32, # area do pixel
                        dy = 0.1*111.32, # area do pixel
                        min_volume = 20000, # determinado na reuniao em Apr. 13, 2022
                        variable = "PCP",
                        experiment = "WRF SAAG 3yr",
                        period = "PRESENT", 
                        growing_curve = FALSE) {
  
  periods <- c("present", "future")
  
  if(missing(period)){
    choice <- utils::menu(periods, title="Choose var")
    period <- periods[choice]
  }
  
  dt <- data.table::fread(ifile, fill = TRUE)
  
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
                                neworder = "ID")
  message("Done!")
  
  # Obtain lifespan ####
  message("Obtaining lifespans...")
  dt <- dt[,total_time := max(TIME), by = ID]
  dt <- dt[, HORA_INI:= min(HOUR), by = ID]
  
  message("Done!")
  # Add Time UTC ####
  message("Obtaining time in UTC...")
  dt$timeUTC <- UTC(YEAR = dt$YEAR,
                    MONTH = dt$MONTH,
                    DAY = dt$DAY,
                    HOUR = dt$HORA_INI,
                    TIME = dt$TIME)
  message("Done!")
  
  # Add Local Time ####
  message("Obtaining Local Time")
  message("This may take some minutes...")
  dt$tz <- LT(x = dt,
              coords = c("XLON", "XLAT"))
  dt$timeLT <- dt$timeUTC + dt$tz*60*60
  
  message("Done!")
  
  # Add fonte ####
  message("Writing fonte...")
  dt$experiment <- experiment
  message("Done!")
  
  # Add period ####
  message("Writing period...")
  dt$period <- period
  message("Done!")
  
  dt$SIZE_KM2 <- dt$SIZE*dx*dy
  
  
  # Filtering to SAAG by VOLUME ####
  dt$VOL_PREC_MED <- dt$SIZE_KM2*dt$PMED # km2 mm/h
  dt$VOL_PREC_MAX <- dt$SIZE_KM2*dt$PMAX  # km2 mm/h
  dt$criteria_volume <- ifelse(dt$VOL_PREC_MAX >= min_volume, "GOOD", "BAD")
  IDS <- unique(dt[criteria_volume %in% "GOOD", ]$ID)
  if (length(IDS) == 0) {
    dt <- dt
  } else {
    dt <- dt[dt$ID%in%IDS,]
  }
  dt$criteria_volume <- NULL
  rm(IDS)
  
  # Filtering to SAAG by RATE and LIFESPAN ####
  # 1 pixel with >= 10 mm/h by 4 consecutive hours:
  dt$criteria_pmax <- ifelse(dt$PMAX >= 10, 1, 0)
  dt <- dt[, soma := cumsum(criteria_pmax), by =.(ID)]
  dt$criteria_soma <-  ifelse(dt$soma >= 4, "GOOD", "BAD")
  IDS <- unique(dt[criteria_soma %in% "GOOD", ]$ID)
  if (length(IDS) == 0) {
    dt <- dt
  } else {
    dt <- dt[dt$ID%in%IDS,]
  }
  dt$criteria_pmax <- dt$criteria_soma <- dt$soma <-  NULL
  
  
  # Obtain phases ####
  message("Obtaining phases...")
  # add three fields, one for each phase
  # maturation (max. extension) can occurs more than 1 time
  dt[,  `:=` (genesis = data.table::fifelse(TIME == min(TIME), TRUE, FALSE), 
              maturation = data.table::fifelse(SIZE == max(SIZE), TRUE, FALSE), 
              dissipation = data.table::fifelse(TIME == max(TIME), TRUE, FALSE)),
     by = ID]
  message("Obtaining Phases: Done!")
  
  # Obtain the position/timestep in which maturation happens 
  message("Pre-filtering curves...")
  dt[, mat2 := (1:.N)*maturation, by = ID]
  # Remakes the maturation field with TRUE in the last time
  # where the maximum extension happens
  dt[, maturation := data.table::fifelse(mat2 == max(mat2), TRUE, FALSE),
     by = ID]
  
  # Growing curve filter
  if (growing_curve == TRUE) {
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
    
  } else {
    dtt <- dt
  }
  
  # Create an unique field for phase:
  dtt$phase <- ifelse(
    dtt$genesis == TRUE, "genesis",
    ifelse(
        dtt$maturation == TRUE & dtt$dissipation == FALSE, "maturation",
        ifelse(
          dtt$dissipation == TRUE, "dissipation",
          NA)))
  dtt$genesis <- dtt$maturation <- dtt$dissipation <- dtt$mat2 <- NULL
  message("Phases: Done!")
  
  # Obtain distances ####
  message("Calculating distances... (This will take some minutes)")
  
  x <-dtt[phase %in% c("genesis", "dissipation")]
  lx <- split(x, x$ID)
  dx <- lapply(1:length(lx), function(i){
    
    if (nrow(lx[[i]]) < 2) {
      data.frame(distkm = 0,
                 ID = lx[[i]]$ID)  
    } else {
    data.frame(distkm = geosphere::distHaversine(p1 = cbind(lx[[i]]$XLON,lx[[i]]$XLAT)[1,], 
                                                 p2 = cbind(lx[[i]]$XLON,lx[[i]]$XLAT)[2,])/1000,
               ID = lx[[i]]$ID) }
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

