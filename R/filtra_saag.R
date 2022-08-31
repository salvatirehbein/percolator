#' Obtain SAAG Deep Convection Working Group MCSs from ForTraCC outputs
#'
#' @description This function filter the ForTraCC outputs to select MCSs that match 
#' the criteria defined by the SAAG Deep Convection Working Group at April 13, 2022 meeting 
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
#' @param ofile Character. Output filename. Suggested: WRF_PCP_WRF_YYYY.csv
#' @param dx Numeric. Used to determine the area of the pixel. E.g.: 11.132 (default)
#' @param dy Numeric. Used to determine the area of the pixel. E.g.: 11.132 (default)
#' @param min_volume Numeric. Used for filter as determined in the SAAG DCWG Meeting 
#' at April 13, 2022.
#' @param dist_km Boolean. If TRUE will calculate the distance in km between one
#' timestep and another. Being TRUE it will also allows for filter the maximum distante
#' between one timestep and another using 'dist_max'. However dist_km = TRUE takes 
#' very very long time for performing calculations. Therefore the default is FALSE. 
#' @param dist_max Numeric. Maximum distance in km allowed between two consecutive timesteps.
#' ForTraCC sometimes connects two no related cloud systems. This filter remove the 
#' entire family when the distance between two consecutive timesteps are larger than 
#' the threshold defined. By default it is 300 km (recommended for 30 min timesteps)
#' @param variable Character. "PCP" (default) or "Tb".
#' @param experiment Character. Suggested: "WRF SAAG 3yr" (default) or "WRF SAAG 20yr"
#' @param period Character. Choose between "present" (default) or "future".
#' @param growing_curve Boolean. If TRUE (default) families in which the
#' genesis, maturation, and/or dissipation happened at the same time are removed.
#' It is a filter.
#' @return data.table
#' @importFrom data.table fread fwrite setcolorder fifelse
#' @importFrom utils menu
#' @importFrom geosphere distHaversine
#' @export
#' @examples \dontrun{
#' filtra_saag(ifile <- "/glade/work/arehbein/SAAG/3yr/SAIDAS_FORTRACC/fam_SAAG_PCP_WRF_201906302300.txt", 
#'            ofile = "/glade/work/arehbein/SAAG/3yr/SAIDAS_FORTRACC/SAAG_PCP_WRF_2010.txt")
#'}
filtra_saag <- function(ifile,
                        ofile,
                        dx = 0.1*111.32, # area do pixel
                        dy = 0.1*111.32, # area do pixel
                        min_volume = 20000, # determinado na reuniao em Apr. 13, 2022
                        dist_km = FALSE, # Calcular a dist entre um timestep e outro? eh demorado
                        dist_max = 300, # somente sera usado se dist_km = TRUE
                        variable = "PCP",
                        experiment = "WRF SAAG 3yr",
                        period = "present", 
                        growing_curve = FALSE) {
  
  # periods <- c("present", "future")
  # 
  # if(missing(period)){
  #   choice <- utils::menu(periods, title="Choose var")
  #   period <- periods[choice]
  # }
  
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
    rm(ids)
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
  rm(dt, IDS); gc()
  
  # Obtain distances ####
  message("Calculating total distances... (This will take some minutes)")
  
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
  
  if (dist_km == TRUE) {
    
    message("Calculating distances between consecutive timesteps... (This may take some minutes)")
    x_ini <- dtt2$XLON
    y_ini <- dtt2$XLAT
    x_fim <- x_ini[c(2:length(x_ini), NA)]
    y_fim <- y_ini[c(2:length(y_ini), NA)]
    
    df2 <- data.frame(ID = dtt2$ID,
                      phase = dtt2$phase,
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
    dtt2$dist_km <- df2$distkm
    
    # filter out IDs that with dist_km >  dist_max in at least one timestep of the MCSs
    message("Filtering objects connected unrealisticaly.")
    dtt2$criteria_length <- ifelse(as.numeric(dtt2$dist_km) > dist_max,
                                   "BAD", "GOOD")
    IDS <- dtt2[dtt2$criteria_length %in% "BAD", ]$ID
    
    if (length(IDS) == 0) {
      df <- dtt2
    } else {
      df <- dtt2[!dtt2$ID%in%IDS,]
    }
    
    df$criteria_length <- NULL
    message("Done!")
    rm(dtt2, dtt, dx, x, IDS, df2)
    
  } else {
    df <- dtt2
  }
  
  # # Obtain spatial features ####
  # dtt$LATY <- dtt$YLAT
  # dtt$LONX <- dtt$XLON
  # dtt <- sf::st_as_sf(dtt, coords = c("LONX", "LATY"), crs = 4326)
  
  # Add Time UTC ####
  message("Obtaining time in UTC...")
  df$timeUTC <- UTC(YEAR = df$YEAR,
                    MONTH = df$MONTH,
                    DAY = df$DAY,
                    HOUR = df$HORA_INI,
                    TIME = df$TIME)
  message("Done!")
  
  # Add Local Time ####
  message("Obtaining Local Time")
  message("This may take some minutes...")
  df$tz <- LT(x = df,
              coords = c("XLON", "XLAT"))
  df$timeLT <- df$timeUTC + df$tz*60*60
  
  message("Done!")
  
  # Output:
  if(!missing(ofile)){
    message(paste0("Saving final dataset in: \n", ofile))
    data.table::fwrite(x = df, 
                       file = ofile, 
                       quote = T, 
                       row.names = F)
    message("Done!")
  } else {
    message(paste0("Final dataset returned as ", class(df)[1]))
    return(df)
  }
}

