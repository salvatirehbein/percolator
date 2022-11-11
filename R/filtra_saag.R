#' Filter Fortracc outputs to match MCSs definition from the SAAG Deep Convection Group
#'
#' @description  This function filter the ForTraCC outputs to select MCSs that match 
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
#' @param xmin Numeric. Western coord. of the native data. EX:
#' ncdump merg_2010010100_4km-pixel.nc | less
#' @param xmax Numeric. Eastern coord. of the native data. EX:
#' @param ymin Numeric. Southern coord. of the native data. EX:
#' @param ymax Numeric. Northern coord. of the native data. EX:
#' @param ncols Integer. Relative to the native data.
#' @param nlins Integer. Relative to the native data.
#' @param type Character. Use "Observational" for GPM IMERG or "WRF" for WRF SAAG Simulations.
#' @param family_file Character. Name of the family file (including the path to it)
#' obtained by Fortracc. Must be one starting with "fam_"
#' @return netCDF
#' @importFrom data.table fread fwrite setorder
#' @importFrom raster raster brick flip
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @export
#' @examples \dontrun{
#' filtra_saag(xmin = -81.95, # Coords. pegas do ncdump --> centroide da celula de grade
#'             xmax = -34.05,
#'             ymin = -55.95,
#'             ymax = 12.95,
#'             ncols = 480,
#'             nlins = 690,
#'             type = "Observational",
#'             family_file = "/glade/work/arehbein/SAAG/3yr/MCSs/OBS/SAIDAS_FORTRACC_Tb/diag.txt/fam_SAAG_Tb_GPM_IMERG_1106_s2.txt",
#'             pathi_to_prec_file = "/glade/work/arehbein/SAAG/3yr/IMERG/",
#'             pathi_to_fortracc_clusters = "/glade/work/arehbein/SAAG/3yr/MCSs/OBS/SAIDAS_FORTRACC_Tb/clusters/",
#'             pathi_to_fortracc_txt = "/glade/work/arehbein/SAAG/3yr/MCSs/OBS/SAIDAS_FORTRACC_Tb/diag.txt/",
#'             pathi_to_masks_files = "/glade/work/arehbein/SAAG/3yr/MCSs/OBS/MCSs_MASKs/")
#' }
filtra_saag <- function(xmin = -81.95,
                        xmax = -34.05,
                        ymin = -55.95,
                        ymax = 12.95,
                        ncols = 480,
                        nlins = 690,
                        type = "Observational",
                        family_file,
                        pathi_to_prec_file = "/glade/work/arehbein/SAAG/3yr/IMERG/",
                        pathi_to_fortracc_clusters = "/glade/work/arehbein/SAAG/3yr/MCSs/OBS/SAIDAS_FORTRACC_Tb/clusters/",
                        pathi_to_fortracc_txt = "/glade/work/arehbein/SAAG/3yr/MCSs/OBS/SAIDAS_FORTRACC_Tb/diag.txt/",
                        pathi_to_masks_files = "/glade/work/arehbein/SAAG/3yr/MCSs/OBS/MCSs_MASKs/"
){
  # Loading the Pixel Area file for Observational GPM IMERG
  if(type == "Observational"){
    nc_area <- ncdf4::nc_open(system.file("extdata/grid_area_saag/area_imerg_km2.nc", 
                                          package = "percolator"))
    area <- ncdf4::ncvar_get(nc_area, "Band1")
    m_area <- matrix(area,
                     nrow = ncols, # precisa ser invertido!!
                     ncol = nlins) # precisa ser invertido!!
    mt_area <- t(m_area)
    r_area <- raster::flip(raster::raster(mt_area,
                                          xmn = xmin,
                                          xmx = xmax,
                                          ymn = ymin,
                                          ymx = ymax), direction = "y")  # north to south
    # plot(r_area, main = "GPM IMERG pixel area (north to south)")
  }
  
  # if(type == "WRF"){
  # COMPLETAR
  # } 
  
  
  # 1) Filter Fortracc Tb #### 
  # output according to the SIZE and LIFESPAN 
  ffile <- family_file
  # source("tb_filter.R")
  # --------------------------------------------------------------- BEGIN
  dt_fam <- data.table::fread(ffile, fill = TRUE)
  dt_fam$HOUR3 <- dt_fam$HOUR
  dt_fam$HOUR <- dt_fam$HOUR2
  dt_fam$HOUR2 <- NULL
  dt_fam$SIZE_km2 <- 10*10*dt_fam$SIZE
  
  # SAAG Tb Criterias ####
  dt_fam$criterio_size <- ifelse(dt_fam$TMED <= 241*100 &
                                   dt_fam$SIZE_km2 >= 40000, 1, 0)
  dt_fam[, counter := data.table::rowid(
    data.table::rleid(criterio_size)), by = FAMILY]
  
  FAMILYS <- dt_fam[criterio_size == 1 & counter >= 4, unique(FAMILY)]
  if (length(FAMILYS) > 0) {
    dt_fam <- dt_fam[FAMILY%in%FAMILYS,]
  }; rm(FAMILYS)
  # tb ≤ 225 K during 4 hrs?
  dt_fam$Tb_le_225K <- ifelse(dt_fam$TMIN <= 225*100, 1, 0)
  dt_fam[, counter2 := data.table::rowid(
    data.table::rleid(Tb_le_225K)), by = FAMILY]
  FAMILYS <- dt_fam[Tb_le_225K == 1 & counter2 >= 4, unique(FAMILY)]
  if (length(FAMILYS) > 0) {
    dt_fam <- dt_fam[FAMILY%in%FAMILYS,]
  }; rm(FAMILYS)
  # --------------------------------------------------------------- END
  
  
  # 2) Obtain info about the precipitation under the cloud shield ####
  message("Obtain info about the precipitation under the cloud shield.\nThis may take some hours!!")
  message("(If it crashes, try not to go crazy & increase the memory.)")
  
  # i index
  ldf <- list()
  b_pcp_fam <- list()  
  uf <- unique(dt_fam$FAMILY)
  
  for(i in seq_along(uf)) {
    
    # Iterar FAMILY by FAMILY (can have memory issue)
    # Running at CASPER, it works with mem=100Gb
    
    df <- dt_fam[FAMILY == uf[i],]
    
    # j index
    l1 <- list()
    lnames <- list()
    lsum <- list()
    lmean <- list()
    lq25 <- list()
    lq50 <- list()
    lq75 <- list()
    lmin <- list()
    lmax <- list()
    larea_pcp <- list()
    lvol <- list()
    lpcp_tot <- list()
    
    for(j in seq_along(df$FAMILY)) {
      
      # Iterate time by time in each FAMILY    
      SYS <- df$`SYS#`[j]
      
      yyyy <- df$YEAR[j]
      yy <- substr(yyyy, 3,4)
      mm <- sprintf(df$MONTH[j], fmt = "%02d")
      dd <- sprintf(df$DAY[j], fmt = "%02d")
      hh <- sprintf(df$HOUR[j], fmt = "%02d")
      mn <- "00"
      print(paste0("FAMILY: ",  df$FAMILY[j]))  
      print(paste0("Date: ",  yyyy, "-", mm, "-", dd, " ", hh, ":", mn))  
      
      # Open precipitation file
      if(type == "Observational"){
        arq_nc <- paste0(pathi_to_prec_file, 
                         yyyy,"/merg_", yyyy, mm, dd, hh,"_4km-pixel.nc")
        nc <- ncdf4::nc_open(filename = arq_nc)
        nc_pcp <- ncdf4::ncvar_get(nc = nc, varid = "precipitationCal")[,,1]
      }
      
      if(type == "WRF"){
        arq_nc <- paste0(pathi_to_prec_file, 
                         "tb_rainrate_", yyyy, "-", mm, "-", dd, "_", hh,":00.nc")
        nc <- ncdf4::nc_open(filename = arq_nc)
        nc_pcp <- ncdf4::ncvar_get(nc = nc, varid = "rainrate")
      } 
      
      ncdf4::nc_close(nc)
      
      m_pcp <- matrix(as.integer(nc_pcp),
                      nrow = ncols, # precisa ser invertido!!
                      ncol = nlins) # precisa ser invertido!!
      mt_pcp <- t(m_pcp)
      r_pcp <- raster::flip(raster::raster(mt_pcp,
                                           xmn = xmin,
                                           xmx = xmax,
                                           ymn = ymin,
                                           ymx = ymax), direction = "y")  # north to south
      
      # Open Tb clusters generated by Fortracc
      cluster <- paste0(pathi_to_fortracc_clusters, 
                        "gs.", yy, mm, dd, ".", hh, mn, "g.raw")
      v_bin <- readBin(cluster,
                       what = "integer",
                       n = ncols*nlins*2,
                       size = 2)
      m_bin <- matrix(v_bin, 
                      nrow = ncols, # precisa ser invertido!!
                      ncol = nlins) # precisa ser invertido!!
      mt_bin <- t(m_bin)
      r_bin <- raster::raster(mt_bin, 
                              xmn = xmin, 
                              xmx = xmax, 
                              ymn = ymin, 
                              ymx = ymax)                       # north to south
      r3 <- r_bin # creating raster for precipitation identical to Tb cluster raster
      
      # selects precipitation below the Tb cluster numbered as SYS in the 
      # remaining FAMILY in fam.txt after SAAG Tb Criterias.
      r3[] <- ifelse(r3[] == SYS, r_pcp[], NA) 
      # Therefore, r3 is a raster with only one precipitation "cluster"
      
      # Save statistics of each precipitation clusters. 
      # They will be used for filtering according to SAAG PCP Criterias. 
      l1[[j]] <- r3 
      lnames[[j]] <- paste0("X", yyyy, mm, dd, hh, mn)
      lsum[[j]] <- sum(r3[], na.rm = TRUE)
      lmean[[j]] <- mean(r3[], na.rm = TRUE)
      lq25[[j]] <- quantile(r3[], 0.25, na.rm = TRUE)
      lq50[[j]] <- quantile(r3[], 0.50, na.rm = TRUE)
      lq75[[j]] <- quantile(r3[], 0.75, na.rm = TRUE)
      lmin[[j]] <- min(r3[], na.rm = TRUE)
      lmax[[j]] <- max(r3[], na.rm = TRUE)
      # obtaining precipitation volume --> integrate the multiplication of the 
      # pcp value in each pixel by the area of each pixel
      vol_in_the_px <- r3 * r_area
      lvol[[j]] <- sum(vol_in_the_px[], na.rm = TRUE)
      # calculate the total precipitation area
      r4 <- r3
      r4[] <- ifelse(!is.na(r4[]), r_area[], NA) 
      larea_pcp[[j]] <- sum(r4[], na.rm = TRUE)
      # total precipitation
      lpcp_tot[[j]] <- sum(ifelse(is.na(r3[]), F, T))
      
    }
    
    df$pcp_sum <- unlist(lsum) 
    df$pcp_ave <- unlist(lmean) 
    df$pcp_q25 <- unlist(lq25)
    df$pcp_q50 <- unlist(lq50)
    df$pcp_q75 <- unlist(lq75)
    df$pcp_min <- unlist(lmin)
    df$pcp_max <- unlist(lmax)
    df$total_pcp_area <- unlist(larea_pcp)
    df$total_vol <- unlist(lvol)
    df$total_pcp <- unlist(lpcp_tot)
    
    ldf[[i]] <- df
    data.table::fwrite(df,
                       file = paste0(pathi_to_masks_files, 
                                     "fam_SAAG_Tb_pcp_info_intermedio_",
                                     min(dt_fam$YEAR), "-", max(dt_fam$YEAR), ".csv"),
                       row.names = FALSE,
                       append = TRUE)
    
    gc() 
  }
  dt <- data.table::rbindlist(ldf)
  
  # Ate aqui tenho a informacao da prec abaixo do escudo de nuvens
  #writeRaster(brick(b_pcp_fam), "/glade/work/arehbein/SAAG/3yr/teste_pcp_below_cloud.nc", overwrite = T)
  # fwrite(dt,
  #        paste0("/glade/work/arehbein/SAAG/3yr/MCSs_MASKs/fam_SAAG_Tb_pcp_info_",
  #               min(dt_fam$YEAR), "-", max(dt_fam$YEAR), ".csv"),
  #        row.names = FALSE)
  
  
  # 3) Filter PCP criterias ####
  message("Filtering the precipitation according to the SAAG criterias.")
  # source("pcp_filter.R")
  # ------------------------------------------------------------ START
  # 1 px ≥10mm/h in T≥4h 
  dt$pcp_min_criteria <- ifelse(dt$pcp_max >= 10, 1, 0)
  dt[, counter3 := data.table::rowid(
    data.table::rleid(pcp_min_criteria)), by = FAMILY]
  FAMILYS <- dt[pcp_min_criteria == 1 & counter3 >= 4, unique(FAMILY)]
  if (length(FAMILYS) > 0) {
    dt <- dt[FAMILY%in%FAMILYS,]
  }; rm(FAMILYS) 
  
  # 20,000 km2 mm/h (e.g., 100 km x 100 km x 2 mm/h) minimum 
  # rainfall volume at least once in the lifetime of the MCS 
  dt$vol_criteria <- ifelse(dt$total_vol >= 20000, 1, 0)
  FAMILYS <- dt[vol_criteria == 1, unique(FAMILY)]
  if (length(FAMILYS) > 0) {
    dt <- dt[FAMILY%in%FAMILYS,]
  }; rm(FAMILYS) 
  dt <- dt[, -c("vol_criteria", "pcp_min_criteria", "counter", "counter2", "counter3")]
  # Quais sao os SYS que devo salvar? 
  # Passar por todas as linhas identificando os times iguais
  # criar uma lista para cada time
  # ------------------------------------------------------------ END
  
  dt[ , FAMILY_new := .GRP, by = "FAMILY"] 
  dt$date <- paste0(sprintf(dt$YEAR, fmt = "%04d"), 
                    sprintf(dt$MONTH, fmt = "%02d"),
                    sprintf(dt$DAY, fmt = "%02d"),
                    sprintf(dt$HOUR, fmt = "%02d"))
  data.table::fwrite(dt,
                     file = paste0(pathi_to_fortracc_txt,
                                   "MCSs_SAAG_", 
                                   min(dt_fam$YEAR), "-", max(dt_fam$YEAR), ".csv"),
                     row.names = FALSE)
  gc()
  lbin <- list()
  
  ud <- sort(unique(dt$date))
  for(l in seq_along(ud)) {
    yy <- substr(ud[l], 3, 4)
    mm <- substr(ud[l], 5, 6)
    dd <- substr(ud[l], 7, 8)
    hh <- substr(ud[l], 9, 10)
    mn <- "00"
    
    cluster <- paste0(pathi_to_fortracc_clusters,
                      "gs.", yy, mm, dd, ".", hh, mn, "g.raw")
    
    print(paste0("Reading the cluster file: ", cluster))
    
    # binary
    v_bin <- readBin(cluster,
                     what = "integer", 
                     n = ncols*nlins*2, 
                     size = 2)
    
    di <- dt[date == ud[l],]
    
    SYS <- di$`SYS#`
    print(paste0("SYS = ", SYS))
    
    
    # added name in di in order to merge df_bin that will be created following.
    di$v_bin2 <- di$`SYS#`
    
    # create data.table with the vector information from the fortracc cluster binary file
    # and add id in the case of the merge change the order
    df_bin <- data.table::data.table(id = 1:length(v_bin),
                                     v_bin = v_bin)
    
    # digo quando tenho MCSs
    df_bin[, v_bin2 :=  ifelse(v_bin %in% SYS, v_bin, NA)]
    
    df_bin <- merge(x = df_bin,
                                y = di[, c("v_bin2", "FAMILY_new")],
                                by = "v_bin2",
                                all.x = TRUE)
    data.table::setorderv(df_bin, "id")
    # df_bin[!is.na(FAMILY_new)]
    m_bin2 <- matrix(df_bin$FAMILY_new,
                     nrow = ncols, # precisa ser invertido!!
                     ncol = nlins) # precisa ser invertido!!
    dd1 <- dim(m_bin2)
    mt_bin2 <- m_bin2[1:dd1[1], dd1[2]:1]
    r_bin2 <- raster::raster(mt_bin2,
                             xmn = xmin,
                             xmx = xmax,
                             ymn = ymin,
                             ymx = ymax)
    lbin[[l]] <- mt_bin2
  }
  
  dbin <- unlist(lbin)
  
  # 4) Save Tb final Masks --> Write the netcdf ####
  # write_nc("write_nc.R")
  write_nc(type = type,
           dt = dt,
           pathi_to_prec_file = pathi_to_prec_file,
           pathi_to_masks_files = pathi_to_prec_file)
}