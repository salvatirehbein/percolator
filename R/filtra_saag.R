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
#' @param mm_ini string. Initial month of the tracking. It will be used for writing the netcdf masks. write_nc uses it for writing the time attribute.
#' @param family_file Character. Name of the family file (including the path to it)
#' obtained by Fortracc. Must be one starting with "fam_"
#' @return netCDF
#' @importFrom data.table fread fwrite setorder
#' @importFrom raster raster brick flip
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @export
#' @examples \dontrun{

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
                        mm_ini,  
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
  
  if(type == "WRF"){
    nc_area <- ncdf4::nc_open(system.file("extdata/grid_area_saag/area_wrf_km2.nc", 
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
                                          ymx = ymax), direction = "y")
    # plot(r_area, main = "WRF pixel area (north to south)")
    
  }
  
  
  
  
  
  # 1) Filter Fortracc Tb #### 
  ffile <- family_file
  ## antigo source("tb_filter.R")
  # --------------------------------------------------------------- BEGIN
  dt_fam <- data.table::fread(ffile, fill = TRUE)
  dt_fam$HOUR3 <- dt_fam$HOUR
  dt_fam$HOUR <- dt_fam$HOUR2
  dt_fam$HOUR2 <- NULL
  # tb ≤ 225 K during 4 hr at least one pixel
  dt_fam$Tb_le_225K <- ifelse(dt_fam$TMIN < 225*100, 1, 0)
  dt_fam[, counter2 := data.table::rowid(data.table::rleid(Tb_le_225K)), by = FAMILY]
  #FAMILYS <- dt_fam1[Tb_le_225K == 1 & counter2 >= 4, unique(FAMILY)]
  FAMILYS <- dt_fam[Tb_le_225K == 1 & counter2 >= 1, unique(FAMILY)]
  if (length(FAMILYS) > 0) {
    dt_fam <- dt_fam[FAMILY%in%FAMILYS,]
  }; rm(FAMILYS)
  # --------------------------------------------------------------- END
  year_ini <- min(dt_fam$YEAR)
  year_fim <- max(dt_fam$YEAR)
  #  mm_fim <- sprintf(max(df$MONTH), fmt = "%02d") # para o paper de comparacao entre os trackings foi "06"

if(year_ini != year_fim){
  ndates <- length(c(list.files(path = pathi_to_fortracc_clusters,
                                pattern = paste0("gs.", substr(year_ini, 3,4))),
                     list.files(path = pathi_to_fortracc_clusters,
                                pattern = paste0("gs.", substr(year_fim, 3,4)))))
  }

if(year_ini == year_fim){
  ndates <- length(list.files(path = pathi_to_fortracc_clusters,
                                pattern = paste0("gs.", substr(year_ini, 3,4))))}

  # 2) Obtain info about the precipitation under the cloud shield ####
  message("Obtain info about the precipitation under the cloud shield.\nThis is a slow process. Please be patient!!")
  message("(Please remember to use iterative job.)")
  
  # i index
  ldf <- list()
  uf <- unique(dt_fam$FAMILY)
  
  for(i in seq_along(uf)) {
    
    # Iterate FAMILY by FAMILY of each month
    df <- dt_fam[FAMILY == uf[i],]
    
    # j index
    l_Tb_SIZE_km2 <- list()
    lsum <- list()
    lmax <- list()
    larea_pcp <- list()
    lvol <- list()
      
    for(j in seq_along(df$FAMILY)) {
      
      # Iterar time by time in each FAMILY    
      SYS <- df$`SYS#`[j]
      #    SYS_ANT <- ifelse(df$CLA[j] == "M", 
      #       c(df$SA01[j], df$SA02[j], df$SA03[j], df$SA04[j], df$SA05[j], 
      #         df$SA06[j], df$SA07[j], df$SA08[j], df$SA09[j], df$SA10[j],
      #         df$SA11[j], df$SA12[j], df$SA13[j], df$SA14[j], df$SA15[j],
      #         df$SA16[j], df$SA17[j], df$SA18[j], df$SA19[j], df$SA20[j]),
      #       0)
      
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
                         "merg_", yyyy, mm, dd, hh,"_4km-pixel.nc")
        nc <- ncdf4::nc_open(filename = arq_nc)
        nc_pcp <- ncdf4::ncvar_get(nc = nc, varid = "precipitationCal")[,,1]
        ncdf4::nc_close(nc)
      }
      
      if(type == "WRF"){
        arq_nc <- paste0(pathi_to_prec_file,"tb_rainrate_", yyyy, "-", mm, "-", dd, "_", hh,":00.nc")
        nc <- ncdf4::nc_open(filename = arq_nc)
        nc_pcp <- ncdf4::ncvar_get(nc = nc, varid = "rainrate")
        ncdf4::nc_close(nc)
      } 
      
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
      cluster <- file(paste0(pathi_to_fortracc_clusters, "gs.",
                             yy, mm, dd, ".", hh, mn, "g.raw"), "rb")
      v_bin <- readBin(cluster,
                       what = "integer",
                       n = ncols*nlins*2,
                       size = 2)
      close(cluster)
      m_bin <- matrix(v_bin,
                      nrow = ncols, # precisa ser invertido!!
                      ncol = nlins) # precisa ser invertido!!
      mt_bin <- t(m_bin)
      r_bin <- raster::raster(mt_bin,
                              xmn = xmin,
                              xmx = xmax,
                              ymn = ymin,
                              ymx = ymax)                       # north to south
      # Calculate the area in km2 of the tb cluster
      rtb <- r_bin
      rtb[] <- ifelse(rtb[] == SYS, r_area[], NA) # without the merging systems
      # rtb[] <- ifelse(rtb[] == SYS | rtb[] %in% SYS_ANT, r_area[], NA) 
      Tb_SIZE_km2 <- sum(rtb[], na.rm = T)
      l_Tb_SIZE_km2[[j]] <- Tb_SIZE_km2
      
      # Calculate the Preciptiation volume ####
      r3 <- r_bin # creating raster for precipitation identical to Tb cluster raster
      
      # selects precipitation below the Tb cluster numbered as SYS in the 
      # remaining FAMILY in fam.txt after SAAG Tb Criterias.
      # Therefore, r3 is a raster with only one (or more in the merging cases) precipitation "cluster"
      r3[] <- ifelse(r3[] == SYS, r_pcp[], NA) # without the merging systems
      #r3[] <- ifelse(r3[] == SYS | r3[] %in% SYS_ANT, r_pcp[], NA) 


      # Save statistics of each precipitation clusters. 
      # They will be used for filtering according to SAAG PCP Criteria. 
      lsum[[j]] <- sum(r3[], na.rm = TRUE)
      lmax[[j]] <- max(r3[], na.rm = TRUE)
      # obtaining precipitation volume --> integrate the multiplication of the 
      # pcp value in each pixel by the area of each pixel
      vol_in_the_px <- r3 * r_area
      lvol[[j]] <- sum(vol_in_the_px[], na.rm = TRUE)
      # calculate the total area occupied by precipitation under the cloud shield
      r4 <- r3
      r4[] <- ifelse(!is.na(r4[]), r_area[], NA)
      larea_pcp[[j]] <- sum(r4[], na.rm = TRUE)
    }
    df$Tb_SIZE_km2 <- unlist(l_Tb_SIZE_km2)
    df$pcp_sum <- unlist(lsum)
    df$pcp_max <- unlist(lmax) # Usar este para: Minimum peak precipitation - 10 mm/h at least one pixel (0.1 x 0.1 deg for 1 h) inside the whole MCS for 4 continuous hours
    df$pcp_total_vol <- unlist(lvol)
    df$pcp_total_area <- unlist(larea_pcp)
    
    ldf[[i]] <- df
    data.table::fwrite(df,
                       file = paste0(pathi_to_masks_files, 
                                     "APAGAR_fam_SAAG_Tb_pcp_info_intermedio_", # funciona como bkp
                                     year_ini, "-", year_fim, ".csv"),
                       row.names = FALSE,
                       append = TRUE)
    gc()
  }
  
  dt <- data.table::rbindlist(ldf)
  data.table::fwrite(dt,
                     file = paste0(pathi_to_masks_files, 
                                   "APAGAR_fam_SAAG_Tb_pcp_info_intermedio_FINAL_", 
                                   year_ini, "-", year_fim, ".csv"),
                     row.names = FALSE)
  
  
  #rm(list = ls()); gc()
  
  # pegos do ncdump --> centroide da celula de grade
  #xmin <- -81.95
  #xmax <- -34.05
  #ymin <- -55.95
  #ymax <- 12.95
  #ncols <- 480
  #nlins <- 690
  #pathi <- "/glade/work/arehbein/SAAG/3yr/MCSs/WRF/"
  #year_ini <- 2011
  #year_fim <- 2011
  #ndates <- 9480 # ((7*31)+(5*30)+28)*24
  
  #dt <- fread(paste0(pathi, "MCSs_MASKs/fam_SAAG_Tb_pcp_info_intermedio_",
  #                   year_ini, "-", year_fim, ".csv"))
  
  print(paste0("# FAMILIES (before filter): ", length(unique(dt$FAMILY))))
  
  # SAAG Criteria for Tb by size ####
  dt$criterio_size <- ifelse(dt$Tb_SIZE_km2 >= 40000, 1, 0)
  dt[, counter := data.table::rowid(data.table::rleid(criterio_size)), by = FAMILY]
  FAMILYS <- dt[criterio_size == 1 & counter >= 4, unique(FAMILY)]
  if (length(FAMILYS) > 0) {
    dt <- dt[FAMILY%in%FAMILYS,]
  }; rm(FAMILYS)
  print(paste0("# FAMILIES (after filter Tb by size): ", length(unique(dt$FAMILY))))
  
  # filter PCP criterias ####
  # --------------------------------------------------------------------------------- BEGIN
  # source("pcp_filter.R")
  # 1 px ≥ 10mm/h in T≥4h 
  # Minimum peak precipitation: 10 mm/h at least one pixel (0.1 x 0.1 deg for 1 h) inside the whole MCS for 4 continuous hours
  dt$pcp_min_criteria <- ifelse(dt$pcp_max >= 10, 1, 0)
  dt[, counter3 := data.table::rowid(data.table::rleid(pcp_min_criteria)), by = FAMILY]
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
  # --------------------------------------------------------------------------------- END
  print(paste0("# FAMILIES (after filter PCP): ", length(unique(dt$FAMILY))))
  
  # Quais sao os SYS que devo salvar?
  # Passar por todas as linhas identificando os times iguais
  # criar uma lista para cada time
  dt[ , FAMILY_new := .GRP, by = "FAMILY"]
  dt$date <- paste0(sprintf(dt$YEAR, fmt = "%04d"),
                    sprintf(dt$MONTH, fmt = "%02d"),
                    sprintf(dt$DAY, fmt = "%02d"),
                    sprintf(dt$HOUR, fmt = "%02d"))
  data.table::fwrite(dt,
                     paste0(pathi_to_masks_files, "MCSs_SAAG_",
                            year_ini, "-", year_fim, ".csv"), row.names = FALSE)
  
  gc()
  lbin <- list()
  
  # INICIO DA SEGUNDA PARTE: SELECIONAR AS MASCARAS ####
  # nao da para simplesmente selecionar as datas de dt, pois se em um dia nao tiver MCSs, vai pular aquele horario
  # alterando de forma errada a sequencia de datas da lista a ser criada e consequentemente do netcdf.
  # ud <- sort(unique(dt$date))
  mm_ini <- ifelse(year_ini == year_fim, "01", "06")
  message("Is the initial month ", mm_ini, "?, if not, please stop me!")
  ud <- sort(unique(seq(c(ISOdate(year_ini,mm_ini,1,0)), by = "hour", length.out = ndates))) #
  print(paste0("Início da sequencia de datas \n", head(ud)))
  print(paste0("Término da sequencia de datas \n", tail(ud)))
  
  lbin <- list()
  for(l in seq_along(ud)) {
    yy <- substr(ud[l], 3, 4)
    mm <- substr(ud[l], 6, 7)
    dd <- substr(ud[l], 9, 10)
    #  hh <- substr(ud[l], 9, 10)
    hh <- ifelse(substr(ud[l], 12, 13) == "", "00", substr(ud[l], 12, 13))
    mn <- "00"
    dt_date <- paste0(substr(ud[l], 1, 4), mm, dd, hh)
    
    # Select the dt lines for this date
    
    di <- dt[date == dt_date,]
    # adicionamos nome em di para fazer merge com o df_bin a ser criado a seguir
    di$v_bin2 <- di$`SYS#`
    SYS <- di$`SYS#`
    # SYS_ANT <- ifelse(df$CLA[j] == "M", 
    #                   c(df$SA01[j], df$SA02[j], df$SA03[j], df$SA04[j], df$SA05[j], 
    #                     df$SA06[j], df$SA07[j], df$SA08[j], df$SA09[j], df$SA10[j],
    #                     df$SA11[j], df$SA12[j], df$SA13[j], df$SA14[j], df$SA15[j],
    #                     df$SA16[j], df$SA17[j], df$SA18[j], df$SA19[j], df$SA20[j]),
    #                   0)
    print(paste0("SYS = ", SYS)) # pode ter mais de um SYS!!!
    # print(paste0("SYS_ANT = ", SYS_ANT)) # pode ter mais de um SYS!!!
    
    # Reading binary from fortracc (presume all file exists)
    cluster_name <-  paste0(pathi_to_fortracc_clusters,
                           "gs.", yy, mm, dd, ".", hh, mn, "g.raw")
    cluster <- file(paste0(pathi_to_fortracc_clusters, 
                           "gs.", yy, mm, dd, ".", hh, mn, "g.raw"), "rb")
    
    ifile <- file.exists(cluster_name)     
    
    if(ifile != 1){
      next
    }
    
 
    print(paste0("Lendo cluster: ", paste0(pathi_to_fortracc_clusters,"gs.", yy, mm, dd, ".", hh, mn, "g.raw")))
    v_bin <- readBin(cluster,
                     what = "integer",
                     n = ncols*nlins*2,
                     size = 2)
    close(cluster)
    
    # criar data.table com o vetor do binario de clusters do fortracc e
    # adicionar id no caso merge mude a ordem
    df_bin <- data.table::data.table(id = 1:length(v_bin),
                         v_bin = v_bin)
    # digo quando tenho MCSs
    df_bin[, v_bin2 :=  ifelse(v_bin %in% SYS, v_bin, NA)] # Without merges
    #df_bin[, v_bin2 :=  ifelse(v_bin %in% SYS | v_bin %in% SYS_ANT, v_bin, NA)]
    
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
    lbin[[l]] <- mt_bin2
  }
  
  dbin <- unlist(lbin)
  saveRDS(dbin, 
          file = paste0(pathi_to_masks_files, "APAGAR_MCSs_SAAG_Masks_", year_ini, "-", year_fim, ".rds"))
  
  
  # 4) Save Tb final Masks --> Write the netcdf ####
  # source("export_mask.R")
  write_nc(type = type,
           year_ini = year_ini,
           year_fim = year_fim,
           mm_fim = mm_fim,
           pathi_to_prec_file = pathi_to_prec_file,
           pathi_to_masks_files = pathi_to_prec_file)
  print("Fim!")
}
