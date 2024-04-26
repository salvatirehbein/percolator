#' Post-processing ForTraCC outputs to obtain MCSs using the SAAG Deep Convection Group criteria
#'
#' This function identifies and filters Mesoscale Convective Systems (MCSs) based on specific criteria 
#' established by the SAAG Deep Convection Working Group. It creates masks for further analysis or visualization.
#'
#' @description 
#' The function performs the following steps:
#' 
#' - Filtering by Tb (brightness temperature) thresholds.
#' - Filtering by size criteria.
#' - Filtering by precipitation (PCP) criteria.
#' - Data processing and preparation.
#' - Mask creation for MCSs.
#'
#' @param year_start Integer. Starting year for filtering the data.
#' @param month_start Integer. Starting month for filtering the data.
#' @param day_start Integer. Starting day for filtering the data.
#' @param hour_start Integer. Starting hour for filtering the data.
#' @param year_end Integer. Ending year for filtering the data.
#' @param month_end Integer. Ending month for filtering the data.
#' @param day_end Integer. Ending day for filtering the data.
#' @param hour_end Integer. Ending hour for filtering the data.
#' @param ncols Integer. Number of columns relative to the native data.
#' @param nlins Integer. Number of lines relative to the native data.
#' @param family_file Character. Path to the family file obtained by get_mcs_csv.R
#' @param path_to_fortracc_clusters Character. Path to the directory containing Fortracc clusters.
#' @param cluster_prefix Character. Prefix for cluster files.
#' @param ofile_mask Character. Output NetCDF file path for masks.
#' @param attribute Character. Description of the attribute for the NetCDF file.
#' @param reference_file Character. Path to the reference file.
#' @return netCDF
#' @importFrom data.table fread setorder :=
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @importFrom utils head tail
#' @export
#' @examples \dontrun{
#' get_mcs_masks(year_start = 2001,
#'             month_start = 1,
#'             day_start = 1,
#'             hour_start = 0,
#'             year_end = 2001,
#'             month_end = 1,
#'             day_end = 31,
#'             hour_end = 23,
#'             ncols = 480,
#'             nlins = 690,
#'             family_file = "MCSs_MASK/MCS_SAAG_WRF_p_2000.csv",
#'             path_to_fortracc_clusters = "clusters/",
#'             cluster_prefix = "WRF_20y_p_",
#'             ofile_mask = "MCSs_MASK/2000_p_WRF_SAAG-MCS-mask-file.nc",
#'             attribute = "Description of the attribute",
#'             reference_file = system.file("extdata/merg_2011010100_4km-pixel.nc", 
#'                                          package = "percolator")
#' ) }
get_mcs_masks <- function(year_start,
                          month_start,
                          day_start,
                          hour_start,
                          year_end,
                          month_end,
                          day_end,
                          hour_end,
                          ncols,
                          nlins,
                          family_file,
                          path_to_fortracc_clusters,
                          cluster_prefix,
                          ofile_mask,
                          attribute = "Tb MCSs Masks from ForTraCC-percolator",
                          reference_file = system.file("extdata/merg_2011010100_4km-pixel.nc", 
                                                       package = "percolator")
) {
  
  # Read the data table from get_mcs_csv.R
  dt <- data.table::fread(family_file)
  
  ud <- sort(unique(seq(ISOdate(year_start, month_start, day_start, hour_start, tz = "UTC"),
                        ISOdate(year_end, month_end, day_end, hour_end, tz = "UTC"),
                        by = "hour")))
  ud2 <- strftime(ud, format = "%Y%m%d%H", tz = "UTC")
  print(paste0("Start of dates sequency \n", utils::head(ud)))
  print(paste0("End of dates sequency \n", utils::tail(ud)))
  
  # Initialize list for masks
  lbin <- list()
  
  # Loop over unique dates to create masks
  for (l in seq_along(ud)) {
    
    dt_date <- ud2[l] 
    
    # Select the data for this date
    di <- dt[date == dt_date, ]
    
    if (nrow(di) != 0) {
      
      # Extract SYS values
      di$v_bin2 <- di$`SYS#`
      SYS <- di$`SYS#`
      
      # Print SYS values (for debugging)
      print(paste0("date = ", dt_date, "  SYS = ", SYS))
      
      # Construct cluster file name
      cluster_name <- list.files(path = path_to_fortracc_clusters,
                                 pattern = paste0(cluster_prefix,
                                                  substr(ud2[l], 1, 4),
                                                  substr(ud2[l], 5, 6), 
                                                  substr(ud2[l], 7, 8),
                                                  substr(ud2[l], 9, 10)),
                                 full.names = TRUE)
      
      # Check if cluster file exists
      if (file.exists(cluster_name)) {
        print(paste0("Reading cluster: ", cluster_name))
        
        # Read binary cluster file
        cluster <- file(cluster_name, "rb")
        v_bin <- readBin(cluster,
                         what = "integer",
                         n = ncols * nlins * 2,
                         size = 2)
        close(cluster)
        
        # Create data table for binary clusters
        df_bin <- data.table::data.table(id = 1:length(v_bin),
                                         v_bin = v_bin)
        
        # Identify MCSs in binary clusters
        df_bin[, v_bin2 := ifelse(v_bin %in% SYS, v_bin, NA)]
        
        # Merge binary clusters with filtered families
        df_bin <- merge(x = df_bin,
                        y = di[, c("v_bin2", "FAMILY_new")],
                        by = "v_bin2",
                        all.x = TRUE)
        data.table::setorderv(df_bin, "id")
        
        # Convert binary clusters to matrix format
        m_bin2 <- matrix(df_bin$FAMILY_new,
                         nrow = ncols,
                         ncol = nlins)
        lbin[[l]] <- m_bin2
      } else {
        print(paste0("File cluster ", cluster_name, " doesn`t exists!"))
        # If cluster file does not exist, create a matrix of NA values
        m_bin2 <- matrix(NA,
                         nrow = ncols,
                         ncol = nlins)
        lbin[[l]] <- m_bin2
      }
      
    } else {
      m_bin2 <- matrix(NA,
                       nrow = ncols,
                       ncol = nlins)
      lbin[[l]] <- m_bin2
    }
  }
  
  # Create a single vector containing all the masks for MCSs
  dbin <- unlist(lbin)
  
  # # Define the filename for the RDS file that will store the masks
  # fileo <- paste0(path_to_masks_files,
  #                "MCSs_SAAG_Masks_", year_start, "-", year_end, ".rds")
  # 
  # # Check if a file with the same name already exists
  #  if (file.exists(fileo)) {
  #    file.remove(fileo)
  #  } else {
  #    print("no files to remove")
  #  }
  # 
  # # Save the dbin vector as an RDS file with the defined filename
  #  saveRDS(dbin, file = fileo)
  
  # Save netCDF with MCSs masks
  date_ini <- paste0(year_start, "-", 
                     sprintf("%02d", month_start), "-",
                     sprintf("%02d", day_start), " ",
                     sprintf("%02d", hour_start), ":00:00")
  
  write_nc(attribute=attribute,
           values=dbin,
           date_ini=date_ini,
           ofile=ofile_mask,
           ntime=length(ud2),
           reference_file = reference_file)
  
  print(paste0("MCSs final masks at netCDF for ", year_start,
               " to ", year_end, " were just obtained. "))
  print(paste0("See: ", ofile_mask))
  print("FIM !!!!! ")
  # Need to run write_nc for generating the Mask files."
}
