#' Post-processing ForTraCC outputs to obtain MCSs using the SAAG Deep Convection Group criteria
#'
#' This function identifies and filters Mesoscale Convective Systems (MCSs) based on specific criteria 
#' established by the SAAG Deep Convection Working Group. It creates final plain text dataset
#' for analysis and visualization.
#'
#' @description 
#' The function performs the following steps:
#' 
#' - Filtering by Tb (brightness temperature) thresholds.
#' - Filtering by size criteria.
#' - Filtering by precipitation (PCP) criteria.
#'
#' @param family_file Character. Path to the family file obtained by Fortracc. Must start with "fam_".
#' @param ofile_csv Character. Output CSV file path for filtered data.
#' @return csv
#' @importFrom data.table fread fwrite setorder :=
#' @export
#' @examples \dontrun{
#' get_mcs_masks(family_file = "fam_SAAG_Tb_pcp_info_intermedio_FINAL_2001-2001_01.csv",
#'               ofile_csv = "MCSs_MASK/MCS_SAAG_WRF_p_2000.csv"
#' ) }
get_mcs_csv <- function(family_file,
                        ofile_csv
) {
  
  # Read the data table from the family file
  dt <- data.table::fread(family_file)
  
  # Print the number of families before filtering
  print(paste0("# FAMILIES (before filter): ", length(unique(dt$FAMILY))))
  
  # Filter by Tb (brightness temperature) =< 225 K during 4 hours at least one pixel
  dt$Tb_le_225K <- ifelse(dt$TMIN < 225 * 100, 1, 0)
  dt[, counter2 := data.table::rowid(data.table::rleid(Tb_le_225K)), by = FAMILY]
  FAMILIES <- dt[Tb_le_225K == 1 & counter2 >= 4, unique(FAMILY)]
  if (length(FAMILIES) > 0) {
    dt <- dt[FAMILY %in% FAMILIES,]
  }; rm(FAMILIES)
  
  # Apply size criteria: minimum area of 40,000 km2 for at least 4 continuous hours
  dt$criterio_size <- ifelse(dt$Tb_SIZE_km2 >= 40000, 1, 0)
  dt[, counter := data.table::rowid(data.table::rleid(criterio_size)), by = FAMILY]
  FAMILIES <- dt[criterio_size == 1 & counter >= 4, unique(FAMILY)]
  if (length(FAMILIES) > 0) {
    dt <- dt[FAMILY %in% FAMILIES,]
  }; rm(FAMILIES)
  
  # Filter by precipitation (PCP) criteria
  # 1 px >= 10mm/h in T>=4h
  dt$pcp_min_criteria <- ifelse(dt$pcp_max >= 10, 1, 0)
  dt[, counter3 := data.table::rowid(data.table::rleid(pcp_min_criteria)), by = FAMILY]
  FAMILIES <- dt[pcp_min_criteria == 1 & counter3 >= 4, unique(FAMILY)]
  if (length(FAMILIES) > 0) {
    dt <- dt[FAMILY %in% FAMILIES,]
  }; rm(FAMILIES)
  
  # Minimum rainfall volume of 20,000 km2 mm/h at least once in the lifetime of the MCS
  dt$vol_criteria <- ifelse(dt$pcp_total_vol >= 20000, 1, 0)
  FAMILIES <- dt[vol_criteria == 1, unique(FAMILY)]
  if (length(FAMILIES) > 0) {
    dt <- dt[FAMILY %in% FAMILIES,]
  }; rm(FAMILIES)
  
  # Remove unnecessary variables
  dt <- dt[, -c("vol_criteria", "pcp_min_criteria", 
                "counter", "counter2", "counter3")]
  
  # Print the number of families after filtering
  print(paste0("# FAMILIES (after filter PCP): ", length(unique(dt$FAMILY))))
  
  # Assign unique IDs to each family
  dt[, FAMILY_new := .GRP, by = "FAMILY"]
  dt$date <- paste0(sprintf(dt$YEAR, fmt = "%04d"),
                    sprintf(dt$MONTH, fmt = "%02d"),
                    sprintf(dt$DAY, fmt = "%02d"),
                    sprintf(dt$HOUR, fmt = "%02d"))
  
  if (file.exists(ofile_csv)) {
    file.remove(ofile_csv)
  } else {
    print("no files to remove")
  }
  
  # Write the filtered data to a CSV file
  data.table::fwrite(dt, ofile_csv, row.names = FALSE)
  print(paste0("Filtered data saved to CSV file: ", ofile_csv))
  
  print("FIM !!!!! ")
  # Need to run write_nc for generating the Mask files."
}