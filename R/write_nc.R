#' Save Network Common Data Form (NetCDF) files
#' 
#' @description Export data with TIME, XLAT, XLONG dimensions to a netCDF file. 
#'
#' @param attribute String. Any information to serve as an attribute. Ex:
#' "Observational Tb (GPM MERGEIR) MCSs Masks from ForTraCC-percolator"
#' @param values Vector with numeric or integer values to be written in the
#'  netCDF file.
#' @param date_ini String. First date for the TIME attribute. EX: "2001-01-01 00:00:00"
#' @param ofile Character. Character with the name of the output file.
#' @param ntime Integer. Number of timesteps for the TIME dimension. 
#' EX: ntime = 365*24.
#' @return netCDF
#' @importFrom data.table fread fwrite setorder
#' @importFrom raster raster brick flip
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @export
#' @examples \dontrun{
#'
#' }
write_nc <- function(attribute,
                     values,
                     date_ini = "2001-01-01 00:00:00",
                     ofile = "ofile.nc",
                     ntime,
                     reference_file = "DATA/IMERG/2001/merg_2001010100_4km-pixel.nc",
                     latname="lat",
                     lonname="lon",
                     unit = "",
                     longname = "Tb MCSs Masks",
                     variable_name = "mcs_mask",
                     author_name = "Amanda Rehbein",
                     institution_name = "Climate Group of Studies (GrEC)/University of Sao Paulo (USP)",
                     additional_comments = "Machado et al. (1998), Vila et al. (2008), Rehbein et al. (2020)"){
  
  
  # Obtain a reference file for precipitation (must be from imerg because it has the lat long info)
  arq_nc <- reference_file
  
  nc <- ncdf4::nc_open(filename = arq_nc)
  lon <- ncdf4::ncvar_get(nc = nc, varid = lonname)
  lat <- ncdf4::ncvar_get(nc = nc, varid = latname)
  ncdf4::nc_close(nc)
  nlon <- length(lon)
  nlat <- length(lat)
  # definition of dimensions
  TIME <- ncdf4::ncdim_def(name = "time",
                           vals = 0:(ntime-1), #0:length(unique(dt$date)),
                           units = paste0("hours since ", date_ini),
                           calendar = "proleptic_gregorian")
  XLONG <- ncdf4::ncdim_def(name = "lon",
                            units = "degrees_east",
                            vals = as.double(lon))
  XLAT <- ncdf4::ncdim_def(name = "lat",
                           units = "degrees_north",
                           vals = as.double(lat))
  
  # definition of variables
  var <- ncdf4::ncvar_def(name = variable_name,
                          units = unit, 
                          dim = list(XLONG, XLAT, TIME),
                          longname=longname, 
                          missval = NA) 
  
  vars_file <- ncdf4::nc_create(filename = ofile,
                                vars = var)
  
  cat(paste("The file has", vars_file$nvars, "variables\n"))
  cat(paste("The file has", vars_file$ndim, "dimensions\n"))
  
  # Global attribute to the file when varid = 0
  # otherwise write the variable name
  ncdf4::ncatt_put(nc = vars_file,
                   varid = 0, # 0 para o arquivo
                   attname = "title",
                   # attval = "Observational Tb (GPM MERGEIR) MCSs Masks from ForTraCC-percolator")
                   # attval = "WRF Tb MCSs Masks from ForTraCC-percolator")
                   attval = attribute)
  ncdf4::ncatt_put(nc = vars_file,
                   varid = 0, # 0 para o arquivo
                   attname = "Author",
                   attval = author_name)
  ncdf4::ncatt_put(nc = vars_file,
                   varid = 0, # 0 para o arquivo
                   attname = "institution",
                   attval = institution_name)
  ncdf4::ncatt_put(nc = vars_file,
                   varid = 0, # 0 para o arquivo
                   attname = "history",
                   attval = paste0("Created on ", Sys.time()))
  ncdf4::ncatt_put(nc = vars_file,
                   varid = 0, # 0 para o arquivo
                   attname = "references",
                   attval = additional_comments)
  
  # Add variables to the ofile
  ncdf4::ncvar_put(nc = vars_file,
                   varid = var,
                   vals = values,
                   start =  c(1,1,1),
                   count = c(nlon, nlat, ntime))
  ncdf4::nc_close(nc = vars_file)
}
