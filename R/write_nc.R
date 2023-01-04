#' Save the MASK files from the filtra_saag function
#' 
#' @description Save the Tb MCSs mask files in netCDF format. 
#'
#' @param type Character. Use "Observational" for GPM IMERG or "WRF" for WRF SAAG Simulations.
#' @param year_ini Integer. First year of the tracking (used for writing filename outputs).
#' @param year_fim Integer. First year of the tracking (used for writing filename outputs).
#' @param mm_ini   String. First month of the tracking. It is important for writing the correct time attribute 
#' @param ndates Integer. Number of timesteps ou dates. Used to write the netcdf.
#' @return netCDF
#' @importFrom data.table fread fwrite setorder
#' @importFrom raster raster brick flip
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @export
#' @examples \dontrun{
#' write_nc(type = "Observational",
#'          dt = dt,
#'          pathi_to_prec_file = "/glade/work/arehbein/SAAG/3yr/IMERG/",
#'          pathi_to_masks_files = "/glade/work/arehbein/SAAG/3yr/MCSs/OBS/MCSs_MASKs/")
#' }
write_nc <- function(type,
                     year_ini,
                     year_fim,
                     mm_ini,
                     ndates,
                     pathi_to_prec_file,
                     pathi_to_masks_files){

sufixo <- ifelse(type == "Observational", "OBS", "WRF")
ofile <- paste0(pathi_to_masks_files, 
                "APAGAR_Rehbein_WY", year_fim, "_", sufixo,"_SAAG-MCS-mask-file.nc")

# Obtain a reference file
if(type == "Observational"){
arq_nc <- paste0(pathi_to_prec_file, 
                 "merg_", year_ini, mm_ini,"0100_4km-pixel.nc")
atributo <- "Observational Tb (GPM MERGEIR) MCSs Masks from ForTraCC-percolator"
}
if(type == "WRF"){
arq_nc <- paste0(pathi_to_prec_file, 
                 "tb_rainrate_", year_ini, "-", mm_ini,"-02_12:00.nc")
atributo <- "WRF Tb MCSs Masks from ForTraCC-percolator"
}
nc <- ncdf4::nc_open(filename = arq_nc)
lon <- ncdf4::ncvar_get(nc = nc, varid = "lon")
lat <- ncdf4::ncvar_get(nc = nc, varid = "lat")
ncdf4::nc_close(nc)
nlon <- length(lon)
nlat <- length(lat)
ntime <- ndates #length(unique(dt$date))
# definition of dimensions
TIME <- ncdf4::ncdim_def(name = "time",
                         vals = 0:(ntime-1), #0:length(unique(dt$date)),
                         units = paste0("hours since ",
                                        year_ini, "-", mm_ini,"-01 00:00:00"),
                         calendar = "proleptic_gregorian")
XLONG <- ncdf4::ncdim_def(name = "lon",
                          units = "degrees_east",
                          vals = as.double(lon))
XLAT <- ncdf4::ncdim_def(name = "lat",
                         units = "degrees_north",
                         vals = as.double(lat))

# definition of variables
MCSs_MASKS <- ncdf4::ncvar_def(name = "mcs_mask",
                               units = "", 
                               dim = list(XLONG, XLAT, TIME),
                               longname="Tb MCSs Masks", 
                               missval = NA) 

vars_file <- ncdf4::nc_create(filename = ofile,
                              vars = MCSs_MASKS)

cat(paste("The file has", vars_file$nvars, "variables\n"))
cat(paste("The file has", vars_file$ndim, "dimensions\n"))

# Global attribute to the file when varid = 0
# otherwise write the variable name
ncdf4::ncatt_put(nc = vars_file,
                 varid = 0, # 0 para o arquivo
                 attname = "title",
                 # attval = "Observational Tb (GPM MERGEIR) MCSs Masks from ForTraCC-percolator")
                 # attval = "WRF Tb MCSs Masks from ForTraCC-percolator")
                 attval = atributo)
ncdf4::ncatt_put(nc = vars_file,
                 varid = 0, # 0 para o arquivo
                 attname = "Author",
                 attval = "Amanda Rehbein")
ncdf4::ncatt_put(nc = vars_file,
                 varid = 0, # 0 para o arquivo
                 attname = "institution",
                 attval = "Climate Group of Studies (GrEC)/University of Sao Paulo (USP)")
ncdf4::ncatt_put(nc = vars_file,
                 varid = 0, # 0 para o arquivo
                 attname = "history",
                 attval = paste0("Created on ", Sys.time()))
ncdf4::ncatt_put(nc = vars_file,
                 varid = 0, # 0 para o arquivo
                 attname = "references",
                 attval = "Machado et al. (1998), Vila et al. (2008), Rehbein et al. (2020)")

# Add variables to the ofile
ncdf4::ncvar_put(nc = vars_file,
                 varid = MCSs_MASKS,
                 vals = dbin,
                 start =  c(1,1,1),
                 count = c(nlon, nlat, ntime))
ncdf4::nc_close(nc = vars_file)
}
