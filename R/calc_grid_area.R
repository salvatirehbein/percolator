# library(stars)
# library(sf)
# 
# # obter a area dos pixels do IMERG
# filepath <- system.file("extdata",
#                        # "merg_2011010100_4km-pixel.nc",
#                        #"NICAM_sa_tppn_200001010000.nc",
#                        "olr_pcp_Summer_OBS_t1.nc",
#                         package = "percolator")
# 
# nc <- ncdf4::nc_open(filepath)
# nc_lat <- ncdf4::ncvar_get(nc = nc, varid = "lat")
# nc_lon <- ncdf4::ncvar_get(nc = nc, varid = "lon")
# # x <- ncdf4::ncvar_get(nc = nc, varid = "sa_tppn")
# x <- ncdf4::ncvar_get(nc = nc, varid = "precipitation")
# # xmin <- min(nc_lat)
# 
# rr <- raster::raster(t(x[1:dim(x)[1],
#                          dim(x)[2]:1]),
#                      xmn = min(nc_lon),
#                      xmx = max(nc_lon),
#                      ymn = min(nc_lat),
#                      ymx = max(nc_lat),
#                      crs = "EPSG:4326")
# rss <- st_area(st_as_stars(rr))
# # plot(rss/1000000, axes = T)
# # stars::write_stars(rss/1000000, "/glade/work/arehbein/SAAG/area_imerg_km2.nc")
# # stars::write_stars(rss/1000000,
# #                    system.file(lib.loc = "extdata/",
# #                                "area_imerg_km2.nc",
# #                                package = "percolator"))
# 
# # stars::write_stars(obj = rss/1000000,
# #                    dsn = "inst/extdata/area_nicam14km_km2.nc")
# stars::write_stars(obj = rss/1000000,
#                    dsn = "inst/extdata/area_MCSMIP_km2.nc")


# # multiplication test ####
# # rm(list = ls())
# # nc_area <- nc_open("/glade/work/arehbein/SAAG/area_imerg_km2.nc")
# # area <- t(ncvar_get(nc_area, "Band1"))
# 
# #fakeval <- t(x[1:dim(x)[1],
# #               dim(x)[2]:1])
# #fakeval[] <- NA
# #fakeval[1:1, 1:10] <- 10
# 
# #dim(fakeval)
# #dim(area)
# 
# #class(fakeval)
# #class(area)
# 
# # volume of first line and 10 firsts columns:
# #fakeval[1:1, 1:10]
# #area[1:1,1:10]
# #class(fakeval[1:1, 1:10])    # numeric
# #class(area[1:1,1:10])        # numeric
# 
# #sum(fakeval[1, 1:10]*area[1,1:10], na.rm = T) # two numeric vectors
# #sum(fakeval*area, na.rm = T) # two matrices
# 
