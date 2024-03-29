#' Calculate the time in UTC for each timestep of the MCSs
#'
#' @description Calculate the time in UTC for each timestep of the MCSs
#'
#' @param YEAR String. Year field name.
#' @param MONTH String. Month field name.
#' @param DAY String. Day field name.
#' @param HOUR String. Hour of genesis field name. 
#' @param TIME String. TIME counter lifespan field. E.g.: 0, 0.5, 1.0, 1.5.
#' @return data.frame
#' @export
#' @examples \dontrun{
#' dt$timeUTC <- UTC(YEAR = dt$YEAR,
#'                   MONTH = dt$MONTH,
#'                   DAY = dt$DAY,
#'                   HOUR = dt$HOUR,
#'                   TIME = dt$TIME)
#' }
UTC <- function(YEAR, MONTH, DAY, HOUR, TIME) {
  decimales <- TIME - trunc(TIME)
  decimales2 <- data.table::fifelse(
    decimales < 0.25, 0,
    data.table::fifelse(
      decimales >=0.25 & decimales < 0.75, 0.5,
      1
    )            
  )
  TIMEv2 <- trunc(TIME) + decimales2
  # TIMEv2 <- round(TIME2, 1)
  
  HOURv2 <- round(HOUR, 1)
  HOURv3 <- ifelse((HOURv2 %% 1) > 0.4, #avoid ring of hell!!
                   # se usamos >0.3, vai pegar o valor 0.3000000001
                   round(HOURv2),
                   HOURv2)
  timeINI <-  paste0(YEAR,
                     "-",
                     ifelse(MONTH < 10,
                            paste0("0", MONTH),
                            MONTH),
                     "-",
                     ifelse(DAY < 10,
                            paste0("0", DAY),
                            DAY),
                     " ",
                     ifelse(trunc(HOURv3) < 10,
                            paste0("0", trunc(HOURv3)),
                            trunc(HOURv3)),
                     ":",
                     ifelse((HOURv3 %% 1) != 0,
                            "30:00",
                            "00:00"))
  timeINI <- as.POSIXct(x = timeINI,
                        tz = "GMT",
                        format = "%Y-%m-%d %H:%M:%S")
  timeUTC <- timeINI + TIMEv2*3600
  return(timeUTC)
}
