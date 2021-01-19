#' Lê arquivos output do read_family.R e filtra pela bacia,
#' separando por oceano e continente
#'
#' @description This function reads the output from read_family.R to filter MCSs
#' Over the AMazon basin
#'
#' @param ANO ano
#' @param MES mes
#' @param DIA dia
#' @param HORAINI hora inicial do SCM em UTC
#' @param TIME horas passadas apos o inicio do sistema. Ex: 0, 0.5, 1.0, 1.5
#' @return data.frame
#' @export
#' @examples \dontrun{
#' # Do not run Isso vai aparecer na documentação
#' head(df)
#' hist(df$DURACAO) #
#' }
UTC <- function(ANO, MES, DIA, HORAINI, TIME) {
  TIMEv2 <- round(TIME, 1)
  HORAINIv2 <- round(HORAINI, 1)
  HORAINIv3 <- ifelse((HORAINIv2 %% 1) > 0.4, #avoid ring of hell!!
                      # se usamos >0.3, vai pegar o valor 0.3000000001
                      round(HORAINIv2),
                      HORAINIv2)
  timeINI <-  paste0(ANO,
                     "-",
                     ifelse(MES < 10,
                            paste0("0", MES),
                            MES),
                     "-",
                     ifelse(DIA < 10,
                            paste0("0", DIA),
                            DIA),
                     " ",
                     ifelse(trunc(HORAINIv3) < 10,
                            paste0("0", trunc(HORAINIv3)),
                            trunc(HORAINIv3)),
                     ":",
                     ifelse((HORAINIv3 %% 1) != 0,
                            "30:00",
                            "00:00"))
  timeINI <- as.POSIXct(x = timeINI,
                        tz = "GMT",
                        format = "%Y-%m-%d %H:%M:%S")
  timeUTC <- timeINI + TIMEv2*3600
  return(timeUTC)
}
