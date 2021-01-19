#' Filtra pela distancia entre um timestep e outro. Tambem calcula deslocamento total de cada SCM.
#'
#' @description A funcao filtra_d
#'
#' @param arqi Arquivo de entrada: c_YYYY.csv
#' @param arqo Arquivo de saida: d_YYYY.csv
#' @param dist_max Maxima distancia permitida em km entre um passo de tempo e outro.
#' No mestrado usei 300 km a cada 30 min.
#' @param coords Nomes das colunas das coordenas longitude e latitude (nesta ordem)
#' @return data.frame
#' @importFrom data.table fread
#' @importFrom utils write.csv
#' @importFrom geosphere distm distHaversine
#' @export
#' @examples \dontrun{
#' filtra_d(arqi = "../c_200101.csv")
#' }
arqi <- "/media/amanda/Elements/AR/SAIDAS_FORTRACC/CMORPH/c_200012.csv"
filtra_d <- function(arqi,
                     arqo,
                     dist_max = 300,
                     coords = c("LONX", "LATY")) {
  cat("Starting filtra_d... \n")
  df <- data.table::fread(arqi)
  arqi <- as.data.frame(df)
  x_ini <- arqi[[coords[1]]]
  y_ini <- arqi[[coords[2]]]
  x_fim <- x_ini[c(2:length(x_ini), NA)]
  y_fim <- y_ini[c(2:length(y_ini), NA)]


  df2 <- data.frame(ID = df$ID,
                    fase = df$fase,
                    x_ini,
                    y_ini,
                    x_fim,
                    y_fim)

  for (i in 1:nrow(df2)){
    df2$distm[i] <- geosphere::distm(c(df2$x_ini[i], df2$y_ini[i]),
                                     c(df2$x_fim[i], df2$y_fim[i]),
                                     fun = geosphere::distHaversine)
  }

  df2$distkm <- ifelse(df2$fase == "dissipation" & !is.na(df2$fase),
                       0,
                       (df2$distm)/1000)
  df$dist_km <- df2$distkm

  # Filtrar
  df$criteria_length <- ifelse(as.numeric(df$dist_km) > dist_max,
                               "BAD", "GOOD")
  IDS <- df[df$criteria_length %in% "BAD", ]$ID

  if (length(IDS) == 0) {
    dff <- df
  } else {
    dff <- df[!df$ID%in%IDS,]
  }

  dff$criteria_length <- NULL

  nrow(dff); nrow(df); nrow(dff) == nrow(df)

  # Deslocamento: selecionar 1 e ultimo passo de tempo por id
  dd <- dff[dff$fase %in% c("genesis", "dissipation"), ]

  x_ini <- dd[[coords[1]]]
  y_ini <- dd[[coords[2]]]
  x_fim <- x_ini[c(2:length(x_ini), NA)]
  y_fim <- y_ini[c(2:length(y_ini), NA)]

  dd1 <- data.frame(ID = dd$ID,
                    fase = dd$fase,
                    x_ini,
                    y_ini,
                    x_fim,
                    y_fim)
  dd2 <- dd1[dd1$fase != "dissipation" |  is.na(dd1$fase) , ]
  nrow(dd2) == length(unique(dff$ID)) # TRUE

  for (i in 1:nrow(dd2)){
    dd2$desloc_m[i] <- geosphere::distm(c(dd2$x_ini[i], dd2$y_ini[i]),
                                        c(dd2$x_fim[i], dd2$y_fim[i]),
                                        fun = geosphere::distHaversine)
  }

  dd2$desloc_km <- (dd2$desloc_m)/1000
  dd3 <- data.frame(ID = dd2$ID,
                    desloc_km = dd2$desloc_km)

  ddf <- merge(as.data.frame(dff), dd3, by = "ID", all.x = TRUE)
  
  # Output:
  if(!missing(arqo)){
    data.table::fwrite(x = ddf, file = arqo, quote = T, row.names = F)
  } else {
    return(ddf)
  }
}
