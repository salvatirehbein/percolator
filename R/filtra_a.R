#' Lê arqvuivos fam_*.csv, coloca o header e retorna arq a_YYYY.csv para
#' ser filtrado pela bacia, curva e deslocamento.
#'
#' @description This function reads the fam_*.csv files,
#' excludes the white spaces between the families,
#' gives the names to the columns, creates IDs, and
#' save the new data.frame.
#'
#' @param arqi Arquivo de entrada. Se for "OLR" tera formato fam_YYYYMM.csv;
#' Se for "PCP" fam_R12534354_YYYYMMDDHHMM.csv)
#' @param header Boolean. Default is FALSE. Has the arqi file a header?
#' @param arqo Arquivo de saida: a_YYYY.csv
#' @param variavel Qual variavel dos dados de entrada para o ForTraCC. 
#' Escolher entre "PCP" (precipitacao) e "OLR"
#' @param fonte Fonte de dados: "CMORPH", "NICAM AMIP" ou "NICAM HigResMIP"
#' @param epoca Escolher entre "presente" ou "futuro"
#' @param filtra_time SCMs devem ter no minimo filtra_time horas de duracao.
#'  Em geral, SCMs sao maiores ou iguais a 3 horas
#' @param classifica_lifespan Limiar para classificar SCMs em short- ou long-lived
#' @return data.frame
#' @importFrom data.table fread setcolorder
#' @importFrom readr read_csv
#' @importFrom stats na.omit
#' @export
#' @examples \dontrun{
#' filtra_a(arqi = "200001.csv", 
#'          arqo = "a_200001.csv",
#'          fonte = "CMORPH",
#'          variavel = "PCP",
#'          epoca = "presente",
#'          filtra_time = 3)
#' }
filtra_a <- function(arqi,
                     arqo,
                     fonte,
                     variavel,
                     epoca,
                     header = FALSE,
                     filtra_time = 3.0,
                     classifica_lifespan = 5.7) {
  cat("Starting filter a... \n")
  cat(paste0("\n Filtrando pelo ciclo de vida!!!!!!! No final teremos familias de 
             sistemas com ciclo de vida acima de ", filtra_time, " horas \n"))
  fontes <- c("Tb", "CMORPH", "IMERG", "NICAM AMIP", "NICAM HighResMIP")
  if(missing(fonte)){
    choice <- utils::menu(fontes, title="Choose var")
    fonte <- fontes[choice]
  }
  variaveis <- c("PCP", "OLR")
  if(missing(variavel)){
    choice <- utils::menu(variaveis, title="Choose var")
    variavel <- variaveis[choice]
  }
  epocas <- c("presente", "futuro")
  if(missing(epoca)){
    choice <- utils::menu(epocas, title="Choose var")
    epoca <- epocas[choice]
  }
  
  # Antes 
  # df <- data.table::fread(arqi, fill = T)
  # df$V1 <- NULL
  
  # dff <- readLines(arqi)
  # # Separa os elementos pela virgula e cada linha em elementos de uma lista
  # dx <- lapply(dff, strsplit, split = ",")
  # lx <- lapply(dx, unlist)
  # lxx <- lapply(lx, length)
  # max(unlist(lxx))
    
  lx <- readr::read_csv(file = arqi, col_names = header)
  ## sapply(lx, class)
  
  # # Apagando o cabecalho original de IR & header = TRUE
  # if (header == TRUE) {
  #   lx <- lx[-1,]
  # } else {
  #   lx <- lx
  # }
  
  df <- as.data.frame(lx)
  data.table::setDT(df)
  n1 <- nrow(df)
  df <- unique(df)
  cat(paste0("Removed ", n1 - nrow(df), " duplicated\n"))
  
  # Removing blank lines:
  if (header == FALSE) {
    df$X1 <- NULL
    df <- stats::na.omit(df, cols="X2")
  } else {
    df$ID <- NULL
    df <- stats::na.omit(df, cols="FAMILY")
  }
  
  # Giving names to the columns:
  if (variavel == "PCP") {
    if (fonte %in% c("CMORPH", "IMERG")) {
      nomes <- c("FAMILY", "YEAR", "MONTH", "DAY", "HOUR",
                 "DURATION", "STAR", "SYS", "XLAT", "XLON",
                 "TIME", "SIZE", "DSIZE", "PMED", "DPMED",
                 "PMAX", "DPMAX", "PMAX9", "DPMAX9", "FRAC",
                 "VEL", "DIR", "T_INI", "CLA", "SYS_ANT")
      names(df)[1:25] <- nomes
      # Usava abaixo para o shell mas decidi manter
      df$FALHA <- ifelse(df$STAR == "*", 1, 0);
      df$STAR <- NULL
      
    } else {
      nomes <- c("FAMILY", "YEAR", "MONTH", "DAY", "HOUR",
                 "DURATION", "SYS", "XLAT", "XLON",
                 "TIME", "SIZE", "DSIZE", "PMED", "DPMED",
                 "PMAX", "DPMAX", "PMAX9", "DPMAX9", "FRAC",
                 "VEL", "DIR", "T_INI", "CLA", "SYS_ANT")
      names(df)[1:24] <- nomes
      df$FALHA <- 0
    }
  } else if (variavel == "OLR"){
    nomes <- c("FAMILY", "YEAR", "MONTH", "DAY", "HOUR", "DURATION",
               "STAR", "SYS", "XLAT", "XLON","TIME", "SIZE","DSIZE",
               "TMED", "DTMED", "TMIN", "DTMIN", "TMIN9", "DTMIN9",
               "CBNUM", "CBMED", "VEL", "DIR", "INCLI", "ECCE",
               "T_INI", "T_FIN", "CLA", "SYS_ANT")
    names(df)[1:29] <- nomes
    df$FALHA <- ifelse(df$STAR == "*", "virtual images",
                       ifelse(df$STAR == "+", "forecast",
                              ifelse(df$STAR == "-", "previous images",
                                     "actual time")))
  } else {
    nomes <- c("FAMILY", "YEAR", "MONTH", "DAY", "HOUR", "CLASSIF",
               "DURATION", "STAR", "SYS", "XLAT", "XLON","TIME", "SIZE",
               "DSIZE","TMED", "DTMED", "TMIN", "DTMIN", "TMIN9",
               "DTMIN9","CBNUM", "CBMED", "VEL", "DIR", "INCLI",
               "ECCE","T_INI", "T_FIN", "CLA", "SYS_ANT")
    names(df)[1:31] <- nomes
    df$FALHA <- ifelse(df$STAR == "STAR", 1, 0);
    df$STAR <- NULL
  }
  
  code <- ifelse(fonte == "Tb", "Tb",
                 ifelse(fonte == "CMORPH", "CMORPH",
                        ifelse(fonte == "IMERG", "IMERG",
                               ifelse(fonte == "NICAM AMIP" & epoca == "presente",
                                      "NICAM_AMIP_p",
                                      ifelse(fonte == "NICAM AMIP" & epoca == "futuro",
                                             "NICAM_AMIP_f",
                                             ifelse(fonte == "NICAM HighResMIP" & epoca == "presente",
                                                    "NICAM_HighResMIP_p",
                                                    ifelse(fonte == "NICAM HighResMIP" & epoca == "futuro",
                                                           "NICAM_HighResMIP_f", NA)))))))
  
  # Writing ID's:
  cat("Creating ID\n")
  df$ID <- paste0(code, "_",
                  df$YEAR, 
                  ifelse(df$MONTH > 9, 
                         as.character(df$MONTH), 
                         paste0("0", df$MONTH)),
                  ifelse(df$DAY > 9, 
                         df$DAY, 
                         paste0("0", df$DAY)), "_", 
                  df$FAMILY)
  
  cat(paste0("\n Total families: ", 
             length(unique(df$ID))),"\n")
  
  # Adding Time UTC in which MCSs Occurred:
  cat("Adding time UTC ID\n")
  df$timeUTC <- UTC(ANO = df$YEAR,
                    MES = df$MONTH,
                    DIA = df$DAY,
                    HORAINI = df$HOUR,
                    TIME = df$TIME)
  # Reorder collumns for put ID in the first collumn:
  df <- data.table::setcolorder(x = df,
                                neworder = c(length(df),
                                             1:(length(df)-1)))
  # Adding new columns
  df$fonte <- fonte
  df$epoca <- epoca
  df$variavel <- variavel
  df$LIFESPAN <- ifelse(df$DURATION < classifica_lifespan, "Short-lived", "Long-lived")
  
  # FIltra pelo tempo de vida:
  #dff <- subset(df, df$DURATION >= filtra_time)
  dff <- df[DURATION >= filtra_time]
  
  
  # filtering size
  minsize <- ifelse(
      fonte == "Tb", 150,
      ifelse(
        fonte == "CMORPH", 37,
        ifelse(
          fonte == "IMERG", 20,
          12)))
  cat(paste0("Filtering min size of ",minsize, "\n"))
  idsruim <- unique(dff[SIZE < minsize, ]$ID)
  dff <- dff[!ID %in% idsruim]

  cat(paste0("\n Filtra A Families: ", 
             length(unique(dff$ID))),"\n ")
  
  # Output:
  if(!missing(arqo)){
    data.table::fwrite(x = dff, file = arqo, quote = T, row.names = F)
  } else {
    return(dff)
  }
}

