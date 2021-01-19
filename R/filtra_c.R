#' Filtro pela curva de crescimento, onde a maxima extensao eh o pico da curva.
#' Esta funcao tambem identifica as fases (genesis, maturation, dissipation) dos SCMs..
#'
#' @description Esta funcao le os arquivos filtrados pela bacia e filtra pela curva de crescimento.
#'
#' @param arqi Arquivo de entrada: b_YYYY.csv
#' @param arqo Arquivo de saida: c_YYYY.csv
#' @return data.frame
#' @importFrom data.table fread fwrite fifelse
#' @export
#' @examples \dontrun{
#' filtra_c(arqi = "../b_200101.csv")
#' }
filtra_c <- function(arqi,
                     arqo) {
  cat("Starting filter c... \n")
  f <- data.table::fread(arqi)
  f$nr <- 1:nrow(f)
  
  # Adicionar tres colunas: uma para genese, outra mat, outra diss
  f[,  `:=` (genesis = data.table::fifelse(TIME == min(TIME), TRUE, FALSE), 
             maturation = data.table::fifelse(SIZE == max(SIZE), TRUE, FALSE), 
             dissipation = data.table::fifelse(TIME == max(TIME), TRUE, FALSE)),
    by = ID]
  
  # Acessa o timestep em que acontece a maturacao e cria uma coluna com esta posicao
  f[, mat2 := (1:.N)*maturation, by = ID]
  # Refaz a coluna maturation. Agora sera TRUE no ultimo tempo de maior area.
  f[, maturation := data.table::fifelse(mat2 == max(mat2), TRUE, FALSE), by = ID]
  
  # Teste:
  x <- f[, sum(maturation), by = ID]
  # print(paste0("Tempos com maturacao: ", unique(x$V1)), ". PRECISA ser 1!!!") # Precisa ser 1
  
  # Seleciona os IDs que tem ao mesmo tempo a genese e maturacao ou maturacao e dissipacao
  ids <- unique(f[genesis+maturation+dissipation  > 1]$ID)
  # Filtra, tirando fora os ids com genese e maturacao ocorrendo ao mesmo tempo e/ou 
  # SCMs com maturacao e dissipacao ocorrendo ao mesmo tempo. Isso elimina a necessidade de 
  # usar as derivadas. Porem, se os dados forem de maior resolucao (a cada 10 minutos, por exemplo),
  # acredito que seja melhor incluir mais algum filtro, pois se a maturacao ocorrer no passo seguinte
  # da genese, nao daria tempo para desenvolvimento.
  dfica <- f[!ID %in% ids]
  
  # dim(f)
  # dim(dfica)
  
  dfica$fase <- ifelse(
    dfica$genesis == TRUE, "genesis",
    ifelse(
      dfica$maturation == TRUE, "maturation",
      ifelse(
        dfica$dissipation == TRUE, "dissipation",
        NA)))  
  dfica$genesis <- dfica$maturation <- dfica$dissipation <- dfica$nr <- dfica$mat2 <- NULL
  
  if(!missing(arqo)){
    data.table::fwrite(dfica, arqo, row.names = FALSE)
  } else {
    return(dfica)
  }
}
# 