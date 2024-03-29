#' Filter MCS by SIZE x TIME curve. Deprecated.
#'
#' @description This function filter MCSs by their SIZE x TIME curve.
#' The pre_filter function, eliminates those families that have more than
#' one phase at the same time. For instance, the maturation is the 1st or last
#' timestep of the MCSs. 
#' But, it doesn't guarantee that the genesis will be followed by a development 
#' until the maturation, or decayment until dissipation. Which means fases can 
#' happen one imediately another. 
#' If you wish to discard those MCSs with short development before maturation and/or
#' short decayment after maturation, please use this filter (filtra_c).
#'
#' @param ifile Character. Input filename. Generally as b_YYYYMM.txt
#' @param ofile Character. Output filename. Sugested: c_YYYYMM.txt
#' @param threshold Numeric. Define the fraction of time that the system must have before 
#' and after the maturation. Default is 0.5, so MCSs must have at least 50% of 
#' the time increasing/decreasing before/after maturation.
#' @return data.table
#' @importFrom data.table fread rbindlist fwrite
#' @export
#' @examples \dontrun{
#' ifile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/b_200101010000.csv"
#' filtra_c(ifile, 
#'          ofile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/c_200101010000.txt")
#' }
filtra_c <- function(ifile,
                     ofile,
                     threshold = 0.5) {
  message("Starting filter c... \n")
  
  dt <- data.table::fread(ifile)

  ###############################################
  # par(mfrow = c(1, 3))
  # for(i in c(3, 5, 6)) dt[total_time ==3][ID %in% unique(ID)[i], plot(TIME, SIZE, type = "b", pch = 16, main = ID[i])]
  # dt <- dt[total_time ==3][ID %in% unique(ID)[c(3,5,6)]]
  # i = 2
  # dt[total_time ==3][ID %in% unique(ID)[i], plot(TIME, SIZE, type = "b", pch = 16, main = ID[i])]
  ###############################################
  
  
  
  # 1) Verify the times before, after, and during the maturation:
  message("Indicating the times before, during, and after maturation...")
  message("This will take some minutes...")
  # diff_size is the SIZE difference from the timestep one ahead and the before.
  # Positive values indicate growing, while negative indicate decayment. 
  # dt$diff_size <- c(diff(dt$SIZE), NA)
  lx <- split(dt, dt$ID)
  dx <- lapply(1:length(lx), function(i){
    data.frame(class = ifelse(lx[[i]]$timeUTC < lx[[i]][lx[[i]]$phase == "maturation"]$timeUTC,
                              "before_mat",
                              ifelse(lx[[i]]$phase == "maturation", "peak", "after_mat")),
               ID = lx[[i]]$ID,
               diff_size = c(diff(lx[[i]]$SIZE), NA), 
               timeUTC = lx[[i]]$timeUTC)
  })
  dx <- data.table::rbindlist(dx)
  dtt <- merge(dt, dx, 
               by = c("ID", "timeUTC"), all.x = TRUE)
  
  dtt[, diff_size2 := c(NA, diff(SIZE)), by=.(ID)]
  
  dtt$diff_size3 <- ifelse(dtt$class == "before_mat", dtt$diff_size,
                           ifelse(dtt$class == "after_mat", dtt$diff_size2,NA))
  
  # 2) 
  # Se porcentagem de (lenght(diff_size) > 0) > 50% antes do pico &
  #    porcentagem de (lenght(diff_size) < 0) > 50% depois do pico --> eh SCM
  
  dtt$um <- 1
  # Calcula quantos % do tempo de vida do MCSs esteve antes e depois da maturacao
  dtt[, perc := um / sum(um), by = .(ID, class) ]
  
  # Coloca a % calculada anteriormente negativa quando diff_size eh negativa
  dtt$perc_posneg <- ifelse(dtt$diff_size3 < 0, dtt$perc*(-1), dtt$perc)
  
  # soma perc_posneg antes e depois da maturacao
  dtt[ (class == "before_mat" & perc_posneg > 0 |
         class == "after_mat" & perc_posneg < 0), 
       perc2 := abs(sum(perc_posneg, na.rm = TRUE)), by = .(ID, class) ]
  
  # 3)
  # Verifica se a soma % de positivos (antes da maturacao) é maior que 50%
  #        e se a soma % de negativos (depois da maturacao) é menor que 50%
  dtt$cond1 <- ifelse(dtt$perc2 <= threshold, "sairia", "ficaria")
  
  id_sai <- unique(dtt[cond1 == "sairia"]$ID)

  # 4) Filtra e salva
  df <- dtt[!ID %in% id_sai]

  df <- df[, -c("class", "diff_size", "diff_size2", "diff_size3", "um", "perc", "perc_posneg", "perc2", "cond1")]
  
  if(!missing(ofile)){
    data.table::fwrite(df, ofile, row.names = FALSE)
  } else {
    return(df)
  }
}
# 