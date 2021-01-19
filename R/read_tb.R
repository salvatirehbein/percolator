#' Transform ForTraCC outputs from block format to plain text
#'
#' @description This function transform the ForTraCC output 
#' (from tb) that has a block format (with head 
#' and tail information) in a plain text, with no blank lines
#' in between the families. It takes  information from the
#' header and write it as many time as the duration (or TIME).
#' 
#' This function calls a fortran subroutine for reading ForTraCC
#' "fam" archives outuput and rewrite them in .csv format (or other).
#' This routine must not be used with files with more than 40000 families!!!
#' 
#' @param filei character. Name of input file. Generally starts
#' with "fam". Maximum of 100 characters.
#' @param fileo character. Name of output file. For running the other filters,
#' it must be "csv" format. Maximum of 100 characters.
#' @return character
#' @useDynLib percolator
#' @export
#' @examples \dontrun{
#' read_tb(filei = "/home/amanda/Documents/filtra_scms/TESTE/tb_fam1403_s2.txt",
#'         fileo = "/home/amanda/Documents/filtra_scms/TESTE/tb_out.txt")
#' }
read_tb <- function(filei, fileo) {
  out <- .Fortran("tb",
                  filei = as.character(filei),
                  fileo = as.character(fileo))
  warning("Attention!!! This routine must not be used 
          with files with more than 40000 families!!!")
  return(out)
}