#' Transform ForTraCC outputs from block format to plain text
#'
#' @description This function transform the ForTraCC output 
#' (from precipitation) that has a block format (with head 
#' and tail information) in a plain text, with no blank lines
#' in between the families. It takes  information from the
#' header and write it as many time as the duration (or TIME).
#' 
#' This function calls a fortran subroutine for reading ForTraCC
#' "fam" archives outuput and rewrite them in .csv format (or other).
#' 
#' @param filei character. Name of input file. Generally starts
#' with "fam". Maximum of 100 characters.
#' @param fileo character. Name of output file. For running the other filters,
#' it must be "csv" format. Maximum of 100 characters.
#' @return character
#' @useDynLib percolator
#' @export
#' @examples \dontrun{
#' read_pcp(filei = "/home/amanda/Documents/filtra_scms/TESTE/pcp_fam_20001231.txt",
#'          fileo = "/home/amanda/Documents/filtra_scms/TESTE/pcp_out_20001231.txt")
#' }
read_pcp <- function(filei, fileo) {
  out <- .Fortran("pcp",
                  filei = as.character(filei),
                  fileo = as.character(fileo))
  return(out)
}