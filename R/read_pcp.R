#' Transform ForTraCC outputs from block format to plain text
#'
#' @description This function transform the ForTraCC output 
#' (from precipitation) that has a table format (with header 
#' and footnote information) in a plain text, with no blank lines
#' in between the families. It takes information from the
#' header and write it as many time as the duration (or TIME).
#' 
#' This function calls a fortran subroutine for reading ForTraCC
#' "fam" outputs and rewrite them in a .csv format.
#' Due to memory limitation, this function cannot be used for cloud
#' systems producing more than 200 lines in the table body.
#' 
#' @param ifile character. Name of input file. Generally starts
#' with "fam". Maximum of 100 characters.
#' @param ofile character. Name of output file. For running the other filters,
#' it must be "csv" format. Maximum of 100 characters.
#' @return character
#' @useDynLib percolator
#' @export
#' @examples \dontrun{
#' read_pcp(ifile = system.file("extdata", "fam_20001231.txt", package = "percolator"),
#'          ofile = "~/Documents/pos_doc/FAPESP/SAAG/teste.csv")
#' }
read_pcp <- function(ifile, ofile) {
  out <- .Fortran("pcp",
                  ifile = as.character(ifile),
                  ofile = as.character(ofile))
  return(out)
}
