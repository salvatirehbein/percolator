% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filtra_a.R
\name{filtra_a}
\alias{filtra_a}
\title{Filter MCSs by lifespan and area of genesis}
\usage{
filtra_a(
  ifile,
  ofile,
  classif = "N",
  filtra_time = 3,
  classifica_lifespan = 5.7
)
}
\arguments{
\item{ifile}{Character. Input filename. Generally as YYYYMM.txt}

\item{ofile}{Character. Output filename. Sugested: a_YYYYMM.csv}

\item{classif}{Character. Select only new systems if classif="N" (default). 
Classif=NULL doesn't filter the systems.}

\item{filtra_time}{Numeric. Tells the minimum lifespan the remaining MCSs 
should have. In general, they live 3 hours or more. This is the default value.}

\item{classifica_lifespan}{Numeric. Allows to classify between long- or 
short-lived MCS. Require a threshold to identify the lifespans. 
Default is 5 hours and 30 minutes.}
}
\value{
data.table
}
\description{
It is strongly recommended using filtra_a function before the 
other filters. This function reads the files ouputed by fortran routine/function
(YYYYMM.txt), creates IDs for each family of cloud cluster, creates lifespan, timeUTC,
fonte, and period fields, filter data by lifespan above a given threshold, 
classifies each family by its total lifespan, and then save the final dataset.
}
\examples{
\dontrun{
filtra_a(ifile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/p_200101010000.txt", 
         ofile = "/media/amanda/Elements/AR/SAIDAS_FORTRACC/fam/IMERG/a_200101010000.txt")
}
}
