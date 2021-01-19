NEWS
===========

## TODO

- filtra_c usar funcao em C++ para calc distancias
- Documentar em ingles
- Adicionar exemplos

### percolator 0.3.0 (Release date: 2021-01-19)
- Change package name from fortracc.deal to percolator.
- Added fortran subroutines (src/pcp.f90 and src/tb.f90) for
  managing or transforming the ForTraCC outputs (fam_* files) 
  from "block" style to plain text files to be read by the percolator.
  These subroutines are called by the read_pcp and read_tb functions.

### percolator 0.2.0.2 (Release date: 2020-12-04)
- Include option for removing header if it exists
- read lines first and then explode in the commas

### percolator 0.2.0.1 (Release date: 2020-12-04)
- Transform data.table in data.frame before merge in filtra_d.
- Substitute utils::write.csv by data.table::fwrite

### percolator 0.2.0 (Release date: 2020-12-02)
- Add filter by growing curve using data.table instead of derivatives.
- Add the directories "inst/extdata" and the shapefiles for South America and Amazon basin
- Add "Tb" and "IMERG" to the "variaveis" option at the "filtra_a"
- Add "IMERG" in "# Giving names to the columns:" functionality
- Add an unique universal ID at "filtra_a" function. This "ID" is created 
  according to the date, family, and fonte (using a code).
- Fix typo "NICAM HigResMIP" to "NICAM HighResMIP"


### percolator 0.1.0 (Release date: 2017)

- First version. Not public. 