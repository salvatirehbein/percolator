NEWS
===========

## TODO

- adicionar uma opcao para digitar a fonte caso esquece-la na funcao pre_filter
- filtra_d usar funcao em C++ para calc distancias
- Revisar Ingles e terminar de traduzir

### percolator 0.4.8 (Release date: 2022-08-18)
- Create filtra_saag function for obtaining MCSs to SAAG Deep Convection Working 
  Group project.

### percolator 0.4.7 (Release date: 2022-07-02)
- Add if...else in pre_filter for choosing not filtering by growing curve.

### percolator 0.4.6 (Release date: 2021-11-26)
- Add function for calculate density maps (calc_density)

### percolator 0.4.5 (Release date: 2021-11-09)
- Add timeLT function for obtaining local time (see pre_filter)

### percolator 0.4.4 (Release date: 2021-05-12)
- Add a condition for those files with no rows to be read in filtra_d function.

### percolator 0.4.3 (Release date: 2021-03-13)
- Fix UTC function

### percolator 0.4.2 (Release date: 2021-03-11)
- Fix filtra_c  function.

### percolator 0.4.1 (Release date: 2021-03-06)
- Correct filtra_b classification of the oceanic and continental MCSs.

### percolator 0.4.0 (Release date: 2021-01-27)
- Translated UTC function to English
- Add a filter in filtra_d function for filter by the minimum size 
  the MCSs should have. 
- Add function pre_filter that includes the fields necessary to
  the other filters. In this sense, one can run any filter in the
  file after read_tb or read_pcp.
- Add classif filter in filtra_a in order to select the new systems, 
  etc. This was the major fault in the filters before.
- Updated filtra_a, filtra_b, and filtra_c in order to be more
  eficient and doesn't depend one each other.
- Add examples and documentation in English
  
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