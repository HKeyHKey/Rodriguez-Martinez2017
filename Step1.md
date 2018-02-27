# 0. Fetching and normalizing raw data

Raw microarray data are accessible at NCBI's GEO, under [accession number GSE86651](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE86651 "GEO dataset").

Create a directory named "Raw_Data/" and copy the ".pair" file for the sample type of interest (either "2-40 cell", or "Mix", or the "L1" negative control, or the "RNase" negative control) in that directory.

Then for each of these 4 sample types, run the first R command script:

`#copy the "2-40 cell" data in Raw_Data/, then:`
`R CMD BATCH 2_40_cells_01_Lecture_des_donnees_+_Normalisation.R`
`#copy the "Mix" data in Raw_Data/, then:`
`R CMD BATCH Mix_01_Lecture_des_donnees_+_Normalisation.R`
`#copy the L1 data in Raw_Data/, then:`
`R CMD BATCH L1_01_Lecture_des_donnees_+_Normalisation.R`
`#copy the RNase data in Raw_Data/, then:`
`R CMD BATCH RNase_01_Lecture_des_donnees_+_Normalisation.R`
