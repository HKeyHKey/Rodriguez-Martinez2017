# 1. Fetching and normalizing raw data

Raw microarray data are accessible at NCBI's GEO, under [accession number GSE86651](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE86651 "GEO dataset").

In directory "$PWD/Aurore_Emmanuelle_scripts_par_Herve/": create a directory named "Raw_Data/" and copy the ".pair" file for the sample type of interest (either "2-40 cell", or "Mix", or the "L1" negative control, or the "RNase" negative control) in that directory.

Then for each of these 4 sample types, run the first R command script:

Copy the "2-40 cell" data in Raw_Data/, then (in directory "$PWD/Aurore_Emmanuelle_scripts_par_Herve/"):

`R CMD BATCH 2_40_cells_01_Lecture_des_donnees_+_Normalisation.R`

Copy the "Mix" data in Raw_Data/, then:

`R CMD BATCH Mix_01_Lecture_des_donnees_+_Normalisation.R`

Copy the L1 data in Raw_Data/, then:

`R CMD BATCH L1_01_Lecture_des_donnees_+_Normalisation.R`

Copy the RNase data in Raw_Data/, then:

`R CMD BATCH RNase_01_Lecture_des_donnees_+_Normalisation.R`

Among other things, these scripts will create one .txt file per replicate (hence 8 .txt files in total: 3 "2-40 cell" replicates, 3 "Mix" replicates, 1 "L1" replicate and 1 "RNase" replicate) in directory "Data/".


# 2. Averaging and p-value calculation:


``for i in `seq 1 8`;do R CMD BATCH Herve_02_Calcul_pvalues_+_mediane_glissante_step$i'.R';done``

These scripts create a directory named 'Data_ce6_res', with output files for each of the 8 analyzed replicates (3 "2-40 cells", 3 "Mix", 1 "L1" and 1 "RNase").


# 3. Subtracting L1 values (when positive) from 2-40 cell and Mix values (when positive), and formatting the results so they can be handled by 'Herve_01_Analyse_donnees_combinees.R':

``for f in `ls Data_ce6_res/ech2.*`;do name=`echo $f | sed -e 's|.*/||' -e 's|\.control\.neg|.minus.control.neg|'`;./Module_subtract_L1_data.pl $f Data_ce6_res/echL1.68225405.control.neg.txt > Data_ce6_res/$name;done;for f in `ls Data_ce6_res/echRNase.*.txt`;do name=`echo $f | sed -e 's|.*/||' -e 's|\.control\.neg|.minus.control.neg|'`;./Module_subtract_L1_data.pl $f Data_ce6_res/echL1.68225405.control.neg.txt > Data_ce6_res/$name;done``


# 4. Combining replicates:

``for i in `seq 1 8`;do R CMD BATCH Herve_01_Analyse_donnees_combinees_step$i'.R';done``


# 5. Replication origin ("ORI") identification:

``rm -f .RData; R CMD BATCH Herve_01_Identification_des_regions.R``


# 6. Conversion of ORI coordinates in the GFF format:

In directory 'Regions_ORIs_comb/':

``file=oris.ech2.40a.40b.40c.3rep.txt;name=`echo $file | sed 's|\.txt$|.gff|'`;grep -Pv '^CHROMOSOME\t' $file | nl | awk '{print $2"\tAurore_scripts_run_by_Herve\t'$file'\t"$3"\t"$4"\t"$10"\t.\t.\tORI_2_40_cells_"$1}' > $name;file=oris.ech2.Mixa.Mixb.Mixc.3rep.txt ;name=`echo $file | sed 's|\.txt$|.gff|'`;grep -Pv '^CHROMOSOME\t' $file | nl | awk '{print $2"\tAurore_scripts_run_by_Herve\t'$file'\t"$3"\t"$4"\t"$10"\t.\t.\tORI_Mix_"$1}' > $name``


# 7. Generation of BedGraph files:

In directory 'Data_ce6_res/':

``for f in `ls *[0-9].control.neg.txt`;do name=`echo $f | sed 's|\.txt$|.bedgraph|'`;echo 'track type=bedGraph name="BedGraph Format"' > $name;tail -n +2 $f | awk -F '\t' '{print $12,$3-1,$3+$13-1,$22}' >> $name;done;for f in `ls *.minus.control.neg.txt`;do name=`echo $f | sed 's|\.txt$|.bedgraph|'`;echo 'track type=bedGraph name="BedGraph Format"' > $name;tail -n +2 $f | awk -F '\t' '{print $2,$3-1,$3+$5-1,$11}' >> $name;done``


# 8. Comparison of genome-wide signals:

In directory 'Data_ce6_res/':

``i=1;for f in `ls *minus.control.neg.txt`;do name=`echo $f | sed 's|\.[0-9]*\.minus\.control\.neg\.txt$||'`;awk '{print $11}' $f | sed '1 s|.*|&_in_'$name'|' > tmp_$i;i=`echo $i"+1" | bc`;done;paste -d ' ' tmp_* > genome-wide_signals.dat`
