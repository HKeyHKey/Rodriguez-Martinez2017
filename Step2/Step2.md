# 1. Identifying Mix ORI's that have their highest peak in a "2-40 cell" ORI:

``cd $PWD/Sample_ORI_comparison/;./Module_common_ORIs_based_on_peak_in_other_ORI.pl ../Aurore_Emmanuelle_scripts_par_Herve/Regions_ORIs_comb/oris.ech2.40a.40b.40c.3rep.gff ../Maximum_probe_coordinates/maximum_probes_coordinates_oris.ech2.Mixa.Mixb.Mixc.3rep.txt``


# 2. Counting common and sample-specific ORIs:

``tail -n +2 Peaks_in_maximum_probes_coordinates_oris.ech2.Mixa.Mixb.Mixc.3rep.txt_falling_in_ORIs_from_oris.ech2.40a.40b.40c.3rep.csv | grep -c ',$';tail -n +2 Peaks_in_maximum_probes_coordinates_oris.ech2.Mixa.Mixb.Mixc.3rep.txt_falling_in_ORIs_from_oris.ech2.40a.40b.40c.3rep.csv | grep -vc ',$'``

Result: there are 7658 common ORIs, and the remaining 8214 "Mix" ORIs are "Mix_only" (not detected in "4-20 cells").

# 3. Extraction of maximum probe coordinates for '2-40_cells' and 'Mix':

``cd $PWD/Maximum_probe_coordinates;./Module_maximum_probe_coordinates.pl $PWD/Aurore_Emmanuelle_scripts_par_Herve/Regions_ORIs_comb/oris.ech2.40a.40b.40c.3rep.gff $PWD/Aurore_Emmanuelle_scripts_par_Herve/Data_comb/ech2.40a.40b.40c.3rep.txt;./Module_maximum_probe_coordinates.pl $PWD/Aurore_Emmanuelle_scripts_par_Herve/Regions_ORIs_comb/oris.ech2.Mixa.Mixb.Mixc.3rep.gff $PWD/Aurore_Emmanuelle_scripts_par_Herve/Data_comb/ech2.Mixa.Mixb.Mixc.3rep.txt``


# 4. Extraction of maximum probe coordinates for 'Mix_only':

``./Module_maximum_probe_coordinates.pl ../Sample_ORI_comparison/oris.Mix_only.gff $PWD/Aurore_Emmanuelle_scripts_par_Herve/Data_comb/ech2.Mixa.Mixb.Mixc.3rep.txt``

# 5. Conversion of microarray signal values from the Ce6 assembly version to Ce10:

``cd $PWD/Ce6_to_Ce10_conversion;for f in `ls $PWD/Aurore_Emmanuelle_scripts_par_Herve/Regions_ORIs_comb/*.gff` $PWD/Sample_ORI_comparison/oris.Mix_only.gff;do name=`echo $f | sed -e 's|\.gff$|.bed|' -e 's|.*/||'`;awk '{print $1,$4,$5,$9}' $f > $name;done``

Then conversion of these Ce6 BED to Ce10 BED using http://genome.ucsc.edu/cgi-bin/hgLiftOver with default parameters ("Minimum ratio of bases that must overlap" = 0.95; BED 4 to BED 6 Options: "Allow multiple output regions" unchecked; "Minimum hit size in query" = 0; "Minimum chain size in target" = 0; BED12 Options: "Min ratio of alignment blocks or exons that must map" = 1; "If thickStart/thickEnd is not mapped, use the closest mapped base" unckecked); output files saved as 'Ce10_oris.ech2.40a.40b.40c.3rep.bed', 'Ce10_oris.ech2.Mixa.Mixb.Mixc.3rep.bed' and 'Ce10_oris.Mix_only.bed'.

Conversion of ORI Ce10 BED to GFF:

``for f in `ls Ce10*.bed`;do name=`echo $f | sed 's|\.bed|.gff|'`;awk '{print $1"\tLiftOver_by_UCSC_from_Ce6_to_Ce10\t'$name'\t"$2"\t"$3"\t.\t.\t.\t"$4}' $f > $name;done``

Conversion of "maximum probe" TXT to BED:

``for f in `ls ../Maximum_probe_coordinates/maximum_probes_coordinates_oris.*`;do name=`echo $f | sed -e 's|.*/||' -e 's|\.txt$|.bed|'`;grep -v '^#' $f | awk '{print $1,$4,$5,$2"_"$3"_"$6"_"$7}' > $name;done # BED files with maximum probe coordinates;for f in `ls ../Maximum_probe_coordinates/maximum_probes_coordinates_oris.*`;do name=`echo $f | sed -e 's|.*/||' -e 's|\.txt$|_ORI.bed|'`;grep -v '^#' $f | awk '{print $1,$2,$3,$4"_"$5"_"$6"_"$7}' > $name;done # BED files with ORI coordinates``

Then conversion of these Ce6 BED to Ce10 BED using http://genome.ucsc.edu/cgi-bin/hgLiftOver with default parameters ("Minimum ratio of bases that must overlap" = 0.95; BED 4 to BED 6 Options: "Allow multiple output regions" unchecked; "Minimum hit size in query" = 0; "Minimum chain size in target" = 0; BED12 Options: "Min ratio of alignment blocks or exons that must map" = 1; "If thickStart/thickEnd is not mapped, use the closest mapped base" unckecked); output files saved as 'Ce10_maximum_probes_coordinates_oris.*.bed'.

Conversion of "maximum probe" Ce10 BED to TXT:

``for f in `ls ../Maximum_probe_coordinates/maximum_probes_coordinates_oris.* | sed 's|.*/||'`;do name1=`echo $f | sed 's|^|Ce10_|'`;name2=`echo $name1 | sed 's|\.txt$|_ORI.bed|'`;name3=`echo $name1 | sed 's|\.txt$|.bed|'`;head -1 ../Maximum_probe_coordinates/$f > $name1;paste -d ' ' $name2 $name3 | sed 's|_| |g' | awk '{print $1,$2,$3,$9,$10,$13,$14}' >> $name1;done``

Conversion of microarray signal value files into Ce10 for "maximum probe coordinate" extraction for Ce10-based negative controls:

``for f in $PWD/Aurore_Emmanuelle_scripts_par_Herve/Data_comb/ech2.40a.40b.40c.3rep.txt #/$PWD/Aurore_Emmanuelle_scripts_par_Herve/Data_comb/ech2.Mixa.Mixb.Mixc.3rep.txt (only one is needed: the output files would be identical);do name=`echo $f | sed -e 's|.*/||' -e 's|\.txt$|.bed|'`;   tail -n +2 $f | awk '{print $2,$3,$3+$4-1,$1}' > $name;done # this will generate BED versions of microarray signal value files``

Then conversion of these Ce6 BED to Ce10 BED using http://genome.ucsc.edu/cgi-bin/hgLiftOver with default parameters ("Minimum ratio of bases that must overlap" = 0.95; BED 4 to BED 6 Options: "Allow multiple output regions" unchecked; "Minimum hit size in query" = 0; "Minimum chain size in target" = 0; BED12 Options: "Min ratio of alignment blocks or exons that must map" = 1; "If thickStart/thickEnd is not mapped, use the closest mapped base" unckecked); output file saved as 'Ce10_ech2.40a.40b.40c.3rep.bed'.
The output contains one less probe: "chrIII 10505362    10505411    CHRIIIFS010505362" could not be converted by hgLiftOver.

Then:

``./Module_Ce10_conversion_for_maximum_probe_coordinates.pl Ce10_ech2.40a.40b.40c.3rep.bed $PWD/Aurore_Emmanuelle_scripts_par_Herve/Data_comb/ech2.40a.40b.40c.3rep.txt;./Module_Ce10_conversion_for_maximum_probe_coordinates.pl Ce10_ech2.40a.40b.40c.3rep.bed $PWD/Aurore_Emmanuelle_scripts_par_Herve/Data_comb/ech2.Mixa.Mixb.Mixc.3rep.txt``


Result: files '$PWD/Ce6_to_Ce10_conversion/Ce10_ech2.40a.40b.40c.3rep.txt' and '$PWD/Ce6_to_Ce10_conversion/Ce10_ech2.Mixa.Mixb.Mixc.3rep.txt', with probe positions updated to Ce10.


# 6. Extraction of maximum probe coordinates for negative controls:

Parallelized on 8 processors:

``for i in `seq 1 8`;do mkdir Chunk_$i;done;for i in `seq 1 8`;do cp Module_maximum_probe_coordinates.pl Chunk_$i;done;for i in `seq 1 8`;do j=`echo "("$i"-1)*7+46" | bc`;ls ../Negative_control/Random_set_of_boxes_2-40_cells_*.gff | tail -n +$j | head -7 | sed 's|^|../|' > Chunk_$i/todo;done;ls ../Ce6_to_Ce10_conversion/Ce10_oris.ech2.40a.40b.40c.3rep.gff | sed 's|^|../|' >> Chunk_8/todo;for i in `seq 1 8`;do j=`echo "("$i"-1)*13+1" | bc`;ls ../Negative_control/Random_set_of_boxes_Mix_only_*.gff ../Ce6_to_Ce10_conversion/Ce10_oris.Mix_only.gff | tail -n +$j | head -13 | sed 's|^|../|' >> Chunk_$i/todo;done;for i in `seq 1 8`;do echo "for f in \`cat todo\`;do if test \`echo \$f | grep -c 'Random_set_of_boxes_2-40_cells'\` -ne 0 -o \`echo \$f | grep -c 'Ce10_oris.ech2.40a.40b.40c.3rep'\` -ne 0;then ref='$PWD/Ce6_to_Ce10_conversion/Ce10_ech2.40a.40b.40c.3rep.txt';else ref='$PWD/Ce6_to_Ce10_conversion/Ce10_ech2.Mixa.Mixb.Mixc.3rep.txt';fi;./Module_maximum_probe_coordinates.pl \$f \$ref;done" > Chunk_$i/Script_$i.sh;chmod u+x Chunk_$i/Script_$i.sh;done``

(then: launched one by one, under nohup, in each of the 8 directories)
