# 1. Extraction of maximum probe coordinates for '2-40_cells' and 'Mix':

``./Module_maximum_probe_coordinates.pl $PWD/Aurore_Emmanuelle_scripts_par_Herve/Regions_ORIs_comb/oris.ech2.40a.40b.40c.3rep.gff $PWD/Aurore_Emmanuelle_scripts_par_Herve/Data_comb/ech2.40a.40b.40c.3rep.txt;./Module_maximum_probe_coordinates.pl $PWD/Aurore_Emmanuelle_scripts_par_Herve/Regions_ORIs_comb/oris.ech2.Mixa.Mixb.Mixc.3rep.gff $PWD/Aurore_Emmanuelle_scripts_par_Herve/Data_comb/ech2.Mixa.Mixb.Mixc.3rep.txt``


# 2. Then for 'Mix_only' (once the GFF file for Mix_only origins has been created: see ../Sample_ORI_comparison/oris.Mix_only.gff):

``./Module_maximum_probe_coordinates.pl ../Sample_ORI_comparison/oris.Mix_only.gff /mnt/data/home/herve.seitz/Analyses_for_paper_July2015/Aurore_Emmanuelle_scripts_par_Herve/Data_comb/ech2.Mixa.Mixb.Mixc.3rep.txt``

# 3. Then for negative controls (once the array signal value files have been updated to Ce10: see directory $PWD/Ce6_to_Ce10_conversion/):

Parallelized on 8 processors:

``for i in `seq 1 8`;do mkdir Chunk_$i;done;for i in `seq 1 8`;do cp Module_maximum_probe_coordinates.pl Chunk_$i;done;for i in `seq 1 8`;do j=`echo "("$i"-1)*7+46" | bc`;ls ../Negative_control/Random_set_of_boxes_2-40_cells_*.gff | tail -n +$j | head -7 | sed 's|^|../|' > Chunk_$i/todo;done;ls ../Ce6_to_Ce10_conversion/Ce10_oris.ech2.40a.40b.40c.3rep.gff | sed 's|^|../|' >> Chunk_8/todo;for i in `seq 1 8`;do j=`echo "("$i"-1)*13+1" | bc`;ls ../Negative_control/Random_set_of_boxes_Mix_only_*.gff ../Ce6_to_Ce10_conversion/Ce10_oris.Mix_only.gff | tail -n +$j | head -13 | sed 's|^|../|' >> Chunk_$i/todo;done;for i in `seq 1 8`;do echo "for f in \`cat todo\`;do if test \`echo \$f | grep -c 'Random_set_of_boxes_2-40_cells'\` -ne 0 -o \`echo \$f | grep -c 'Ce10_oris.ech2.40a.40b.40c.3rep'\` -ne 0;then ref='$PWD/Ce6_to_Ce10_conversion/Ce10_ech2.40a.40b.40c.3rep.txt';else ref='$PWD/Ce6_to_Ce10_conversion/Ce10_ech2.Mixa.Mixb.Mixc.3rep.txt';fi;./Module_maximum_probe_coordinates.pl \$f \$ref;done" > Chunk_$i/Script_$i.sh;chmod u+x Chunk_$i/Script_$i.sh;done``

(then: launched one by one, under nohup, in each of the 8 directories)
