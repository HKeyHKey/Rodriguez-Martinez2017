# 1. Download of the Ce10 C. elegans genome assembly from http://hgdownload.soe.ucsc.edu/goldenPath/ce10/bigZips/chromFa.tar.gz (done on July 8, 2015):

``wget http://hgdownload.soe.ucsc.edu/goldenPath/ce10/bigZips/chromFa.tar.gz;tar -xzf chromFa.tar.gz;cat chr*.fa > Ce10.fa;./Fuses_lines_clean.pl Ce10.fa > Fused_Ce10.fa``
rm chr* Ce10.fa


# 2. Extraction of 1000 bp context on each side of the middle of the maximum probe of each origin (or: shorter than 1000 bp if the origin is too close from a chromosome extremity; files 'Homogeneous_*' contain only the ORIs that were far enough from extremities; files 'Context_*' contain every ORI, with a shorter context for the ones that are too close from the extremities):

``./Module_context_extraction.pl $PWD/Maximum_probe_coordinates/maximum_probes_coordinates_Ce10_oris.Mix_only.txt /mnt/data/home/herve.seitz/Genomes/Fused_Ce10.fa;./Module_context_extraction.pl $PWD/Maximum_probe_coordinates/maximum_probes_coordinates_Ce10_oris.ech2.40a.40b.40c.3rep.txt /mnt/data/home/herve.seitz/Genomes/Fused_Ce10.fa``


# 3. Same thing, for negative controls:

``for f in `ls ../Maximum_probe_coordinates/maximum_probes_coordinates_Random_set_of_boxes_*`;do ./Module_context_extraction.pl $f /mnt/data/home/herve.seitz/Genomes/Fused_Ce10.fa;done``

# 4. Comparison of genomic coordinates for replication origins with those of other known genomic elements:

``for f in `ls ../Ce6_to_Ce10_conversion/Ce10_*.gff`;do ./Module_overlap_between_ORIs_and_genomic_features.pl $f CpG_coordinates.txt ../Negative_control/Chromosome_organization.dat;done;for f in `ls ../Ce6_to_Ce10_conversion/Ce10_*.gff`;do ./Module_overlap_between_ORIs_and_genomic_features.pl $f HOT_coordinates.txt ../Negative_control/Chromosome_organization.dat;done;for f in `ls ../Ce6_to_Ce10_conversion/Ce10_*.gff`;do ./Module_overlap_between_ORIs_and_genomic_features.pl $f Enhancer_coordinates.txt ../Negative_control/Chromosome_organization.dat;done;for f in `ls ../Ce6_to_Ce10_conversion/Ce10_*.gff`;do ./Module_overlap_between_ORIs_and_genomic_features.pl $f IR_coordinates_Marta_corrected.txt ../Negative_control/Chromosome_organization.dat;done``

# 5. Proportion of Negative control ORIs overlapping CpG islands, HOT sequences and enhancers:

``for f in `ls ../Negative_control/Random_*.gff`;do ./Module_overlap_between_ORIs_and_genomic_features.pl $f CpG_coordinates.txt ../Negative_control/Chromosome_organization.dat;done;for f in `ls ../Negative_control/Random_*.gff`;do ./Module_overlap_between_ORIs_and_genomic_features.pl $f HOT_coordinates.txt ../Negative_control/Chromosome_organization.dat;done;for f in `ls ../Negative_control/Random_*.gff`;do ./Module_overlap_between_ORIs_and_genomic_features.pl $f Enhancer_coordinates.txt ../Negative_control/Chromosome_organization.dat;done``

# 6. Extraction of numbers of ORIs overlapping CpG islands, enhancers, HOT regions or inverted repeats:

``for element in CpG Enhancer HOT IR_Marta_corrected;do for radical in 2-40_cells Mix_only;do case "$radical" in "2-40_cells") sample='ech2.40a.40b.40c.3rep';;"Mix") sample='Mixa.Mixb.Mixc.3rep';;"Mix_only") sample='Mix_only';;esac;   echo "Set ORIs_overlapped_by_"$element" Total_ORIs Arm_ORIs_overlapped_by_"$element" Total_arm_ORIs Center_ORIs_overlapped_by_"$element" Total_center_ORIs" > Statistics_$radical'_ORIs_overlapped_by_'$element'.dat';tail -1 Overlaps_between_Ce10_oris.$sample'.gff_and_'$element'_coordinates.txt' | sed 's|[();]||g' | awk '{print "Real",$3,$6,$11+$24,$14+$27,$17,$20}' >> Statistics_$radical'_ORIs_overlapped_by_'$element'.dat';for set in `seq 1 100`;do tail -1 Overlaps_between_Random_set_of_boxes_$radical'_'$set'.gff_and_'$element'_coordinates.txt' | sed 's|[();]||g' | awk '{print "Random_"'$set',$3,$6,$11+$24,$14+$27,$17,$20}';done >> Statistics_$radical'_ORIs_overlapped_by_'$element'.dat';done;done``

# 7. Extraction of numbers of ORIs overlapped by CpG islands, enhancers, HOT regions or inverted repeats:

``for element in CpG Enhancer HOT IR_Marta_corrected;do for radical in 2-40_cells Mix_only;do case "$radical" in "2-40_cells") sample='ech2.40a.40b.40c.3rep';;"Mix") sample='Mixa.Mixb.Mixc.3rep';;"Mix_only") sample='Mix_only';;esac;echo "Set "$element"_overlapped_by_ORIs Total_"$element" Arm_"$element"_overlapped_by_ORIs Total_arm_"$element" Center_"$element"_overlapped_by_ORIs Total_center_"$element > Statistics_$element'_overlapped_by_'$radical'_ORIs.dat';head -1 Overlaps_between_Ce10_oris.$sample'.gff_and_'$element'_coordinates.txt' | sed 's|[();\.]||g' | awk '{print "Real",$4,$7,$13+$28,$16+$31,$20,$23}' >> Statistics_$element'_overlapped_by_'$radical'_ORIs.dat';for set in `seq 1 100`;do head -1 Overlaps_between_Random_set_of_boxes_$radical'_'$set'.gff_and_'$element'_coordinates.txt' | sed 's|[();]||g' |  awk '{print "Random_"'$set',$4,$7,$13+$28,$16+$31,$20,$23}';done >> Statistics_$element'_overlapped_by_'$radical'_ORIs.dat';done;done``

# 8. Calculating overlap frequencies between IRs and ORIs separately for arm and center ORIs:

``for f in `ls ../Negative_control/Arms_and_Centers/Arms_ORIs_from_*.gff ../Negative_control/Arms_and_Centers/Center_ORIs_from_*.gff`;do ./Module_overlap_between_ORIs_and_genomic_features.pl $f IR_coordinates.txt ../Negative_control/Chromosome_organization.dat;done``
