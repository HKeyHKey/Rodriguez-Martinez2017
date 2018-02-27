# 1. Download of the Ce10 C. elegans genome assembly from http://hgdownload.soe.ucsc.edu/goldenPath/ce10/bigZips/chromFa.tar.gz (done on July 8, 2015):

``wget http://hgdownload.soe.ucsc.edu/goldenPath/ce10/bigZips/chromFa.tar.gz;tar -xzf chromFa.tar.gz;cat chr*.fa > Ce10.fa;./Fuses_lines_clean.pl Ce10.fa > Fused_Ce10.fa``
rm chr* Ce10.fa


# 2. Extraction of 1000 bp context on each side of the middle of the maximum probe of each origin (or: shorter than 1000 bp if the origin is too close from a chromosome extremity; files 'Homogeneous_*' contain only the ORIs that were far enough from extremities; files 'Context_*' contain every ORI, with a shorter context for the ones that are too close from the extremities):

``./Module_context_extraction.pl $PWD/Maximum_probe_coordinates/maximum_probes_coordinates_Ce10_oris.Mix_only.txt /mnt/data/home/herve.seitz/Genomes/Fused_Ce10.fa;./Module_context_extraction.pl $PWD/Maximum_probe_coordinates/maximum_probes_coordinates_Ce10_oris.ech2.40a.40b.40c.3rep.txt /mnt/data/home/herve.seitz/Genomes/Fused_Ce10.fa``


# 3. Same thing, for negative controls:

``for f in `ls ../Maximum_probe_coordinates/maximum_probes_coordinates_Random_set_of_boxes_*`;do ./Module_context_extraction.pl $f /mnt/data/home/herve.seitz/Genomes/Fused_Ce10.fa;done``
