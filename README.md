# Rodriguez-Martinez2017
Scripts and data for the generation of figures in [Rodríguez-Martínez et al. (Nature Structural and Molecular Biology, 2017)](https://www.ncbi.nlm.nih.gov/pubmed/28112731 "PubMed link").

The following instructions assume that you will be working in a directory called $PWD/:

1. Data extraction (normalization, background subtraction, peak identification): directory 'Step1'. These scripts were written by Emmanuelle Beyne with some help from Aurore Puy, then adapted by Hervé Seitz for homogeneity and compatibility with the following steps. See instructions in file 'Step1.md' in directory 'Step1'.
2. Extraction of highest array peaks in each origin, identification of replication origins that are specific to the "Mix" sample (and do not appear in the "2-40 cells" sample), and conversion of Ce6 assembly coordinates to Ce10 assembly coordinates: directory 'Step2'. These scripts were written by Natalia Pinzón and Hervé Seitz. See instructions in file 'Step2.md' in directory 'Step2'.
3. Extraction of sequence contexts (2000 bp on each side of the maximum probe of each ORI), and comparison of replication origin locations with various annotated genomic features (CpG islands, enhancers, inverted repeats, ...) and with published ChIP experiments. These scripts were written by Natalia Pinzón and Hervé Seitz. See instructions in file 'Step3.md' in directory 'Step3'.
