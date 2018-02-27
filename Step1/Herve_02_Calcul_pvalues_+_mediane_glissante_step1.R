#########################################################################################################################
############## CALCUL DE LA MEDIANE GLISSANTE AVEC DES FENETRES DE TAILLE DIFFERENTES + CALCUL DES PVALUES ##############
#########################################################################################################################

# --- On charge les fonctions necessaires a l'analyse des donnees --- #
source("Fonctions_01_Lecture+Normalisation+Runmed+Pval.R")


###***************************************###
### --- Lecture des données des puces --- ###
###***************************************###
# --- Control Negatif --- #
#E3Rnase  <- read.table("Data_ce6/E3Rnase.68716205.control.neg.txt", sep="\t", dec=".", header=TRUE)
#L1 <- read.table("Data/

# --- Embryons 2-40 cellules  --- #
ech2_40a <- read.table("Data/2-40_cells_replicate1.txt", sep="\t", dec=".", header=TRUE)
ech2_40a <- calc.runmed2(ech2_40a, window = 5, data.M = "M.NORM", nom = "M.NORM.w")
ech2_40a <- calc.pvalue(ech2_40a, data.M = "M.NORM", window = 5, method = "mode.mad", plot = TRUE, adjust = FALSE)
safe.dir.create("Data_ce6_res")
write.table(ech2_40a, file = "Data_ce6_res/ech2.40a.65350205.control.neg.txt", quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

