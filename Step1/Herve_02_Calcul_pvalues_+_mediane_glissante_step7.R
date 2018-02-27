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
echL1 <- read.table("Data/L1_replicate1.txt", sep="\t", dec=".", header=TRUE)
echL1 <- calc.runmed2(echL1, window = 5, data.M = "M.NORM", nom = "M.NORM.w")
echL1 <- calc.pvalue(echL1, data.M = "M.NORM", window = 5, method = "mode.mad", plot = TRUE, adjust = FALSE)
safe.dir.create("Data_ce6_res")
write.table(echL1, file = "Data_ce6_res/echL1.68225405.control.neg.txt", quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

