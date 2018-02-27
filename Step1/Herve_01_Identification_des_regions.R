############################################################################################################
############################ --- IDENTIFICATION DES ORIGINES DE REPLICATION --- ############################
####################### --- ESSAIS AVEC DIFFERENTS PARAMETRES POUR SELECT ORIS --- #########################
############################################################################################################

### --- On charge les fonctions nécéssaires à l'analyse des données --- ###
source("Fonctions_03_Identification_regions.R")

###***************************************###
### --- Lecture des données des puces --- ###
###***************************************###

ech2.40a.40b.40c.3rep        <- read.table("Data_comb/ech2.40a.40b.40c.3rep.txt", sep="\t", dec=".", header=TRUE)
ech2.Mixa.Mixb.Mixc.3rep        <- read.table("Data_comb/ech2.Mixa.Mixb.Mixc.3rep.txt", sep="\t", dec=".", header=TRUE)


###**********************************************************************************************************************************###
###**********************************************************************************************************************************###
### 3- Correction FDR=5%, distance min entre 2 region = 200, taille min de region = 600, nb sondes signif min = 3, taille repeat=600 ###
###**********************************************************************************************************************************###
###**********************************************************************************************************************************###

###****************************************************###
### --- Identification des origines de réplication --- ###
###****************************************************###

oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes  <- select.oris(ech2.40a.40b.40c.3rep, "M.comb.eBayes", "pval.comb.eBayes.adj", nb.hits.signif = 3, taille.min.region = 600,  dist.min.region = 200, 0, pval.seuil = 0.05, consecutif = TRUE, taille.repeat = 600)

oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes  <- select.oris(ech2.Mixa.Mixb.Mixc.3rep, "M.comb.eBayes", "pval.comb.eBayes.adj", nb.hits.signif = 3, taille.min.region = 600,  dist.min.region = 200, 0, pval.seuil = 0.05, consecutif = TRUE, taille.repeat = 600)


###******************************************###
### --- Nombre d'origines de réplication --- ###
###******************************************###
			
dim(oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes)
dim(oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes)        


#dim par chromosome

dim(oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes[oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes$CHROMOSOME=="chrI",])
dim(oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes[oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes$CHROMOSOME=="chrII",])
dim(oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes[oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes$CHROMOSOME=="chrIII",])
dim(oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes[oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes$CHROMOSOME=="chrIV",])
dim(oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes[oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes$CHROMOSOME=="chrV",])
dim(oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes[oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes$CHROMOSOME=="chrX",])

dim(oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes[oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes$CHROMOSOME=="chrI",])
dim(oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes[oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes$CHROMOSOME=="chrII",])
dim(oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes[oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes$CHROMOSOME=="chrIII",])
dim(oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes[oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes$CHROMOSOME=="chrIV",])
dim(oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes[oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes$CHROMOSOME=="chrV",])
dim(oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes[oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes$CHROMOSOME=="chrX",])

###********************************************************###
### --- Flag des origines de réplication sur le génome --- ###
###********************************************************###

ech2.40a.40b.40c.3rep.FDR5.600.3sondes  <- flag.oris(ech2.40a.40b.40c.3rep, oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes, nom.flag = "oris.minus.neg.w5.FDR5.600.3sondes")

ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes  <- flag.oris(ech2.Mixa.Mixb.Mixc.3rep, oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes, nom.flag = "oris.minus.neg.w5.FDR5.600.3sondes")


###***********************************************###
### --- Exportation des données au format.gff --- ###
###***********************************************###

export.genome.oris.gff(ech2.40a.40b.40c.3rep.FDR5.600.3sondes, data.M = "M.comb.eBayes", nom.flag = "oris.minus.neg.w5.FDR5.600.3sondes")
export.genome.oris.gff(ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes, data.M = "M.comb.eBayes", nom.flag = "oris.minus.neg.w5.FDR5.600.3sondes")


###***********************************************###
### --- Exportation des régions au format.txt --- ###
###***********************************************###
safe.dir.create("Regions_ORIs_comb")

write.table(oris.ech2.40a.40b.40c.3rep.FDR5.600.3sondes, "Regions_ORIs_comb/oris.ech2.40a.40b.40c.3rep.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(oris.ech2.Mixa.Mixb.Mixc.3rep.FDR5.600.3sondes, "Regions_ORIs_comb/oris.ech2.Mixa.Mixb.Mixc.3rep.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)



