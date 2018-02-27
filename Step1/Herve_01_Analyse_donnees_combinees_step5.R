##################################################################################
############## ---------- ANALYSE DES REPLICATS COMBINES ---------- ##############
##################################################################################

#load("comb_data.RData")

### --- On charge les fonctions et librairies --- ###
source("Fonctions_01_Lecture+Normalisation+Runmed+Pval.R")
source("Fonctions_02_Analyse_combi.R")
#source("http://www.bioconductor.org/biocLite.R")
#biocLite()
library(limma)

###****************************************###
### --- Lecture des données à combiner --- ###
###****************************************###

# --- Embryons 500 cellules  --- #
ech2.Mixa <- read.table("Data_ce6_res/ech2.Mixa.55226301.minus.control.neg.txt", sep="\t", dec=".", header=TRUE)
ech2.Mixb <- read.table("Data_ce6_res/ech2.Mixb.55227101.minus.control.neg.txt", sep="\t", dec=".", header=TRUE)

###*******************************************************************************************###
### --- Renommage des colonnes pour que la fonction "analyse.replicats.eBayes" fonctionne --- ###
###*******************************************************************************************###

# --- Embryons 500 cellules  --- #
names(ech2.Mixa) <- c("PROBE_ID","CHROMOSOME", "POSITION","END","LENGTH","A.NORM.neg","M.NORM.neg","A.NORM.ech2","M.NORM.ech2","M.NORM.minus.neg","M.NORM.minus.neg.w5")
names(ech2.Mixb) <- c("PROBE_ID","CHROMOSOME", "POSITION","END","LENGTH","A.NORM.neg","M.NORM.neg","A.NORM.ech2","M.NORM.ech2","M.NORM.minus.neg","M.NORM.minus.neg.w5")


###*******************************###
### --- Analyse des réplicats --- ###
###*******************************###

vect.col.ech2	<- c("A.NORM.ech2","M.NORM.minus.neg.w5")

# --- Embryons Mix  --- #
## Mixa.Mixb ##							 
ech2.Mixa.Mixb.2rep <- analyse.replicats.eBayes(ech2.Mixa, ech2.Mixb,
				   			nom.rep1 = "ech2.Mixa", nom.rep2 = "ech2.Mixb",
				   			data.M = "M.NORM.minus.neg.w5", data.A="A.NORM.ech2", method.between.array ="scale", adjust = TRUE, lower.tail=FALSE, vect.col=vect.col.ech2)


							 				 
#**************************************************************#
# ********** REPRESENTATION GRAPHIQUE DES REPLICATS ********** #
#**************************************************************#

#### --- BOXPLOTS ET DENSITES DES LOG RATIOS NORMALISES --- ###

# --- Embryons 2-40 cellules  --- #
## 40a.40b ##
#M.NORM.BETWEEN.ech2.40a.40b.2rep 	<- list(ech2.40a.40b.2rep$M.NORM.minus.neg.w5.between.ech2.40a, ech2.40a.40b.2rep$M.NORM.minus.neg.w5.between.ech2.40b, ech2.40a.40b.2rep$M.comb.eBayes)
#names(M.NORM.BETWEEN.ech2.40a.40b.2rep) 	<- c("ech2.40a","ech2.40b","ech2.40a.40b.2rep.M.comb.eBayes")
#box.plots(M.NORM.BETWEEN.ech2.40a.40b.2rep, "Boxplots de la repartition des log-ratios des puces ech2.40a.40b.2rep apres normalisation inter-puces (methode scale)", width=1500, ylim=NULL, names=names(M.NORM.BETWEEN.ech2.40a.40b.2rep), col=rainbow(3))
#rm(M.NORM.BETWEEN.ech2.40a.40b.2rep);

## 40a.40c ##
#M.NORM.BETWEEN.ech2.40a.40c.2rep 	<- list(ech2.40a.40c.2rep$M.NORM.minus.neg.w5.between.ech2.40a, ech2.40a.40c.2rep$M.NORM.minus.neg.w5.between.ech2.40c, ech2.40a.40c.2rep$M.comb.eBayes)
#names(M.NORM.BETWEEN.ech2.40a.40c.2rep) 	<- c("ech2.40a","ech2.40c","ech2.40a.40c.2rep.M.comb.eBayes")
#box.plots(M.NORM.BETWEEN.ech2.40a.40c.2rep, "Boxplots de la repartition des log-ratios des puces ech2.40a.40c.2rep apres normalisation inter-puces (methode scale)", width=1500, ylim=NULL, names=names(M.NORM.BETWEEN.ech2.40a.40c.2rep), col=rainbow(3))
#rm(M.NORM.BETWEEN.ech2.40a.40c.2rep);

## 40b.40c ##
#M.NORM.BETWEEN.ech2.40b.40c.2rep 	<- list(ech2.40b.40c.2rep$M.NORM.minus.neg.w5.between.ech2.40b, ech2.40b.40c.2rep$M.NORM.minus.neg.w5.between.ech2.40c, ech2.40b.40c.2rep$M.comb.eBayes)
#names(M.NORM.BETWEEN.ech2.40b.40c.2rep) 	<- c("ech2.40b","ech2.40c","ech2.40b.40c.2rep.M.comb.eBayes")
#box.plots(M.NORM.BETWEEN.ech2.40b.40c.2rep, "Boxplots de la repartition des log-ratios des puces ech2.40b.40c.2rep apres normalisation inter-puces (methode scale)", width=1500, ylim=NULL, names=names(M.NORM.BETWEEN.ech2.40b.40c.2rep), col=rainbow(3))
#rm(M.NORM.BETWEEN.ech2.40b.40c.2rep);

## 40a.40b.40c ##
#M.NORM.BETWEEN.ech2.40a.40b.40c.3rep 	<- list(ech2.40a.40b.40c.3rep$M.NORM.minus.neg.w5.between.ech2.40a, ech2.40a.40b.40c.3rep$M.NORM.minus.neg.w5.between.ech2.40b, ech2.40a.40b.40c.3rep$M.NORM.minus.neg.w5.between.ech2.40c, ech2.40a.40b.40c.3rep$M.comb.eBayes)
#names(M.NORM.BETWEEN.ech2.40a.40b.40c.3rep) 	<- c("ech2.40a","ech2.40b","ech2.40c", "ech2.40a.40b.40c.3rep.M.comb.eBayes")
#box.plots(M.NORM.BETWEEN.ech2.40a.40b.40c.3rep, "Boxplots de la repartition des log-ratios des puces ech2.40a.40b.40c.3rep apres normalisation inter-puces (methode scale)", width=1500, ylim=NULL, names=names(M.NORM.BETWEEN.ech2.40a.40b.40c.3rep), col=rainbow(3))
#rm(M.NORM.BETWEEN.ech2.40a.40b.40c.3rep);


# --- Embryons 500 cellules  --- #
#M.NORM.BETWEEN.ech500a.500b.2rep 	<- list(ech500a.500b.2rep$M.NORM.minus.neg.w5.between.ech500a, ech500a.500b.2rep$M.NORM.minus.neg.w5.between.ech500b,  ech500a.500b.2rep$M.comb.eBayes)
#names(M.NORM.BETWEEN.ech500a.500b.2rep) 	<- c("ech500a","ech500b","ech500a.500b.2rep.M.comb.eBayes")
#box.plots(M.NORM.BETWEEN.ech500a.500b.2rep, "Boxplots de la repartition des log-ratios des puces ech500a.500b.2rep apres normalisation inter-puces (methode scale)", width=1500, ylim=NULL, names=names(M.NORM.BETWEEN.ech500a.500b.2rep), col=rainbow(3))
#rm(M.NORM.BETWEEN.ech500a.500b.2rep);


#******************************************************************************#
# ********** CONSTRUCTION FICHIER GFF POUR LE SIGNAL M.comb.eBayes  ********** #
#******************************************************************************#

# --- Embryons Mix  --- #
export.gff(ech2.Mixa.Mixb.2rep, data.M = "M.comb.eBayes")


export.gff(ech2.Mixa.Mixb.2rep, data.M = "M.comb.eBayes", pval = "pval.comb.eBayes.adj", seuil = 0.05)




#*********************************************************#
# ********** EXPORTATION DES DONNEES COMBINEES ********** #
#*********************************************************#
safe.dir.create("Data_comb")
write.table(ech2.Mixa.Mixb.2rep, file = "Data_comb/ech2.Mixa.Mixb.2rep.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
save.image(file = "comb_data.RData")






