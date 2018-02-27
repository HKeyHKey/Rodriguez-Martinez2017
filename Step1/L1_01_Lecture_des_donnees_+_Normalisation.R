#########################################################################################
############## Normalisation des donnees de toutes les puces simultanement ##############
#########################################################################################


# --- On charge les fonctions necessaires Ã  l'analyse des donnees --- #
source("Fonctions_01_Lecture+Normalisation+Runmed+Pval.R")


#***********************************************************#
# ********** LECTURE ET MISE EN FORME DES DONNEES **********#
#***********************************************************#

pos_file="100718_Celegans180_ChIP_HX1.pos"
puce_array1=168225405
name_array1='L1_replicate1'
analysis='L1'

# Fonction format.data: 1er argument : nom de la puce (ex. : 65350205 pour le premier réplicat de 2-40) ; 3ème argument : nom du fichier .pos décrivant les sondes de cette puce

array1 <- format.data(puce_array1, export = FALSE, design = pos_file)


#************************************************************#
# ********** RECUPERATION DES POSITIONS DES SONDES **********#
#************************************************************#
# Pour les 3 réplicats de P19, même désign de puces : Génération 385K 
positions.sondes   <- array1$POSITION
length(positions.sondes)
min(positions.sondes)
max(positions.sondes)

safe.dir.create("Data_Positions")
write.table(positions.sondes, paste("Data_Positions/positions.sondes.",pos_file,".txt",sep=''), quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)


#*******************************************************************************#
# ********** REPRESENTATION GRAPHIQUE DES DONNEES AVANT NORMALISATION **********#
#*******************************************************************************#

########## ---------- BOX-PLOT MA-PLOT RG-PLOT Densites RG et Histogramme de densite des log-ratios ---------- ##########
#plot.data(array1, type = "Avant normalisation")


#******************************************************************************************#
# ********** NORMALISATION DES DONNEES AVEC LOWESS + REPRESENTATIONS GRAPHIQUES ********** #
#******************************************************************************************#
array1 <- norma.lowess(array1,puce_array1, plot = TRUE, export = FALSE)


#****************************************************************************************#
# ********** REPRESENTATIONS GRAPHIQUES DES DONNEES APRES NORMALISATION LOWESS **********#
#****************************************************************************************#

########## ---------- BOX-PLOT MA-PLOT RG-PLOT Densites RG et Histogramme de densite des log-ratios ---------- ##########
#plot.data(array1 , type = "Apres normalisation Lowess")


#************************************************************************#
# ********** REPRESENTATIONS SIMULTANEES DE TOUTES LES DONNEES **********#
#************************************************************************#

### --- BOXPLOTS DES LOG INTENSITES VERTES --- ###
#LOG.V.all 	<- list(log2(array1$PM.V),log2(array2$PM.V),log2(array3$PM.V))
#names(LOG.V.all) 	<- c(paste('array1',puce_array1,sep='\n'), paste('array2',puce_array2,sep='\n'), paste('array3',puce_array3,sep='\n'))
#box.plots(LOG.V.all, "Boxplots de la repartition des log intensites vertes des puces avant normalisation", width=1500, ylim=NULL, names=names(LOG.V.all), col=rep("green",9))
#rm(LOG.V.all); 


### --- BOXPLOTS DES LOG INTENSITES ROUGES --- ###
#LOG.R.all       <- list(array1$LOGR,array2$LOGR,array3$LOGR)
#names(LOG.R.all)        <- c(paste('array1',puce_array1,sep='\n'), paste('array2',puce_array2,sep='\n'), paste('array3',puce_array3,sep='\n'))
#box.plots(LOG.R.all, "Boxplots de la repartition des log intensites rouges des puces avant normalisation", width=1500, ylim=NULL, names=names(LOG.R.all), col=rep("red",9))
#rm(LOG.R.all); 


#### --- BOXPLOTS DES RATIOS NON NORMALISES --- ###
#RATIO.all 	<- list(array1$RATIO,array2$RATIO,array3$RATIO)
#names(RATIO.all) 	<- c(paste('array1',puce_array1,sep='\n'), paste('array2',puce_array2,sep='\n'), paste('array3',puce_array3,sep='\n'))
#box.plots(RATIO.all, "Boxplots de la repartition des ratios des puces avant normalisation", width=1500, ylim=c(0,2), names=names(RATIO.all), col=rainbow(9))
#rm(RATIO.all); 


#### --- BOXPLOTS ET DENSITES DES LOG RATIOS NON NORMALISES --- ###
#M.all 	<- list(array1$M,array2$M,array3$M)
#names(M.all) 	<- c(paste('array1',puce_array1,sep='\n'), paste('array2',puce_array2,sep='\n'), paste('array3',puce_array3,sep='\n'))
#box.plots(M.all, "Boxplots de la repartition des log-ratios (M) des puces avant normalisation", width=1500, ylim=NULL, names=names(M.all), col=rainbow(9))
#plot.densite(M.all, "Densites des log-ratios (M) des puces avant normalisation", col=rainbow(9))
#rm(M.all);


# Les trucs en-dessous : flemme de tout modifier à la main, de toute façon ces graphiques on ne les regarde pas

### --- DENSITES DES LOG INTENSITES VERTES ET ROUGE AVANT NORMALISATION --- ###
#R.V.all	<- list(log2(p19.AS.27248802$PM.V),log2(p19.AS.42771002$PM.V),log2(p19.AS.42776202$PM.V),log2(ES.AS.M3.2_7_10$PM.V),log2(ES.AS.M2.23_7_10$PM.V),log2(MEF.G1.M2.5_10_10$PM.V),log2(MEF.G1.M3.13_07_10$PM.V),log2(MEF.G1.26_08_11$PM.V),log2(MEF.G1.M2.14_08_11$PM.V),
#			p19.AS.27248802$LOGR,p19.AS.42771002$LOGR,p19.AS.42776202$LOGR, ES.AS.M3.2_7_10$LOGR,  ES.AS.M2.23_7_10$LOGR, MEF.G1.M2.5_10_10$LOGR, MEF.G1.M3.13_07_10$LOGR,MEF.G1.26_08_11$LOGR,MEF.G1.M2.14_08_11$LOGR )
#plot.densite(R.V.all, "Densites des log intensites rouges et vertes des puces avant normalisation", col=c(rep("green",9), rep("red",9)))
#rm(R.V.all);


### --- BOXPLOTS DES LOG INTENSITES VERTES NORMALISEES --- ###
#LOG.V.NORM.all 	<- list(p19.AS.27248802$LOGV.NORM,p19.AS.42771002$LOGV.NORM,p19.AS.42776202$LOGV.NORM, ES.AS.M3.2_7_10$LOGV.NORM,  ES.AS.M2.23_7_10$LOGV.NORM, MEF.G1.M2.5_10_10$LOGV.NORM, MEF.G1.M3.13_07_10$LOGV.NORM,MEF.G1.26_08_11$LOGV.NORM,MEF.G1.M2.14_08_11$LOGV.NORM)
#names(LOG.V.NORM.all) 	<- c("p19.AS\n27248802", "p19.AS\n42771002", "p19.AS\n42776202","ES.AS.M3\n2_7_10","ES.AS.M2\n23_7_10","MEF.G1.M2\n5_10_10","MEF.G1.M3\n13_07_10","MEF.G1\n26_08_11","MEF.G1.M2\n14_08_11")
#box.plots(LOG.V.NORM.all, "Boxplots de la repartition des log intensites vertes des puces apres normalisation", width=1500, ylim=NULL, names=names(LOG.V.NORM.all), col=rep("green",9))
#rm(LOG.V.NORM.all); 


#### --- BOXPLOTS ET DENSITES DES LOG RATIOS NORMALISES --- ###
#M.NORM.all 	<- list(p19.AS.27248802$M.NORM,p19.AS.42771002$M.NORM,p19.AS.42776202$M.NORM, ES.AS.M3.2_7_10$M.NORM, ES.AS.M2.23_7_10$M.NORM, MEF.G1.M2.5_10_10$M.NORM, MEF.G1.M3.13_07_10$M.NORM,MEF.G1.26_08_11$M.NORM,MEF.G1.M2.14_08_11$M.NORM)
#names(M.NORM.all) 	<- c("p19.AS\n27248802", "p19.AS\n42771002", "p19.AS\n42776202","ES.AS.M3\n2_7_10","ES.AS.M2\n23_7_10","MEF.G1.M2\n5_10_10","MEF.G1.M3\n13_07_10","MEF.G1\n26_08_11","MEF.G1.M2\n14_08_11")
#box.plots(M.NORM.all, "Boxplots de la repartition des log-ratios (M.NORM) des puces apres normalisation", width=1500, ylim=NULL, names=names(M.NORM.all), col=rainbow(9))
#plot.densite(M.NORM.all, "Densites des log-ratios (M.NORM) des puces apres normalisation", col=rainbow(9))
#rm(M.NORM.all); 


### --- DENSITES DES LOG INTENSITES VERTES ET ROUGE APRES NORMALISATION --- ###
#R.V.NORM.all	<- list(p19.AS.27248802$LOGV.NORM,p19.AS.42771002$LOGV.NORM,p19.AS.42776202$LOGV.NORM, ES.AS.M3.2_7_10$LOGV.NORM, ES.AS.M2.23_7_10$LOGV.NORM, MEF.G1.M2.5_10_10$LOGV.NORM, MEF.G1.M3.13_07_10$LOGV.NORM,MEF.G1.26_08_11$LOGV.NORM,MEF.G1.M2.14_08_11$LOGV.NORM,
#			p19.AS.27248802$LOGR,p19.AS.42771002$LOGR,p19.AS.42776202$LOGR, ES.AS.M3.2_7_10$LOGR,  ES.AS.M2.23_7_10$LOGR, MEF.G1.M2.5_10_10$LOGR, MEF.G1.M3.13_07_10$LOGR,MEF.G1.26_08_11$LOGR,MEF.G1.M2.14_08_11$LOGR)
#
#plot.densite(R.V.NORM.all, "Densites des log intensites rouges et vertes des puces apres normalisation", col=c(rep("green",9), rep("red",9)))
#rm(R.V.NORM.all);

#**************************************************************************#
# ********** EXPORTATION DES DONNEES NORMALISEES AU FORMAT .GFF ********** #
#**************************************************************************#

########## ---------- DONNEES NORMALISEES ---------- ##########
export.gff(array1, data.M = "M.NORM")


#*********************************************************#
# ********** EXPORTATION DES DONNEES DES PUCES ********** #
#*********************************************************#

safe.dir.create("Data")
write.table(array1, file = paste("Data/",name_array1,".txt",sep=''), quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

save.image(file = paste("Lecture_Normalisation_",analysis,".RData",sep=''))

