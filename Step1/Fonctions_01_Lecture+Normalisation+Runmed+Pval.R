#################################################################################################################################################
# FONCTIONS CONTENUES DANS LE SCRIPT
#################################################################################################################################################
# format.fichier 		<- function(titre, type = c("", "ps", "pdf", "png", "jpeg", "bmp"), width = NULL, height = NULL, bg = "white",...)
# safe.dir.create 		<- function(path)
# format.data 			<- function(num.puce, nom.sondes = "BLOCK1", export = FALSE, design = NULL)
# plot.data 			<- function(x, type = "Avant normalisation", format = "jpeg")
# plot.genome 			<- function(data.puce, data.M = "M", pval = NULL, seuil = NULL, inf = min(data.puce$POSITION), sup = max(data.puce$POSITION), fen = 1000000, format = "jpeg")
# norma.lowess			<- function(x, num.puce, plot = FALSE, export = FALSE, inv.ratio = FALSE, format = "jpeg")
# biais.correction 		<- function(data.puce, nom, inv.ratio = FALSE, d = 201, plot = FALSE, format = "jpeg") 
# box.plots 			<- function(liste, titre, col = NULL, ylim = NULL, names = NULL, width = 3000, height = 1200, format = "jpeg")
# plot.densite			<- function(liste, titre, col = NULL, width = 2000, height = 1200, format = "jpeg")
# export.gff 			<- function(data.puce, data.M = "M", pval = NULL, seuil = NULL, taille.pos = NULL, val.chr = NULL, val.strand = NULL)
# calc.runmed 			<- function(data.puce, window = 5, data.M = "M.NORM", nom = "M.w") 
# calc.runmed2 			<- function(data.puce, window = 5, data.M = "M.NORM", nom = "M.w") 
# calc.pvalue 			<- function(data.puce, data.M = "M.NORM", window = NULL, method = c("rang", "moy.sdv", "med.mad", "mode.mad", "sdv.neg"), adjust = FALSE, plot = FALSE, format = "jpeg")
# erf 					<- function(z) [Note : Fonction qui nest plus utilisee - Utilisation de la fonction "pnorm" qui lui est equivalente a  une transformation pres
#					 	Cf. help(pnorm) and see the indicated bibliographie ]
# MA.plot 				<- function(data.puce, data.M = "M.NORM", data.A = "A.NORM", pval = "pval.rang.M", seuil = 0.05, format="jpeg")
# normalizeBetweenArrays 	<- function(object, method="Aquantile", targets=NULL, ...) 
# normalizeQuantiles 		<- function(A, ties=TRUE) {
# normalizeMedianAbsValues 	<- function(x) 
# merge.exp.control.neg <- function(data.puce.neg, data.puce.exp, nom)
# substract.control.neg <- <- function(data.puce.neg, data.puce.exp, nom, data.M, nom.M.out)
# plot.cor.replicats <- function(replicat1,replicat2,replicat3=NULL,signal="M.NORM",titre="Nuage de points du signal entre replicats",width=600,height=600) 
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-#
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-#


#***********************************************************************************************************************************************#
# FONCTION FORMAT.FICHIER -> Pour choisir le format des graphiques en sortie
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Cette fonction permet de choisir le format des graphiques en sortie. Il y a le choix parmi 5 formats differents : 
# postscript ("ps"), PDF ("pdf"), PNG ("png"), JPEG ("jpeg") ou bitmap ("bmp").
#################################################################################################################################################

format.fichier <- function(titre, type = c("", "ps", "pdf", "png", "jpeg", "bmp"), width = NULL, height = NULL, bg = "white",...)
	
	#########################################################################################################################################
	# titre 	-> 	Le nom que l'on veut donner au fichier graphique en sortie. Le nom doit etre entre guillemets et il n'y a pas 
	#			besoin d'indiquer l'extension.
	# type		-> 	Le format du fichier que l'on veut en sortie.
	# width 	->	La largeur du graphique en sortie.
	# height	->	La hauteur du graphique en sortie.
	# bg		->	La couleur du fond du graphique.
	# ...		->	Les autres parametres possibles qui peuvent etre pris en parametre par les fonctions utilisees par la suite.
	#
	# Les formats disponibles sont les suivants :
	#	- "ps" : 		fichier au format '.ps' (par defaut)
	#	- "pdf" : 		fichier au format '.pdf'
	#	- "png" : 		fichier au format '.png'
	#	- "jpeg" :  		fichier au format '.jpeg'
	#	- "bmp" :  		fichier au format '.bmp' (image bitmap)
	#########################################################################################################################################
{	
if (is.element("ps", type)) 
	{
	if (is.null(width) || is.null(height)){postscript(paste(titre,".ps",sep=""), horizontal=TRUE, ...)}
	else {postscript(paste(titre,".ps",sep=""), horizontal=TRUE, width=width, height=height, ...)}
	}
else
	{
	if (is.element("pdf", type)) 
		{
		if (is.null(width) || is.null(height)){pdf(paste(titre,".pdf",sep=""),bg=bg, ...)}
		else {pdf(paste(titre,".pdf",sep=""),bg=bg, width=width, height=height, ...)}
		}
	else
		{
		if (is.element("png", type)) 
			{
			if (is.null(width) || is.null(height)){png(paste(titre,".png",sep=""),bg=bg, ...)}
			else {png(paste(titre,".png",sep=""),bg=bg, width=width, height=height, ...)}
			}
		else
			{
			if (is.element("jpeg", type)) 
				{
				if (is.null(width) || is.null(height)){jpeg(paste(titre,".jpeg",sep=""), quality = 100, bg=bg, ...)}
				else {jpeg(paste(titre,".jpeg",sep=""), quality = 100, width=width, height=height, bg=bg, ...)}
				}
			else
				{
				if (is.element("bmp", type)) 
					{
					if (is.null(width) || is.null(height)){bitmap(paste(titre,".bmp",sep=""), ...)}
					else {bitmap(paste(titre,".bmp",sep=""), width=width, height=height, ...)}
					}
				else
					{
						stop("Le format choisi n'est pas valide.\n")
					}
				}
			}
		}
	}
}

#***********************************************************************************************************************************************#
# FONCTION SAFE.DIR.CREATE -> Pour creer un dossier dans le repertoire courant (celui dans lequel on travail)
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Cette fonction permet de creer un nouveau dossier dans le repertoire courant en utilisant la commande "safe.dir.create(file.path(name="XXX"))" 
# oa¹ le parametre "name" correspond au nom que l'on veut donner au dossier que l'on veut creer. Cette fonction est issue de la fonction R cran
# "package.skeleton" qui permet de creer un package.
#################################################################################################################################################

safe.dir.create <- function(path)
	
	#########################################################################################################################################
	# path 	-> 	Le nom du dossier que l'on veut creer dans le repertoire courant.
	#########################################################################################################################################
{
	dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir
	if (!dirTest(path) && !dir.create(path)) 
	stop(gettextf("cannot create directory '%s'", path), domain = NA)
}

#***********************************************************************************************************************************************#
# FONCTION FORMAT.DATA -> Pour formater les donnees avant de passer a  l'analyse
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Pour formater les donnees avant de passer a  l'analyse, les etapes sont les suivantes :
# 	-> Lecture des donnees.
# 	-> Concatenation des fichiers.
# 	-> Calcul du ratio de A et de M.
# 	-> Si export = TRUE, les donnees sont exportees dans 3 fichiers texte (".txt") dans le dossier "Raw_Data/" :
#		- un contenant les donnees de toutes les sondes (suffixe "_tout.txt").
#		- un contenant les donnees des sondes Random (suffixe "_random.txt").
#		- un contenant les donnees des sondes specifiques (suffixe "_sondes.txt").
#################################################################################################################################################

format.data <- function(num.puce, nom.sondes = "BLOCK1", export = FALSE, design = NULL)

	#########################################################################################################################################
	# num.puce	-> Le numero de la puce.
	# nom.sondes 	-> Le nom des sondes specifiques dans les donnees chargees ("FORWARD" pour les CGH Arrays et "BLOCK1" pour les tiling Arrays)
	# export 	-> Si export = TRUE, les donnees sont exportees dans 3 fichiers texte.
	# design        -> Si design != NULL les informations sur le design seront lues dans le fichier specifie du repertoire Design_Data
	#########################################################################################################################################
{
	# --- Lecture du fichier .pos --- #
	if(is.null(design) == FALSE) {data.pos <- read.table(paste("Design_Data/", design, sep=""), sep="\t", dec=".", header=TRUE)}

	# --- Lecture des fichiers de donnees rouge et vert --- #
	data.rouge <- read.table(paste("Raw_Data/", num.puce, "_635.pair", sep=""), sep="\t", dec=".", header=TRUE)
	data.vert  <- read.table(paste("Raw_Data/", num.puce, "_532.pair", sep=""), sep="\t", dec=".", header=TRUE)
	cat(paste("Le fichier ", num.puce, "_635.pair a ", dim(data.rouge)[[1]], " lignes et ", dim(data.rouge)[[2]], " colonnes. \n", sep=""))
	cat(paste("Le fichier ", num.puce, "_532.pair a ", dim(data.vert)[[1]], " lignes et ", dim(data.vert)[[2]], " colonnes. \n \n", sep=""))

	# --- Concatenation des deux fichiers en une seule table --- #
	data.rouge$IMAGE_ID 	<- num.puce
	data.vert$IMAGE_ID 	<- num.puce
	data.merge <- merge(	data.rouge[c("IMAGE_ID","GENE_EXPR_OPTION","SEQ_ID","PROBE_ID","POSITION","X","Y","MATCH_INDEX","SEQ_URL","PM")],
				data.vert[c("IMAGE_ID","GENE_EXPR_OPTION","SEQ_ID","PROBE_ID","POSITION","X","Y","MATCH_INDEX","SEQ_URL","PM")],
				by.x=c("IMAGE_ID","GENE_EXPR_OPTION","SEQ_ID","PROBE_ID","POSITION","X","Y","MATCH_INDEX","SEQ_URL"),
				by.y=c("IMAGE_ID","GENE_EXPR_OPTION","SEQ_ID","PROBE_ID","POSITION","X","Y","MATCH_INDEX","SEQ_URL"),
				suffixes = c(".R",".V"))
	cat(paste("La table des deux fichiers concatenes a ", dim(data.merge)[[1]], " lignes et ", dim(data.merge)[[2]], " colonnes. \n", sep=""))
	
	### --- Ajout de la taille des positions dans le fichier merge--- ###
	if(is.null(design) == FALSE) 
		{
		data.merge <- 	merge(data.merge,data.pos[c("SEQ_ID", "CHROMOSOME", "PROBE_ID", "POSITION", "LENGTH")],
				by.x = c("SEQ_ID", "PROBE_ID", "POSITION"), by.y = c("SEQ_ID","PROBE_ID","POSITION"), all.x = TRUE, sort = FALSE)
		}

	# --- Calcul du ratio, du log ratio (M) et de A --- #
        data.merge$RATIO	<- (data.merge$PM.R) / (data.merge$PM.V)
        data.merge$M		<- log2(data.merge$RATIO)
        data.merge$A         	<- 0.5 * log2(data.merge$PM.R * data.merge$PM.V)

	# --- Separation des donnees en deux tables : une qui contient toutes les donnees, une que les randoms et une que les valeurs des sondes specifiques --- #
        data.sondes <- as.data.frame(unique(data.merge[data.merge$GENE_EXPR_OPTION != "RANDOM",]))
        data.random <- as.data.frame(unique(data.merge[data.merge$GENE_EXPR_OPTION == "RANDOM",]))

	cat(paste("La table SONDES a", dim(data.sondes)[[1]], "lignes et", dim(data.sondes)[[2]], "colonnes. \n"))
	cat(paste("La table RANDOM a", dim(data.random)[[1]], "lignes et", dim(data.random)[[2]], "colonnes. \n"))

	for(level in levels(data.merge$GENE_EXPR_OPTION))
		{
		if(is.element(level, nom.sondes) == FALSE) 
			{
			data.random <- rbind(data.random, data.sondes[data.sondes$GENE_EXPR_OPTION == level,])
			data.sondes <- data.sondes[data.sondes$GENE_EXPR_OPTION != level,]
			}
		}

	cat(paste("La table SONDES a", dim(data.sondes)[[1]], "lignes et", dim(data.sondes)[[2]], "colonnes. \n"))
	cat(paste("La table RANDOM a", dim(data.random)[[1]], "lignes et", dim(data.random)[[2]], "colonnes. \n"))

	if(is.element("RANDOM_GC46_DUAL",levels(data.sondes$SEQ_ID)) == TRUE)
		{
		data.random <- rbind(data.random, data.sondes[data.sondes$SEQ_ID == "RANDOM_GC46_DUAL",])
		data.sondes <- data.sondes[data.sondes$SEQ_ID != "RANDOM_GC46_DUAL",]
		}

	cat(paste("La table SONDES a", dim(data.sondes)[[1]], "lignes et", dim(data.sondes)[[2]], "colonnes. \n"))
	cat(paste("La table RANDOM a", dim(data.random)[[1]], "lignes et", dim(data.random)[[2]], "colonnes. \n"))

	# --- Exportation des donnees --- #
	if(export == TRUE)
		{
		write.table(data.merge,file=paste("Raw_Data/", num.puce, "_tout.txt", sep=""), quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
		write.table(data.sondes,file=paste("Raw_Data/", num.puce, "_sondes.txt", sep=""), quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
		write.table(data.random,file=paste("Raw_Data/", num.puce, "_random.txt", sep=""), quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

		cat(paste("Creation des fichiers suivants dans le dossier Raw_Data/ : \n --> ", 
		num.puce, "_tout.txt qui contient les donnees de la totalite des sondes \n --> ", 
		num.puce, "_sondes.txt qui contient les donnees des sondes specifiques \n --> ",
		num.puce, "_random.txt qui contient les donnees des randoms \n \n",sep=""))
		}
return(data.sondes)
}

#***********************************************************************************************************************************************#
# FONCTION PLOT.DATA -> Graphiques de la repartition des donnees avant ou apres normalisation
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Graphiques obtenus en sortie :
#	-> Box-Plots des donnees (log-intensites rouge et verte et log-ratios).
#	-> MA Plot et RG Plot des donnees
#	-> Densites RG
#	-> Histogramme de densite des log-ratios
#################################################################################################################################################

plot.data <- function(x, type = "Avant normalisation", format = "jpeg")

	#########################################################################################################################################
        # x     	-> Le nom de la table contenant les donnees de la puce a  representer graphiquement.
        # type 		-> Pour preciser plus en detail a  quoi correspondent les donnees : "Avant normalisation" ou "Apres normalisation Lowess".
	# format	-> Le format de sortie des graphiques.
	#########################################################################################################################################
{
	nom	<- deparse(substitute(x))

	# --- Creation d'un fichier ou mettre les donnees (si il n'existe pas) --- #
	nom.doss <- paste("Graphiques -",type)
	safe.dir.create(nom.doss)
	
	# --- Donnees pour les graphiques --- #
	if (type == "Avant normalisation")
		{
		A 	<- x$A
		M 	<- x$M
		LOG.V 	<- log2(x$PM.V)
		LOG.R 	<- log2(x$PM.R)
		}
	if (type == "Apres normalisation Lowess")
		{
		A 		<- x$A.NORM
		M 		<- x$M.NORM
		LOG.V	 	<- log2(x$PM.V) + x$y.LOWESS
		LOG.R 		<- log2(x$PM.R)
		}

	if (type == "Apres normalisation rotation + Lowess")
		{
		A 		<- x$A.CORRECTED
		M 		<- x$M.CORRECTED
		LOG.V	 	<- log2(x$PM.V) + x$y.CORRECTED
		LOG.R 		<- log2(x$PM.R)
		}

        dens.LOG.V            	<- density(LOG.V)
        dens.LOG.R             	<- density(LOG.R)
        dens.M	          	<- density(M)
        dens.LOG.min.x          <- min(min(dens.LOG.V$x),min(dens.LOG.R$x))
        dens.LOG.min.y          <- min(min(dens.LOG.V$y),min(dens.LOG.R$y))
        dens.LOG.max.x          <- max(max(dens.LOG.V$x),max(dens.LOG.R$x))
       	dens.LOG.max.y          <- max(max(dens.LOG.V$y),max(dens.LOG.R$y))
       	dens.M.min.x    	<- min(dens.M$x)
        dens.M.min.y    	<- min(dens.M$y)
        dens.M.max.x    	<- max(dens.M$x)
        dens.M.max.y    	<- max(dens.M$y)
	nb.tot  		<- length(M)

        # --- Box-Plots des donnees --- #
	format.fichier(paste(nom.doss, "/Box-Plots - ", nom, sep=""), type=format, width=3000, height=1500)
       	par(mfrow=c(1,3))
		# --- Box-Plot des log Intensites vertes --- #
		boxplot(LOG.V, col=rainbow(30)[14], xlab=nom, ylim=c(4,20), main = "Boxplot de la repartition des log-intensites vertes")
		
		# --- Box-Plot des log Intensites rouges --- #
		boxplot(LOG.R, col=rainbow(30)[30], xlab=nom, ylim=c(4,20), main = "Boxplot de la repartition des log-intensites rouges")

		# --- Box-Plot des log ratios --- #
		boxplot(M, col=rainbow(30)[17], xlab=nom, ylim=c(-5,5), main = "Boxplot de la repartition des log-ratios")
                abline(h=0,col=rainbow(9)[9], lty=2, lwd=2)
        dev.off()
        cat("Creation des Box-Plots.\n")

        # --- MA-Plot et RG-Plot des donnees --- #
	format.fichier(paste(nom.doss, "/MA-Plot et RG-Plot - ", nom, sep=""), type=format, width=2500, height=1200)
       	par(mfrow=c(1,2))
		# --- MA-Plot --- #
                plot(A, M, type="n", xlim=c(6,20), ylim=c(-6,6), xlab="A", ylab="M", main=paste("MA-Plot de la puce", nom, "\n", type, "des donnees"))
		points(A, M, pch=20, cex=0.3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.25))
                abline(h=0,col=rainbow(9)[9],lwd=2)
		if (type == "Apres normalisation Lowess"){lines(lowess(A,M),col=rainbow(30)[17],lwd=2)}
		legend(6,6,legend=paste("nb tot =",nb.tot),fill="black",xjust=0,cex=1.5)

		# --- RG-Plot --- #
                plot(LOG.V, LOG.R, type="n", xlim=c(6,20), ylim=c(6,16), xlab="log(Vert)",ylab="log(Rouge)", main=paste("RG-Plot en log de la puce", nom, "\n", type, "des donnees"))
    		points(LOG.V, LOG.R, pch=20, cex=0.3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.25))
                abline(a=0,b=1,col=rainbow(9)[9],lwd=2)
		legend(6,16,legend=paste("nb tot =",nb.tot),fill="black",xjust=0,cex=1.5)
        dev.off()
        cat("Creation du MA-Plot et du RG-Plot.\n")

        # --- Densites RG : Densite des intensites rouge et verte --- #
	format.fichier(paste(nom.doss, "/Densites RG - ", nom, sep=""), type=format, width=2500, height=1200)
       	par(mfrow=c(1,1))
                # --- Densites RG des log-intensites --- #
                plot(dens.LOG.V,col=rainbow(30)[14],lwd=2,xlim=c(dens.LOG.min.x,dens.LOG.max.x),ylim=c(dens.LOG.min.y,dens.LOG.max.y),xlab="Log des intensites Rouge et Verte",
                ylab="Densite",main=paste("Densites des log intensites de la puce \n",nom,type))
                points(dens.LOG.R,type="l",col=rainbow(30)[30],lwd=2)
                legend(dens.LOG.max.x-2,dens.LOG.max.y,c("Vert","Rouge"),col=c(rainbow(30)[14],rainbow(30)[30]),cex=1,bty="n",lwd=c(2,2))
        dev.off()
        cat("Creation du graphique representant les densites des intensites rouge et verte.\n")

        # --- Histogramme de la repartition des log ratio --- #
	format.fichier(paste(nom.doss, "/Histogramme - ", nom, sep=""), type=format, width=2500, height=1200)
                hist(M, xlab="Log Ratio (R/V)", ylab="Densite", probability=TRUE, xlim =c(dens.M.min.x,dens.M.max.x), ylim = c(dens.M.min.y,dens.M.max.y),border="paleturquoise",
                xaxt="n",col="paleturquoise",main=paste("Densite des log Ratios de la puce \n",nom,type,"des donnees"),breaks=seq(from=-8,to=8,by=0.05))
                axis(1,at=seq(from=-8,to=8,by=0.5),cex.axis=0.8)
                abline(v=0,col=rainbow(30)[27],lwd=3,lty=2)
                text(0,0,"x = 0",col=rainbow(30)[27],adj=c(-0.5,1.3),cex=1.2,font=4)
                points(dens.M,type="l",col="turquoise4",lwd=2)
                legend(dens.M.max.x-2,dens.M.max.y,"Densite des donnees",col="turquoise4",cex=1,bty="n",lty=c(1,2),lwd=c(2,2))
        dev.off()
        cat("Creation de l'histogramme de densite des log ratios.\n")
}

#***********************************************************************************************************************************************#
# FONCTION PLOT.GENOME -> Fonction qui permet de representer graphiquement les donnees du genome 
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Les graphiques obtenus sont les suivants :
# 	-> Un graphique representant la totalite du genome couvert par la puce.
#	-> Un graphique representant la zone ou se trouve le locus HoxB.
#	-> Les graphiques de tout le genome en fragmente si inf != NULL et sup != NULL.
#################################################################################################################################################

plot.genome <- function(data.puce, data.M = "M", pval = NULL, seuil = NULL, inf = min(data.puce$POSITION), sup = max(data.puce$POSITION), fen = 1000000, format = "jpeg", hox = FALSE)

	#########################################################################################################################################
        # data.puce    	-> Le nom de la table contenant les donnees de la puce.
        # data.M	-> Le nom de la colonne contenant les valeurs de M a  representer graphiquement. 
	#		   ("M", "M.NORM.LOWESS", "M.runmed.w5", "M.runmed.w7", "M.runmed.w9", "M.runmed.w11", etc)
	# pval		-> Le nom de la colonne contenant les valeurs des pvalues que l'on veut representer en couleur si elles sont significatives.
	#		   ("pval.rang.M", "pval.norm.M", etc)
	# seuil		-> Le seuil pour la p-value (Les p-values < seuil (i.e. significatives) seront representees en couleur sur les graphiques.
        # inf   	-> La position de depart pour le decoupage en fenetres.(correspondant a la position sur le genome)
        # sup   	-> La position de fin pour le decoupage en fenetres. (correspondant a la position sur le genome)
        # fen   	-> La taille de la fenetre pour la representation graphique du genome en fragmente.
	# format	-> Le format des graphiques en sortie.
	# hox		-> Si hox = TRUE, le locus HoxB est representegraphiquement (uniquement pour la souris)
	#
	# REMARQUE :	-> Si pval = NULL alors les donnees sont representees sans distinguer les valeurs significatives.
	# 		-> Si inf = NULL ou sup = NULL, le decoupage du gena´me en fenaªtre n'est pas realise.
	#########################################################################################################################################
{ 
	nom	<- deparse(substitute(data.puce))

	# --- Creation d'un fichier ou mettre les donnees (si il n'existe pas) --- #
	safe.dir.create("Graphiques - Genome")	
	if(is.null(pval) == TRUE){nom.doss <- paste("Graphiques - Genome/M=", data.M, sep="")}
	else			 {nom.doss <- paste("Graphiques - Genome/M=", data.M, " - pval=", pval, " - alpha=" , seuil, sep="")}
	safe.dir.create(nom.doss)

	# --- Donnees pour les graphiques --- #
	x.M 	<- data.puce[[data.M]]
	x.RATIO <- 2**data.puce[[data.M]]

	if(is.null(pval) == FALSE)
		{
		col.flag	<- ifelse(data.puce[[pval]] <= seuil,"red","black")
		col.flag.pos	<- ifelse(data.puce[[pval]][x.M >= 0] <= seuil,"red","black")
		flag		<- ifelse(data.puce[[pval]] <= seuil, 1, 0)
		}
	else
		{
		col.flag	<- as.vector(rep("black",length(x.M)))
		col.flag.pos	<- as.vector(rep("black",length(x.M[x.M >= 0])))
		}

	if (data.M == "M")
		{
		if(is.null(pval) == FALSE)
			{
			width  	<- 32500
			height 	<- 6000
			}
		else
			{
			width  	<- 32500
			height 	<- 5000
			}

    		# ------ Graphique representant la totalite du genome couvert par la puce ------ #
		format.fichier(paste(nom.doss, "/Genome - Tout - ", nom, sep=""), type = format, width = width, height = height)
		if(is.null(pval) == FALSE) 	{par(mfrow=c(6,1), las=2)}
		else				{par(mfrow=c(5,1), las=2)}
               		# --- Representation des intensites brutes rouges
               		plot(data.puce$POSITION, data.puce$PM.R, type = "h", xlim = c(min(data.puce$POSITION), max(data.puce$POSITION)), ylim = c(0,70000), 
			main = paste(nom, "- Position sur le genome"), xlab = "", ylab = "Intensites brutes rouges", xaxt = "n", col = "red")
               		axis(1, at = seq(from = min(data.puce$POSITION), to = max(data.puce$POSITION), by = 150000), cex.axis = 1)
              		abline(h = 0)

               		# --- Representation des intensites brutes vertes
              		plot(data.puce$POSITION, -data.puce$PM.V, type = "h", xlim = c(min(data.puce$POSITION), max(data.puce$POSITION)), ylim = c(-70000,0), 
			main = paste(nom,"- Position sur le genome"), xlab = "", ylab = "Intensites brutes vertes", xaxt = "n", col = "green")
                	axis(1, at = seq(from = min(data.puce$POSITION), to = max(data.puce$POSITION), by = 150000), cex.axis = 1)
                	abline(h = 0)

              		# --- Representation des ratios
               		plot(data.puce$POSITION, x.RATIO, type = "h", xlim = c(min(data.puce$POSITION), max(data.puce$POSITION)), ylim = c(0,5), 
			main = paste(nom, "- Position sur le genome"), xlab = "", ylab = "Ratio R/V", xaxt = "n", col = "blue")
               		axis(1, at = seq(from = min(data.puce$POSITION), to = max(data.puce$POSITION), by = 150000), cex.axis = 1)
               		abline(h = 1, col = rainbow(9)[9])

                	# --- Representation des log(ratio)
                	plot(data.puce$POSITION, x.M, type = "h", xlim = c(min(data.puce$POSITION), max(data.puce$POSITION)), ylim = c(-5,5), 
			main = paste(nom, "- Position sur le genome"), xlab = "", ylab = "Log2(Ratio R/V)", col=col.flag, xaxt = "n")
                	axis(1, at = seq(from = min(data.puce$POSITION), to = max(data.puce$POSITION), by = 150000), cex.axis = 1)
                	abline(h = 0, col = rainbow(9)[9])

			# --- Representation des log(ratio)>0
                	plot(data.puce$POSITION[x.M >= 0], x.M[x.M >= 0], type = "h", xlim = c(min(data.puce$POSITION), max(data.puce$POSITION)), ylim = c(0,5), 
			main = paste(nom,"- Position sur le genome"), xlab = "", ylab = "Log2(Ratio R/V)", col = col.flag.pos, xaxt = "n")
                	axis(1, at = seq(from = min(data.puce$POSITION), to = max(data.puce$POSITION), by = 150000), cex.axis = 1)
                	abline(h = 0, col = rainbow(9)[9])

			if(is.null(pval) == FALSE)
				{
				# --- Representation des log(ratio)>0 et significatifs
                		plot(data.puce$POSITION[flag == 1], x.M[flag == 1], type="h", xlim = c(min(data.puce$POSITION), max(data.puce$POSITION)), ylim = c(0,5), 
				main = paste(nom,"- Position sur le genome"), xlab = "", ylab = "Log2(Ratio R/V)", col = "red", xaxt = "n")
                		axis(1, at = seq(from = min(data.puce$POSITION), to = max(data.puce$POSITION), by = 150000), cex.axis = 1)
                		abline(h = 0, col = rainbow(9)[9])
				}
        	dev.off()
        	cat("Creation des graphiques representant la totalite du genome.\n")
		}
	else
		{
		if(is.null(pval) == FALSE)
			{
			width  	<- 32500
			height 	<- 4000
			}
		else
			{
			width  	<- 32500
			height 	<- 3000
			}
       		# ------ Graphique representant la totalite du genome couvert par la puce ------ #
		format.fichier(paste(nom.doss, "/Genome - Tout - ", nom,  sep=""), type = format, width = width, height = height)
		if(is.null(pval) == FALSE) 	{par(mfrow=c(4,1), las=2)}
		else				{par(mfrow=c(3,1), las=2)}
              		# --- Representation des ratios
               		plot(data.puce$POSITION, x.RATIO, type = "h", xlim = c(min(data.puce$POSITION), max(data.puce$POSITION)), ylim = c(0,5), 
			main = paste(nom, "- Position sur le genome"), xlab = "", ylab = "Ratio R/V", xaxt = "n", col = "blue")
               		axis(1, at = seq(from = min(data.puce$POSITION), to = max(data.puce$POSITION), by = 150000), cex.axis = 1)
               		abline(h = 1, col = rainbow(9)[9])

               		# --- Representation des log(ratio)
       			plot(data.puce$POSITION, x.M, type = "h", xlim = c(min(data.puce$POSITION), max(data.puce$POSITION)), ylim = c(-5, 5) , 
			main = paste(nom, "- Position sur le genome"), xlab = "", ylab = "Log2(Ratio R/V)", xaxt = "n")
                	axis(1, at = seq(from = min(data.puce$POSITION),to = max(data.puce$POSITION),by = 150000), cex.axis = 1)
               		abline(h = 0, col = rainbow(9)[9])

			# --- Representation des log(ratio)>0
                	plot(data.puce$POSITION[x.M >= 0], x.M[x.M >= 0], type = "h", xlim = c(min(data.puce$POSITION), max(data.puce$POSITION)), ylim = c(0,5), 
			main = paste(nom,"- Position sur le genome"), xlab = "", ylab = "Log2(Ratio R/V)", col = col.flag.pos, xaxt = "n")
                	axis(1, at = seq(from = min(data.puce$POSITION), to = max(data.puce$POSITION), by = 150000), cex.axis = 1)
                	abline(h = 0, col = rainbow(9)[9])

			if(is.null(pval) == FALSE)
				{
				# --- Representation des log(ratio)>0 et significatifs
                		plot(data.puce$POSITION[flag == 1], x.M[flag == 1], type="h", xlim = c(min(data.puce$POSITION), max(data.puce$POSITION)), ylim = c(0,5), 
				main = paste(nom,"- Position sur le genome"), xlab = "", ylab = "Log2(Ratio R/V)", col = "red", xaxt = "n")
                		axis(1, at = seq(from = min(data.puce$POSITION), to = max(data.puce$POSITION), by = 150000), cex.axis = 1)
                		abline(h = 0, col = rainbow(9)[9])
				}
        	dev.off()
        	cat("Creation des graphiques representant la totalite du genome.\n")
		}	
	if(hox == TRUE)
		{
        	# --- Localisation du locus HoxB --- #
        	locus.inf <- 96085000
        	locus.sup <- 96085000 + 182000

		if (data.M == "M")
			{
			if(is.null(pval) == FALSE)
				{
				width  	<- 10000
				height 	<- 6000
				}
			else
				{
				width  	<- 10000
				height 	<- 5000
				}
        		
			# ------ Graphique representant le locus HoxB ------ #
			format.fichier(paste(nom.doss, "/Genome - Locus HoxB - ", nom, sep=""), type = format, width = width, height = height)
			if(is.null(pval) == FALSE) 	{par(mfrow=c(6,1), las=2)}
			else				{par(mfrow=c(5,1), las=2)}
               			# --- Representation des intensites brutes rouges
               			plot(data.puce$POSITION, data.puce$PM.R, type = "h", xlim = c(locus.inf, locus.sup), ylim = c(0,70000), main = paste(nom, "- Position sur le genome"), 
				xlab = "", ylab = "Intensites brutes rouges", xaxt = "n", col = "red")
                		axis(1,at = seq(from = locus.inf, to = locus.sup, by = 1000),cex.axis = 1)
               			abline(h = 0)

               			# --- Representation des intensites brutes vertes
              			plot(data.puce$POSITION, -data.puce$PM.V, type = "h", xlim = c(locus.inf, locus.sup), ylim = c(-70000,0), main = paste(nom,"- Position sur le genome"),
				xlab = "", ylab = "Intensites brutes vertes", xaxt = "n", col = "green")
                		axis(1,at = seq(from = locus.inf, to = locus.sup, by = 1000),cex.axis = 1)
                		abline(h = 0)

              			# --- Representation des ratios
               			plot(data.puce$POSITION, x.RATIO, type = "h", xlim = c(locus.inf, locus.sup), ylim = c(0,5), main = paste(nom, "- Position sur le genome"), 
				xlab = "", ylab = "Ratio R/V", xaxt = "n", col = "blue")
                		axis(1,at = seq(from = locus.inf, to = locus.sup, by = 1000),cex.axis = 1)
               			abline(h = 1, col = rainbow(9)[9])

                		# --- Representation des log(ratio)
                		plot(data.puce$POSITION, x.M, type = "h", xlim = c(locus.inf, locus.sup), ylim = c(-5,5), main = paste(nom, "- Position sur le genome"),
				xlab = "", ylab = "Log2(Ratio R/V)", col=col.flag, xaxt = "n")
                		axis(1,at = seq(from = locus.inf, to = locus.sup, by = 1000),cex.axis = 1)
                		abline(h = 0, col = rainbow(9)[9])

				# --- Representation des log(ratio)>0
                		plot(data.puce$POSITION[x.M >= 0], x.M[x.M >= 0], type = "h", xlim = c(locus.inf, locus.sup), ylim = c(0,5), main = paste(nom,"- Position sur le genome"), 
				xlab = "", ylab = "Log2(Ratio R/V)", col = col.flag.pos, xaxt = "n")
                		axis(1,at = seq(from = locus.inf, to = locus.sup, by = 1000),cex.axis = 1)
                		abline(h = 0, col = rainbow(9)[9])

				if(is.null(pval) == FALSE)
					{
					# --- Representation des log(ratio)>0 et significatifs
                			plot(data.puce$POSITION[flag == 1], x.M[flag == 1], type="h", xlim = c(locus.inf, locus.sup), ylim = c(0,5), main = paste(nom,"- Position sur le genome"), 
					xlab = "", ylab = "Log2(Ratio R/V)", col = "red", xaxt = "n")
                			axis(1,at = seq(from = locus.inf, to = locus.sup, by = 1000),cex.axis = 1)
                			abline(h = 0, col = rainbow(9)[9])
					}
        		dev.off()
        		cat("Creation des graphiques representant le Locus HoxB.\n")
			}
		else
			{
			if(is.null(pval) == FALSE)
				{
				width  	<- 10000
				height 	<- 4000
				}
			else
				{
				width  	<- 10000
				height 	<- 3000
				}
        		# ------ Graphique representant le locus HoxB ------ #
			format.fichier(paste(nom.doss, "/Genome - Locus HoxB - ", nom, sep=""), type = format, width = width, height = height)
			if(is.null(pval) == FALSE) 	{par(mfrow=c(4,1), las=2)}
			else				{par(mfrow=c(3,1), las=2)}
              			# --- Representation des ratios
               			plot(data.puce$POSITION, x.RATIO, type = "h", xlim = c(locus.inf, locus.sup), ylim = c(0,5), main = paste(nom, "- Position sur le genome"), 
				xlab = "", ylab = "Ratio R/V", xaxt = "n", col = "blue")
                		axis(1,at = seq(from = locus.inf, to = locus.sup, by = 1000),cex.axis = 1)
               			abline(h = 1, col = rainbow(9)[9])

               			# --- Representation des log(ratio)
       				plot(data.puce$POSITION, x.M, type = "h", xlim = c(locus.inf, locus.sup), ylim = c(-5, 5) , main = paste(nom, "- Position sur le genome"), 
				xlab = "", ylab = "Log2(Ratio R/V)", xaxt = "n")
                		axis(1,at=seq(from=locus.inf,to=locus.sup,by=1000),cex.axis=1)
               			abline(h = 0, col = rainbow(9)[9])

				# --- Representation des log(ratio)>0
                		plot(data.puce$POSITION[x.M >= 0], x.M[x.M >= 0], type = "h", xlim = c(locus.inf, locus.sup), ylim = c(0,5), main = paste(nom,"- Position sur le genome"), 
				xlab = "", ylab = "Log2(Ratio R/V)", col = col.flag.pos, xaxt = "n")
                		axis(1,at=seq(from=locus.inf,to=locus.sup,by=1000),cex.axis=1)
                		abline(h = 0, col = rainbow(9)[9])

				if(is.null(pval) == FALSE)
					{
					# --- Representation des log(ratio)>0 et significatifs
                			plot(data.puce$POSITION[flag == 1], x.M[flag == 1], type="h", xlim = c(locus.inf, locus.sup), ylim = c(0,5), main = paste(nom,"- Position sur le genome"), 
					xlab = "", ylab = "Log2(Ratio R/V)", col = "red", xaxt = "n")
                			axis(1,at=seq(from=locus.inf,to=locus.sup,by=1000),cex.axis=1)
                			abline(h = 0, col = rainbow(9)[9])
					}
        		dev.off()
        		cat("Creation des graphiques representant le Locus HoxB.\n")
			}
		}

	if(is.null(inf) == FALSE && is.null(sup) == FALSE)
		{
		if (data.M == "M")
			{
			if(is.null(pval) == FALSE)
				{
				width  	<- 32500
				height 	<- 6000
				}
			else
				{
				width  	<- 32500
				height 	<- 5000
				}

        		# ------ Representation de tout le genome en fragmente------ #
       			# On decoupe la puce en fenetres de taille fen pour une meilleure visualisation
        		while(inf < sup)
                		{
				format.fichier(paste(nom.doss, "/Genome - position ", inf," a ", inf+fen," - ", nom, sep=""), type = format, width = width, height = height)
				if(is.null(pval) == FALSE) 	{par(mfrow=c(6,1), las=2)}
				else				{par(mfrow=c(5,1), las=2)}
               				# --- Representation des intensites brutes rouges
                			plot(data.puce$POSITION, data.puce$PM.R, type="h", xlim=c(inf,inf+fen), ylim=c(0,70000), main=paste(nom,"- Position sur le genome"), 
					xlab="", ylab="Intensites brutes rouges", xaxt="n", col="red")
                        		axis(1, at = seq(from = inf, to = inf + fen, by = 2500), cex.axis = 1)
                			abline(h=0)

                			# --- Representation des intensites brutes vertes
                			plot(data.puce$POSITION, -data.puce$PM.V, type="h", xlim=c(inf,inf+fen), ylim=c(-70000,0), main=paste(nom,"- Position sur le genome"), 
					xlab="", ylab="Intensites brutes vertes", xaxt="n", col="green")
                        		axis(1, at = seq(from = inf, to = inf + fen, by = 2500), cex.axis = 1)
                			abline(h=0)

               				# --- Representation des ratios
               				plot(data.puce$POSITION, x.RATIO, type="h", xlim=c(inf,inf+fen), ylim=c(0,5), main=paste(nom,"- Position sur le genome"), 
					xlab="", ylab="Ratio R/V", xaxt="n", col="blue")
                        		axis(1, at = seq(from = inf, to = inf + fen, by = 2500), cex.axis = 1)
               				abline(h=1,col=rainbow(9)[9])

         				# --- Representation des log(ratio)
                			plot(data.puce$POSITION, x.M, type = "h", xlim = c(inf,inf+fen), ylim = c(-5,5), main = paste(nom, "- Position sur le genome"),
					xlab = "", ylab = "Log2(Ratio R/V)", col=col.flag, xaxt = "n")
                        		axis(1, at = seq(from = inf, to = inf + fen, by = 2500), cex.axis = 1)
                			abline(h = 0, col = rainbow(9)[9])

					# --- Representation des log(ratio)>0
                			plot(data.puce$POSITION[x.M >= 0], x.M[x.M >= 0], type = "h", xlim = c(inf,inf+fen), ylim = c(0,5), main = paste(nom,"- Position sur le genome"), 
					xlab = "", ylab = "Log2(Ratio R/V)", col = col.flag.pos, xaxt = "n")
                        		axis(1, at = seq(from = inf, to = inf + fen, by = 2500), cex.axis = 1)
                			abline(h = 0, col = rainbow(9)[9])

					if(is.null(pval) == FALSE)
						{
						# --- Representation des log(ratio)>0 et significatifs
                				plot(data.puce$POSITION[flag == 1], x.M[flag == 1], type="h", xlim = c(inf,inf+fen), ylim = c(0,5), main = paste(nom,"- Position sur le genome"), 
						xlab = "", ylab = "Log2(Ratio R/V)", col = "red", xaxt = "n")
                        			axis(1, at = seq(from = inf, to = inf + fen, by = 2500), cex.axis = 1)
                				abline(h = 0, col = rainbow(9)[9])
						}
        			dev.off()
                		inf <- inf+fen
				}
        		cat("Creation des graphiques representant la totalite du genome en fragmente.\n")
			}
		else
			{
			if(is.null(pval) == FALSE)
				{
				width  	<- 32500
				height 	<- 4000
				}
			else
				{
				width  	<- 32500
				height 	<- 3000
				}

        		# ------ Representation de tout le genome en fragmente------ #
       			# On decoupe la puce en 26 fenetres pour une meilleure visualisation
        		while(inf < sup)
                		{
				format.fichier(paste(nom.doss, "/Genome - position ", inf," a ", inf+fen," - ", nom, sep=""), type = format, width = width, height = height)
				if(is.null(pval) == FALSE) 	{par(mfrow=c(4,1), las=2)}
				else				{par(mfrow=c(3,1), las=2)}
               				# --- Representation des ratios
               				plot(data.puce$POSITION, x.RATIO, type="h", xlim=c(inf,inf+fen), ylim=c(0,5), main=paste(nom,"- Position sur le genome"), 
					xlab="", ylab="Ratio R/V", xaxt="n", col="blue")
                        		axis(1, at = seq(from = inf, to = inf + fen, by = 2500), cex.axis = 1)
               				abline(h=1,col=rainbow(9)[9])

         				# --- Representation des log(ratio)
                			plot(data.puce$POSITION, x.M, type = "h", xlim = c(inf,inf+fen), ylim = c(-5,5), main = paste(nom, "- Position sur le genome"),
					xlab = "", ylab = "Log2(Ratio R/V)", col=col.flag, xaxt = "n")
                        		axis(1, at = seq(from = inf, to = inf + fen, by = 2500), cex.axis = 1)
                			abline(h = 0, col = rainbow(9)[9])

					# --- Representation des log(ratio)>0
                			plot(data.puce$POSITION[x.M >= 0], x.M[x.M >= 0], type = "h", xlim = c(inf,inf+fen), ylim = c(0,5), main = paste(nom,"- Position sur le genome"), 
					xlab = "", ylab = "Log2(Ratio R/V)", col = col.flag.pos, xaxt = "n")
                        		axis(1, at = seq(from = inf, to = inf + fen, by = 2500), cex.axis = 1)
                			abline(h = 0, col = rainbow(9)[9])

					if(is.null(pval) == FALSE)
						{
						# --- Representation des log(ratio)>0 et significatifs
                				plot(data.puce$POSITION[flag == 1], x.M[flag == 1], type="h", xlim = c(inf,inf+fen), ylim = c(0,5), main = paste(nom,"- Position sur le genome"), 
						xlab = "", ylab = "Log2(Ratio R/V)", col = "red", xaxt = "n")
                        			axis(1, at = seq(from = inf, to = inf + fen, by = 2500), cex.axis = 1)
                				abline(h = 0, col = rainbow(9)[9])
						}
        			dev.off()
                		inf <- inf+fen
				}
        		cat("Creation des graphiques representant la totalite du genome en fragmente.\n")
			}
		}
}

#***********************************************************************************************************************************************#
# FONCTION NORMA.LOWESS -> Normalisation des donnees par la methode de Lowess + Graphiques
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Normalisation des donnees par la methode de lowess avec le parametre de lissage par default f = 2/3 = 0.66
#################################################################################################################################################

norma.lowess <- function(x, num.puce, plot = FALSE, export = FALSE, inv.ratio = FALSE, format = "jpeg")

	#########################################################################################################################################
        # x             -> 	Le nom de la table contenant les donnees de la puce a  normaliser.
	# num.puce	->	Le numero de la puce.
	# plot		->	Si plot = TRUE, on a les graphiques des MA-plots en sortie.
	# export	->	Si export = TRUE, la table de donnees creee avec les donnees normalisees est exportee dans un fichier texte.
	# inv.ratio     ->	Si inv.ratio = TRUE, le ratio rouge/vert est inverseapres normalisation. C'est pour les cas ou il y a eu une 
	# 			inversion des fluorochromes lors du marquage, et qu'au depart on a rouge : ADN control et vert : ADN specifique.
	# format	-> 	Le format des graphiques en sortie.
        #
        # Retourne la table entree en parametre avec en plus les coordonnees de la droite de regression de Lowess, les valeurs normalisees de M et 
	# de LOGV apres normalisation de Lowess et la valeur de LOGR.
        # Si plot = TRUE, on a en sortie le graphique du MA-Plot avant et apres normalisation.
	# Si export = TRUE, la nouvelle table de donnees est exportee dans un fichier texte avec le suffixe "_sondes_norma.txt" dans le dossier Raw_Data/
	#########################################################################################################################################
{
	# --- Creation d'un fichier ou mettre les donnees (si il n'existe pas) --- #
	nom	 <- deparse(substitute(x))
	nom.doss <- "Graphiques - Normalisation Lowess"
	safe.dir.create(nom.doss)

        ordre           <- order(x$A)
        data.lowess     <- lowess(x$A,x$M)
        x$y.LOWESS      <- data.lowess$y[order(ordre)]
	nb.tot  	<- length(x$M.NORM)
	#norm.biais 	<- biais.correction(x, nom = nom, inv.ratio = inv.ratio, d = 201, plot = plot, format = format) 

	if(inv.ratio == TRUE)
		{
		x$M.NORM 	<- log2(x$PM.V) + x$y.LOWESS - log2(x$PM.R)
		x$A.NORM 	<- 0.5 * (log2(x$PM.R) + log2(x$PM.V) + x$y.LOWESS)
		x$LOGR		<- log2(x$PM.R) 
        x$LOGV.NORM    	<- log2(x$PM.V)+ x$y.LOWESS
		#x$y.CORRECTED	<- norm.biais$y.corrected
		#x$M.CORRECTED	<- log2(x$PM.V) + norm.biais$y.corrected - log2(x$PM.R)
		#x$A.CORRECTED	<- 0.5 * (log2(x$PM.R) + log2(x$PM.V) + norm.biais$y.corrected)
		}
	else
		{
        x$M.NORM	<- x$M - x$y.LOWESS
		x$A.NORM        <- 0.5 * (log2(x$PM.R) + log2(x$PM.V) + x$y.LOWESS)
		x$LOGR		<- log2(x$PM.R)
        x$LOGV.NORM    	<- log2(x$PM.V) + x$y.LOWESS
		#x$y.CORRECTED	<- norm.biais$y.corrected
		#x$M.CORRECTED	<- norm.biais$M.corrected
		#x$A.CORRECTED	<- norm.biais$A.corrected
		}

	if(plot == TRUE)
		{
       	 	# --- MA-Plot des donnees  --- #
		format.fichier(paste(nom.doss, "/MA-Plot + Lowess - ", nom, sep=""), type=format, width=2500, height=1200)
        	par(mfrow=c(1,2))
                	# --- Avant normalisation avec la courbe de Lowess representee --- #
                	plot(x$A, x$M, type="n", xlim=c(6,16), ylim=c(-6,6), xlab="A", ylab="M", main=paste("MA-Plot et courbe de Lowess de la puce",nom,"\n avant normalisation des donnees"))
			points(x$A, x$M, pch=20, cex=0.3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.25))
                	abline(h=0,col=rainbow(9)[9],lwd=2,lty=2)
                	lines(data.lowess,col=rainbow(30)[17],lwd=2)
			legend(6,16,legend=paste("nb tot =",nb.tot),fill="black",xjust=0,cex=1.5)

                	# --- Apres normalisation --- #
                	plot(x$A.NORM,x$M.NORM, type="n", xlim=c(6,16), ylim=c(-6,6), xlab="A", ylab="M normalise", 
			main=paste("MA-Plot et courbe de Lowess de la puce",nom,"\n apres normalisation des donnees"))
			points(x$A.NORM, x$M.NORM, pch=20, cex=0.3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.25))
                	abline(h=0,col=rainbow(9)[9],lwd=2,lty=2)
                	lines(lowess(x$A.NORM, x$M.NORM),col=rainbow(30)[17],lwd=2)
			legend(6,16,legend=paste("nb tot =",nb.tot),fill="black",xjust=0,cex=1.5)
        	dev.off()
        	cat("Normalisation des donnees avec Lowess.\nCreation des MA-Plot avant et apres normalisation des donnees par la methode de Lowess.\n")
		}
	if(export == TRUE)
		{
		# --- Exportation des donnees --- #
		write.table(x,file=paste("Raw_Data/", num.puce, "_sondes_norma.txt", sep=""), quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
		cat(paste("Creation du fichier suivant dans le dossier Raw_Data/ : \n --> ", 
		num.puce, "_sondes_norma.txt qui contient les donnees normalisees avec Lowess des sondes specifiques \n \n",sep=""))	
		}
return(x)
}

#***********************************************************************************************************************************************#
# FONCTION BIAIS.CORRECTION -> Correction du biais par rotation du nuage de points puis normalisation Lowess
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Normalisation des donnees par rotation du nuage de points puis normalisation de Lowess
#################################################################################################################################################

biais.correction <- function(data.puce, nom, inv.ratio = FALSE, d = 201, plot = FALSE, format = "jpeg") 

	#########################################################################################################################################
        # data.puce	-> 	Les donnees de la puce.
	# nom		-> 	Le nom de la table de donnees des puces.
	# inv.ratio     -> 	Si inv.ratio = TRUE, le ratio rouge/vert est inverseapres normalisation. C'est pour les cas ou il y a eu une 
	# 		   	inversion des fluorochromes lors du marquage, et qu'au depart on a rouge : ADN control et vert : ADN specifique.
	# d		-> 	La distance entre deux sondes pour l'evaluation de l'intensite du biais (rotation).
	# plot		->	Si plot = TRUE, on a les graphiques des MA-plots en sortie.
	# format	-> 	Le format des graphiques en sortie.
	#########################################################################################################################################
{
	M   <- data.puce$M
	A   <- data.puce$A
	safe.dir.create("Graphiques - Normalisation Lowess + rotation")

	# On centre les valeurs de A et de M sur leur mode respectif
	#M.density <- density(M, na.rm=TRUE)     
	#M.mode    <- M.density$x[which(M.density$y==max(M.density$y))]
	#M         <- M - M.mode

	#A.density <- density(A, na.rm=TRUE)
	#A.mode    <- A.density$x[which(A.density$y==max(A.density$y))]
	#A         <- A - A.mode

	# On applique la normalisation de Lowess au donnees brutes (centrees sur le mode)
        ordre       <- order(A)
        norm.only   <- lowess(A,M)
        y.norm.only <- norm.only$y[order(ordre)]
        M1	    <- M - y.norm.only
	A1          <- A + y.norm.only/2
	remaining   <- lowess(A1, M1)

	# Evaluation de l'intensite du biais en calculant le gradient de A et de M / gradient method = lagged differences
	dM <- diff(M, d) 
	dA <- diff(A,d) 
	M[(d+1):length(M)]-M[1:(length(M)-d)]
	A[(d+1):length(A)]-A[1:(length(A)-d)]

    	# Below are two alternative (slower and not validated) methods to calculate the gradient of A and M
	# dM <- fod(M,d,s) # gradient method = convolution by a first order gaussian derivative, needs gradient calculation functions to be tested
	# dA <- fod(A,d,s) # gradient method = convolution by a first order gaussian derivative, needs gradient calculation functions to be tested
	# dM <- gradient(M,d) # gradient method = average differences, needs gradient calculation functions to be tested
	# dA <- gradient(A,d) # gradient method = average differences, needs gradient calculation functions to be tested

	# Evaluation de l'intensite du biais en utilisant une regression lineaire
	pca    <- prcomp(as.matrix(data.frame(dA,dM)))
	slope1 <- abs(pca$rotation[c(1,3)])
	slope1 <- -slope1[which.min(slope1)]
	droite <- lm(dM ~ dA, weights = (dM^2 + dA^2)^2)
	slope2 <- -abs(droite$coefficients["dA"])
	slope  <- (slope1 + slope2)/2 # We take the average bias slope
	#msglog("PCA bias slope",slope1)
	#msglog("LM  bias slope",slope2)
	#msglog("Average bias slope",slope)

	# Compute bias rotation angle according to the linear bias slope
	theta1 <- -asin(slope1 / sqrt(1 + slope1^2))
	theta2 <- -asin(slope2 / sqrt(1 + slope2^2))
	theta  <- -asin(slope / sqrt(1 + slope^2)) # We take the average bias angle
	#msglog("PCA correction angle",round(180*theta1/pi))
	#msglog("LM  correction angle",round(180*theta2/pi))
	#msglog("Average correction angle",round(180*theta/pi))

	# Apply bias correction by rotating the A and M signals in 2D
	M2 <-  M * cos(theta) + A * sin(theta)
	A2 <- -M * sin(theta) + A * cos(theta)

	# Apply a lowess normalization on the bias corrected signal
	ordre           <- order(A2)
	normalization   <- lowess(A2, M2)
        y.normalization <- normalization$y[order(ordre)]
	M3              <- M2 - y.normalization
	A3              <- A2 + y.normalization/2
	remaining.final <- lowess(A3, M3)

	# Graphique du gradient de A et de M
	format.fichier(paste("Graphiques - Normalisation Lowess + rotation/", nom, " - gradients:", d, sep=""), type=format, width=1200, height=1200)
                plot(dA, dM, type="n", xlim=c(-8,8), ylim=c(-8,8), xlab="dA", ylab="dM", main=paste("Gradients de la puce",nom))
		points(dA, dM, pch=20, cex=0.3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.25))
                abline(h=0,col=rainbow(9)[9],lwd=2,lty=2)
		abline(0, slope1, col=rainbow(32)[5])
		abline(0, slope2, col=rainbow(32)[15])
		abline(0, slope, col=rainbow(32)[30])
	dev.off()

	#xymin <- min(min(A, na.rm=TRUE), min(M, na.rm=TRUE), min(A1, na.rm=TRUE), min(M1, na.rm=TRUE), min(A2, na.rm=TRUE), min(M2, na.rm=TRUE), min(A3, na.rm=TRUE), min(M3, na.rm=TRUE))
	#xymax <- max(max(A, na.rm=TRUE), max(M, na.rm=TRUE), max(A1, na.rm=TRUE), max(M1, na.rm=TRUE), max(A2, na.rm=TRUE), max(M2, na.rm=TRUE), max(A3, na.rm=TRUE), max(M3, na.rm=TRUE))

	format.fichier(paste("Graphiques - Normalisation Lowess + rotation/", nom, " - MA correction", d, sep=""), type=format, width=3200, height=800)
	par(mfrow = c(1,4))

                plot(A, M, type="n", xlim=c(5,20), ylim=c(-8,8), xlab="A", ylab="M", main=paste("MA-Plot et courbe de Lowess de la puce",nom,"\n avant normalisation des donnees"))
		points(A, M, pch=20, cex=0.3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.5))
                abline(h=0,col=rainbow(9)[9],lwd=2,lty=2)
                lines(norm.only,col=rainbow(30)[17],lwd=2)

                plot(A1, M1, type="n", xlim=c(5,20), ylim=c(-8,8), xlab="A", ylab="M", main=paste("MA-Plot et courbe de Lowess de la puce",nom,"\n apres normalisation Lowess"))
		points(A1, M1, pch=20, cex=0.3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.5))
                abline(h=0,col=rainbow(9)[9],lwd=2,lty=2)
                lines(remaining,col=rainbow(30)[10],lwd=2)

                plot(A2, M2, type="n", xlim=c(5,20), ylim=c(-8,8), xlab="A", ylab="M", main=paste("MA-Plot et courbe de Lowess de la puce",nom,"\n apres rotation (correction du biais)"))
		points(A2, M2, pch=20, cex=0.3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.5))
                abline(h=0,col=rainbow(9)[9],lwd=2,lty=2)
                lines(normalization,col=rainbow(30)[20],lwd=2)

		plot(A3, M3, type="n", xlim=c(5,20), ylim=c(-8,8), xlab="A", ylab="M", main=paste("MA-Plot et courbe de Lowess de la puce",
		nom,"\n apres rotation (correction du biais) + normalisation Lowess"))
		points(A3, M3, pch=20, cex=0.3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.5))
                abline(h=0,col=rainbow(9)[9],lwd=2,lty=2)
                lines(remaining.final,col=rainbow(30)[7],lwd=2)
	dev.off()

	y.corrected <- M - M3
	res         <- data.frame(A, M, A1, M1, A2, M2, A3, M3, y.corrected)
	names(res)  <- c("A", "M", "A.lowess", "M.lowess", "A.rotation", "M.rotation", "A.corrected", "M.corrected", "y.corrected")
	return(res)
}

#***********************************************************************************************************************************************#
# FONCTION BOX.PLOTS -> Boxplots simultannes de toutes les donnees de la liste entree en parametre
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Fonction qui permet de representer simultanement sur un maªme graphique, tous les boxplots des donnees de la liste entree en parametre.
#################################################################################################################################################

box.plots <- function(liste, titre, col = NULL, ylim = NULL, names = NULL, width = 3000, height = 1200, format = "jpeg")

	#########################################################################################################################################
        # liste 	->      Une liste qui contient les donnees a  representer (un boxplot par liste).
	# titre		->	Le titre du graphique.
	# col		->	Les couleurs des boxplots.
	# ylim		-> 	Les limites de l'axe y.
	# names		-> 	Les noms des donnees de la liste affichees sur les boxplots.
	# width		->	La largeur du graphique de sortie.
	# height	->	La hauteur du graphique de sortie.
	# format	-> 	Le format des graphiques en sortie.
	#########################################################################################################################################
{
	# --- Creation d'un fichier ou mettre les donnees (si il n'existe pas) --- #
	safe.dir.create("Graphiques - Tout")

        # --- Boxplots --- #
        jpeg(paste("Graphiques - Tout/",titre,sep=""),quality=100,width=width,height=height,pointsize=12,bg="white")
        #par(las=2)
		boxplot(liste, col=col, ylim = ylim, main=titre, names=names)
		abline(h=0, col="blue", lty=2, lwd=2)
        dev.off()
        cat(paste("Creation du graphique :", titre,".\n"))
}

#***********************************************************************************************************************************************#
# FONCTION PLOT.DENSITES -> Represente simultanement les densites des elements de la liste entree en parametre
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Represente simultanement les densites des log-ratios, des intensites vertes ou des intensites rouges de de toutes les puces. 
# On peut aussi representer simultanement toutes les densites des intensites rouges et vertes afin de les comparer entre elles.
#################################################################################################################################################

plot.densite<- function(liste, titre, col = NULL, width = 2000, height = 1200, format = "jpeg")

	#########################################################################################################################################
        # liste 	->      Une liste qui contient les donnees a  representer (une courbe par element de la liste).
	# titre		->	Le titre du graphique.
	# col		-> 	Les couleurs des courbes.
	# width		->	La largeur du graphique de sortie.
	# height	->	La hauteur du graphique de sortie.
	# format	-> 	Le format des graphiques en sortie.
	#########################################################################################################################################
{
	# --- Creation d'un fichier ou mettre les donnees (si il n'existe pas) --- #
	safe.dir.create("Graphiques - Tout")

	# --- Initialisation des parametres --- #
        min.log.x       <- 100000
        max.log.x       <- 0
        min.log.y       <- 0
        max.log.y       <- 0

        for (i in 1:length(liste))
                {
                dens.log        <- density(liste[[i]])
                if (min(dens.log$x) < min.log.x) {min.log.x <- min(dens.log$x)}
                if (max(dens.log$x) > max.log.x) {max.log.x <- max(dens.log$x)}
                if (min(dens.log$y) < min.log.y) {min.log.y <- min(dens.log$y)}
                if (max(dens.log$y) > max.log.y) {max.log.y <- max(dens.log$y)}
                }

        # --- Densites des donnees de la liste --- #
        jpeg(paste("Graphiques - Tout/",titre,sep=""),quality=100,width=width,height=height,pointsize=12,bg="white")
       		plot(density(liste[[1]]),type="n",xlim=c(min.log.x,max.log.x),ylim=c(min.log.y,max.log.y), ylab="Densite",main=titre)
		abline(v=0,lty=2,lwd=2)
               	for (i in 1:length(liste))
                        {
                       if (is.null(col))
				{
				points(density(liste[[i]]),type="l",col="blue",lwd=2)
				}
			else
				{
                       		points(density(liste[[i]]),type="l",col=col[[i]],lwd=2)
                       		}
			}
        dev.off()
        cat(paste("Creation du graphique :", titre,".\n"))
}

#***********************************************************************************************************************************************#
# FONCTION EXPORT.GFF -> Exportation des donnees au format .gff afin d'etre visualisees dans Signalmap
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Exportation des donnees au format .gff avec la possibilite de representer les p-values significatives en couleur. 
# Les fichiers .gff sont visualisables dans SignalMap.
# Les 4 tables suivantes sont exportees (si p.value=TRUE, sinon juste la 1ere table) :
# 	-> La 1ere permet de representer graphiquement la totalite du genome apres normalisation des donnees sans distinction des pics significatifs.
# 	-> La 2eme permet de representer graphiquement la totalite du genome avec les pics dont la p-value est significative en noir et le reste en gris.
# 	-> La 3eme permet de representer graphiquement uniquement les pics pour lesquels le log ratio est > 0 avec les pics dont la p-value est 
#	   significative en noir et le reste en gris.
#	-> La 4eme permet de representer graphiquement uniquement les pics pour lesquels la p-value est significative (en noir)
#################################################################################################################################################

export.gff <- function(data.puce, data.M = "M", pval = NULL, seuil = NULL, taille.pos = NULL, val.chr = NULL, val.strand = NULL)

	#########################################################################################################################################
        # data.puce     -> Les donnees de la puce contenant les p-values.
	# data.M 	-> Le nom de la colonne contenant les valeurs des log-ratios a  exporter au format.gff.
	#		   ("M", "M.NORM.LOWESS", "M.runmed.w5", "M.runmed.w7", "M.runmed.w9", "M.runmed.w11", etc)
	# pval		-> Le nom de la colonne contenant les valeurs des pvalues que l'on veut representer en couleur si elles sont significatives.
	#		   ("pval.rang.M", "pval.norm.M", etc)
	# seuil		-> Le seuil pour la p-value (Les p-values < seuil (i.e. significatives) seront representees en couleur sur les graphiques.
	# taille.pos	-> Si taille.pos = NULL, alors c'est la colonne LENGTH qui est prise en compte
	# val.chr	-> Correspond au nom de la colonne qui contient le nom du chromosomes correspondant a la sonde. De facon generale le nom de
	# 		   cette colonne est "SEQ_ID" mais pour les regions a  haute densiteil faut utiliser le nom : "GENE_EXPR_OPTION"
	# val.strand    -> Correspond au nom de la colonne qui contient le sens sur le brin.
	#
	# REMARQUE :	-> Si pval = NULL alors les donnees sont representees sans distinguer les valeurs significatives.
	#########################################################################################################################################
{
	nom	<- deparse(substitute(data.puce))
	if(is.null(pval) == FALSE && is.null(seuil) == TRUE) {seuil <- 0.05}

	# --- Creation d'un fichier ou mettre les donnees (si il n'existe pas) --- #
	safe.dir.create("Fichiers GFF")	
	if(is.null(pval) == TRUE)
		{
		nom.doss <- paste("Fichiers GFF/M=", data.M, sep="")
		nom 	 <- paste(nom, "_", data.M, sep="")
		methode	 <- ""
		}
	else			 
		{
		nom.doss <- paste("Fichiers GFF/M=", data.M, " - pval=", pval, " - alpha=" , seuil, sep="")
		nom 	 <- paste(nom, "_", data.M, "_", pval, "_" , seuil, sep="")
		methode	 <- paste(":method=p-value inf ", seuil, " en noir", sep="")
		}
	safe.dir.create(nom.doss)

	# --- Donnees pour les graphiques --- #
	x.M 	<- data.puce[[data.M]]
	x.RATIO <- 2**data.puce[[data.M]]
	if(is.null(taille.pos) == TRUE)	{pos <- data.puce$POSITION + data.puce$LENGTH}
	else 				{pos <- data.puce$POSITION + taille.pos}
	if(is.null(val.chr) == TRUE)	{chrom <- data.puce$CHROMOSOME}
	else 				{chrom <- val.chr}
	if(is.null(val.strand) == TRUE)	{strand <- data.puce$GENE_EXPR_OPTION}
	else 				{strand <- data.puce[[val.strand]]}

	scan <- "NimbleScan"

	if(is.null(pval) == FALSE)
		{
		couleur	<- ifelse(data.puce[[pval]] <= seuil,"color=000000","color=C0C0C0")
		pval <- data.puce[[pval]]
		}
	else
		{
		couleur	<- as.vector(rep("color=000000",length(x.M)))
		}
	
	# --- On cree une table contenant les donnees des log ratios de toutes les sondes avec en couleur les pics dont la p-value est significative (i.e. inferieure au seuil) si pval = TRUE --- #
	fic.gff.tt <- 	data.frame(chrom, scan, paste(nom, ":", strand, ":log-ratios", methode, sep=""), data.puce$POSITION, pos, x.M, ".", ".",
      			paste(couleur, ";", "seq_id=", data.puce$SEQ_ID, ";probe_id=", data.puce$PROBE_ID, ";count=1", sep=""))

	# --- Exportation des donnees --- #
	write.table(fic.gff.tt, file = paste(nom.doss, "/", nom, "_logratio.gff", sep=""), quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
	cat(paste("Creation du fichier suivant dans le dossier Fichiers GFF/ : \n --> ", 
	nom, "_log-ratio.gff qui contient les donnees des log ratios des sondes specifiques.\n \n",sep=""))

	# --- On cree une table contenant les donnees des ratios de toutes les sondes avec en couleur les pics dont la p-value est significative (i.e. inferieure au seuil) si pval = TRUE --- #
#	ratio.gff.tt <- data.frame(chrom, scan, paste(nom, ":", strand, ":ratios", methode, sep=""), data.puce$POSITION, pos, x.RATIO, ".", ".",
#      			paste(couleur, ";", "seq_id=", data.puce$SEQ_ID, ";probe_id=", data.puce$PROBE_ID, ";count=1", sep=""))

	# --- Exportation des donnees --- #
#	write.table(ratio.gff.tt, file = paste(nom.doss, "/", nom, "_ratio.gff", sep=""), quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
#	cat(paste("Creation du fichier suivant dans le dossier Fichiers GFF/ : \n --> ", 
#	nom, "_ratio.gff qui contient les donnees des ratios des sondes specifiques.\n \n",sep=""))

#	# --- On cree une table contenant les donnees des log ratios de toutes les sondes > 0 avec en couleur les pics dont la p-value est significative si pval = TRUE--- #
#	fic.gff.pos <-	data.frame(data.puce$SEQ_ID[x.M>0], scan, paste(nom, ":", data.puce$GENE_EXPR_OPTION[x.M>0], ":log-ratios positifs", methode, sep=""), data.puce$POSITION[x.M>0], 
#			pos[x.M>0], x.M[x.M>0], ".", ".", paste(couleur[x.M>0], ";", "seq_id=", data.puce$SEQ_ID[x.M>0], ";probe_id=", data.puce$PROBE_ID[x.M>0], ";count=1", sep=""))
#
#	# --- Exportation des donnees --- #
#	write.table(fic.gff.pos, file = paste(nom.doss, "/", nom, "_log_ratio_positif.gff", sep=""), quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
#	cat(paste("Creation du fichier suivant dans le dossier Fichiers GFF/ : \n --> ", 
#	nom, "_log_ratio_positif.gff qui contient les donnees des log ratios positifs des sondes specifiques.\n \n",sep=""))
#
#	# --- On cree une table contenant les donnees des ratios de toutes les sondes > 0 avec en couleur les pics dont la p-value est significative si pval = TRUE--- #
#	ratio.gff.pos <- data.frame(data.puce$SEQ_ID[x.RATIO>1], scan, paste(nom, ":", data.puce$GENE_EXPR_OPTION[x.RATIO>1], ":ratios > 1", methode, sep=""), data.puce$POSITION[x.RATIO>1], 
#			 pos[x.RATIO>1],x.RATIO[x.RATIO>1],".",".", paste(couleur[x.RATIO>1],";","seq_id=",data.puce$SEQ_ID[x.RATIO>1],";probe_id=",data.puce$PROBE_ID[x.RATIO>1],";count=1", sep=""))
#
#	# --- Exportation des donnees --- #
#	write.table(ratio.gff.pos, file = paste(nom.doss, "/", nom, "_ratio_positif.gff", sep=""), quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
#	cat(paste("Creation du fichier suivant dans le dossier Fichiers GFF/ : \n --> ", 
#	nom, "_ratio_positif.gff qui contient les donnees des ratios positifs des sondes specifiques.\n \n",sep=""))

	if(is.null(pval) == FALSE)
		{
		# --- On cree une table contenant les donnees des log ratios de toutes les sondes dont la p-value est significative (i.e. inferieure au seuil) --- #
		fic.gff.sign <-	data.frame(chrom[pval <= seuil], scan, paste(nom, ":", strand[pval <= seuil], ":log-ratios significatifs", methode, sep=""), 
				data.puce$POSITION[pval <= seuil], pos[pval <= seuil], x.M[pval <= seuil], ".", ".", paste(couleur[pval <= seuil], ";", "seq_id=", 
				data.puce$SEQ_ID[pval <= seuil], ";probe_id=", data.puce$PROBE_ID[pval <= seuil], ";count=1", sep=""))

		# --- Exportation des donnees --- #
		write.table(fic.gff.sign, file = paste(nom.doss, "/", nom, "_logratio_signif.gff", sep=""), quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
		cat(paste("Creation du fichier suivant dans le dossier Fichiers GFF/ : \n --> ", 
		nom, "_log-ratio_signif.gff qui contient les donnees des log ratios significatifs des sondes specifiques.\n \n",sep=""))

		# --- On cree une table contenant les donnees des ratios de toutes les sondes dont la p-value est significative (i.e. inferieure au seuil) --- #
#		ratio.gff.sign <- data.frame(chrom[pval <= seuil], scan, paste(nom, ":", strand[pval <= seuil], ":ratios significatifs", methode, sep=""), 
#				  data.puce$POSITION[pval <= seuil], pos[pval <= seuil], x.RATIO[pval <= seuil], ".", ".", paste(couleur[pval <= seuil], ";", "seq_id=", 
#				  data.puce$SEQ_ID[pval <= seuil], ";probe_id=", data.puce$PROBE_ID[pval <= seuil], ";count=1", sep=""))

		# --- Exportation des donnees --- #
#		write.table(ratio.gff.sign, file = paste(nom.doss, "/", nom, "_ratio_signif.gff", sep=""), quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
#		cat(paste("Creation du fichier suivant dans le dossier Fichiers GFF/ : \n --> ", 
#		nom, "_ratio_signif.gff qui contient les donnees des ratios significatifs des sondes specifiques.\n \n",sep=""))
		}
invisible()	
}

#***********************************************************************************************************************************************#
# FONCTION CALC.RUNMED <- Calcul de la mediane glissante sur une fenetre de largeur w a definir en parametre.
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Cette fonction permet de calculer la mediane glissante des log-ratios, dont le nom de la colonne est a definir en parametre.
# La mediane glissante se calcul par pas de 1, sur une fenetre de largeur impair et de taille a definir en parametre.
#################################################################################################################################################

calc.runmed <- function(data.puce, window = 5, data.M = "M.NORM", nom = "M.w") 

	#####################################################################################################
	# data.puce 	-> Les donnees de la puce
	# windows 	-> Taille de la fenetre glissante (en nombre d'element de x consecutive pour calculer la mediane
	# data.M	-> Nom de la colonne contenant les valeurs sur lesquelles calculer la mediane glissante
	# nom		-> Le prefixe qui servira d'identifiant de la colonne
	#####################################################################################################
{
	# --- On trie les donnees selon la position --- #
	data.puce 	<- data.puce[order(data.puce$CHROMOSOME,data.puce$POSITION),]

	# --- On calcule la mediane glissante --- ###
	M		<- data.puce[[data.M]]
	M.runmed 	<- runmed(M, window)[(floor(window/2) + 1):(length(M) - floor(window/2))]

	# --- On rajoute aux bords la valeur zero --- #
	M.runmed <- c(rep(0, floor(window/2)), M.runmed, rep(0, floor(window/2)))

	# --- Ajout des valeurs dans la table de donnees --- ###
	data.puce[[paste(nom, window, sep="")]]	<- M.runmed
return(data.puce)
}


#*****************************************************************************************************************************************************************************************#
# FONCTION CALC.RUNMED2 <- Calcul de la mediane glissante sur une fenetre de largeur w a definir en parametre en attribuant une valeur constante aux [w/2] premieres et dernieres valeurs
# Note : La fonction CALC.RUNMED (Cf. Fonctions_01_Lecture+Normalisation+unmed+Pval.R) attribue la valeur 0 aux [w/2] premieres et dernieres valeurs
#******************************************************************************************************************************************************************************************#

###############################################################################################################################################################
# Cette fonction permet de calculer la mediane glissante des log-ratios, dont le nom de la colonne est a definir en parametre.
# La mediane glissante se calcul par pas de 1, sur une fenetre de largeur impair et de taille a definir en parametre.
# Concernant les valeurs attribuees au [w/2] premieres et dernieres valeurs:  median(y[1:k2]) to the first values and analogously for the last ones is copied
# Dans la fonction runmed de R, ooption : endrule ="constant"
###############################################################################################################################################################

calc.runmed2 <- function(data.puce, window = 5, data.M = "M.NORM", nom = "M.w") 

	#####################################################################################################
	# data.puce 	-> Les donnees de la puce
	# window 	-> Taille de la fenetre glissante (en nombre d'element de x consecutive pour calculer la mediane
	# data.M	-> Nom de la colonne contenant les valeurs sur lesquelles calculer la mediane glissante
	# nom		-> Le prefixe qui servira d'identifiant de la colonne
	#####################################################################################################
{
	# --- Tri des donnees selon la position --- #
	data.puce 	<- data.puce[order(data.puce$CHROMOSOME,data.puce$POSITION),]
	
	# --- Preparation dune boucle sur les levels d'une colonne --- #
	chrom <- unique(levels(as.factor(data.puce$CHROMOSOME)))
	
	# Initialisation du vecteur resultat
	M.res <- vector()
	
	
	for (j in 1:length(chrom))
		{	
			# --- Recuperation la table temporaire correspondant a CHROMOSOME =chrom[j] --- #
			data.temp <- data.puce[(as.character(data.puce$CHROMOSOME) == chrom[j]),]
			
			# --- On calcule la mediane glissante --- ###
			M		<- data.temp[[data.M]]
			M.runmed 	<- runmed(M, window, endrule="constant")
			
			# On met a jour le vecteur M.res
			M.res <- c(M.res,M.runmed)
		}
	# --- Ajout des valeurs dans la table de donnees --- ###
	data.puce[[paste(nom, window, sep="")]]	<- M.res
	
	return(data.puce)
}


#***********************************************************************************************************************************************#
# FONCTION CALC.PVALUE -> Calcul les p-values avec deux methodes differentes
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Calcul des p-values selon 2 methodes differentes : 
# 	- La premiere methode est basee sur les rangs
#	- La seconde methode calcul les p-values par comparaison a  la distribution de Gauss (loi normale) selon different parametres mu et sigma
# La fonction calcule la densite de repartition des donnees ainsi que celle de loi normale.
# Pour la loi normale, on teste differents parametres pour mu et sigma.
#	-> mu = moyenne ,sigma = ecart-type
#	-> mu = mediane ,sigma = mad
#	-> mu = mode    ,sigma = mad
#	-> mu = mode    ,sigma = ecart-type estimesur les negatifs
#	-> mu = mode    ,sigma = ecart-type
#	-> mu = mode    ,sigma = mad (center = mode)
# On represente graphiquement ces densites ainsi que le mirroir des donnees par rapport au mode.
# On calcule 5 Z-score et les p-values associees avec les parametres suivants :
#	-> Z.moy.sdv		= (M - moyenne) / sdv
#	-> Z.med.mad		= (M - mediane) / mad.mediane
#	-> Z.mode.mad		= (M - mode) / mad.mediane
#	-> Z.mode.mad.mode	= (M - mode) / mad.mode
#	-> Z.mode.sdv.neg	= x.M / sdv.neg.mode
# Le calcul des p-values se fait en calculant 1-AUC (aire sous la courbe de la loi normale selectionnee) grace a  la fdr de la loi normale (fonction "pnorm")
#################################################################################################################################################

calc.pvalue <- function(data.puce, data.M = "M.NORM", window = NULL, method = c("rang", "moy.sdv", "med.mad", "mode.mad", "sdv.neg"), adjust = FALSE, plot = FALSE, format = "jpeg")

	#########################################################################################################################################
        # data.puce    	-> 	Le nom de la table de donnees.
	# data.M	->	Le nom de la comonne contenant les valeurs des log-ratios a  normaliser.
	# window	->	Preciser la taille de la fenetre utilisee si jamais les donnees des log-ratios sont des donnees lissees par mediane glissante.
	# method	->	Le nom de la methode utilisee pour calculer les p-values. A choisir parmi : "rang", "moy.sdv", "med.mad", "mode.mad" et "sdv.neg".
	#			Par defaut les p-values pour toutes les methodes sont calculees.
	# adjust	-> 	Si adjust = TRUE, les p-values sont ajustees par FDR.
	# plot		->	Si plot = TRUE, on a les graphiques des densites des donnees et des differentes Lois Normales en sortie.
	# format	-> 	Le format des graphiques en sortie.
	#########################################################################################################################################
{
       	nom	<- deparse(substitute(data.puce))

	# --- On recupere les valeurs des log-ratios normalises --- #
	if(is.null(window) == FALSE) 	{x.M <- data.puce[[data.M]][(floor(window/2)+1):(length(data.puce[[data.M]])-floor(window/2))]}
	else				{x.M <- data.puce[[data.M]]}

	if(is.element("rang",method) == TRUE && is.element("moy.sdv",method) == FALSE && is.element("med.mad",method) == FALSE && is.element("mode.mad",method) == FALSE && 
	   is.element("sdv.neg",method) == FALSE) {plot <- FALSE}

	if(plot == TRUE)
		{
		# --- Creation d'un fichier ou mettre les donnees --- #
		safe.dir.create("Graphiques - P-values")
		legend.1	<- vector()
		couleur.1	<- vector()
		pointil.1	<- vector()
		epais.1		<- vector()
		legend.2	<- "Densite des log-ratios"
		couleur.2	<- "grey50"
		pointil.2	<- 1
		epais.2		<- 2

		moyenne		<- mean(x.M)							# Calcul de la moyenne des log-ratios
		sdv		<- sd(x.M)							# Calcul de l'ecart type des log-ratio
		mediane		<- median(x.M)							# Calcul de la mediane des log-ratio
		mad.mediane 	<- mad(x.M)							# Calcul de la "median absolute deviation (mad)" par rapport a  la mediane
		dens.M 		<- density(x.M, na.rm=T)					# Calcul de la densite des log-ratios 
		mode		<- dens.M$x[which(dens.M$y == max(dens.M$y))]			# Calcul du mode des log-ratio (valeur la plus representee dans les donnees)
		mad.mode	<- mad(x.M,center=mode)						# Calcul de la "median absolute deviation (mad)" par rapport au mode
 		sdv.neg.mode	<- sqrt(sum((x.M[x.M<mode]-mode)**2)/length(x.M[x.M<mode])) 	# Estimation de l'ecart-type a  partir des valeurs inf au mode
		
		f.moy.sdv 	<- function(t) {dnorm(t,mean=moyenne,sd=sdv)}		# Loi Normale avec pour parametre la moyenne et l'ecart type
		f.med.mad 	<- function(t) {dnorm(t,mean=mediane,sd=mad.mediane)}	# Loi Normale avec pour parametre la mediane et la mad
		f.mode.mad.mode	<- function(t) {dnorm(t,mean=mode,sd=mad.mode)}		# Loi Normale avec pour parametre le mode et la mad au mode
		f.mode.sdv.neg 	<- function(t) {dnorm(t,mean=mode,sd=sdv.neg.mode)}	# Loi Normale avec pour parametre le mode et l'ecart type estimesur les negatifs

		# --- Representation graphique de la densite des log ratios et des differentes densites de loi normale --- #
		format.fichier(paste("Graphiques - P-values/Densite normaliteTOUT - ", nom, " - M=", data.M, sep = ""), type = format, width = 3000, height = 1500)
			# --- Densite de la repartition des log-ratios --- #
                	hist(x.M, xlab = "Log Ratio (R/V)", ylab = "Densite", probability = TRUE, breaks = 300, border = "grey90",
                	xaxt = "n", col = "grey90", main = paste("Densite des log ratios de la puce", nom, "\nComparaison avec la densite de la Loi Normale"))
                	axis(1, at = seq(from = -8, to = 8, by = 0.5), cex.axis = 0.8)

			# --- x = 0 (noir) --- #
               		abline(v = 0, lwd = 2, lty = 2, col = "black")
                	text(0, 0, "x = 0" ,adj = c(-0.5,3), cex = 1.2, font = 4, col = "black")

			# --- x = moyenne (orange) --- #
                	abline(v=moyenne, lwd=2, lty=2, col=rainbow(30)[4])
	               	text(moyenne, 0, "x = moyenne", adj=c(-0.5,4), cex=1.2, font=4, col=rainbow(30)[4])

			# --- x = mediane (rose) --- #
                	abline(v=mediane, lwd=2, lty=2, col=rainbow(30)[28])
               		text(mediane, 0, "x = mediane", adj=c(-0.5,3), cex=1.2, font=4, col=rainbow(30)[28])

			# --- x = mode (bleu) --- #
                	abline(v=mode, lwd=2, lty=2, col="blue")
                	text(mode, 0, "x = mode", adj=c(-0.5,2), cex=1.2, font=4, col="blue")

			# --- Courbes de densite--- #
                	lines(density(x.M, na.rm=T), col = "grey50", lwd = 2)	# Gris : Courbe de densite des donnees des log ratios de la puce

			# Calcul + ajout sur le graphique des courbes de normalite
			curve(f.moy.sdv , add=T, col=rainbow(30)[4],lwd=2)	# Orange : 	Courbe de densite de la Loi Normale (moyenne - variance)
			curve(f.med.mad , add=T, col=rainbow(30)[28],lwd=2)	# Rose : Courbe de densite de la Loi Normale (mediane - mad)
			curve(f.mode.mad.mode , add=T, col="blue",lwd=2)	# Bleu : Courbe de densite de la Loi Normale (mode - mad au mode)
			curve(f.mode.sdv.neg , add=T, col="green",lwd=2)	# Bleu : Courbe de densite de la Loi Normale (mode - ecart type estimesur les valeurs < mode

			legend(max(x.M, na.rm=TRUE), max(dens.M$y), legend = c(paste("moyenne =", round(moyenne,3)), paste("mediane =", round(mediane,3)), paste("mode =", round(mode,3)), 
			"Loi N(moyenne,ecart-type)","Loi N(mediane,mad)","Loi N(mode,mad)","Loi N(mode,sdv.neg)"), col = c(rainbow(30)[4], rainbow(30)[28], "blue", rainbow(30)[4], 
			rainbow(30)[28], "blue", "green"), lty=c(2,2,2,1,1,1,1), lwd=c(2,2,2,2,2,2,2), xjust=1, cex=1.5)
		dev.off(); 
		cat("Creation du graphique representant la densite des log ratios et toutes les densites de loi normale.\n")

		# --- Representation graphique de la densite des log ratios et des differentes densites de loi normale --- #
		format.fichier(paste("Graphiques - P-values/Densite normalite- ", nom, " - M=", data.M, sep = ""), type = format, width = 3000, height = 1500)
			# --- Densite de la repartition des log-ratios --- #
                	hist(x.M, xlab = "Log Ratio (R/V)", ylab = "Densite", probability = TRUE, breaks = 300, border = "grey90",
                	xaxt = "n", col = "grey90", main = paste("Densite des log ratios de la puce", nom, "\nComparaison avec la densite de la Loi Normale"))
                	axis(1, at = seq(from = -8, to = 8, by = 0.5), cex.axis = 0.8)

			# --- x = 0 (noir) --- #
               		abline(v = 0, lwd = 2, lty = 2, col = "black")
                	text(0, 0, "x = 0" ,adj = c(-0.5,3), cex = 1.2, font = 4, col = "black")

			# --- Courbes de densite--- #
                	lines(density(x.M, na.rm=T), col = "grey50", lwd = 2)	# Gris : Courbe de densite des donnees des log ratios de la puce
		}
	if(is.element("rang",method))
		{
		longueur <- length(x.M)
		val.r	 <- longueur - rank(x.M) + 1
		val.q	 <- (val.r - 0.5) / longueur

		if(is.null(window) == FALSE) 	{data.puce[[paste("p.rang.", data.M, sep="")]]<- c(rep(1, floor(window/2)), val.q, rep(1, floor(window/2)))}
		else				{data.puce[[paste("p.rang.", data.M, sep="")]]<- val.q}
		}
	if(is.element("moy.sdv",method))
		{
		# --- Representation des donnees avec une distribution normale de moyenne et ecart-type : mu et sigmaÂ² --- #
		# --- Calcul des statistiques utilisees comme parametres pour la loi normale --- #
		dens.M 		<- density(x.M, na.rm=T)	# Calcul de la densite des log-ratios
		moyenne		<- mean(x.M)			# Calcul de la moyenne des log-ratios
		sdv		<- sd(x.M)			# Calcul de l'ecart type des log-ratio

		# --- Calcul de la p-value --- ###
		# Version 1.0		(written by Alice Vigneron)
		#Z.moy.sdv	<- (x.M - moyenne) / sdv
		#pval.moy.sdv	<- ifelse(Z.moy.sdv >=0, 0.5*(1+Re(erf(Z.moy.sdv/sqrt(2)))), 1-0.5*(1+Re(erf(abs(Z.moy.sdv)/sqrt(2)))))
		#pval.moy.sdv	<- 1 - pval.moy.sdv
		
		# Version 2.0		(written by Aurore Puy - 2010, 28th, July)
		pval.moy.sdv	<- 1-pnorm(x.M, mean=moyenne,sd=sdv)
		pval.moy.sdv[pval.moy.sdv == 0]<- 1.110223e-16 # Pourquoi : The relative machine precision (eps) is taken to be 1.110223e-16	
		if(adjust == TRUE) 
			{
			pval.moy.sdv <- p.adjust(pval.moy.sdv, method = "fdr")
			if(is.null(window) == FALSE) 	{data.puce[[paste("p.moy.sdv.", data.M, ".adj", sep="")]]<- c(rep(1, floor(window/2)), pval.moy.sdv, rep(1, floor(window/2)))}
			else				{data.puce[[paste("p.moy.sdv.", data.M, ".adj", sep="")]]<- pval.moy.sdv}
			}
		else
			{
			if(is.null(window) == FALSE) 	{data.puce[[paste("p.moy.sdv.", data.M, sep="")]]<- c(rep(1, floor(window/2)), pval.moy.sdv, rep(1, floor(window/2)))}
			else				{data.puce[[paste("p.moy.sdv.", data.M, sep="")]]<- pval.moy.sdv}
			}
		if(plot == TRUE)
			{
			# Calcul + ajout sur le graphique de la courbe de normalite
			f.moy.sdv <- function(t) {dnorm(t,mean=moyenne,sd=sdv)}	# Loi Normale avec pour parametre la moyenne et l'ecart type
			curve(f.moy.sdv , add=T, col=rainbow(30)[4],lwd=2)	# Orange : 	Courbe de densite de la Loi Normale (moyenne - variance)

			# --- x = moyenne (orange) --- #
                	abline(v=moyenne, lwd=2, lty=2, col=rainbow(30)[4])
	               	text(moyenne, 0, "x = moyenne", adj=c(-0.5,4), cex=1.2, font=4, col=rainbow(30)[4])

			# --- Legende --- #
			legend.1[[length(legend.1)+1]]		<- paste("moyenne =",round(moyenne,3))
			couleur.1[[length(couleur.1)+1]]	<- rainbow(30)[4]
			pointil.1[[length(pointil.1)+1]]	<- 2
			epais.1[[length(epais.1)+1]]		<- 2
			legend.2[[length(legend.2)+1]]		<- "Loi N(moyenne,ecart-type)"
			couleur.2[[length(couleur.2)+1]]	<- rainbow(30)[4]
			pointil.2[[length(pointil.2)+1]]	<- 1
			epais.2[[length(epais.2)+1]]		<- 2
			}
		}
	if(is.element("med.mad",method))
		{
		# --- Representation des donnees avec une distribution normale de moyenne et ecart-type : mediane et deviation absolue a  la mediane --- #
		# --- Calcul des statistiques utilisees comme parametres pour la loi normale --- #
		dens.M 		<- density(x.M, na.rm=T)	# Calcul de la densite des log-ratios
		mediane		<- median(x.M)			# Calcul de la mediane des log-ratio
		mad.mediane 	<- mad(x.M)			# Calcul de la "median absolute deviation (mad)" par rapport a  la mediane

		# --- Calcul de la p-value --- ###
		# Version 1.0		(written by Alice Vigneron)
		#Z.med.mad	<- (x.M - mediane) / mad.mediane
		#pval.med.mad	<- ifelse(Z.med.mad >=0 , 0.5*(1+Re(erf(Z.med.mad/sqrt(2)))), 1-0.5*(1+Re(erf(abs(Z.med.mad)/sqrt(2)))))
		#pval.med.mad	<- 1 - pval.med.mad
		
		# Version 2.0		(written by Aurore Puy - 2010, 28th, July)
		pval.med.mad	<- 1-pnorm(x.M, mean=mediane,sd=mad.mediane)
		pval.med.mad[pval.med.mad == 0]<- 1.110223e-16 # Pourquoi : The relative machine precision (eps) is taken to be 1.110223e-16		
		if(adjust == TRUE) 
			{
			pval.med.mad <- p.adjust(pval.med.mad, method = "fdr")
			if(is.null(window) == FALSE) 	{data.puce[[paste("p.med.mad.", data.M, ".adj", sep="")]]<- c(rep(1, floor(window/2)), pval.med.mad, rep(1, floor(window/2)))}
			else				{data.puce[[paste("p.med.mad.", data.M, ".adj", sep="")]]<- pval.med.mad}
			}
		else
			{
			if(is.null(window) == FALSE) 	{data.puce[[paste("p.med.mad.", data.M, sep="")]]<- c(rep(1, floor(window/2)), pval.med.mad, rep(1, floor(window/2)))}
			else				{data.puce[[paste("p.med.mad.", data.M, sep="")]]<- pval.med.mad}
			}
		if(plot == TRUE)
			{
			# Calcul + ajout sur le graphique de la courbe de normalite
			f.med.mad <- function(t) {dnorm(t,mean=mediane,sd=mad.mediane)}	# Loi Normale avec pour parametre la mediane et la mad
			curve(f.med.mad , add=T, col=rainbow(30)[28],lwd=2)		# Rose : Courbe de densite de la Loi Normale (mediane - mad)

			# --- x = mediane (rose) --- #
                	abline(v=mediane, lwd=2, lty=2, col=rainbow(30)[28])
               		text(mediane, 0, "x = mediane", adj=c(-0.5,3), cex=1.2, font=4, col=rainbow(30)[28])

			# --- Legende --- #
			legend.1[[length(legend.1)+1]]		<- paste("mediane =",round(mediane,3))
			couleur.1[[length(couleur.1)+1]]	<- rainbow(30)[28]
			pointil.1[[length(pointil.1)+1]]	<- 2
			epais.1[[length(epais.1)+1]]		<- 2
			legend.2[[length(legend.2)+1]]		<- "Loi N(mediane,mad)"
			couleur.2[[length(couleur.2)+1]]	<- rainbow(30)[28]
			pointil.2[[length(pointil.2)+1]]	<- 1
			epais.2[[length(epais.2)+1]]		<- 2
			}
		}
	if(is.element("mode.mad",method))
		{
		# --- Representation des donnees avec une distribution normale de moyenne et ecart-type : mode et deviation absolue a  la mediane (par rapport au mode) --- #
		# --- Calcul des statistiques utilisees comme parametres pour la loi normale --- #
		dens.M 		<- density(x.M, na.rm=T)			# Calcul de la densite des log-ratios 
		mode		<- dens.M$x[which(dens.M$y == max(dens.M$y))]	# Calcul du mode des log-ratio (valeur la plus representee dans les donnees)
		mad.mode	<- mad(x.M,center=mode)				# Calcul de la "median absolute deviation (mad)" par rapport au mode
	
		# --- Calcul de la p-value --- ###
		# Version 1.0		(written by Alice Vigneron)
		#Z.mode.mad	<- (x.M - mode) / mad.mode
		#pval.mode.mad	<- ifelse(Z.mode.mad >=0 , 0.5*(1+Re(erf(Z.mode.mad/sqrt(2)))), 1-0.5*(1+Re(erf(abs(Z.mode.mad)/sqrt(2)))))
		#pval.mode.mad 	<- 1 - pval.mode.mad
		
		# Version 2.0		(written by Aurore Puy - 2010, 28th, July)
		pval.mode.mad	<- 1-pnorm(x.M, mean=mode,sd=mad.mode)
		pval.mode.mad[pval.mode.mad == 0]<- 1.110223e-16 # Pourquoi : The relative machine precision (eps) is taken to be 1.110223e-16	
		if(adjust == TRUE) 
			{
			pval.mode.mad <- p.adjust(pval.mode.mad, method = "fdr")
			if(is.null(window) == FALSE) 	{data.puce[[paste("p.mode.mad.", data.M, ".adj", sep="")]]<- c(rep(1, floor(window/2)), pval.mode.mad, rep(1, floor(window/2)))}
			else				{data.puce[[paste("p.mode.mad.", data.M, ".adj", sep="")]]<- pval.mode.mad}
			}
		else
			{
			if(is.null(window) == FALSE) 	{data.puce[[paste("p.mode.mad.", data.M, sep="")]]<- c(rep(1, floor(window/2)), pval.mode.mad, rep(1, floor(window/2)))}
			else				{data.puce[[paste("p.mode.mad.", data.M, sep="")]]<- pval.mode.mad}
			}
		if(plot == TRUE)
			{
			# Calcul + ajout sur le graphique de la courbe de normalitenormalite
			f.mode.mad.mode	<- function(t) {dnorm(t,mean=mode,sd=mad.mode)}	# Loi Normale avec pour parametre le mode et la mad au mode
			curve(f.mode.mad.mode , add=T, col="blue",lwd=2)		# Bleu : Courbe de densite de la Loi Normale (mode - mad au mode)

			# --- x = mode (bleu) --- #
                	abline(v=mode, lwd=2, lty=2, col="blue")
                	text(mode, 0, "x = mode", adj=c(-0.5,2), cex=1.2, font=4, col="blue")

			# --- Legende --- #
			if(is.element(paste("mode =",round(mode,3)), legend.1) == FALSE) {couleur.1[[length(couleur.1)+1]]	<- "blue"}
			if(is.element(paste("mode =",round(mode,3)), legend.1) == FALSE) {pointil.1[[length(pointil.1)+1]]	<- 2}
			if(is.element(paste("mode =",round(mode,3)), legend.1) == FALSE) {epais.1[[length(epais.1)+1]]		<- 2}
			if(is.element(paste("mode =",round(mode,3)), legend.1) == FALSE) {legend.1[[length(legend.1)+1]]	<- paste("mode =",round(mode,3))}
			legend.2[[length(legend.2)+1]]		<- "Loi N(mode,mad)"
			couleur.2[[length(couleur.2)+1]]	<- "blue"
			pointil.2[[length(pointil.2)+1]]	<- 1
			epais.2[[length(epais.2)+1]]		<- 2
			}
		}
	if(is.element("sdv.neg",method))
		{
		# --- Representation des donnees avec une distribution normale de moyenne et ecart-type : mode et sdv estimesur les valeurs negatives --- #
		# --- Calcul des statistiques utilisees comme parametres pour la loi normale --- #
		dens.M 		<- density(x.M, na.rm=T)			# Calcul de la densite des log-ratios
		mode		<- dens.M$x[which(dens.M$y == max(dens.M$y))]	# Calcul du mode des log-ratio (valeur la plus representee dans les donnees)
 		sdv.neg.mode	<- sqrt(sum((x.M[x.M<mode]-mode)**2)/length(x.M[x.M<mode])) 	# Estimation de l'ecart-type a  partir des valeurs sup au mode
		
		# --- Calcul de la p-value --- ###
		# Version 1.0		(written by Alice Vigneron)
		#Z.sdv.neg 	<- x.M / sdv.neg.mode
		#pval.sdv.neg	<- ifelse(Z.sdv.neg >=0 , 0.5*(1+Re(erf(Z.sdv.neg/sqrt(2)))), 1-0.5*(1+Re(erf(abs(Z.sdv.neg)/sqrt(2)))))
		#pval.sdv.neg	<- 1 - pval.sdv.neg
		
		# Version 2.0		(written by Aurore Puy - 2010, 28th, July)
		pval.sdv.neg	<- 1-pnorm(x.M, mean=mode,sd=sdv.neg.mode)
		pval.sdv.neg[pval.sdv.neg == 0]<- 1.110223e-16 # Pourquoi : The relative machine precision (eps) is taken to be 1.110223e-16	
		if(adjust == TRUE) 
			{
			pval.sdv.neg <- p.adjust(pval.sdv.neg, method = "fdr")
			if(is.null(window) == FALSE) 	{data.puce[[paste("p.sdv.neg.", data.M, ".adj", sep="")]]<- c(rep(1, floor(window/2)), pval.sdv.neg, rep(1, floor(window/2)))}
			else				{data.puce[[paste("p.sdv.neg.", data.M, ".adj", sep="")]]<- pval.sdv.neg}
			}
		else
			{
			if(is.null(window) == FALSE) 	{data.puce[[paste("p.sdv.neg.", data.M, sep="")]]<- c(rep(1, floor(window/2)), pval.sdv.neg, rep(1, floor(window/2)))}
			else				{data.puce[[paste("p.sdv.neg.", data.M, sep="")]]<- pval.sdv.neg}
			}
		if(plot == TRUE)
			{
			# Calcul + ajout sur le graphique de la courbe de normalitenormalite
			f.mode.sdv.neg 	<- function(t) {dnorm(t,mean=mode,sd=sdv.neg.mode)}	# Loi Normale avec pour parametre le mode et l'ecart type estimesur les negatifs
			curve(f.mode.sdv.neg , add=T, col="green",lwd=2)		# Bleu : Courbe de densite de la Loi Normale (mode - ecart type estimesur les valeurs < mode

			# --- x = mode (bleu) --- #
                	abline(v=mode, lwd=2, lty=2, col="blue")
                	text(mode, 0, "x = mode", adj=c(-0.5,2), cex=1.2, font=4, col="blue")

			# --- Legende --- #
			if(is.element(paste("mode =",round(mode,3)), legend.1) == FALSE) {couleur.1[[length(couleur.1)+1]]	<- "blue"}
			if(is.element(paste("mode =",round(mode,3)), legend.1) == FALSE) {pointil.1[[length(pointil.1)+1]]	<- 2}
			if(is.element(paste("mode =",round(mode,3)), legend.1) == FALSE) {epais.1[[length(epais.1)+1]]		<- 2}
			if(is.element(paste("mode =",round(mode,3)), legend.1) == FALSE) {legend.1[[length(legend.1)+1]]	<- paste("mode =",round(mode,3))}
			legend.2[[length(legend.2)+1]]		<- "Loi N(mode,sdv.neg)"
			couleur.2[[length(couleur.2)+1]]	<- "green"
			pointil.2[[length(pointil.2)+1]]	<- 1
			epais.2[[length(epais.2)+1]]		<- 2
			}
		}
	if(plot == TRUE) 
		{
		# --- Legende --- #
		legend(max(x.M, na.rm=TRUE), max(dens.M$y), legend=c(legend.1,legend.2), col=c(couleur.1,couleur.2), lty=c(pointil.1,pointil.2), lwd=c(epais.1,epais.2), xjust=1, cex=1.5)
		dev.off(); 
		cat("Creation du graphique representant la densite des log ratios les densites de loi normale.\n")
		}
return(data.puce)
}

#***********************************************************************************************************************************************#
# FONCTION ERF -> Fonction qui calcule l'integrale de la distribution de Gauss (en faisant appel a  la fonction Wofz programmees en Fortran)
#***********************************************************************************************************************************************#

#################################################################################################################################################
# ERF : 	Fonction d'erreur de Gauss, integrale de la distribution de Gauss.
# ERFC : 	Fonction d'erreur complementaire.
#################################################################################################################################################

erf <- function(z)

	#########################################################################################################################################
        # z 	-> 	argument de la fonction d'erreur => nombre complex
	# erfc(z) = 1 - erf(z)
	#	  = 2/âˆšÏ€ x integrale(z a  +âˆž) {exp(-t) dt}
	# Cette fonction fait appel a  la fonction wofz (algorithm TOMS 680), qui calcule la fonction de Faddeeva qui permet d'obtenir avec 
	# quelques modifications, la fonction erfc d'un complex.
	#########################################################################################################################################
{
	# --- A executer dans un terminal Linux pour partager la bibliotheque
	# --- R CMD SHLIB toms680.f
	# --- Pour charger le script Fortran dans R
	dyn.load(paste("toms680", .Platform$dynlib.ext, sep = ""))

	x 	<- as.double(Re(z)) 	
	y 	<- as.double(Im(z)) 	
	ansx	<- vector(length=length(z))
	ansy	<- vector(length=length(z))

	for (i in 1:length(z))
		{
		a 	<- x[[i]]
		b 	<- y[[i]]
		f 	<- b*b - a*a
		g 	<- 2 * a * b

		# Pour calculer w(ia-b) : On fait appel a  la fonction WOFZ chargee prealablement et ecrite en Fortran
		b 	<- -b
		result 	<- .Fortran("WOFZ", x=b, x=a, ansx=b, ansy=a, error = as.integer(0))
		d	<- result$ansx
		e	<- result$ansy

		outa 	<- exp(f) * (d * cos(g) + e * sin(g))
		outb	<- exp(f) * (-d * sin(g) + e * cos(g))
		ansx[[i]] <-outa
		ansy[[i]] <-outb
		}
	erfc <- complex(real=ansx, im=ansy)
	return(1-erfc)
}

#***********************************************************************************************************************************************#
# FONCTION MA.PLOT -> On represente graphiquement le MA-plot des donnees avec les sondes dont la p-value est significative en couleur
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Representation du MA-plot des donnees normalisees avec les sondes dont la p-value est significative au seuil alpha en couleur.
#################################################################################################################################################

MA.plot <- function(data.puce, data.M = "M.NORM", data.A = "A.NORM", pval = "pval.rang.M", seuil = 0.05, format="jpeg")

	#########################################################################################################################################
        # data.puce    	-> Les donnees de la puce avec les p-values.
	# data.M 	-> Le nom de la colonne contenant les valeurs de M a  representer graphiquement.
	#		   ("M.runmed.w5", "M.runmed.w7", "M.runmed.w9", "M.runmed.w11", etc) 
	# pval	 	-> Le nom de la colonne contenant les valeurs des p-values a  representer.
	#		   ("pval.rang.M", "pval.med.mad.M", "pval.mode.mad.M", "pval.sdv.neg.M", "pval.rang.M.w5", "pval.med.mad.M.w5", 
	#		    "pval.mode.mad.M.w5", "pval.sdv.neg.M.w5", "pval.rang.M.w7", "pval.med.mad.M.w7", "pval.mode.mad.M.w7", 
	#		    "pval.sdv.neg.M.w7", "pval.rang.M.w9", "pval.med.mad.M.w9", "pval.mode.mad.M.w9", "pval.sdv.neg.M.w9", "pval.rang.M.w11", 
	#		    "pval.med.mad.M.w11", "pval.mode.mad.M.w11", "pval.sdv.neg.M.w11", "pval.rang.M.adj", "pval.med.mad.M.adj", 
	#		    "pval.mode.mad.M.adj", "pval.sdv.neg.M.adj", "pval.rang.M.w5.adj", "pval.med.mad.M.w5.adj", "pval.mode.mad.M.w5.adj", 
	#		    "pval.sdv.neg.M.w5.adj", "pval.rang.M.w7.adj", "pval.med.mad.M.w7.adj", "pval.mode.mad.M.w7.adj", "pval.sdv.neg.M.w7.adj", 
	#		    "pval.rang.M.w9.adj", "pval.med.mad.M.w9.adj", "pval.mode.mad.M.w9.adj", "pval.sdv.neg.M.w9.adj", "pval.rang.M.w11.adj", 
	#		    "pval.med.mad.M.w11.adj", "pval.mode.mad.M.w11.adj", "pval.sdv.neg.M.w11.adj",  
	# seuil 	-> Le seuil pour alpha (par defaut 0.05).
	# format	-> Format des graphiques en sortie.
	#########################################################################################################################################
{
	# --- Creation d'un fichier ou mettre les donnees (si il n'existe pas) --- #
        nom	 <- deparse(substitute(data.puce))
	nom.doss <- ("Graphiques - MA-Plot")
	safe.dir.create(nom.doss)

	M		<- data.puce[[data.M]]
	A		<- data.puce[[data.A]]
	pvalues		<- data.puce[[pval]]
	couleur 	<- sample(rainbow(30),1)
	col.MA		<- ifelse(pvalues <= seuil, rgb(t(col2rgb(couleur))/255, alpha = 0.5), rgb(red = 0.4, green = 0.4, blue = 0.4, alpha = 0.5)); 
	col.legend 	<- couleur
	nb.sign		<- length(pvalues[pvalues <= seuil])
	nb.tot  	<- length(pvalues)

	if(data.M == "M" || data.M == "M.NORM")
		{
		xlim = c(6,16)
		ylim = c(-6,6)
		}
	else
		{
		xlim = c(6,16)
		ylim = c(-3,3)
		}
	
       	# --- MA-Plot des donnees  --- #
	format.fichier(paste(nom.doss, "/", nom, " - MA-Plot + p-values - M=", data.M, " - pval=", pval, " - alpha=", seuil, sep=""), type=format, width=2500, height=2000)
        par(mfrow=c(1,1))
                # --- Apres normalisation + p-values en couleur--- #
                plot(A, M, type = "n", xlim = xlim, ylim = ylim, xlab = "A", ylab = data.M, 
		main=paste("MA-Plot de la puce", nom, "\n avec en couleur les sondes dont la p-value est significative i.e. <=", seuil))
		points(A, M, pch = 20, cex = 0.3, col = col.MA)
                abline(h = 0,col = rainbow(9)[9], lwd = 2, lty = 2)
                lines(lowess(A, M),col = rainbow(30)[17], lwd = 2)
		legend(max(ylim),max(xlim),legend=c(paste("nb inf",seuil,"=",nb.sign),paste("nb tot =",nb.tot)),fill=c(col.legend,rgb(red = 0.4, green = 0.4, blue = 0.4, alpha = 1)),xjust=0,cex=1.5)
        dev.off()
        cat("MA-Plot de la puce",nom, "avec en couleur les sondes dont la p-value est inferieure a ",seuil,"\n")
}

#***********************************************************************************************************************************************#
# BETWEEN ARRAY NORMALIZATION -> Essais de normalisation inter-puce
#***********************************************************************************************************************************************#
#################################################################################################################################################
# Fonction issue du package limma de bioconductor
#################################################################################################################################################

normalizeBetweenArrays <- function(object, method="Aquantile", targets=NULL, ...) 
	{
	choices <- c("none","scale","quantile","Aquantile","Gquantile","Rquantile","Tquantile")
	method  <- match.arg(method,choices)
	switch(method,scale = {
			object$M <- normalizeMedianAbsValues(object$M)
			object$A <- normalizeMedianAbsValues(object$A)
		},
		quantile = {
			narrays  <- NCOL(object$M)
			Z 	 <- normalizeQuantiles(cbind(object$A-object$M/2,object$A+object$M/2),...)
			G 	 <- Z[,1:narrays]
			R 	 <- Z[,narrays+(1:narrays)]
			object$M <- R-G
			object$A <- (R+G)/2
		},
		Aquantile = {
			object$A <- normalizeQuantiles(object$A,...)
		},
		Gquantile = {
			G 	 <- object$A-object$M/2
			E 	 <- normalizeQuantiles(G,...) - G
			object$A <- object$A + E
		},
		Rquantile = {
			R 	 <- object$A+object$M/2
			E 	 <- normalizeQuantiles(R,...) - R
			object$A <- object$A + E
		},
		Tquantile = {
			narrays <- NCOL(object$M)
			if(NCOL(targets)>2) targets <- targets[,c("Cy3","Cy5")]
			targets <- as.vector(targets)
			Z <- cbind(object$A-object$M/2,object$A+object$M/2)
			for (u in unique(targets)) {
				j <- targets==u
				Z[,j] <- normalizeQuantiles(Z[,j],...)
			}
			G <- Z[,1:narrays]
			R <- Z[,narrays+(1:narrays)]
			object$M <- R-G
			object$A <- (R+G)/2
		})
	object
}

normalizeQuantiles <- function(A, ties=TRUE) {
#	Normalize columns of a matrix to have the same quantiles, allowing for missing values.
#	Gordon Smyth
#	25 June 2002.  Last revised 8 June 2006.

	n <- dim(A)
	if(is.null(n)) return(A)
	if(n[2]==1) return(A)
	O <- S <- array(,n)
	nobs <- rep(n[1],n[2])
	i <- (0:(n[1]-1))/(n[1]-1)
	for (j in 1:n[2]) {
		Si <- sort(A[,j], method="quick", index.return=TRUE)
		nobsj <- length(Si$x)
		if(nobsj < n[1]) {
			nobs[j] <- nobsj
			isna <- is.na(A[,j])
			S[,j] <- approx((0:(nobsj-1))/(nobsj-1), Si$x, i, ties="ordered")$y
			O[!isna,j] <- ((1:n[1])[!isna])[Si$ix]
		} else {
			S[,j] <- Si$x
			O[,j] <- Si$ix
		}
	}
	m <- rowMeans(S)
	for (j in 1:n[2]) {
		if(ties) r <- rank(A[,j])
		if(nobs[j] < n[1]) {
			isna <- is.na(A[,j])
			if(ties)
				A[!isna,j] <- approx(i, m, (r[!isna]-1)/(nobs[j]-1), ties="ordered")$y
			else
				A[O[!isna,j],j] <- approx(i, m, (0:(nobs[j]-1))/(nobs[j]-1), ties="ordered")$y
		} else {
			if(ties)
				A[,j] <- approx(i, m, (r-1)/(n[1]-1), ties="ordered")$y
			else
				A[O[,j],j] <- m
		}
	}
	A
}

normalizeMedianAbsValues <- function(x) 
	{
	#	Normalize columns of a matrix to have the same median absolute value
	narrays <- NCOL(x)
	if(narrays==1) return(x)
	cmed <- log(apply(abs(x), 2, median, na.rm=TRUE))
	cmed <- exp(cmed - mean(cmed))
	t(t(x)/cmed)
	}

#***********************************************************************************************************************************************#
# PLOT.COR -> Nuage de point representant la correlation entre 2 vecteurs
#***********************************************************************************************************************************************#
#################################################################################################################################################
# Permet de representer la correlation entre les donnees des log-ratios de deux puces
#################################################################################################################################################

plot.cor <- function(vec1, vec2, format = "jpeg")

	#########################################################################################################################################
        # vec1   -> Premier vecteur de donnees.
        # vec2   -> Second vecteur de donnees.
	# format -> Format des graphiques en sortie.
	#########################################################################################################################################
	{
	nom1 <- deparse(substitute(vec1))
	nom2 <- deparse(substitute(vec2))

	nom.doss <- "Graphiques - Correlation"
	safe.dir.create(nom.doss)

	format.fichier(paste(nom.doss, "/Nuage de points - ", nom1, " vs ", nom2, sep=""), type=format, width=1500, height=1500)
                plot(vec1, vec2, type="n", xlim=c(-4,4), ylim=c(-4,4), xlab=nom1,ylab=nom2, main=paste(nom1, "vs", nom2))
    		points(vec1, vec2, pch=20, cex=0.3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.25))
                abline(a=0,b=1,col=rainbow(9)[9],lwd=2)
		legend(6,16,legend=paste("nb sondes =",length(vec1)),fill="black",xjust=0,cex=1.5)
        dev.off()
	}

#********************************************************************************************************************************************************************************#
# FONCTION MERGE.EXP.CONTROL.NEG  <-  Merge de 2 tables (par PROBE_ID)
#********************************************************************************************************************************************************************************#

#################################################################################################################################################
# A partir de 2 jeux de donnees (un control negatif et une autre experience) avec un meme design de microarray (c.a.d les memes PROBE_ID),
# cette fonction permet de merger les 2 tables (par PROBE_ID).
#################################################################################################################################################

merge.exp.control.neg <- function(data.puce.neg, data.puce.exp, nom) 
{
	#####################################################################################################
	# data.puce.neg 	-> Les donnees de la puce du control negatif
	# data.puce.exp		-> Les donnees de la puce de l'autre experience
	# nom 				-> Nom de la puce de l'autre experience
	#####################################################################################################
	
	# --- Selection des colonnes --- #
	data.puce.neg.temp <- data.puce.neg[,c("PROBE_ID","CHROMOSOME","POSITION","LENGTH","A.NORM","M.NORM")]
	data.puce.exp.temp <- data.puce.exp[,c("PROBE_ID","CHROMOSOME","POSITION","LENGTH","A.NORM","M.NORM")]
	
	# --- Tri des tables --- #
	data.puce.neg.temp <- data.puce.neg.temp[order(data.puce.neg.temp$PROBE_ID,data.puce.neg.temp$CHROMOSOME,data.puce.neg.temp$POSITION),]
	data.puce.exp.temp <- data.puce.exp.temp[order(data.puce.exp.temp$PROBE_ID,data.puce.exp.temp$CHROMOSOME,data.puce.exp.temp$POSITION),]
	
	# --- Merge des 2 tables --- #
	data.res <- merge(data.puce.neg.temp,data.puce.exp.temp, by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"), by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
				suffixes=c(".neg",paste(".", nom, sep="")))

	return(data.res)
}	


#********************************************************************************************************************************************************************************#
# FONCTION SUBSTRACT.CONTROL.NEG  <-  Soustraction du control negatif dans le cas où data.M de l'experience (data.M.exp) et data.M du control negatif (data.M.neg) sont positives.
#********************************************************************************************************************************************************************************#

#################################################################################################################################################
# A partir de 2 jeux de donnees (un control negatif et une autre experience) avec un meme design de microarray (c.a.d les memes PROBE_ID),
# cette fonction permet de merger les 2 tables (par PROBE_ID) et de soustraire la valeur data.M du control negatif (data.M.neg)
# uniquement dans le cas où data.M de l'experience (data.M.exp) et data.M du control negatif (data.M.neg) sont positives.
#################################################################################################################################################

substract.control.neg <- function(data.puce.neg, data.puce.exp, nom, data.M, nom.M.out) 
{
	#####################################################################################################
	# data.puce.neg 	-> Les donnees de la puce du control negatif
	# data.puce.exp		-> Les donnees de la puce de l'autre experience
	# nom 				-> Nom de la puce de l'autre experience
	# data.M			-> Nom de la colonne contenant les valeurs a soustraire (ex: M.NORM.w5)
	# nom.M.out			-> Nom de la colonne contenant le resultat de la soustraction (ex: M.NORM.neg.w5)
	#####################################################################################################
	
	# --- Selection des colonnes --- #
	data.puce.neg.temp <- data.puce.neg[,c("PROBE_ID","CHROMOSOME","POSITION","LENGTH","A.NORM","M.NORM")]
	data.puce.exp.temp <- data.puce.exp[,c("PROBE_ID","CHROMOSOME","POSITION","LENGTH","A.NORM","M.NORM")]
	
	# --- Tri des tables --- #
	data.puce.neg.temp <- data.puce.neg.temp[order(data.puce.neg.temp$PROBE_ID,data.puce.neg.temp$CHROMOSOME,data.puce.neg.temp$POSITION),]
	data.puce.exp.temp <- data.puce.exp.temp[order(data.puce.exp.temp$PROBE_ID,data.puce.exp.temp$CHROMOSOME,data.puce.exp.temp$POSITION),]
	
	# --- Merge des 2 tables --- #
	data.res <- merge(data.puce.neg.temp,data.puce.exp.temp, by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"), by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
				suffixes=c(".neg",paste(".", nom, sep="")))
				
	# --- Soustraction de la valeur data.M du control negatif uniquement dans le cas où data.M de l'experience et data.M du control negatif sont positives --- #

	data.res[[nom.M.out]] <-0 
	nom.M.exp <- paste(data.M,".",nom,sep="")
	nom.M.neg <- paste(data.M,".neg",sep="")
	
	for (i in 1:nrow(data.res))
		{ 		
		if((data.res[[nom.M.exp]][i]>0) & (data.res[[nom.M.neg]][i]>0)) 
			{data.res[[nom.M.out]][i] <- data.res[[nom.M.exp]][i] - data.res[[nom.M.neg]][i] }
		else
			{data.res[[nom.M.out]][i] <- data.res[[nom.M.exp]][i]}
		}		
		
	return(data.res)
}	



#***********************************************************************************************************************************************#
# FONCTION plot.cor.replicats -> Trace les nuages de points d'un signal entre 2 ou 3 replicats de puces
#***********************************************************************************************************************************************#

plot.cor.replicats <- function(replicat1,replicat2,replicat3=NULL,signal="M.NORM",titre="Nuage de points du signal entre replicats",width=600,height=600)

	#################################################################################################################################################
	# replicat1 : donnes de puces du 1er replicat (data.frame)
	# replicat2 : donnes de puces du 2e replicat (data.frame)
	# replicat3 : donnes de puces du 3e replicat (data.frame)
	# signal : nom de la colonne des tables replicat{1/2/3} utilisee pour tracer le nuage de point
	# titre : nom du fichier de sortie (format jpeg)
	# width : largeur de la fenetre de dessin
	# height : hauteur de la fenetre de dessin
	#################################################################################################################################################

{	
	M.rep1<-replicat1[[signal]]
	M.rep2<-replicat2[[signal]]
	nom.M.rep1<-deparse(substitute(replicat1))
	nom.M.rep2<-deparse(substitute(replicat2))
	
	format <- "jpeg"
	safe.dir.create("Graphiques - Correlation")
	format.fichier(paste("Graphiques - Correlation/",titre,sep=""), type=format, width=width, height=height, pointsize=16)
	
	if(!is.null(replicat3)){
		M.rep3<-replicat3[[signal]]
		nom.M.rep3<-deparse(substitute(replicat3))
		par(mfrow = c(2,2))
	}
		
	plot(M.rep1, M.rep2, type="n", xlab=nom.M.rep1, ylab=nom.M.rep2, xlim=c(-6,6), ylim=c(-6,6), 
		main=paste(nom.M.rep1,"versus",nom.M.rep2))
	points(M.rep1, M.rep2, pch=20)
	abline(a=0, b=1, col=rainbow(9)[9], lwd=2, lty=2)
	r2<- cor(M.rep1,M.rep2)
	legend("topleft", legend=c(paste("R2(square) = ",round(r2,digits=2))), xjust=0, cex=1.2)
	
	if(!is.null(replicat3)){
	
		plot(M.rep1, M.rep3, type="n", xlab=nom.M.rep1, ylab=nom.M.rep3, xlim=c(-6,6), ylim=c(-6,6), 
		main=paste(nom.M.rep1,"versus",nom.M.rep3))
		points(M.rep1, M.rep3, pch=20)
		abline(a=0, b=1, col=rainbow(9)[9], lwd=2, lty=2)
		r2<- cor(M.rep1,M.rep3)
		legend("topleft", legend=c(paste("R2 = ",round(r2,digits=2))), xjust=0, cex=1.2)
		
		plot(M.rep2, M.rep3, type="n", xlab=nom.M.rep2, ylab=nom.M.rep3, xlim=c(-6,6), ylim=c(-6,6), 
		main=paste(nom.M.rep2,"versus",nom.M.rep3))
		points(M.rep2, M.rep3, pch=20)
		abline(a=0, b=1, col=rainbow(9)[9], lwd=2, lty=2)
		r2<- cor(M.rep2,M.rep3)
		legend("topleft", legend=c(paste("R = ",round(r2,digits=2))), xjust=0, cex=1.2)
	}
	dev.off()
}

	
