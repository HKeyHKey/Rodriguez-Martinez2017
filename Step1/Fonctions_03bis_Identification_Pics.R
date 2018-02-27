#################################################################################################################################################
# FONCTIONS CONTENUES DANS LE SCRIPT
#################################################################################################################################################
# format.fichier 		<- function(titre, type=c("", "ps", "pdf", "png", "jpeg", "bmp"), width=NULL, height=NULL, bg = "white",...)
# safe.dir.create 		<- function(path)
# ident.sommet.oris.chrom 		<- function(data.regions, data.puce, data.M = "M.NORM")
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-#
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-#


#***********************************************************************************************************************************************#
# FONCTION FORMAT.FICHIER -> Pour choisir le format des graphiques en sortie
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Cette fonction permet de choisir le format des graphiques en sortie. Il y a le choix parmi 5 formats differents : 
# postscript ("ps"), PDF ("pdf"), PNG ("png"), JPEG ("jpeg") ou bitmap ("bmp").
#################################################################################################################################################

format.fichier <- function(titre, type=c("", "ps", "pdf", "png", "jpeg", "bmp"), width=NULL, height=NULL, bg = "white",...)
	
	#########################################################################################################################################
	# titre 	-> 	Le nom que l'on veut donner au fichier graphique en sortie. Le nom doit  aªtre entre guillemets et il n'y a pas 
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
# o a¹ le parametre "name" correspond au nom que l'on veut donner au dossier que l'on veut creer. Cette fonction est issue de la fonction R cran
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


#***************************************************************************************************************************************************#
# FONCTION IDENT.SOMMET.ORIS.CHROM 
#****************************************************************************************************************************************************#

#################################################################################################################################################
# Pour chaque region de chaque chromosome(du fichier data.regions), cette fonction renvoie le fichier data.regions avec une colonne supplémentaire représentant
# la position où se trouve le pic (valeur max du signal "M.NORM" ou "M.comb" ou "M.comb.eBayes").
#################################################################################################################################################

ident.sommet.oris.chrom <- function(data.regions, data.puce, data.M = "M.NORM")
	
	#########################################################################################################################################
	# data.regions <- fichier des regions où on cherche à détecter la position du pic (format: table à 3 colonnes CHROMOSOME START END)
	# data.puce <- fichier de données  contenant la colonne di signal nommée "data.M"
	# data.M <- nom de la colonne contenant la valeur du signal (Attention: colonne présente dans la table data.puce) 
	#########################################################################################################################################
{
	data.regions <- data.regions[order(data.regions$CHROMOSOME, data.regions$START, data.regions$END),]
		
	# Préparation dune boucle sur les levels d'une colonne 
	chrom <- unique(as.character(data.regions$CHROMOSOME))
	
	### --- On initialise une table vide --- ####
	res.data.regions <- data.frame(0,0,0,0,0,0,0,0,0,0,0) [-1,] # Le -1 pour pas avoir une première colonne quavec des 0
	
	for (j in 1:length(chrom))
	{
		# Récupération des tables temporaires correspondant à CHROMOSOME =chrom[j]
		data.puce.temp <- data.puce[(as.character(data.puce$CHROMOSOME) == chrom[j]),]
		data.regions.temp <- data.regions[(as.character(data.regions$CHROMOSOME) == chrom[j]),]
				
		vec.res      <- vector(length = dim(data.regions.temp)[[1]])
		
		for(i in 1:dim(data.regions.temp)[[1]])
			{
			start <- data.regions.temp$START[[i]]
			end   <- data.regions.temp$END[[i]]
			
			data.temp    <- data.puce.temp[data.puce.temp$POSITION >= start & data.puce.temp$POSITION <= end,]
			val.max      <- max(data.temp[[data.M]])
			vec.res.temp <- data.temp$POSITION[data.temp[[data.M]] == val.max]
			
			if(length(vec.res.temp) == 1) 
				{
				vec.res[[i]] <- vec.res.temp
				}
			else
				{
				vec.res[[i]] <- round(mean(vec.res.temp),0)
				}
			}
			
			data.regions.temp$ORIS <- paste( chrom[j], ".ORIS-", vec.res, sep="")
			data.regions.temp$SOMMET <- vec.res
			
			# On met à jour la table data.regions
			res.data.regions <- rbind.data.frame(res.data.regions,data.regions.temp)
			
	}
# On réordonne les colonnes
res.data.regions <- res.data.regions[,c("CHROMOSOME","START","END","LENGTH","NB.SIGNIF","NB.MAX.SIGNIF.CONS","LOG.NEG.PVAL.COMB","MEAN.M","MEDIAN.M","ORIS","SOMMET")]
return(res.data.regions)
}
