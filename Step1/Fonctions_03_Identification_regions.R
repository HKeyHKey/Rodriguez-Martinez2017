#################################################################################################################################################
# FONCTIONS CONTENUES DANS LE SCRIPT
#################################################################################################################################################

# format.fichier 		<- function(titre, type=c("", "ps", "pdf", "png", "jpeg", "bmp"), width=NULL, height=NULL, bg = "white",...)
# safe.dir.create 		<- function(path)
# flag.oris 			<- function(data.puce, data.oris, nom.flag)
# select.oris  			<- function(data.puce, data.M, data.pval, nb.hits.signif, taille.min.region = NULL, dist.min.region = NULL, val.M.min = 0, pval.seuil = 0.05, 
#				   consecutif = FALSE, taille.repeat = NULL)
# select.oris.detect.neg  			<- function(data.puce, data.M, data.pval, nb.hits.signif, taille.min.region = NULL, dist.min.region = NULL, val.M.min = 0, pval.seuil = 0.05, 
#				   consecutif = FALSE, taille.repeat = NULL)
# flag.position 		<- function(data.position, data.repeat, nom.flag = "flag")
# export.genome.oris.gff 	<- function(data.puce, data.M, nom.flag, taille.pos = NULL, val.chr = NULL, val.strand = NULL)
# export.oris.gff 		<- function(data.oris, chrom = "")
# taille.dist.chrom 		<- function(data.oris)
# plot.region			<- function(data.oris, data.col, format)
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


#***********************************************************************************************************************************************#
# FONCTION FLAG.ORIS -> Identification sur le genome par un flag  a  1, des regions selectionnees par l'application du Modele
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Cette fonction identifie par "1" les positions appartenant aux regions selectionnees apres application du modele et par "0" le reste.
#################################################################################################################################################

flag.oris <- function(data.puce, data.oris, nom.flag)

	#########################################################################################################################################
        # data.puce <- La table de donnees contenant les donnees de la puce
	# data.oris <- La table de donnees contenant les regions selectionnees par le modele
	# nom.flag  <- Le nom qui va etre donne  a la colonne contenant les flag
	#########################################################################################################################################
{
  
  # gerer le cas ou il n'y a aucune ori
  if ( is.null(data.oris$CHROMOSOME) == TRUE )
    {
      cat(paste("pas d'ori pour",  deparse(substitute(data.puce)), "\n", sep=" "))
      return(data.puce)
    }
  else
    {
        # ********** On tri les donnees et on parcours les 2 tables simultanement ********** #
      data.puce <- data.puce[order(data.puce$CHROMOSOME,data.puce$POSITION),]
      data.oris 	<- data.oris[order(data.oris$CHROMOSOME,data.oris$START, data.oris$END),]
      
 	# Préparation dune boucle sur les levels d'une colonne 
      chrom <- unique(as.character(data.puce$CHROMOSOME))
	
      ### --- On initialise un vecteur vide --- ####
      res.vector <- vector()
      
      for (j in 1:length(chrom))
        {
         # Récupération la table temporaire correspondant à CHROMOSOME =chrom[j]

          if (is.null(data.oris[(as.character(data.oris$CHROMOSOME) == chrom[j]),]) == FALSE )
            {
              data.temp.puce <- data.puce[(as.character(data.puce$CHROMOSOME) == chrom[j]),]
              data.temp.oris <- data.oris[(as.character(data.oris$CHROMOSOME) == chrom[j]),]
              
              print(nrow(data.temp.puce))
              print(nrow(data.temp.oris))
              
              position	<- data.temp.puce$POSITION
              flag.oris	<- vector(mode = "integer", length = length(data.temp.puce$POSITION))
              num.pos	<-1

              # permettre de gerer le cas où il y n'y a pas d'ori pour un chromosome
              if ( dim(data.temp.oris)[[1]] >= 1 )
                {
                  for(i in 1:dim(data.temp.oris)[[1]])
                    {
                      start 	<- data.temp.oris$START[[i]]
                      end   	<- data.temp.oris$END[[i]]
                      bool	<- FALSE
                      
                      while(num.pos <= length(position) && bool == FALSE) 
                        {
                          if(position[[num.pos]] < start) {num.pos <- num.pos + 1}
                          if(position[[num.pos]] > end) 	{bool <- TRUE}
                          if(position[[num.pos]] >= start && position[[num.pos]] <= end) 
                            {
                              flag.oris[[num.pos]] <- 1
                              num.pos 	     <- num.pos + 1
                            }
                        }
                    }
                }
              res.vector <- c(res.vector,flag.oris)
            }
        }
      data.puce[[nom.flag]] <- res.vector
      
      return(data.puce)
    }
}


#***********************************************************************************************************************************************#
# FONCTION SELECT.ORIS -> Recuperation des regions significatives positives a partir des donnees du genome
#***********************************************************************************************************************************************#

select.oris  <- function(data.puce, data.M, data.pval, nb.hits.signif, taille.min.region = NULL, dist.min.region = NULL, val.M.min = 0, pval.seuil = 0.05, consecutif = FALSE, taille.repeat = NULL)

	#########################################################################################################################################
	# data.puce		<- La table de donnees contenant les donnees de la puce
	# data.M	        	<- Le nom de la colonne dans la table 'data.puce' contenant les valeurs de M qui nous interessent
	# data.pval		<- Le nom de la colonne dans la table 'data.puce' contenant les valeurs des p-values qui nous interessent
	# nb.hits.signif 	<- Le nombre de sondes significatives que doit contenir la region pour  etre selectionnees
	# taille.min.region	<- La taille minimale que doit faire la region pour  etre consideree comme une ORIs (non artefactuelle)
	# dist.min.region	<- Si la distance entre deux regions positives/significatives est inferieure  a 'dist.min.region' alors les deux
	#			            regions sont agregees en une seule et meme region (i.e. meme origine de replication)
	# val.M.min		<- La valeur minimale de M pour laquelle on considere qu'on rentre dans une region potentiellement significative
	# pval.seuil		<- Le seuil de significativite pour la p-value
	# consecutif		<- Les sondes selectionnees par le parametre 'nb.hits.signif' doivent  etre consecutive si consecutif = TRUE
	# taille.repeat		<- Si la distance entre 2 sondes est superieure  a  'taille.repeat', alors cette region est consideree comme une region 
	#			            repeat du genome et les 2 sondes sont considerees comme trop eloignees pour appartenir  a la meme region.
	#########################################################################################################################################
{	
        data.puce  <- data.puce[order(data.puce$CHROMOSOME,data.puce$POSITION),]
	
	# Préparation d'une boucle sur les levels d'une colonne 
	chrom      <- unique(as.character(data.puce$CHROMOSOME))

	### --- On initialise une table vide --- ####
	res.data    <- data.frame(0,0,0,0,0,0,0,0,0) [-1,] # Le -1 pour pas avoir une première colonne quavec des 0
	
	chisq.pval <- function(x) 
		{
      		combi      <- -2 * (sum(log(x))) 
		res.combi <- pchisq(combi, df=2*length(x), lower.tail = FALSE, log.p = TRUE)
      		return(-res.combi)
  		}

for (j in 1:length(chrom))
{			
	# Récupération la table temporaire correspondant à CHROMOSOME =chrom[j]
	data.temp <- data.puce[(as.character(data.puce$CHROMOSOME) == chrom[j]),]
	print(nrow(data.temp))
	
	M		<- data.temp[[data.M]]
	pval 	<- data.temp[[data.pval]]
	res.start	<- vector(mode = "integer")
	res.end		<- vector(mode = "integer")
	res.length	<- vector(mode = "integer")
	res.nb.sign	<- vector(mode = "integer")
	res.nbcons.sign <- vector(mode = "integer")
	res.pval.comb   <- list()
	res.mean	        <- list()
	res.med	        <- list()
	num.pos		<- 0
	bool 		<- FALSE

	for(i in 1:length(M))
		{
		if(M[[i]] > val.M.min && bool == FALSE)
			{
			num.pos               <- num.pos + 1
			res.start[[num.pos]]  <- data.temp$POSITION[[i]]
			res.end[[num.pos]]    <- data.temp$POSITION[[i]] + data.temp$LENGTH[[i]]
			bool 		      <- TRUE
			val.pval              <- vector(mode = "integer")
			num.pval              <- 1
			val.pval[[num.pval]]  <- ifelse(pval[[i]] <= pval.seuil, 1, 0)
			val.pval2             <- vector(mode = "numeric")
			num.pval2             <- 1
			val.pval2[[num.pval2]]<- pval[[i]]
			val.M                 <- vector(mode = "numeric")
			num.M                 <- 1
			val.M[[num.M]]        <- M[[i]]

			if(is.null(taille.repeat) == FALSE && i+1 <= length(data.temp$POSITION))
				{
				if((data.temp$POSITION[[i+1]] - data.temp$POSITION[[i]]) >= taille.repeat)
					{
					num.pos   	  <- num.pos - 1
					length(res.start) <- length(res.start) - 1
					length(res.end)   <- length(res.end) - 1
					bool 		  <- FALSE
					}
				}
				
			}
		else
			{
			if(M[[i]] > val.M.min && bool == TRUE)
				{
				res.end[[num.pos]]     <- data.temp$POSITION[[i]] + data.temp$LENGTH[[i]]
				num.pval 	       <- num.pval + 1
				val.pval[[num.pval]]   <- ifelse(pval[[i]] <= pval.seuil, 1, 0)
				num.pval2 	       <- num.pval2 + 1
				val.pval2[[num.pval2]] <- pval[[i]]
				num.M 		       <- num.M + 1
				val.M[[num.M]] 	       <- M[[i]]

				if(is.null(taille.repeat) == FALSE && i+1 <= length(data.temp$POSITION))
					{
					if((data.temp$POSITION[[i+1]] - data.temp$POSITION[[i]]) >= taille.repeat)
						{
						bool 	      <- FALSE
						taille.region <- res.end[[num.pos]] - res.start[[num.pos]] + 1 #e.b. '+1'

						if(length(val.pval) >= nb.hits.signif)
							{
							res.consec    <- matrix(nrow = length(val.pval) - nb.hits.signif + 1, ncol = nb.hits.signif)
							for(i in 1:nb.hits.signif) {res.consec[,i] <- val.pval[i:(length(val.pval) - nb.hits.signif + i)]}
							res.sum       <- max(apply(res.consec, 1, sum))
							}

						if(consecutif == TRUE)
							{
							if(length(val.pval) < nb.hits.signif) {sum.pval <- 0}
							else				      {sum.pval <- res.sum}
							}
						else
							{
							sum.pval <- sum(val.pval)
							}
						if(sum.pval < nb.hits.signif)
							{
							num.pos   	  <- num.pos - 1
							length(res.start) <- length(res.start) - 1
							length(res.end)   <- length(res.end) - 1
							}
						else
							{
							res.length[[num.pos]]      <- taille.region
							res.nb.sign[[num.pos]]     <- sum(val.pval)
							res.nbcons.sign[[num.pos]] <- res.sum
							res.mean[[num.pos]]        <- val.M
							res.med[[num.pos]]         <- val.M
							res.pval.comb[[num.pos]]   <- val.pval2
							}
						}
					}
				}
			}
		if((M[[i]] <= val.M.min && bool == TRUE) || (i == length(M) && bool == TRUE))
			{
			bool 	   <- FALSE
			taille.region <- res.end[[num.pos]] - res.start[[num.pos]] + 1 # e.b. '+1'

			if(length(val.pval) >= nb.hits.signif)
				{
				res.consec    <- matrix(nrow = length(val.pval) - nb.hits.signif + 1, ncol = nb.hits.signif)
				for(i in 1:nb.hits.signif) {res.consec[,i] <- val.pval[i:(length(val.pval) - nb.hits.signif + i)]}
				res.sum       <- max(apply(res.consec, 1, sum))
				}

			if(consecutif == TRUE)
				{
				if(length(val.pval) < nb.hits.signif) {sum.pval <- 0}
				else				      {sum.pval <- res.sum}
				}
			else
				{
			  	sum.pval <- sum(val.pval)
				}
			if(sum.pval < nb.hits.signif)
				{
				num.pos   	  <- num.pos - 1
				length(res.start) <- length(res.start) - 1
				length(res.end)   <- length(res.end) - 1
				}
			else
				{
				res.length[[num.pos]]      <- taille.region
				res.nb.sign[[num.pos]]     <- sum(val.pval)
				res.nbcons.sign[[num.pos]] <- res.sum
				res.mean[[num.pos]]        <- val.M
				res.med[[num.pos]]         <- val.M
				res.pval.comb[[num.pos]]   <- val.pval2
				}
			}
		} # end of 'for(i in 1:length(M))' l.232
                # partie ok jusque là...

              if (length(res.start) < 2) { # gerer le cas ou il n'y a rien
                cat(paste("pas d'ori pour", chrom[j],"\n", sep=" "))

                res.data <- res.data
                
              } else {
        
		if(is.null(dist.min.region) == FALSE)
		{
		res.filtre.start       <- vector()
		res.filtre.end	       <- vector()
		res.filtre.length      <- vector()
		res.filtre.nb.signif   <- vector()
		res.filtre.nbcons.sign <- vector()
		res.filtre.pval.comb   <- vector()
		res.filtre.mean	       <- vector()
		res.filtre.med         <- vector()

		val.filtre.length      <- vector()
		val.filtre.nb.signif   <- vector()
		val.filtre.nbcons.sign <- vector()
		val.filtre.pval.comb   <- vector()
		val.filtre.mean        <- vector()
		val.filtre.med         <- vector()

		res.filtre.start[[1]]  <- res.start[[1]]
		compt                  <- 1

  
                for(i in 2:length(res.start))
			{
			end   <- res.end[[i-1]]
			start <- res.start[[i]]

			if(start - end > dist.min.region  && res.end[[i]] >= start)
				{
				if(compt == 1)
					{
					res.filtre.start[[length(res.filtre.start)+1]]	           <- start
					res.filtre.end[[length(res.filtre.end)+1]] 	           <- end
					res.filtre.length[[length(res.filtre.length)+1]]           <- res.length[[i-1]]
					res.filtre.nb.signif[[length(res.filtre.nb.signif)+1]]     <- res.nb.sign[[i-1]]
					res.filtre.nbcons.sign[[length(res.filtre.nbcons.sign)+1]] <- res.nbcons.sign[[i-1]]
					res.filtre.pval.comb[[length(res.filtre.pval.comb)+1]]     <- chisq.pval(res.pval.comb[[i-1]])
					res.filtre.mean[[length(res.filtre.mean)+1]]               <- mean(res.mean[[i-1]])
					res.filtre.med[[length(res.filtre.med)+1]]                 <- median(res.med[[i-1]])
					}
				else
					{
					res.filtre.end[[length(res.filtre.end)+1]] 	           <- end
					res.filtre.start[[length(res.filtre.start)+1]]	           <- start
					res.filtre.length[[length(res.filtre.length)+1]]           <- end - res.filtre.start[[length(res.filtre.end)]] + 1 # e.b. '+1'
					res.filtre.nb.signif[[length(res.filtre.nb.signif)+1]]     <- sum(val.filtre.nb.signif)
					res.filtre.nbcons.sign[[length(res.filtre.nbcons.sign)+1]] <- max(val.filtre.nbcons.sign)
					res.filtre.pval.comb[[length(res.filtre.pval.comb)+1]]     <- chisq.pval(val.filtre.pval.comb)
					res.filtre.mean[[length(res.filtre.mean)+1]]               <- mean(val.filtre.mean)
					res.filtre.med[[length(res.filtre.med)+1]]                 <- median(val.filtre.med)
					compt <- 1
					}
				}
			else
				{
				if(compt == 1)
					{
					val.filtre.length      <- c(res.length[[i-1]], res.length[[i]])
					val.filtre.nb.signif   <- c(res.nb.sign[[i-1]], res.nb.sign[[i]])
					val.filtre.nbcons.sign <- c(res.nbcons.sign[[i-1]], res.nbcons.sign[[i]])
					val.filtre.pval.comb   <- c(res.pval.comb[[i-1]], res.pval.comb[[i]])
					val.filtre.mean        <- c(res.mean[[i-1]], res.mean[[i]])
					val.filtre.med         <- c(res.med[[i-1]], res.med[[i]])
					compt 	               <- compt + 1
					}
				else
					{
					val.filtre.length      <- c(val.filtre.length, res.length[[i]])
					val.filtre.nb.signif   <- c(val.filtre.nb.signif, res.nb.sign[[i]])
					val.filtre.nbcons.sign <- c(val.filtre.nbcons.sign, res.nbcons.sign[[i]])
					val.filtre.pval.comb   <- c(val.filtre.pval.comb, res.pval.comb[[i]])
					val.filtre.mean        <- c(val.filtre.mean, res.mean[[i]])
					val.filtre.med         <- c(val.filtre.med, res.med[[i]])
					compt                  <- compt + 1
					}
				}
			}

		if(compt == 1)
			{
			res.filtre.end[[length(res.filtre.end)+1]]                 <- res.end[[length(res.end)]]
			res.filtre.length[[length(res.filtre.length)+1]]           <- res.length[[length(res.length)]]
			res.filtre.nb.signif[[length(res.filtre.nb.signif)+1]]     <- res.nb.sign[[length(res.nb.sign)]]
			res.filtre.nbcons.sign[[length(res.filtre.nbcons.sign)+1]] <- res.nbcons.sign[[length(res.nbcons.sign)]]
			res.filtre.pval.comb[[length(res.filtre.pval.comb)+1]]     <- chisq.pval(res.pval.comb[[length(res.pval.comb)]])
			res.filtre.mean[[length(res.filtre.mean)+1]]               <- mean(res.mean[[length(res.mean)]])
			res.filtre.med[[length(res.filtre.med)+1]]                 <- median(res.med[[length(res.med)]])
			}
		else
			{
			res.filtre.end[[length(res.filtre.end)+1]]                 <- res.end[[length(res.end)]]
			res.filtre.length[[length(res.filtre.length)+1]]           <- res.filtre.end[[length(res.filtre.end)]] - res.filtre.start[[length(res.filtre.start)]] + 1 # e.b. '+1'
			res.filtre.nb.signif[[length(res.filtre.nb.signif)+1]]     <- sum(val.filtre.nb.signif)
			res.filtre.nbcons.sign[[length(res.filtre.nbcons.sign)+1]] <- max(val.filtre.nbcons.sign)
			res.filtre.pval.comb[[length(res.filtre.pval.comb)+1]]     <- chisq.pval(val.filtre.pval.comb)
			res.filtre.mean[[length(res.filtre.mean)+1]]               <- mean(val.filtre.mean)
			res.filtre.med[[length(res.filtre.med)+1]]                 <- median(val.filtre.med)
			}

		res <- cbind.data.frame(chrom[j],res.filtre.start,res.filtre.end,res.filtre.length,res.filtre.nb.signif,res.filtre.nbcons.sign,res.filtre.pval.comb,res.filtre.mean,res.filtre.med)
		names(res)<- c("CHROMOSOME","START", "END", "LENGTH", "NB.SIGNIF", "NB.MAX.SIGNIF.CONS", "LOG.NEG.PVAL.COMB", "MEAN.M", "MEDIAN.M")
		} # end of 'if(is.null(dist.min.region) == FALSE)' l.358
	else
		{
		res.mean      <- as.vector(lapply(res.mean, mean), mode = "numeric")
		res.med       <- as.vector(lapply(res.med, median), mode = "numeric")
		res.pval.comb <- as.vector(lapply(res.pval.comb, chisq.pval), mode = "numeric")
		res           <- cbind.data.frame(chrom[j],res.start, res.end, res.length, res.nb.sign, res.nbcons.sign, res.pval.comb, res.mean, res.med)
		names(res)    <- c("CHROMOSOME","START", "END", "LENGTH", "NB.SIGNIF", "NB.MAX.SIGNIF.CONS", "LOG.NEG.PVAL.COMB", "MEAN.M", "MEDIAN.M")
		}

	if(is.null(taille.min.region) == FALSE)
		{
		res.filtre.start       <- vector()
		res.filtre.end	       <- vector()
		res.filtre.length      <- vector()
		res.filtre.nb.signif   <- vector()
		res.filtre.nbcons.sign <- vector()
		res.filtre.pval.comb   <- vector()
		res.filtre.mean	       <- vector()
		res.filtre.med         <- vector()

		for(i in 1:dim(res)[[1]])
			{
			if(res$END[[i]] - res$START[[i]] + 1 > taille.min.region && res$END[[i]] >= res$START[[i]]) # e.b. '+1'
				{
				res.filtre.start[[length(res.filtre.start) + 1]]             <- res$START[[i]]
				res.filtre.end[[length(res.filtre.end) + 1]]	             <- res$END[[i]]
				res.filtre.length[[length(res.filtre.length) + 1]]           <- res$LENGTH[[i]]
				res.filtre.nb.signif[[length(res.filtre.nb.signif) + 1]]     <- res$NB.SIGNIF[[i]]
				res.filtre.nbcons.sign[[length(res.filtre.nbcons.sign) + 1]] <- res$NB.MAX.SIGNIF.CONS[[i]]
				res.filtre.pval.comb[[length(res.filtre.pval.comb) + 1]]     <- res$LOG.NEG.PVAL.COMB[[i]]
				res.filtre.mean[[length(res.filtre.mean) + 1]]	             <- res$MEAN.M[[i]]
				res.filtre.med[[length(res.filtre.med) + 1]]                 <- res$MEDIAN.M[[i]]
				}
			}
		res             <- cbind.data.frame(chrom[j],res.filtre.start,res.filtre.end,res.filtre.length,res.filtre.nb.signif,res.filtre.nbcons.sign,res.filtre.pval.comb,res.filtre.mean,res.filtre.med)
		names(res)<- c("CHROMOSOME","START", "END", "LENGTH", "NB.SIGNIF", "NB.MAX.SIGNIF.CONS", "LOG.NEG.PVAL.COMB", "MEAN.M", "MEDIAN.M")
		
		}

  	# On met à jour la table res.data
	res.data <- rbind.data.frame(res.data,res)
}	
}
return(res.data)
}

#***********************************************************************************************************************************************#
# FONCTION SELECT.ORIS.DETECT.NEG -> Recuperation des regions significatives négatives a partir des donnees du genome
#***********************************************************************************************************************************************#

select.oris.detect.neg  <- function(data.puce, data.M, data.pval, nb.hits.signif, taille.min.region = NULL, dist.min.region = NULL, val.M.min = 0, pval.seuil = 0.05, consecutif = FALSE, taille.repeat = NULL)

	#########################################################################################################################################
	# data.puce		<- La table de donnees contenant les donnees de la puce
	# data.M		<- Le nom de la colonne dans la table 'data.puce' contenant les valeurs de M qui nous interessent
	# data.pval		<- Le nom de la colonne dans la table 'data.puce' contenant les valeurs des p-values qui nous interessent
	# nb.hits.signif 	<- Le nombre de sondes significatives que doit contenir la region pour  aªtre selectionnees
	# taille.min.region	<- La taille minimale que doit faire la region pour  aªtre consideree comme une ORIs (non artefactuelle)
	# dist.min.region	<- Si la distance entre deux regions positives/significatives est inferieure  a  'dist.min.region' alors les deux
	#			   regions sont agregees en une seule et m aªme region (i.e. m aªme origine de replication)
	# val.M.min		<- La valeur minimale de M pour laquelle on considere qu'on rentre dans une region potentiellement significative
	# pval.seuil		<- Le seuil de significativite pour la p-value
	# consecutif		<- Les sondes selectionnees par le parametre 'nb.hits.signif' doivent  etre consecutive si consecutif = TRUE
	# taille.repeat		<- Si la distance entre 2 sondes est superieure  a  'taille.repeat', alors cette region est consideree comme une region 
	#			   repeat du genome et les 2 sondes sont considerees comme trop eloignees pour appartenir  a  la m aªme region.
	#########################################################################################################################################
{	
	data.puce	<- data.puce[order(data.puce$CHROMOSOME,data.puce$POSITION),]
	
	# Préparation dune boucle sur les levels d'une colonne 
	chrom <- unique(as.character(data.puce$CHROMOSOME))
	
	### --- On initialise une table vide --- ####
	res.data <- data.frame(0,0,0,0,0,0,0,0,0) [-1,] # Le -1 pour pas avoir une première colonne quavec des 0
	

	chisq.pval <- function(x) 
		{
      		combi     <- -2 * (sum(log(x))) 
		res.combi <- pchisq(combi, df=2*length(x), lower.tail = FALSE, log.p = TRUE)
      		return(-res.combi)
  		}

for (j in 1:length(chrom))

{			
	# Récupération la table temporaire correspondant à CHROMOSOME =chrom[j]
	data.temp <- data.puce[(as.character(data.puce$CHROMOSOME) == chrom[j]),]
	print(nrow(data.temp))
	
	M		<- data.temp[[data.M]]
	pval 		<- data.temp[[data.pval]]
	res.start	<- vector(mode = "integer")
	res.end		<- vector(mode = "integer")
	res.length	<- vector(mode = "integer")
	res.nb.sign	<- vector(mode = "integer")
	res.nbcons.sign <- vector(mode = "integer")
	res.pval.comb   <- list()
	res.mean	<- list()
	res.med	        <- list()
	num.pos		<- 0
	bool 		<- FALSE
	
	for(i in 1:length(M))
		{
		if(M[[i]] < val.M.min && bool == FALSE) # Modif n°1 (par rapport à la fonction select.oris)
			{
			num.pos               <- num.pos + 1
			res.start[[num.pos]]  <- data.temp$POSITION[[i]]
			res.end[[num.pos]]    <- data.temp$POSITION[[i]] + data.temp$LENGTH[[i]]
			bool 		      <- TRUE
			val.pval              <- vector(mode = "integer")
			num.pval              <- 1
			val.pval[[num.pval]]  <- ifelse(pval[[i]] <= pval.seuil, 1, 0)
			val.pval2             <- vector(mode = "numeric")
			num.pval2             <- 1
			val.pval2[[num.pval2]]<- pval[[i]]
			val.M                 <- vector(mode = "numeric")
			num.M                 <- 1
			val.M[[num.M]] 	      <- M[[i]]

			if(is.null(taille.repeat) == FALSE && i+1 <= length(data.temp$POSITION))
				{
				if((data.temp$POSITION[[i+1]] - data.temp$POSITION[[i]]) >= taille.repeat)
					{
					num.pos   	  <- num.pos - 1
					length(res.start) <- length(res.start) - 1
					length(res.end)   <- length(res.end) - 1
					bool 		  <- FALSE
					}
				}
				
			}
		else
			{
			if(M[[i]] < val.M.min && bool == TRUE) # Modif n°2 (par rapport à la fonction select.oris)
				{
				res.end[[num.pos]]   	<- data.temp$POSITION[[i]] + data.temp$LENGTH[[i]]
				num.pval 		<- num.pval + 1
				val.pval[[num.pval]] 	<- ifelse(pval[[i]] <= pval.seuil, 1, 0)
				num.pval2 		<- num.pval2 + 1
				val.pval2[[num.pval2]] 	<- pval[[i]]
				num.M 		        <- num.M + 1
				val.M[[num.M]] 	        <- M[[i]]

				if(is.null(taille.repeat) == FALSE && i+1 <= length(data.temp$POSITION))
					{
					if((data.temp$POSITION[[i+1]] - data.temp$POSITION[[i]]) >= taille.repeat)
						{
						bool 	      <- FALSE
						taille.region <- res.end[[num.pos]] - res.start[[num.pos]]

						if(length(val.pval) >= nb.hits.signif)
							{
							res.consec    <- matrix(nrow = length(val.pval) - nb.hits.signif + 1, ncol = nb.hits.signif)
							for(i in 1:nb.hits.signif) {res.consec[,i] <- val.pval[i:(length(val.pval) - nb.hits.signif + i)]}
							res.sum       <- max(apply(res.consec, 1, sum))
							}

						if(consecutif == TRUE)
							{
							if(length(val.pval) < nb.hits.signif) {sum.pval <- 0}
							else				      {sum.pval <- res.sum}
							}
						else
							{
							sum.pval <- sum(val.pval)
							}
						if(sum.pval < nb.hits.signif)
							{
							num.pos   	  <- num.pos - 1
							length(res.start) <- length(res.start) - 1
							length(res.end)   <- length(res.end) - 1
							}
						else
							{
							res.length[[num.pos]]      <- taille.region
							res.nb.sign[[num.pos]]     <- sum(val.pval)
							res.nbcons.sign[[num.pos]] <- res.sum
							res.mean[[num.pos]]        <- val.M
							res.med[[num.pos]]         <- val.M
							res.pval.comb[[num.pos]]   <- val.pval2
							}
						}
					}
				}
			}
		if((M[[i]] >= val.M.min && bool == TRUE) || (i == length(M) && bool == TRUE)) # Modif n°3 (par rapport à la fonction select.oris)
			{
			bool 	      <- FALSE
			taille.region <- res.end[[num.pos]] - res.start[[num.pos]] + 1 # eb "+1" à vérifier

			if(length(val.pval) >= nb.hits.signif)
				{


				res.consec    <- matrix(nrow = length(val.pval) - nb.hits.signif + 1, ncol = nb.hits.signif)
				for(i in 1:nb.hits.signif) {res.consec[,i] <- val.pval[i:(length(val.pval) - nb.hits.signif + i)]}
				res.sum       <- max(apply(res.consec, 1, sum))
				}

			if(consecutif == TRUE)
				{
				if(length(val.pval) < nb.hits.signif) {sum.pval <- 0}
				else				      {sum.pval <- res.sum}
				}
			else
				{
			  	sum.pval <- sum(val.pval)
				}
			if(sum.pval < nb.hits.signif)
				{
				num.pos   	  <- num.pos - 1
				length(res.start) <- length(res.start) - 1
				length(res.end)   <- length(res.end) - 1
				}
			else
				{
				res.length[[num.pos]]      <- taille.region
				res.nb.sign[[num.pos]]     <- sum(val.pval)
				res.nbcons.sign[[num.pos]] <- res.sum
				res.mean[[num.pos]]        <- val.M
				res.med[[num.pos]]         <- val.M
				res.pval.comb[[num.pos]]   <- val.pval2
				}
			}
		}
		
		if(is.null(dist.min.region) == FALSE)
		{
		res.filtre.start       <- vector()
		res.filtre.end	       <- vector()
		res.filtre.length      <- vector()
		res.filtre.nb.signif   <- vector()
		res.filtre.nbcons.sign <- vector()
		res.filtre.pval.comb   <- vector()
		res.filtre.mean	       <- vector()
		res.filtre.med         <- vector()

		val.filtre.length      <- vector()
		val.filtre.nb.signif   <- vector()
		val.filtre.nbcons.sign <- vector()
		val.filtre.pval.comb   <- vector()
		val.filtre.mean        <- vector()
		val.filtre.med         <- vector()

		res.filtre.start[[1]]  <- res.start[[1]]
		compt                  <- 1

		for(i in 2:length(res.start))
			{
			end   <- res.end[[i-1]]
			start <- res.start[[i]]

			if(start - end > dist.min.region  && res.end[[i]] >= start)
				{
				if(compt == 1)
					{
					res.filtre.start[[length(res.filtre.start)+1]]	           <- start
					res.filtre.end[[length(res.filtre.end)+1]] 	           <- end
					res.filtre.length[[length(res.filtre.length)+1]]           <- res.length[[i-1]]
					res.filtre.nb.signif[[length(res.filtre.nb.signif)+1]]     <- res.nb.sign[[i-1]]
					res.filtre.nbcons.sign[[length(res.filtre.nbcons.sign)+1]] <- res.nbcons.sign[[i-1]]
					res.filtre.pval.comb[[length(res.filtre.pval.comb)+1]]     <- chisq.pval(res.pval.comb[[i-1]])
					res.filtre.mean[[length(res.filtre.mean)+1]]               <- mean(res.mean[[i-1]])
					res.filtre.med[[length(res.filtre.med)+1]]                 <- median(res.med[[i-1]])
					}
				else
					{
					res.filtre.end[[length(res.filtre.end)+1]] 	           <- end
					res.filtre.start[[length(res.filtre.start)+1]]	           <- start
					res.filtre.length[[length(res.filtre.length)+1]]           <- end - res.filtre.start[[length(res.filtre.end)]]
					res.filtre.nb.signif[[length(res.filtre.nb.signif)+1]]     <- sum(val.filtre.nb.signif)
					res.filtre.nbcons.sign[[length(res.filtre.nbcons.sign)+1]] <- max(val.filtre.nbcons.sign)
					res.filtre.pval.comb[[length(res.filtre.pval.comb)+1]]     <- chisq.pval(val.filtre.pval.comb)
					res.filtre.mean[[length(res.filtre.mean)+1]]               <- mean(val.filtre.mean)
					res.filtre.med[[length(res.filtre.med)+1]]                 <- median(val.filtre.med)
					compt <- 1
					}
				}
			else
				{
				if(compt == 1)
					{
					val.filtre.length      <- c(res.length[[i-1]], res.length[[i]])
					val.filtre.nb.signif   <- c(res.nb.sign[[i-1]], res.nb.sign[[i]])
					val.filtre.nbcons.sign <- c(res.nbcons.sign[[i-1]], res.nbcons.sign[[i]])
					val.filtre.pval.comb   <- c(res.pval.comb[[i-1]], res.pval.comb[[i]])
					val.filtre.mean        <- c(res.mean[[i-1]], res.mean[[i]])
					val.filtre.med         <- c(res.med[[i-1]], res.med[[i]])
					compt 	               <- compt + 1
					}
				else
					{
					val.filtre.length      <- c(val.filtre.length, res.length[[i]])
					val.filtre.nb.signif   <- c(val.filtre.nb.signif, res.nb.sign[[i]])
					val.filtre.nbcons.sign <- c(val.filtre.nbcons.sign, res.nbcons.sign[[i]])
					val.filtre.pval.comb   <- c(val.filtre.pval.comb, res.pval.comb[[i]])
					val.filtre.mean        <- c(val.filtre.mean, res.mean[[i]])
					val.filtre.med         <- c(val.filtre.med, res.med[[i]])
					compt                  <- compt + 1
					}
				}
			}
		if(compt == 1)
			{
			res.filtre.end[[length(res.filtre.end)+1]]                 <- res.end[[length(res.end)]]
			res.filtre.length[[length(res.filtre.length)+1]]           <- res.length[[length(res.length)]]
			res.filtre.nb.signif[[length(res.filtre.nb.signif)+1]]     <- res.nb.sign[[length(res.nb.sign)]]
			res.filtre.nbcons.sign[[length(res.filtre.nbcons.sign)+1]] <- res.nbcons.sign[[length(res.nbcons.sign)]]
			res.filtre.pval.comb[[length(res.filtre.pval.comb)+1]]     <- chisq.pval(res.pval.comb[[length(res.pval.comb)]])
			res.filtre.mean[[length(res.filtre.mean)+1]]               <- mean(res.mean[[length(res.mean)]])
			res.filtre.med[[length(res.filtre.med)+1]]                 <- median(res.med[[length(res.med)]])
			}
		else
			{
			res.filtre.end[[length(res.filtre.end)+1]]                 <- res.end[[length(res.end)]]
			res.filtre.length[[length(res.filtre.length)+1]]           <- res.filtre.end[[length(res.filtre.end)]] - res.filtre.start[[length(res.filtre.start)]] + 1 #eb. ajout +1
			res.filtre.nb.signif[[length(res.filtre.nb.signif)+1]]     <- sum(val.filtre.nb.signif)
			res.filtre.nbcons.sign[[length(res.filtre.nbcons.sign)+1]] <- max(val.filtre.nbcons.sign)
			res.filtre.pval.comb[[length(res.filtre.pval.comb)+1]]     <- chisq.pval(val.filtre.pval.comb)
			res.filtre.mean[[length(res.filtre.mean)+1]]               <- mean(val.filtre.mean)
			res.filtre.med[[length(res.filtre.med)+1]]                 <- median(val.filtre.med)
			}

		res <- cbind.data.frame(chrom[j],res.filtre.start,res.filtre.end,res.filtre.length,res.filtre.nb.signif,res.filtre.nbcons.sign,res.filtre.pval.comb,res.filtre.mean,res.filtre.med)
		names(res)<- c("CHROMOSOME","START", "END", "LENGTH", "NB.SIGNIF", "NB.MAX.SIGNIF.CONS", "LOG.NEG.PVAL.COMB", "MEAN.M", "MEDIAN.M")
		}
	else
		{
		res.mean      <- as.vector(lapply(res.mean, mean), mode = "numeric")
		res.med       <- as.vector(lapply(res.med, median), mode = "numeric")
		res.pval.comb <- as.vector(lapply(res.pval.comb, chisq.pval), mode = "numeric")
		res           <- cbind.data.frame(chrom[j],res.start, res.end, res.length, res.nb.sign, res.nbcons.sign, res.pval.comb, res.mean, res.med)
		names(res)<- c("CHROMOSOME","START", "END", "LENGTH", "NB.SIGNIF", "NB.MAX.SIGNIF.CONS", "LOG.NEG.PVAL.COMB", "MEAN.M", "MEDIAN.M")
		}

	if(is.null(taille.min.region) == FALSE)
		{
		res.filtre.start       <- vector()
		res.filtre.end	       <- vector()
		res.filtre.length      <- vector()
		res.filtre.nb.signif   <- vector()
		res.filtre.nbcons.sign <- vector()
		res.filtre.pval.comb   <- vector()
		res.filtre.mean	       <- vector()
		res.filtre.med         <- vector()

		for(i in 1:dim(res)[[1]])
			{
			if(res$END[[i]] - res$START[[i]] > taille.min.region && res$END[[i]] >= res$START[[i]])
				{
				res.filtre.start[[length(res.filtre.start) + 1]]             <- res$START[[i]]
				res.filtre.end[[length(res.filtre.end) + 1]]	             <- res$END[[i]]
				res.filtre.length[[length(res.filtre.length) + 1]]           <- res$LENGTH[[i]]
				res.filtre.nb.signif[[length(res.filtre.nb.signif) + 1]]     <- res$NB.SIGNIF[[i]]
				res.filtre.nbcons.sign[[length(res.filtre.nbcons.sign) + 1]] <- res$NB.MAX.SIGNIF.CONS[[i]]
				res.filtre.pval.comb[[length(res.filtre.pval.comb) + 1]]     <- res$LOG.NEG.PVAL.COMB[[i]]
				res.filtre.mean[[length(res.filtre.mean) + 1]]	             <- res$MEAN.M[[i]]
				res.filtre.med[[length(res.filtre.med) + 1]]                 <- res$MEDIAN.M[[i]]
				}
			}
		res        <- cbind.data.frame(chrom[j],res.filtre.start,res.filtre.end,res.filtre.length,res.filtre.nb.signif,res.filtre.nbcons.sign,res.filtre.pval.comb,res.filtre.mean,res.filtre.med)
		names(res)<- c("CHROMOSOME","START", "END", "LENGTH", "NB.SIGNIF", "NB.MAX.SIGNIF.CONS", "LOG.NEG.PVAL.COMB", "MEAN.M", "MEDIAN.M")
		
		}
	# On met à jour la table res.data
	res.data <- rbind.data.frame(res.data,res)
	
}
return(res.data)
}




#***********************************************************************************************************************************************#
# FONCTION FLAG.POSITION -> Identification par un flag  a  1 des positions qui se trouvent dans les regions entrees en prametre
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Cette fonction identifie par "1" les positions appartenant aux regions entrees en parametre et par "0" le reste.
#################################################################################################################################################

flag.position <- function(data.position, data.repeat, nom.flag = "flag")

	#########################################################################################################################################
      # data.position <- Le vecteur contenant les positions  a  flagger
	# data.repeat	<- La table de donnees contenant les regions 
	# nom.flag	<- Le nom que prendra la colonne contenant les flag
	#########################################################################################################################################
{
	# ********** On tri les donnees et on parcours les 2 tables simultannement ********** #
	if(class(data.position) == "data.frame") 
		{
		data.position 	<- data.position[order(data.position$POSITION),]
		position 	<- data.position$POSITION
		}
	else 
		{
		data.position	<- sort(data.position)
		position	<- data.position
		data.position 	<- data.frame(data.position)
		names(data.position) <- "POSITION"
		}

	data.repeat 	<- data.repeat[order(data.repeat$START, data.repeat$END),]
	flag.position	<- vector(mode = "integer", length = length(position))
	num.pos  	<- 1

	for(i in 1:dim(data.repeat)[[1]])
		{
		start 	<- data.repeat$START[[i]]
		end   	<- data.repeat$END[[i]]
		bool	<- FALSE

		while(num.pos <= length(position) && bool == FALSE) 
			{
			if(position[[num.pos]] < start) {num.pos <- num.pos + 1}
			if(position[[num.pos]] > end) 	{bool <- TRUE}
			if(position[[num.pos]] >= start && position[[num.pos]] <= end) 
				{
				flag.position[[num.pos]] <- 1
				num.pos <- num.pos + 1
				}
			}
		}
	data.position[[nom.flag]] <- flag.position
return(data.position)
}



#***********************************************************************************************************************************************#
# FONCTION EXPORT.GENOME.ORIS.GFF -> Exportation des donnees au format .gff afin d'etre visualisees dans Signalmap
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Exportation des donnees au format .gff avec en couleur, les pics selectionnes par le modele. 
# Les fichiers .gff sont visualisables dans SignalMap.
#################################################################################################################################################

export.genome.oris.gff <- function(data.puce, data.M, nom.flag, taille.pos = NULL, val.chr = NULL, val.strand = NULL)

	#########################################################################################################################################*
	# data.puce	-> La table de donnees contenant les donnees de la puce
	# data.M 	-> Le nom de la colonne contenant les valeurs des log-ratios  a  exporter au format.gff.
	#		   ("M", "M.NORM.LOWESS", "M.runmed.w5", "M.runmed.w7", "M.runmed.w9", "M.runmed.w11", etc)
	# nom.flag	-> Le nom de la colonne contenant les valeurs des flag  a  representer en couleur au format .gff
	# taille.pos	-> Si taille.pos = NULL, alors c'est la colonne LENGTH qui est prise en compte
	# val.chr	-> Correspond au nom de la colonne qui contient le nom du chromosomes correspondant  a  la sonde. De fa a§on generale le nom de
	# 		   cette colonne est "SEQ_ID" mais pour les regions  a  haute densite il faut utiliser le nom : "GENE_EXPR_OPTION"
	# val.strand    -> Correspond au nom de la colonne qui contient le sens sur le brin.
	#########################################################################################################################################
{
	# --- Creation d'un fichier o a¹ mettre les donnees (si il n'existe pas) --- #
	safe.dir.create("Fichiers GFF - ORIS")
       	nom 	 <- deparse(substitute(data.puce))
	nom.doss <- paste("Fichiers GFF - ORIS/M=", data.M, " - ", nom.flag, sep="")
	nom 	 <- paste(nom, "_", data.M, "_", nom.flag, sep="")
	safe.dir.create(nom.doss)

	# --- Donnees pour les graphiques --- #
	x.M 	<- data.puce[[data.M]]
	x.RATIO <- 2**data.puce[[data.M]]
	flag	<- data.puce[[nom.flag]]
	couleur	<- ifelse(flag != 0, "color=000000", "color=C0C0C0")
	scan 	<- "NimbleScan"
	if(is.null(taille.pos) == TRUE)	{pos <- data.puce$POSITION + data.puce$LENGTH}
	else 				{pos <- data.puce$POSITION + taille.pos}
	if(is.null(val.chr) == TRUE)	{chrom <- data.puce$CHROMOSOME}
	else 				{chrom <- val.chr}
	if(is.null(val.strand) == TRUE)	{strand <- data.puce$GENE_EXPR_OPTION}
	else 				{strand <- data.puce[[val.strand]]}

	# --- On cree une table contenant les donnees normalisees de toutes les sondes avec en couleur les pics selectionnes par le modele --- #
	fic.gff.tt.oris <- 	data.frame(chrom, scan, paste(nom,":", strand, ":pics selectionnes par le modele en noir", sep=""), data.puce$POSITION, 
				pos, x.M, ".", ".", paste(couleur, ";", "seq_id=", data.puce$SEQ_ID, ";probe_id=", data.puce$PROBE_ID, ";count=1", sep=""))

	# --- Exportation des donnees --- #
	titre <- paste(nom.doss, "/", nom, ".gff", sep="")
	write.table(fic.gff.tt.oris, file=titre, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
	cat(paste("Creation du fichier suivant dans le dossier ", nom.doss,"/ : \n --> ", titre,"\n \n",sep=""))

	# --- On cree une table contenant les donnees normalisees de toutes les sondes selectionnes par le modele (uniquement les sondes significatives --- #
#	fic.gff.signif.oris <- 	data.frame(chrom[flag > 0], scan, paste(nom,":",strand[flag > 0], ":log-ratios:pics selectionnes par le modele", sep=""), 
#				data.puce$POSITION[flag > 0], pos[flag > 0], 1, ".", ".",paste(couleur[flag > 0],";", "seq_id=", 
#				data.puce$SEQ_ID[flag > 0], ";probe_id=", data.puce$PROBE_ID[flag > 0], ";count=1", sep=""))

	# --- Exportation des donnees --- #
#	titre <- paste(nom.doss, "/", nom, "_logratio_oris.gff", sep="")
#	write.table(fic.gff.signif.oris, file=titre, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
#	cat(paste("Creation du fichier suivant dans le dossier ",nom.doss,"/ : \n --> ", titre,"\n \n",sep=""))

	# --- On cree une table contenant les ratio normalisees de toutes les sondes avec en couleur les pics  selectionnes par le modele --- #
#	ratio.gff.tt.oris <- 	data.frame(chrom, scan, paste(nom, ":", strand, ":ratio:pics selectionnes par le modele en noir", sep=""), data.puce$POSITION, 
#				pos, x.RATIO, ".", ".", paste(couleur,";", "seq_id=", data.puce$SEQ_ID, ";probe_id=", data.puce$PROBE_ID, ";count=1", sep=""))
#
	# --- Exportation des donnees --- #
#	titre <- paste(nom.doss, "/", nom, "_ratio_tt.gff", sep="")
#	write.table(ratio.gff.tt.oris, file=titre, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
#	cat(paste("Creation du fichier suivant dans le dossier ",nom.doss,"/ : \n --> ", titre,"\n \n",sep=""))

	# --- On cree une table contenant les donnees normalisees de toutes les sondes selectionnes par le modele --- #
#	ratio.gff.signif.oris <- 	data.frame(chrom[flag > 0], scan, paste(nom,":",strand[flag > 0], ":ratio:pics selectionnes par le modele", sep=""), 
#					data.puce$POSITION[flag > 0], pos[flag > 0], x.RATIO[flag > 0], ".", ".",paste(couleur[flag > 0],";", "seq_id=", 
#					data.puce$SEQ_ID[flag > 0], ";probe_id=", data.puce$PROBE_ID[flag > 0], ";count=1", sep=""))

	# --- Exportation des donnees --- #
#	titre <- paste(nom.doss, "/", nom, "_ratio_oris.gff", sep="")
#	write.table(ratio.gff.signif.oris, file=titre, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
#	cat(paste("Creation du fichier suivant dans le dossier ",nom.doss,"/ : \n --> ", titre,"\n \n",sep=""))

invisible()	
}

#***********************************************************************************************************************************************#
# FONCTION EXPORT.ORIS.GFF
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Exportation des regions identifiees comme etant des origines de replication au format .gff
#################################################################################################################################################

export.oris.gff <- function(data.oris, chrom = "")

	#########################################################################################################################################
        # data.oris	-> Le nom de la table contenant les regions identifiees
	# chrom		-> Le nom du chromosome concerne
	#########################################################################################################################################
{
	# --- Creation d'un fichier o a¹ mettre les donnees (si il n'existe pas) --- #
       	nom 	 <- deparse(substitute(data.oris))
	nom.doss <- ("Fichiers GFF - ORIS/ORIS")
	safe.dir.create(nom.doss)

	# --- On cree une table contenant les donnees normalisees de toutes les sondes avec en couleur les pics  selectionnes par le modele --- #
	fic.gff <- data.frame(chrom, "Nimblegen", paste(nom,":origines identifiees", sep=""), data.oris$START, data.oris$END, 1, ".", ".")

	# --- Exportation des donnees --- #
	titre <- paste(nom.doss, "/", nom, "_origines.gff", sep="")
	write.table(fic.gff, file=titre, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
	cat(paste("Creation du fichier suivant dans le dossier ", nom.doss,"/ : \n --> ", titre,"\n \n",sep=""))
invisible()	
}



#***********************************************************************************************************************************************#
# FONCTION TAILLE.DIST.CHROM  -> Calcul de la taille des oris et des distances inter origines pour chaque chromosome
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Calcul de la taille moyenne et mediane des oris sur tout le genome, dans les regions EARLY et dans les regions LATE
# Calcul des distances inter origines moyenne et mediane des oris sur tout le genome, dans les regions EARLY et dans les regions LATE
#################################################################################################################################################

taille.dist.chrom  <- function(data.oris)

	#########################################################################################################################################
	# data.oris <- Les donnees des ORIs avec leur appartenance aux regions EARLY ou LATE
	#########################################################################################################################################
{
	nom.oris       <- deparse(substitute(data.oris))

	cat(paste(nom.oris, " :\n", sep = ""))
	cat(paste(dim(data.oris)[[1]], "\torigines de replication\n\n", sep = ""))

	#cat("Repartition des origines de replication parmi :\n", sep = "")
	#cat(paste(length(levels(as.factor(data.oris$NUM.TIMING[data.oris$TIMING == "EARLY"]))), "\tregions early\n", sep = ""))
	#cat(paste(length(levels(as.factor(data.oris$NUM.TIMING[data.oris$TIMING == "LATE"]))), "\tregions late\n\n", sep = ""))

	#cat(paste(length(data.oris$TIMING[data.oris$TIMING == "EARLY"]), "\torigines de replication dans des regions early\n", sep = ""))
	#cat(paste(length(data.oris$TIMING[data.oris$TIMING == "LATE"]), "\torigines de replication dans regions late\n\n", sep = ""))

	distance <- function(x)
		{
		x <- x[order(x$START, x$END),]
		val.dist <- vector()
		for(i in 2:length(x$START)) {val.dist[length(val.dist) + 1] <- x$START[[i]] - x$END[[i-1]]}
		res        <- list(val.dist, mean(val.dist), median(val.dist))
		names(res) <- c("distance", "dist.mean", "dist.med")
		return(res)
		}

	# Tri du fichier data.oris
	data.oris   <- data.oris[order(data.oris$CHROMOSOME, data.oris$START, data.oris$END),]
	
	# Liste de toutes les valeurs de CHROMOSOME
	chrom <- unique(as.character(data.oris$CHROMOSOME))

	# initialiser 2 listes pour la taille et la distance entre ORIs
	res.taille <- list()
	res.dist.tt <- list()

	for (j  in 1 : length(chrom))
	{
	data.temp.oris <- data.oris[(data.oris$CHROMOSOME == chrom[j]),]
		
	#data.early  <- data.temp.oris[data.temp.oris$TIMING == "EARLY",]
	#data.late   <- data.temp.oris[data.temp.oris$TIMING == "LATE",]
	taille      <- data.temp.oris$LENGTH
	taille.mean <- round(mean(data.temp.oris$LENGTH), 2)
	taille.med  <- round(median(data.temp.oris$LENGTH), 2)
	dist.tt     <- distance(data.temp.oris)$distance
	dist.mean   <- distance(data.temp.oris)$dist.mean
	dist.med    <- distance(data.temp.oris)$dist.med

	print(chrom[j])
	cat(paste("Longueur moyenne des origines :\n", taille.mean, "\n", sep = ""))
	cat(paste("Longueur mediane des origines :\n", taille.med, "\n\n", sep = ""))
	#cat(paste("Longueur moyenne des origines dans les regions early :\n", round(mean(data.early$LENGTH), 2), "\n", sep = ""))
	#cat(paste("Longueur mediane des origines dans les regions early :\n", round(median(data.early$LENGTH), 2), "\n\n", sep = ""))
	#cat(paste("Longueur moyenne des origines dans les regions late :\n", round(mean(data.late$LENGTH), 2), "\n", sep = ""))
	#cat(paste("Longueur mediane des origines dans les regions late :\n", round(median(data.late$LENGTH), 2), "\n\n", sep = ""))
	cat(paste("Distance moyenne entre les origines :\n", round(dist.mean,2), "\n", sep = ""))
	cat(paste("Distance mediane entre les origines :\n", round(dist.med,2), "\n\n", sep = ""))

	res.taille <- c(res.taille, taille)
	res.dist.tt <- c(res.dist.tt, dist.tt)

	}
	res.tt <- list(res.taille, res.dist.tt)
	names(res.tt) <- c("LENGTH.ORIS", "DIST.ORIS")
	
	return(res.tt)


	### --- Regions early --- ###
	#levels.early          <- unique(data.early$NUM.TIMING)
	#res.nom.early         <- vector()
	#res.nb.early          <- vector()
	#res.taille.mean.early <- vector()
	#res.taille.med.early  <- vector()
	#res.dist.mean.early   <- vector()
	#res.dist.med.early    <- vector()
	#res.ori.start.early   <- vector()
	#res.ori.end.early     <- vector()
	#res.ori.middle.early  <- vector()
	#vec.dist.early        <- vector()
	#dist.inter.early      <- vector()

	#for(i in 1:length(levels.early))
	#	{
	#	data.early.temp            <- data.early[data.early$NUM.TIMING == levels.early[[i]],]
	#	data.early.temp            <- data.early.temp[order(data.early.temp$START, data.early.temp$END),]
	#	res.nom.early[[i]]         <- as.character(levels.early[[i]])
	#	res.nb.early[[i]]          <- dim(data.early.temp)[[1]]
	#	res.taille.mean.early[[i]] <- mean(data.early.temp$LENGTH)
	#	res.taille.med.early[[i]]  <- median(data.early.temp$LENGTH)
	#	res.dist.mean.early[[i]]   <- ifelse(dim(data.early.temp)[[1]] == 1 , NA, distance(data.early.temp)$dist.mean)
	#	res.dist.med.early[[i]]    <- ifelse(dim(data.early.temp)[[1]] == 1 , NA, distance(data.early.temp)$dist.med)
	#	res.ori.start.early[[i]]   <- min(data.early.temp$START)
	#	res.ori.end.early[[i]]     <- max(data.early.temp$END)
	#	res.ori.middle.early[[i]]  <- min(data.early.temp$START) + 0.5 *(max(data.early.temp$END)-min(data.early.temp$START))
	#	if(dim(data.early.temp)[[1]] > 1) {vec.dist.early <- c(vec.dist.early, distance(data.early.temp)$distance)}
	#	}
	#tot.dist.mean.early <- mean(vec.dist.early)
	#tot.dist.med.early  <- median(vec.dist.early)

	#cat(paste("Distance moyenne entre les origines dans les regions early :\n", round(tot.dist.mean.early, 2), "\n", sep = ""))
	#cat(paste("Distance mediane entre les origines dans les regions early :\n", round(tot.dist.med.early, 2), "\n\n", sep = ""))

	#res.early        <- cbind.data.frame("EARLY", res.nom.early, res.nb.early, round(res.taille.mean.early,2), round(res.taille.med.early,2), round(res.dist.mean.early,2), 
	#		    round(res.dist.med.early,2), res.ori.start.early, res.ori.end.early, res.ori.middle.early)
	#names(res.early) <- c("TIMING", "NUM.TIMING", "NB.ORIS", "TAILLE.MEAN", "TAILLE.MED", "DIST.MEAN", "DIST.MED", "START" ,"END", "MIDDLE")

	#if(dim(res.early)[[1]] > 1)
	#	{
	#	dist.mean.early <- distance(res.early)$dist.mean
	#	dist.med.early  <- distance(res.early)$dist.med

	#	cat(paste("Distance moyenne entre les clusters de regions early :\n", round(dist.mean.early,2), "\n", sep = ""))
	#	cat(paste("Distance mediane entre les clusters de regions early :\n", round(dist.med.early,2), "\n\n", sep = ""))

	#	dist.inter.early <- res.early$MIDDLE[2:dim(res.early)[[1]]] - res.early$MIDDLE[1:(dim(res.early)[[1]] - 1)]
	#	dist2.mean.early <- mean(dist.inter.early)
	#	dist2.med.early  <- median(dist.inter.early)

	#	cat(paste("Distance moyenne entre les milieux de clusters de regions early :\n", round(dist2.mean.early,2), "\n", sep = ""))
	#	cat(paste("Distance mediane entre les milieux de clusters de regions early :\n", round(dist2.med.early,2), "\n\n", sep = ""))
	#	}
	#else
	#	{
	#	cat("Pas d'origines dans au moins deux regions early\n\n")
	#	}

	### --- Regions late --- ###
	#levels.late          <- unique(data.late$NUM.TIMING)
	#res.nom.late         <- vector()
	#res.nb.late          <- vector()
	#res.taille.mean.late <- vector()
	#res.taille.med.late  <- vector()
	#res.dist.mean.late   <- vector()
	#res.dist.med.late    <- vector()
	#res.ori.start.late   <- vector()
	#res.ori.end.late     <- vector()
	#res.ori.middle.late  <- vector()
	#vec.dist.late        <- vector()
	#dist.inter.late      <- vector()

	#for(i in 1:length(levels.late))
	#	{
	#	data.late.temp            <- data.late[data.late$NUM.TIMING == levels.late[[i]],]
	#	data.late.temp            <- data.late.temp[order(data.late.temp$START, data.late.temp$END),]
	#	res.nom.late[[i]]         <- as.character(levels.late[[i]])
	#	res.nb.late[[i]]          <- dim(data.late.temp)[[1]]
	#	res.taille.mean.late[[i]] <- mean(data.late.temp$LENGTH)
	#	res.taille.med.late[[i]]  <- median(data.late.temp$LENGTH)
	#	res.dist.mean.late[[i]]   <- ifelse(dim(data.late.temp)[[1]] == 1 , NA, distance(data.late.temp)$dist.mean)
	#	res.dist.med.late[[i]]    <- ifelse(dim(data.late.temp)[[1]] == 1 , NA, distance(data.late.temp)$dist.med)
	#	res.ori.start.late[[i]]   <- min(data.late.temp$START)
	#	res.ori.end.late[[i]]     <- max(data.late.temp$END)
	#	res.ori.middle.late[[i]]  <- min(data.late.temp$START) + 0.5 *(max(data.late.temp$END)-min(data.late.temp$START))
	#	if(dim(data.late.temp)[[1]] > 1) {vec.dist.late <- c(vec.dist.late, distance(data.late.temp)$distance)}
	#	}
	#tot.dist.mean.late <- mean(vec.dist.late)
	#tot.dist.med.late  <- median(vec.dist.late)

	#cat(paste("Distance moyenne entre les origines dans les regions late :\n", round(tot.dist.mean.late, 2), "\n", sep = ""))
	#cat(paste("Distance mediane entre les origines dans les regions late :\n", round(tot.dist.med.late, 2), "\n\n", sep = ""))

	#res.late        <- cbind.data.frame("LATE", res.nom.late, res.nb.late, round(res.taille.mean.late,2), round(res.taille.med.late,2), round(res.dist.mean.late,2), 
	#		   round(res.dist.med.late,2), res.ori.start.late, res.ori.end.late, res.ori.middle.late)
	#names(res.late) <- c("TIMING", "NUM.TIMING", "NB.ORIS", "TAILLE.MEAN", "TAILLE.MED", "DIST.MEAN", "DIST.MED", "START" ,"END", "MIDDLE")

	#if(dim(res.late)[[1]] > 1)
	#	{
	#	dist.mean.late <- distance(res.late)$dist.mean
	#	dist.med.late  <- distance(res.late)$dist.med

	#	cat(paste("Distance moyenne entre les clusters de regions late :\n", round(dist.mean.late,2), "\n", sep = ""))
	#	cat(paste("Distance mediane entre les clusters de regions late :\n", round(dist.med.late,2), "\n\n", sep = ""))

	#	dist.inter.late <- res.late$MIDDLE[2:dim(res.late)[[1]]] - res.late$MIDDLE[1:(dim(res.late)[[1]] - 1)]
	#	dist2.mean.late <- mean(dist.inter.late)
	#	dist2.med.late  <- median(dist.inter.late)

	#	cat(paste("Distance moyenne entre les milieux de clusters de regions late :\n", round(dist2.mean.late,2), "\n", sep = ""))
	#	cat(paste("Distance mediane entre les milieux de clusters de regions late :\n", round(dist2.med.late,2), "\n\n", sep = ""))
	#	}
	#else
	#	{
	#	cat("Pas d'origines dans au moins deux regions late\n\n")
	#	}

	### On rassemble les donnees dans la meme table --- ###
	#res <- rbind(res.early, res.late)

	#res.tt        <- list(res, taille, dist.tt, vec.dist.early, vec.dist.late, dist.inter.early, dist.inter.late)
	#names(res.tt) <- c("RECAP", "LENGTH.ORIS", "DIST.ORIS", "DIST.INTRA.EARLY", "DIST.INTRA.LATE", "DIST.INTER.EARLY", "DIST.INTER.LATE")

#        res.tt <-list(taille, dist.tt)
#        names(res.tt) <- c( "LENGTH.ORIS", "DIST.ORIS")

#	return(res.tt)

#	}
	
}


#***********************************************************************************************************************************************#
# FONCTION plot.region  -> representation de la distribution de la taille des oris et des distances inter-oris
#***********************************************************************************************************************************************#

plot.region  <- function(data.name, file, format='jpeg')

	#########################################################################################################################################
	# x		<- Les donnees dont on veut la representation del a distribution
	# format	<- Le format de sortie du graphique
	#########################################################################################################################################
{
	nom     <- deparse(substitute(data.name))
	
	data <- scan(file)
	
        # --- Creation d'un fichier ou mettre les donnees (si il n'existe pas) --- #
        nom.doss <- paste("Graphiques -",format)
        safe.dir.create(nom.doss)
   
	format.fichier(paste(nom.doss, "/Histogramme - ", nom, sep=""), type=format, width=1200, height=600)

	hist(data, ylab='Frequence', breaks="Sturges", xlab=nom, col="turquoise4")
	dev.off()
	cat("Creation de l'histogramme de frequence.\n")
	
}







