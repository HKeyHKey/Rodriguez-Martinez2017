#################################################################################################################################################
# FONCTIONS CONTENUES DANS LE SCRIPT
#################################################################################################################################################
# format.fichier 	<- function(titre, type=c("", "ps", "pdf", "png", "jpeg", "bmp"), width=NULL, height=NULL, bg = "white",...)
# safe.dir.create 	<- function(path)

# Fonctions pour analyse des réplicats en combinant les pvalues avec le modele hierarchique bayesien apres normalisation intra-puce:
# eBayes.one.sided <- function(fit,proportion=0.01,stdev.coef.lim=c(0.1,4),lower.tail=FALSE)
# ebayes.one.sided <- function(fit,proportion=0.01,stdev.coef.lim=c(0.1,4),lower.tail=FALSE)
# analyse.replicats.eBayes <- function(rep1, rep2, rep3=NULL, rep4=NULL, rep5=NULL, rep6=NULL, nom.rep1=NULL, nom.rep2=NULL, nom.rep3=NULL, nom.rep4=NULL, nom.rep5=NULL, nom.rep6=NULL, 
#			      data.M="M.NORM", data.A="A.NORM", method.between.array ="scale", window = 5, adjust=TRUE,lower.tail=FALSE)
# plot.replicats <- function(replicats, seuil = 0.05, seuil.adj = 0.05, adjust = FALSE, format = "jpeg")
# plot.cor.replicats <- function(replicat1,replicat2,replicat3=NULL,signal="M.NORM",titre="Nuage de points du signal entre replicats", seuil=0.05,width=600,height=600)
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-#
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-#


#***********************************************************************************************************************************************#
# FONCTION FORMAT.FICHIER -> Pour choisir le format des graphiques en sortie
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Cette fonction permet de choisir le format des graphiques en sortie. Il y a le choix parmi 5 formats différents : 
# postscript ("ps"), PDF ("pdf"), PNG ("png"), JPEG ("jpeg") ou bitmap ("bmp").
#################################################################################################################################################

format.fichier <- function(titre, type=c("", "ps", "pdf", "png", "jpeg", "bmp"), width=NULL, height=NULL, bg = "white",...)
	
	#########################################################################################################################################
	# titre 	-> 	Le nom que l'on veut donner au fichier graphique en sortie. Le nom doit etre entre guillemets et il n'y a pas 
	#			besoin d'indiquer l'extension.
	# type		-> 	Le format du fichier que l'on veut en sortie.
	# width 	->	La largeur du graphique en sortie.
	# height	->	La hauteur du graphique en sortie.
	# bg		->	La couleur du fond du graphique.
	# ...		->	Les autres paramètres possibles qui peuvent etre pris en paramtre par les fonctions utilisées par la suite.
	#
	# Les formats disponibles sont les suivants :
	#	- "ps" : 		fichier au format '.ps' (par défaut)
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
# FONCTION SAFE.DIR.CREATE -> Pour créer un dossier dans le répertoire courant (celui dans lequel on travail)
#***********************************************************************************************************************************************#

#################################################################################################################################################
# Cette fonction permet de créer un nouveau dossier dans le répertoire courant en utilisant la commande "safe.dir.create(file.path(name="XXX"))" 
# oà  ¹ le paramètre "name" correspond au nom que l'on veut donner au dossier que l'on veut créer. Cette fonction est issue de la fonction R cran
# "package.skeleton" qui permet de créer un package.
#################################################################################################################################################

safe.dir.create <- function(path)
	
	#########################################################################################################################################
	# path 	-> 	Le nom du dossier que l'on veut créer dans le répertoire courant.
	#########################################################################################################################################
{
	dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir
	if (!dirTest(path) && !dir.create(path)) 
	stop(gettextf("cannot create directory '%s'", path), domain = NA)
}


#***********************************************************************************************************************************************#
# FONCTION E.BAYES.ONE.SIDED -> Fonction analogue à la fonction "eBayes" du package Limma, à la seule difference que la pvalue de la statistique
# de Student est calculée de manière unilatérale et non bilaterale (pvaluei= = 2*Prob[-|ti| <= LoiStudent]):
# si lower.tail=TRUE alors pvaluei = Prob[ti <= LoiStudent],
# si lower.tail=FALSE alors  pvaluei = Prob[ti >= LoiStudent] (par defaut lower.tail=FALSE)
# Note : Par rapport à la fonction "eBayes" du package Limma, les calculs concernant la statistique F ont été supprimés.
# Plus précisément, les lignes suivantes ont été supprimées:
#	if(!is.null(fit$design) && is.fullrank(fit$design)) {
#		F.stat <- classifyTestsF(fit,fstat.only=TRUE)
#		fit$F <- as.vector(F.stat)
#		df1 <- attr(F.stat,"df1")
#		df2 <- attr(F.stat,"df2")
#		if(df2[1] > 1e6) # Work around bug in R 2.1
#			fit$F.p.value <- pchisq(df1*fit$F,df1,lower.tail=FALSE)
#		else
#			fit$F.p.value <- pf(fit$F,df1,df2,lower.tail=FALSE)
#***********************************************************************************************************************************************#

eBayes.one.sided <- function(fit,proportion=0.01,stdev.coef.lim=c(0.1,4),lower.tail=FALSE)
{
#	Empirical Bayes statistics to select differentially expressed genes
#	Object orientated version
#	Gordon Smyth
#	4 August 2003.  Last modified 24 August 2005.
	eb <- ebayes.one.sided(fit=fit,proportion=proportion,stdev.coef.lim=stdev.coef.lim,lower.tail=lower.tail) 
	fit$df.prior <- eb$df.prior
	fit$s2.prior <- eb$s2.prior
	fit$var.prior <- eb$var.prior
	fit$proportion <- proportion
	fit$s2.post <- eb$s2.post
	fit$t <- eb$t
	fit$p.value <- eb$p.value
	fit$lods <- eb$lods
	fit
}

ebayes.one.sided <- function(fit,proportion=0.01,stdev.coef.lim=c(0.1,4), lower.tail=FALSE)
 {
#	Empirical Bayes statistics to select differentially expressed genes
#	Gordon Smyth
#	8 Sept 2002.  Last revised 13 April 2009.

	coefficients <- fit$coefficients
	stdev.unscaled <- fit$stdev.unscaled
	sigma <- fit$sigma
	df.residual <- fit$df.residual
	if(is.null(coefficients) || is.null(stdev.unscaled) || is.null(sigma) || is.null(df.residual)) stop("No data, or argument is not a valid lmFit object")
	if(all(df.residual==0)) stop("No residual degrees of freedom in linear model fits")
	if(all(!is.finite(sigma))) stop("No finite residual standard deviations")

#	Moderated t-statistic
	out <- squeezeVar(sigma^2, df.residual)
	out$s2.prior <- out$var.prior
	out$s2.post <- out$var.post
	out$var.prior <- out$var.post <- NULL
	df.total <- df.residual + out$df.prior
	out$t <- coefficients / stdev.unscaled / sqrt(out$s2.post)
	# Le calcul suivant a été modifié pour obtenir un test unilateral (à gauche ou à droite):
	#out$p.value <- 2*pt(-abs(out$t),df=df.total) ### Calcul obtenu avec la focntion eBayes du package Limma
	out$p.value <- pt(out$t,df=df.total,lower.tail=lower.tail)
	
	
#	B-statistic
	var.prior.lim <- stdev.coef.lim^2/out$s2.prior
	out$var.prior <- tmixture.matrix(out$t,stdev.unscaled,df.total,proportion,var.prior.lim)
	if(any(is.na(out$var.prior))) {
		out$var.prior[ is.na(out$var.prior) ] <- 1/out$s2.prior
		warning("Estimation of var.prior failed - set to default value")
	}
	r <- rep(1,NROW(out$t)) %o% out$var.prior
	r <- (stdev.unscaled^2+r) / stdev.unscaled^2
	t2 <- out$t^2
	if(out$df.prior > 10^6)
		kernel <- t2*(1-1/r)/2
	else
		kernel <- (1+df.total)/2*log((t2+df.total) / (t2/r+df.total))
	out$lods <- log(proportion/(1-proportion))-log(r)/2+kernel
	out
}

#************************************************************************************************************************************************************************************#
# FONCTION ANALYSE.REPLICATS.EBAYES -> Analyse des réplicats d'une même condition expérimentale : Calcul dune p-value globale en utilisant le modele hierarchique empirique Bayesien 
# Version 2 : Correction concernant la normalisation inter-puces : Elle s'effectue directement sur le signal M et A respectivement en utilisant les fonctions 
# 			  normalizeMedianAbsValues() (resp. normalizeQuantiles ()) si method.between.array ="scale" (resp. "quantile").
# 			  
#			  Rajout du paramètre vect.col représentant le vecteur des colonnes d'interet. 
#			  Par défaut : vect.col <- c(data.A,"LOGR","LOGV.NORM",data.M)
# Note : 
# La fonction "normalizeBetweenArrays" du package limma (initialement utilisé) effectue la normalisation sur les signaux G (green) et R (red) normalisés [matrice G R]. 
# Les fonctions normalizeMedianAbsValues() et normalizeQuantiles() appartiennent au package limma.
#************************************************************************************************************************************************************************************#

#######################################################################################################################################################################
# Après avoir effectué une normalisation intra-puce (ex: noumalisation lowess +/- fenetre glissante ), 
# cette fonction prend en paramètres jusqu'à 6 replicats d'une même condition expérimentale 
# et permet dobtenir une table contenant une pvalue globale après ou non corrections multiples en effectuant les étapes suivantes :
# - ETAPE1 : Normalisation inter-puces - 2 choix de méthodes ( method.between.array ="scale" and "quantile" ) (Cf. la fonction "normalizeBetweenArrays" du package limma)
# - ETAPE2 : Application dun modèle linéaire pour chaque probe (Cf. la fonction lmFit du package limma)
# - ETAPE3 : Calcul dun estimateur de variance pour chaque probe i (Empirical Bayes shrinkage of the standard errors towards a common value),
#   afin dobtenir une statistique de test ti et pvaluei associée  (Cf. la fonction eBayes du package limma et la variante unilatérale eBayes.onesided)
# - ETAPE4: Si adjust=TRUE : Calcul de la pvalue corrigée par la methode "fdr" de corrections multiples de Benjamini et Hochberg (Cf. la fonction p.adjust)
#########################################################################################################################################################################

analyse.replicats.eBayes <- function(rep1, rep2, rep3=NULL, rep4=NULL, rep5=NULL, rep6=NULL, nom.rep1=NULL, nom.rep2=NULL, nom.rep3=NULL, nom.rep4=NULL, nom.rep5=NULL, nom.rep6=NULL, 
			      data.M="M.NORM", data.A="A.NORM", method.between.array ="scale", adjust=TRUE, lower.tail=FALSE, vect.col = NULL)

	#########################################################################################################################################
	# rep1		->  1er réplicat. 
	# rep2 		-> 2ème réplicat.
	# rep3		-> 3ème réplicat.
	# rep4		-> 4ème réplicat.
	# rep5		-> 5ème réplicat.
	# rep6		-> 6ème réplicat.
	# data.M 	-> nom du signal M normalisé après normalisation intra-puce (ex: normalisation lowess)
	# data.A 	-> nom du signal A
	# method.between.array	-> "quantile" ou "scale" : choix de la méthode pour la normalisation inter-puce ("scale" par defaut) 
	# The idea is simply to scale the log-ratios to have the same median-abolute-deviation (MAD) across arrays.
	# adjust	-> si adjust = TRUE, les p-values obtenues sont ajustées par la méthode FDR.
	# lower.tail -> si lower.tail=FALSE : test unilatéral à droite, si lower.tail=TRUE : test unilatéral à gauche
	# vect.col 	-> vecteur des colonnes d'interêt. Par défaut : vect.col <- c(data.A,"LOGR","LOGV.NORM",data.M)
	########################################################################################################################################
{
	
		if(is.null(vect.col) == TRUE)	{vect.col	<- c(data.A,"LOGR","LOGV.NORM",data.M)}
	 
	### --- Récupération des noms des réplicats --- ###
        if(is.null(nom.rep1) == TRUE)	{nom.rep1	<- deparse(substitute(rep1))}
        if(is.null(nom.rep2) == TRUE)	{nom.rep2	<- deparse(substitute(rep2))}
        if(is.null(nom.rep3) == TRUE && is.null(rep3) == FALSE)	{nom.rep3	<- deparse(substitute(rep3))}
        if(is.null(nom.rep4) == TRUE && is.null(rep4) == FALSE)	{nom.rep4	<- deparse(substitute(rep4))}
        if(is.null(nom.rep5) == TRUE && is.null(rep5) == FALSE)	{nom.rep5	<- deparse(substitute(rep5))}
        if(is.null(nom.rep6) == TRUE && is.null(rep6) == FALSE)	{nom.rep6	<- deparse(substitute(rep6))}
	
	
	####################################
	###### ETAPE 0 : Mise en forme #####
	####################################
	# --- Concaténation des fichiers des réplicats dans une seule et même table --- #
	if(is.null(rep6)) # Si on a 5 réplicats
		{
		if(is.null(rep5)) # Si on a 4 réplicats
			{
			if(is.null(rep4)) # Si on a 3 réplicats
				{
				if(is.null(rep3)) # Si on a 2 réplicats
					{
					# --- 2 réplicats --- #
					replicats <- merge(rep1[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)],
							   rep2[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)],
							   by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
							   by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
							   all=TRUE, suffixes = c(paste(".",nom.rep1,sep=""),paste(".",nom.rep2,sep="")))
					cat(paste("Il y a 2 replicats :", nom.rep1, "et", nom.rep2,"\n\n"))
					}
				else
					{
					# --- 3 réplicats --- #
					replicats <- 	merge(	rep1[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)], 
							
							
							merge(	rep2[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)],
								rep3[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)],
								by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
								by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
								all=TRUE, suffixes = c(paste(".",nom.rep2,sep=""),paste(".",nom.rep3,sep=""))),
								by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
								by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),all=TRUE)
							for (i in 1:length(vect.col))
							{ names(replicats)[[i+4]] 	<- paste(names(replicats)[[i+4]],".",nom.rep1,sep="")}
					cat(paste("Il y a 3 replicats :", nom.rep1, ",", nom.rep2, "et", nom.rep3,"\n\n"))
					
					#print(names(replicats))		
					}
				}
				
		
			else
				{
				# --- 4 réplicats --- #
				replicats <- 	merge( 	merge(	rep1[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)], 
							rep2[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)],
							by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"), 
							by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
							all=TRUE, suffixes = c(paste(".",nom.rep1,sep=""),paste(".",nom.rep2,sep=""))),
						merge(	rep3[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)], 
							rep4[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)],
							by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
							by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
							all=TRUE, suffixes = c(paste(".",nom.rep3,sep=""),paste(".",nom.rep4,sep=""))),
							by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
							by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),all=TRUE)
				cat(paste("Il y a 4 replicats :", nom.rep1, ",", nom.rep2, ",", nom.rep3, "et", nom.rep4,"\n\n"))
				}
			}
		else
			{
			# --- 5 réplicats --- #
			replicats <- 	merge(	rep1[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)],
					merge( 	merge(	rep2[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)], 
							rep3[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)],
							by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
							by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
							all=TRUE, suffixes = c(paste(".",nom.rep2,sep=""),paste(".",nom.rep3,sep=""))),
						merge(	rep4[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)], 
							rep5[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)],
							by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"), 
							by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"), 
							all=TRUE, suffixes = c(paste(".",nom.rep4,sep=""),paste(".",nom.rep5,sep=""))),
							by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"), 
							by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),all=TRUE),
							by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"), 
							by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),all=TRUE)
					#  Rajout des suffixes pour le replicat 1 (Avec n impair, loption suffixe de merge ne suffit pas) 
					for (i in 1:length(vect.col))
					{ names(replicats)[[i+6]] 	<- paste(names(replicats)[[i+6]],".",nom.rep1,sep="")}
			cat(paste("Il y a 5 replicats :", nom.rep1, ",", nom.rep2, ",", nom.rep3, ",", nom.rep4, "et", nom.rep5,"\n\n"))
			}
		}
	else
		{
		# --- 6 réplicats --- #
		replicats <- 	merge(	merge(rep1[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)],
					      rep2[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)],
					      by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
					      by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),all=TRUE, 
					      suffixes = c(paste(".",nom.rep1,sep=""),paste(".",nom.rep2,sep=""))),
					merge(merge(rep3[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)], 
						    rep4[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)],
						    by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
						    by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),all=TRUE,
						    suffixes = c(paste(".",nom.rep3,sep=""),paste(".",nom.rep4,sep=""))),
						merge(rep5[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)], 
						      rep6[c("PROBE_ID","CHROMOSOME","POSITION","LENGTH",vect.col)],
						      by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
						      by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),all=TRUE, 
						      suffixes = c(paste(".",nom.rep5,sep=""),paste(".",nom.rep6,sep=""))),
						      by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
						      by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),all=TRUE),
						      by.x=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),
						      by.y=c("PROBE_ID","CHROMOSOME","POSITION","LENGTH"),all=TRUE)
			cat(paste("Il y a 6 replicats :", nom.rep1, ",", nom.rep2, ",", nom.rep3, ",", nom.rep4, ",", nom.rep5,"et", nom.rep6,"\n\n"))
		}
		
	# --- Mise en forme pour avoir un objet MA-list --- #
	# Design
	PROBE_ID<-replicats$PROBE_ID
	POSITION<-replicats$POSITION
	CHROMOSOME<-replicats$CHROMOSOME
	LENGTH<-replicats$LENGTH
	ID <-cbind(CHROMOSOME,PROBE_ID,POSITION,LENGTH)

	# Signal A, Signal M, design 
	if(is.null(rep6)) # Si on a 5 réplicats
		{
		if(is.null(rep5)) # Si on a 4 réplicats
			{
			if(is.null(rep4)) # Si on a 3 réplicats
				{
				if(is.null(rep3)) # Si on a 2 réplicats
					{
					# --- 2 réplicats --- #
					A.puce1 <-replicats[,c(paste(data.A,".",nom.rep1,sep=""))]
					A.puce2 <-replicats[,c(paste(data.A,".",nom.rep2,sep=""))]
					A <-cbind(A.puce1,A.puce2)
					colnames(A) <- c(c(paste(data.A,".between.",nom.rep1,sep="")),c(paste(data.A,".between.",nom.rep2,sep="")))	
										
					M.puce1 <-replicats[,c(paste(data.M,".",nom.rep1,sep=""))]
					M.puce2 <-replicats[,c(paste(data.M,".",nom.rep2,sep=""))]
					M <-cbind(M.puce1,M.puce2)
					colnames(M) <- c(c(paste(data.M,".between.",nom.rep1,sep="")),c(paste(data.M,".between.",nom.rep2,sep="")))	
										
					design<-rep(1,2)
					
					}
				else
					{
					# --- 3 réplicats --- #
					A.puce1 <-replicats[,c(paste(data.A,".",nom.rep1,sep=""))]
					A.puce2 <-replicats[,c(paste(data.A,".",nom.rep2,sep=""))]
					A.puce3 <-replicats[,c(paste(data.A,".",nom.rep3,sep=""))]
					A <-cbind(A.puce1,A.puce2,A.puce3)
					colnames(A) <- c(c(paste(data.A,".between.",nom.rep1,sep="")),c(paste(data.A,".between.",nom.rep2,sep="")),c(paste(data.A,".between.",nom.rep3,sep="")))	
																				
					M.puce1 <-replicats[,c(paste(data.M,".",nom.rep1,sep=""))]
					M.puce2 <-replicats[,c(paste(data.M,".",nom.rep2,sep=""))]
					M.puce3 <-replicats[,c(paste(data.M,".",nom.rep3,sep=""))]
					M <-cbind(M.puce1,M.puce2,M.puce3)
					colnames(M) <- c(c(paste(data.M,".between.",nom.rep1,sep="")),c(paste(data.M,".between.",nom.rep2,sep="")),c(paste(data.M,".between.",nom.rep3,sep="")))	
					
					design<-rep(1,3)
					}
				}
				else
					{
					# --- 4 réplicats --- #		
					A.puce1 <-replicats[,c(paste(data.A,".",nom.rep1,sep=""))]
					A.puce2 <-replicats[,c(paste(data.A,".",nom.rep2,sep=""))]
					A.puce3 <-replicats[,c(paste(data.A,".",nom.rep3,sep=""))]
					A.puce4 <-replicats[,c(paste(data.A,".",nom.rep4,sep=""))]
					A <-cbind(A.puce1,A.puce2,A.puce3,A.puce4)
					colnames(A) <- c(c(paste(data.A,".between.",nom.rep1,sep="")),c(paste(data.A,".between.",nom.rep2,sep="")),c(paste(data.A,".between.",nom.rep3,sep="")),
									 c(paste(data.A,".between.",nom.rep4,sep="")))	
															
					M.puce1 <-replicats[,c(paste(data.M,".",nom.rep1,sep=""))]
					M.puce2 <-replicats[,c(paste(data.M,".",nom.rep2,sep=""))]
					M.puce3 <-replicats[,c(paste(data.M,".",nom.rep3,sep=""))]
					M.puce4 <-replicats[,c(paste(data.M,".",nom.rep4,sep=""))]
					M <-cbind(M.puce1,M.puce2,M.puce3,M.puce4)
					colnames(M) <- c(c(paste(data.M,".between.",nom.rep1,sep="")),c(paste(data.M,".between.",nom.rep2,sep="")),c(paste(data.M,".between.",nom.rep3,sep="")),
									 c(paste(data.M,".between.",nom.rep4,sep="")))
					
					design<-rep(1,4)
					}
			}
				else
					{
					# --- 5 réplicats --- #
					A.puce1 <-replicats[,c(paste(data.A,".",nom.rep1,sep=""))]
					A.puce2 <-replicats[,c(paste(data.A,".",nom.rep2,sep=""))]
					A.puce3 <-replicats[,c(paste(data.A,".",nom.rep3,sep=""))]
					A.puce4 <-replicats[,c(paste(data.A,".",nom.rep4,sep=""))]
					A.puce5 <-replicats[,c(paste(data.A,".",nom.rep5,sep=""))]
					A <-cbind(A.puce1,A.puce2,A.puce3,A.puce4,A.puce5)
					colnames(A) <- c(c(paste(data.A,".between.",nom.rep1,sep="")),c(paste(data.A,".between.",nom.rep2,sep="")),c(paste(data.A,".between.",nom.rep3,sep="")),
									 c(paste(data.A,".between.",nom.rep4,sep="")),c(paste(data.A,".between.",nom.rep5,sep="")))	
										
					M.puce1 <-replicats[,c(paste(data.M,".",nom.rep1,sep=""))]
					M.puce2 <-replicats[,c(paste(data.M,".",nom.rep2,sep=""))]
					M.puce3 <-replicats[,c(paste(data.M,".",nom.rep3,sep=""))]
					M.puce4 <-replicats[,c(paste(data.M,".",nom.rep4,sep=""))]
					M.puce5 <-replicats[,c(paste(data.M,".",nom.rep5,sep=""))]
					M <-cbind(M.puce1,M.puce2,M.puce3,M.puce4,M.puce5)
					colnames(M) <- c(c(paste(data.M,".between.",nom.rep1,sep="")),c(paste(data.M,".between.",nom.rep2,sep="")),c(paste(data.M,".between.",nom.rep3,sep="")),
									 c(paste(data.M,".between.",nom.rep4,sep="")),c(paste(data.M,".between.",nom.rep5,sep="")))	
					
					design<-rep(1,5)
					}
		}
				else
				{
				# --- 6 réplicats --- #
					A.puce1 <-replicats[,c(paste(data.A,".",nom.rep1,sep=""))]
					A.puce2 <-replicats[,c(paste(data.A,".",nom.rep2,sep=""))]
					A.puce3 <-replicats[,c(paste(data.A,".",nom.rep3,sep=""))]
					A.puce4 <-replicats[,c(paste(data.A,".",nom.rep4,sep=""))]
					A.puce5 <-replicats[,c(paste(data.A,".",nom.rep5,sep=""))]
					A.puce6 <-replicats[,c(paste(data.A,".",nom.rep6,sep=""))]
					A <-cbind(A.puce1,A.puce2,A.puce3,A.puce4,A.puce5,A.puce6)
					colnames(A) <- c(c(paste(data.A,".between.",nom.rep1,sep="")),c(paste(data.A,".between.",nom.rep2,sep="")),c(paste(data.A,".between.",nom.rep3,sep="")),
									 c(paste(data.A,".between.",nom.rep4,sep="")),c(paste(data.A,".between.",nom.rep5,sep="")),c(paste(data.A,".between.",nom.rep6,sep="")) )	
										
					M.puce1 <-replicats[,c(paste(data.M,".",nom.rep1,sep=""))]
					M.puce2 <-replicats[,c(paste(data.M,".",nom.rep2,sep=""))]
					M.puce3 <-replicats[,c(paste(data.M,".",nom.rep3,sep=""))]
					M.puce4 <-replicats[,c(paste(data.M,".",nom.rep4,sep=""))]
					M.puce5 <-replicats[,c(paste(data.M,".",nom.rep5,sep=""))]
					M.puce6 <-replicats[,c(paste(data.M,".",nom.rep6,sep=""))]
					M <-cbind(M.puce1,M.puce2,M.puce3,M.puce4,M.puce5,M.puce6)
					colnames(M) <- c(c(paste(data.M,".between.",nom.rep1,sep="")),c(paste(data.M,".between.",nom.rep2,sep="")),c(paste(data.M,".between.",nom.rep3,sep="")),
									 c(paste(data.M,".between.",nom.rep4,sep="")),c(paste(data.M,".between.",nom.rep5,sep="")),c(paste(data.M,".between.",nom.rep6,sep="")) )	
					
					design<-rep(1,6)
				}
				
	# Objet MA-list
	MA <- list(ID,M,A)
	names(MA)<-c("ID",data.M,data.A)

################################################
###### ETAPE 1 : Normalisation inter-puces #####
################################################
# -- Normalisation inter-puces et calcul de la moyenne M.comb.eBayes et A.comb.eBayes entre tous les réplicats --- #
if(method.between.array == "scale")
{
M.norm.between <- normalizeMedianAbsValues(MA$M)
A.norm.between <- normalizeMedianAbsValues(MA$A)

M.comb.eBayes <- apply(M.norm.between,1,mean)
A.comb.eBayes <- apply(A.norm.between,1,mean)
}

if(method.between.array == "quantile")
{
M.norm.between <- normalizeQuantiles(MA$M)
A.norm.between <- normalizeQuantiles(MA$A)

M.comb.eBayes <- apply(M.norm.between,1,mean)
A.comb.eBayes <- apply(A.norm.between,1,mean)
}


# --- Rajout des colonnes "data.M.between.rep1", "data.M.between.rep2" ...  et "A.comb.eBayes","M.comb.eBayes" à la table replicats  ---*
replicats <- cbind(replicats, A.comb.eBayes, M.norm.between, M.comb.eBayes)

# Creation de l objet MA-list apres normalisation inter-puces
MA.norm.between <- list(ID,M.norm.between,A.norm.between)
names(MA.norm.between)<-c("ID",data.M,data.A)


###########################################################
###### ETAPE 2 : Modele Lineaire ##########################
# Fit linear model for each probe given a series of arrays#
###########################################################
MA.fit<-lmFit(MA.norm.between,design)


################################################
###### ETAPE 3 : Empirical Bayes shrinkage #####
################################################
MA.fit<-eBayes.one.sided(MA.fit,lower.tail=lower.tail)
	
# --- Rajout de la colonne pvalue à la table replicats --- "
colnames(MA.fit$p.value) <- "p.value"
replicats <- cbind(replicats,MA.fit$p.value)


#########################################################################
###### ETAPE 4 : Calcul de la pvalue ajustée avec la correction FDR #####
#########################################################################	
if(adjust == TRUE)
		{replicats$p.value.adj <- p.adjust(replicats$p.value,method="fdr")}
		
# --- Nommage des colonnes pvalues et pvalues.adj en fonction de largument lower.tail --- #
indice 		<- which(names(replicats) == "p.value")
indice.adj  <- which(names(replicats) == "p.value.adj")

if(lower.tail == TRUE)
	{
	names(replicats)[[indice]] <- "pval.comb.eBayes.neg"
	names(replicats)[[indice.adj]] <- "pval.comb.eBayes.neg.adj"
	}
else
	{
	names(replicats)[[indice]] <- "pval.comb.eBayes"
	names(replicats)[[indice.adj]] <- "pval.comb.eBayes.adj"
	}

return(replicats)
}


#***********************************************************************************************************************************************#
# FONCTION plot.cor.replicats -> Trace les nuages de points d'un signal entre 2 ou 3 replicats de puces
#***********************************************************************************************************************************************#

plot.cor.replicats <- function(replicat1,replicat2,replicat3=NULL,signal="M.NORM",titre="Nuage de points du signal entre replicats",width=600,height=600)

	#################################################################################################################################################
	# replicat1 : donnes de puces du 1er replicat (data.frame)
	# replicat2 : donnes de puces du 2e replicat (data.frame)
	# replicat3 : donnes de puces du 3e replicat (data.frame)
	# signal : nom de la colonne des tables replicat{1/2/3} utilisée pour tracer le nuage de point
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
	format.fichier(titre, type=format, width=width, height=height, pointsize=16)
	
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
	legend("topleft", legend=c(paste("R2 = ",round(r2,digits=2))), xjust=0, cex=1.2)
	
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
		legend("topleft", legend=c(paste("R2 = ",round(r2,digits=2))), xjust=0, cex=1.2)
	}
	dev.off()
}








