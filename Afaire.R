
# transformer virgules en points ## points virgules par tabulation ## enlever les accents ##
#Voir fichier exemple pour la mise en page
# Choisir le répertoire de travail dans lequel se trouve les fichiers puis copier coller le code ci dessous

#Importation du fichier contenant la fonction FULL effetuant tous les tests
source("Scripts_FULL.R")
#Importation du fichier contenant l'ensemble des fonctions séparéments
source("script_fonctions.R")


#Création du tableau (affectaiton à une variable)
tableau=read.table("exemple.csv",header=T,sep="\t")

#Affection du nom des colonnes à une variable
Temoins=na.omit(tableau$Temoins)
Patients=na.omit(tableau$Patients)
Echantillon=na.omit(tableau$Echantillon)


#ensemble des tests
full(Patients,Temoins,"Analyse")

#chaque test
Shapishapo(Echantillon)
AB(Echantillon)
Histogramme(Echantillon)
Student(Patients,Temoins)
ROC(Patients,Temoins)


