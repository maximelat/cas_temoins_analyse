Exemple de Comparaison de Patient témoins 
Avant tout 

# transformer virgules en points ## points virgules par tabulation ## enlever les accents ##
#Voir fichier exemple pour la mise en page
# Choisir le répertoire de travail dans lequel se trouve les fichiers puis copier coller le code ci dessous

# scriptRquery.r --> charge la fonction FULLBIS("exemple") pour lancer l'analyse complète 

# FULLBis() --> extrait les données du fichier csv "exemple.csv"" et les analyses 
# full() --> performe la synthèse finale 

#le test de wilcoxon, de welch ou de student est choisis en fonction des resultats du test de shapiro et de la comparason des variances

#testsSWW.R --> Performs one or two samples t-test to include in scriptRquery
#script_fonctions.R --> chargé par FULLBIS permet de créer toutes les fonctions 

#Scripts_FULL.R --> génère la synthèse finale créer les fonctions et extrait les données 

# permet de lancer l'analyse complète du fichier exemple.csv fournit
source("scriptRquery.r")
#permet de lancer analyse+ sur de nouveaux jeux de données "exemple.csv" et les fichiers 1 à 7 
donnees <- FULLBis(paste("exemple"))

Patients <- na.omit(donnees$Patients)
Temoins <- na.omit(donnees$Temoins)
full(Patients , Temoins, "exemple")

# Permet de réer l'analyse synthese++
source("Scripts_FULLRquery.r")
full(Patients , Temoins, "exemple",F,F)

#chaque test issus de Scripts_FULL.R
showdensityplot(Patients,Temoins)
Shapishapo(Echantillon)
AB(Echantillon)
Histogramme(Echantillon)
Student(Patients,Temoins)
ROC(Patients,Temoins)
