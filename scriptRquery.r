FULLBis<-function(DATASET){

##1###########################Import de données ###########################################################
DATASET=gsub(" ","",paste(DATASET,".csv"))
tableauFULL=read.table(gsub(" ","",paste("./rda/",DATASET)),header=T,sep="\t")
                        

require (ROCR); require(gplots) ; require(stats) ; require(utils) ; require(splines) ; require(pROC) ; require(Matrix) ; require(MASS) ; require(methods) ; require(grDevices) ; require(graphics) ; require(gplots) ; require(datasets)

source("./testSWW.R")

DATASET=gsub(" ","",gsub(".csv","",paste(DATASET,"")))
dir.create(gsub(" ","",paste("./figs/",DATASET,"/")))
dir.create(gsub(" ","",paste("./figs/",DATASET,"/synthese")))


##2############################# import colonnes ###########################################################

#Création du tableau (affectaiton à une variable)

#Affection du nom des colonnes à une variable
Temoins=na.omit(tableauFULL$Temoins)
Patients=na.omit(tableauFULL$Patients)

nbr.P=rep(1,length(Patients))
nbr.T=rep(0,length(Temoins))

##3################# TEST de normalité shapiro test si p-value<0,05 alors non normalité #########################
#shapiro test#
shapiro.test(Temoins)
shapiro.test(Patients)



#récupération des p-values#


p.value.Temoins=paste("Quartiles Théoriques \n p-value=",round(shapiro.test(Temoins)$p.value,6))
p.value.Patients=paste("Quartiles Théoriques \n p-value=",round(shapiro.test(Patients)$p.value,6))



#affichage des QQplot avec p-value du shapiro test
#Quantile-Quantile ou QQplot permet d'évaluer la pertinence de l'ajustement d'une distribution donnée à un modèle théorique.#
par(mfrow = c(1,2))



qqnorm(Temoins, ylab="Quartiles échantillons",xlab=p.value.Temoins,main="Temoins")
qqnorm(Patients, ylab="Quartiles échantillons",xlab=p.value.Patients,main="Patients")



dev.print(device = pdf, file = gsub(" ","",paste("./figs/",DATASET,"/1_QQplot-",DATASET,".pdf")), bg="white")

##3################# TEST de normalité shapiro test si p-value<0,05 alors non normalité #########################
#shapiro test#
#########

attribution.Test=tests.SWW(Temoins, Patients)
attribution.Test.T=attribution.Test$Test
attribution.Test.pvalue=attribution.Test$res[3]

dev.print(device = pdf, file = gsub(" ","",paste("./figs/",DATASET,"/Synthese/Synthese-Patient_Temoins-",DATASET,".pdf")), bg="white")


##################### Valeurs abbérantes ############
par(mfrow = c(1,2))


AB<-function(valeurs){
  x=1:length(valeurs);
  X=sample(x,length(valeurs),replace=TRUE)
  Y=valeurs
  
  regression=lm(Y~X);
  #On réalise une régréssion linéaire
  residus=rstudent(regression)
  #On calcule les residus
  plot(residus, type = "p", cex = 0.5, ylab = "Résidus studentisés ",  col = "springgreen2",  main=paste("Valeurs abbérante ",deparse(substitute(valeurs))),ylim=c(min(residus)-1,max(residus+1)))
  #On represente les résidus dans un graphique
  abline(h=c(-2,0,2), col = "red")
  abline(h=c(0), col = "black")
  
}



AB(Temoins)
AB(Patients)




dev.print(device = pdf, file = gsub(" ","",paste("./figs/",DATASET,"/2_Valeurs abbérantes-",DATASET,".pdf")), bg="white")


#6################### Histogrames #####################

par(mfrow = c(1,2))

hist(x =Temoins, ylab="",xlab="",main="Temoins", col = "green")
hist(x =Patients, ylab="",xlab="",main="Patients", col = "orange")


dev.print(device = pdf, file = gsub(" ","",paste("./figs/",DATASET,"/3_Histogramme-",DATASET,".pdf")), bg="white")



##3########################### Comparaison Témoin VS Patient ############################################

#test de student#
t.test(Temoins, Patients, alternative="two.sided")
p.value=t.test(Temoins, Patients, alternative="two.sided")$p.value


##5######## Affichage boxplot avec p-value du test de student si p-value<0,05 alors echantillons différents ###############################
par(mfrow = c(1,1))
boxplot(Patients, Temoins,   main = "Box plot comparatif",names=c("Patients","Témoins"), xlab=paste("p-value",attribution.Test.T,"=",round(as.numeric(attribution.Test.pvalue),5)),ylab = "intégrité")




dev.print(device = pdf, file = gsub(" ","",paste("./figs/",DATASET,"/4_boxplotBis-",DATASET,".pdf")), bg="white")




#################### ROC ##################################### calculé pour un prevalence is 0.5 and cost is 1 so that no weight is applied in effect
par(mfrow=c(1,2))

roc1=roc(c(nbr.P,nbr.T),c(as.numeric(Patients),as.numeric(Temoins)))

# roc
roc1 <- plot.roc(c(nbr.P,nbr.T),c(as.numeric(Patients),as.numeric(Temoins)),  percent=TRUE, thresholds="best", print.thres="best",print.auc=TRUE, main=paste("Umetani \n AUC=",round(auc(roc1),3)))
#ci=TRUE, of="thresholds" pour avoir interval de confiance à 95%
# wang


### ou ACCURACY


## en une ligne ... plot(accuracy ~ threshold, t(coords(roc2, "all", ret = c("threshold", "accuracy"))), type="l")
pred.U.best=coords(roc1, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                       "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                                       "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))

pred.U.all=coords(roc1, "all", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                     "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                                     "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))


plot(pred.U.all[1,],pred.U.all[4,], type="l", ylab="Accuracy", xlab=paste("Threshold \n Se=",round(pred.U.best[3],3),"Sp=",round(pred.U.best[2],3)), main=paste("Umetani \n Accuracy=", round(pred.U.best[4],3),"cutoff=",round(pred.U.best[1],3)))

max.acc = which(pred.U.all[4,]==max(pred.U.all[4,]))
acc = pred.U.all[4,which(pred.U.all[4,]==max(pred.U.all[4,]))]
cutoff = pred.U.all[1,which(pred.U.all[4,]==max(pred.U.all[4,]))]
print(c(accuracy= acc, cutoff = cutoff))


dev.print(device = pdf, file = gsub(" ","",paste("./figs/",DATASET,"/5_ROC-",DATASET,".pdf")), bg="white")

######  6   ### density plot#####

showdensityplot <- function(Patients,Temoins){ 
  
  densityplot <- function(Patients,Temoins) {
    
    dada <- data.frame(Echantillon = c(rep("Temoins",length(Temoins)),rep("Patients",length(Patients))), valeurs = c( na.omit(Temoins),na.omit(Patients)))
    
    
    dada %>% 
      ggplot(aes(valeurs,color = Echantillon,fill = Echantillon)) + 
      labs( x = "Données", y = "Densité",
            title = "Répartition des données ")+
      geom_density(aes(valeurs, color = Echantillon,fill = Echantillon), alpha = 0.2)
    
  }
  
  a <- densityplot(Patients,Temoins)
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  pushViewport(viewport(layout = grid.layout(1, 1)))
  
  print(a, vp = vplayout(1,1))
  return()
}
grid.newpage()
showdensityplot(Patients,Temoins)
dev.print(device = pdf, file = gsub(" ","",paste("./figs/",DATASET,"/6_density_plot-",DATASET,".pdf")), bg="white")
grid.newpage()



#########



###### droite de regression lineaire #####
val1 <- Patients

val2 <- 3^log(Patients^2+3)

par(mfrow = c(1,1))

model<-lm(val1~val2)
coef(model)
fitted(model)
residuals(model)
a=summary(model)$r.squared
predict(model, interval="confidence")

plot(val2,val1, main="Régression linéaire Test")
abline(model)
segments(val2,fitted(model),val2, val1)
pred.frame<-data.frame(val2=2:6)
pc<-predict(model, interval="confidence",
            newdata=pred.frame)
pp<-predict(model, interval="prediction",
            newdata=pred.frame)
matlines(pred.frame, pc[,2:3], lty=c(2,2), col="blue")
matlines(pred.frame, pp[,2:3], lty=c(3,3), col="red")

spear=cor.test(val1, val2, method="spearman")


legend("topleft",c(paste("Intercept b =",round(coef(model)[1],3)),paste("coef a =",round(coef(model)[2],3)),paste("R carré = ",round(a,3)),paste("Rho Spearman = ",round(as.numeric(spear$estimate),3)),paste("p-value Spearman = ",round(as.numeric(spear$p.value),3))))
model<-lm(val1~val2)
summary(model)

dev.print(device = pdf, file = gsub(" ","",paste("./figs/",DATASET,"/7_Regression_LineaireTest-",DATASET,".pdf")), bg="white")

##########
source("Scripts_FULL.R")


DATASET=gsub(".csv","",paste(DATASET,""))

full(Patients,Temoins,DATASET)

donnees <- c(tableauFULL)
return(donnees)
}



source("script_fonctions.R")
source("Scripts_FULL.R")
donnees <- FULLBis("exemple")


