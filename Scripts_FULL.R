

require (ROCR); require(gplots) ; require(stats) ; require(utils) ; require(splines) ; require(pROC) ; require(Matrix) ; require(MASS) ; require(methods) ; require(grDevices) ; require(graphics) ; require(gplots) ; require(datasets)
library(dplyr)
library(ggplot2)
library(dslabs)
source("./testSWW.R")

full<-function(Patients,Temoins,DATASET){
    par(mfrow=c(2,4))

  
    ######## Test de normalité Shapiro
    
    Shapishapo<-function(Patients){
        
        shapiro.test(Patients)
        
        p.value.Patients=paste("Quartiles Théoriques \n p-value=",round(shapiro.test(Patients)$p.value,6))
        
        qqnorm(Patients, ylab="Quartiles Patients",xlab=p.value.Patients,main=paste("Test de shapiro \n",deparse(substitute(Patients))))
        qqline(Patients)
        
        return()
    }
    
        
    ######## Valeurs abérantes
    
    AB<-function(valeurs){
        x=1:length(valeurs);
        X=sample(x,length(valeurs),replace=TRUE)
        Y=valeurs
        
        regression=lm(Y~X);
        #On réalise une régréssion linéaire
        residus=rstudent(regression)
        #On calcule les residus
        plot(residus, type = "p", cex = 0.5, ylab = "Résidus studentisés ",  col = "springgreen2",  main=paste("Valeurs abbérante \n ",deparse(substitute(valeurs))),ylim=c(min(residus)-1,max(residus+1)))
        #On represente les résidus dans un graphique
        abline(h=c(-2,0,2), col = "red")
        abline(h=c(0), col = "black")
        
    }
    
    ##### Histogrammes
    Histogramme<-function(Patients){
        hist(x =Patients, ylab="",xlab="",main=paste("Histogramme de répartition Patients",deparse(substitute(Patients))), col = "green")
        return()
    }

    ##### Test de student avec box plot à 5%
    Student<-function(Patients,Temoins){
        
        t.test(Patients, Temoins, alternative="two.sided")
        p.value=t.test(Patients, Temoins, alternative="two.sided")$p.value
        boxplot(Patients, Temoins,   main = paste(deparse(substitute(Patients)),"VS",deparse(substitute(Temoins)),"\n Boxplot à 5% \n p-value de Student"),names=c("Patients","Temoins"), xlab=paste("p-value=",round(p.value,5)),ylab = "intégrité")
        
        return()
    }
 
    #####ROC
    ROC<-function(Patients,Temoins) {
    
    
    nbr.P=rep(1,length(Patients))
    nbr.T=rep(0,length(Temoins))
    
    roc2=roc(c(nbr.P,nbr.T),c(as.numeric(Patients),as.numeric(Temoins)))
    roc2 <- plot.roc(c(nbr.P,nbr.T),c(as.numeric(Patients),as.numeric(Temoins)),  percent=TRUE, thresholds="best", print.thres="best",print.auc=TRUE, main=paste("Courbe ROC \n AUC=",round(auc(roc2),3)))
    pred.best=coords(roc2, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
    "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
    "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))
    
    pred.all=coords(roc2, "all", ret=c("threshold", "specificity", "sensitivity", "accuracy",
    "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
    "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))
    
    plot(pred.all[1,],pred.all[4,], type="l", ylab="Accuracy", xlab=paste("Threshold \n Se=",round(pred.best[2],3),"Sp=",round(pred.best[3],3)), main=paste("Accuracy=", round(pred.best[4],3),"\n cutoff=",round(pred.best[1],3)))
    
    max.acc = which(pred.all[4,]==max(pred.all[4,]))
    acc = pred.all[4,which(pred.all[4,]==max(pred.all[4,]))]
    cutoff = pred.all[1,which(pred.all[4,]==max(pred.all[4,]))]
    print(c(accuracy= acc, cutoff = cutoff))
    
    
    
    
    return()
    }
    
    ##### Test de student avec box plot à 5%
    Student<-function(Patients,Temoins){
        
        t.test(Patients, Temoins, alternative="two.sided")
        p.value=t.test(Patients, Temoins, alternative="two.sided")$p.value
        boxplot(Patients, Temoins,   main = paste(deparse(substitute(Patients)),"VS",deparse(substitute(Temoins)),"\n Boxplot à 5%\n p-value de Student"),names=c("Patients","Temoins"), xlab=paste("p-value=",round(p.value,5)),ylab = "intégrité")
        
        return()
    }
    
    densityplot <- function(Patients,Temoins) {
      
      dada <- data.frame(Echantillon = c(rep("Temoins",length(Temoins)),rep("Patients",length(Patients))), valeurs = c( na.omit(Temoins),na.omit(Patients)))
      
      
      dada %>% 
        ggplot(aes(valeurs,color = Echantillon,fill = Echantillon)) + 
        labs( x = "Données", y = "Densité",
              title = "Répartition des données ")+
        geom_density(aes(valeurs, color = Echantillon,fill = Echantillon), alpha = 0.2)
     
    
    }


Shapishapo(Patients)
Shapishapo(Temoins)

AB(Patients)
AB(Temoins)
Student(Patients,Temoins)
ROC(Patients,Temoins)

library("ggplot2")
library("grid")


a <- densityplot(Patients,Temoins)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

pushViewport(viewport(layout = grid.layout(2, 4)))

print(a, vp = vplayout(2,4))





dev.print(device = pdf, file = gsub(" ","",paste("./figs/",DATASET,"/Synthese/synthese+.pdf")), bg="white")
return(

)
}









######## Test de normalité Shapiro

Shapishapo<-function(Patients){
  
  shapiro.test(Patients)
  
  p.value.Patients=paste("Quartiles Théoriques \n p-value=",round(shapiro.test(Patients)$p.value,6))
  
  qqnorm(Patients, ylab="Quartiles Patients",xlab=p.value.Patients,main=paste("Test de shapiro \n",deparse(substitute(Patients))))
  qqline(Patients)
  
  return()
}


######## Valeurs abérantes

AB<-function(valeurs){
  x=1:length(valeurs);
  X=sample(x,length(valeurs),replace=TRUE)
  Y=valeurs
  
  regression=lm(Y~X);
  #On réalise une régréssion linéaire
  residus=rstudent(regression)
  #On calcule les residus
  plot(residus, type = "p", cex = 0.5, ylab = "Résidus studentisés ",  col = "springgreen2",  main=paste("Valeurs abbérante \n ",deparse(substitute(valeurs))),ylim=c(min(residus)-1,max(residus+1)))
  #On represente les résidus dans un graphique
  abline(h=c(-2,0,2), col = "red")
  abline(h=c(0), col = "black")
  
}

##### Histogrammes
Histogramme<-function(Patients){
  hist(x =Patients, ylab="",xlab="",main=paste("Histogramme de répartition Patients",deparse(substitute(Patients))), col = "green")
  return()
}

##### Test de student avec box plot à 5%
Student<-function(Patients,Temoins){
  
  t.test(Patients, Temoins, alternative="two.sided")
  p.value=t.test(Patients, Temoins, alternative="two.sided")$p.value
  boxplot(Patients, Temoins,   main = paste(deparse(substitute(Patients)),"VS",deparse(substitute(Temoins)),"\n Boxplot à 5% \n p-value de Student"),names=c("Patients","Temoins"), xlab=paste("p-value=",round(p.value,5)),ylab = "intégrité")
  
  return()
}

#####ROC
ROC<-function(Patients,Temoins) {
  
  
  nbr.P=rep(1,length(Patients))
  nbr.T=rep(0,length(Temoins))
  
  roc2=roc(c(nbr.P,nbr.T),c(as.numeric(Patients),as.numeric(Temoins)))
  roc2 <- plot.roc(c(nbr.P,nbr.T),c(as.numeric(Patients),as.numeric(Temoins)),  percent=TRUE, thresholds="best", print.thres="best",print.auc=TRUE, main=paste("Courbe ROC \n AUC=",round(auc(roc2),3)))
  pred.best=coords(roc2, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                       "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                                       "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))
  
  pred.all=coords(roc2, "all", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                     "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                                     "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))
  
  plot(pred.all[1,],pred.all[4,], type="l", ylab="Accuracy", xlab=paste("Threshold \n Se=",round(pred.best[2],3),"Sp=",round(pred.best[3],3)), main=paste("Accuracy=", round(pred.best[4],3),"\n cutoff=",round(pred.best[1],3)))
  
  max.acc = which(pred.all[4,]==max(pred.all[4,]))
  acc = pred.all[4,which(pred.all[4,]==max(pred.all[4,]))]
  cutoff = pred.all[1,which(pred.all[4,]==max(pred.all[4,]))]
  print(c(accuracy= acc, cutoff = cutoff))
  
  
  
  
  return()
}

##### Test de student avec box plot à 5%
Student<-function(Patients,Temoins){
  
  t.test(Patients, Temoins, alternative="two.sided")
  p.value=t.test(Patients, Temoins, alternative="two.sided")$p.value
  boxplot(Patients, Temoins,   main = paste(deparse(substitute(Patients)),"VS",deparse(substitute(Temoins)),"\n Boxplot à 5%\n p-value de Student"),names=c("Patients","Temoins"), xlab=paste("p-value=",round(p.value,5)),ylab = "intégrité")
  
  return()
}

## créer de density plot


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