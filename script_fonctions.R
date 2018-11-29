

require (ROCR); require(gplots) ; require(stats) ; require(utils) ; require(splines) ; require(pROC) ; require(Matrix) ; require(MASS) ; require(methods) ; require(grDevices) ; require(graphics) ; require(gplots) ; require(datasets)



    
    ######## Test de normalité Shapiro
    
    Shapishapo<-function(Echantillon){
        par(mfrow=c(1,1))


        shapiro.test(Echantillon)
        
        p.value.Echantillon=paste("Quartiles Théoriques \n p-value=",round(shapiro.test(Echantillon)$p.value,6))
        
        qqnorm(Echantillon, ylab="Quartiles Echantillon",xlab=p.value.Echantillon,main=paste("Test de shapiro",deparse(substitute(Echantillon))))
        qqline(Echantillon)
        
        return()
    }
        

    
    ######## Valeurs abérantes
    
    AB<-function(Echantillon){
        par(mfrow=c(1,1))

        x=1:length(Echantillon);
        X=sample(x,length(Echantillon),replace=TRUE)
        Y=Echantillon
        
        regression=lm(Y~X);
        #On réalise une régréssion linéaire
        residus=rstudent(regression)
        #On calcule les residus
        plot(residus, type = "p", cex = 0.5, ylab = "Résidus studentisés ",  col = "springgreen2",  main=paste("Valeurs abbérante ",deparse(substitute(Echantillon))),ylim=c(min(residus)-1,max(residus+1)))
        #On represente les résidus dans un graphique
        abline(h=c(-2,0,2), col = "red")
        abline(h=c(0), col = "black")
        
    }
    
    ##### Histogrammes
    Histogramme<-function(Echantillon){
        par(mfrow=c(1,1))

        hist(x =Echantillon, ylab="",xlab="",main=paste("Histogramme de répartition Patients",deparse(substitute(Echantillon))), col = "green")
        return()
    }

    
    ##### Test de student avec box plot à 5%
    Student<-function(Patients,Témoins){
        par(mfrow=c(1,1))

        t.test(Patients, Témoins, alternative="two.sided")
        p.value=t.test(Patients, Témoins, alternative="two.sided")$p.value
        boxplot(Patients, Témoins,   main = paste(deparse(substitute(Patients)),"VS",deparse(substitute(Témoins)),"\n Boxplot à 5% avec p-value de Student"),names=c("Patients","Témoins"), xlab=paste("p-value=",round(p.value,5)),ylab = "intégrité")
        
        return()
    }

    
    
    
#####ROC
ROC<-function(Patients,Témoins)
{


par(mfrow=c(1,2))
nbr.P=rep(1,length(Patients))
nbr.T=rep(0,length(Témoins))

roc2=roc(c(nbr.P,nbr.T),c(as.numeric(Patients),as.numeric(Témoins)))
roc2 <- plot.roc(c(nbr.P,nbr.T),c(as.numeric(Patients),as.numeric(Témoins)),  percent=TRUE, thresholds="best", print.thres="best",print.auc=TRUE, main=paste("Wang \n AUC=",round(auc(roc2),3)))
pred.best=coords(roc2, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
"tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
"1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))

pred.all=coords(roc2, "all", ret=c("threshold", "specificity", "sensitivity", "accuracy",
"tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
"1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))

plot(pred.all[1,],pred.all[4,], type="l", ylab="Accuracy", xlab=paste("Threshold \n Se=",round(pred.best[2],3),"Sp=",round(pred.best[3],3)), main=paste("Wang \n Accuracy=", round(pred.best[4],3),"cutoff=",round(pred.best[1],3)))

max.acc = which(pred.all[4,]==max(pred.all[4,]))
acc = pred.all[4,which(pred.all[4,]==max(pred.all[4,]))]
cutoff = pred.all[1,which(pred.all[4,]==max(pred.all[4,]))]
print(c(accuracy= acc, cutoff = cutoff))


return()
}





