

require (ROCR); require(gplots) ; require(stats) ; require(utils) ; require(splines) ; require(pROC) ; require(Matrix) ; require(MASS) ; require(methods) ; require(grDevices) ; require(graphics) ; require(gplots) ; require(datasets)

#source("/Users/maximelat/Desktop/Bureau 2017/Stage/data/testSWWbis.R")

source("./testSWWbis.R")

full<-function(Patients,Témoins,DATASET,chimio = FALSE , logi=false){

if(!chimio){
    ######## Test de normalité Shapiro
    par(mfrow=c(3,3))

    Test=tests.SWW(Patients, Témoins)
    Test.T=Test$Test
    Test.pvalue=Test$res[3]

   
   
    ######## Valeurs abérantes
    #inclu dans SWW
    

    
    ##### Test de student avec box plot à 5%
    Student<-function(Patients,Témoins,Test.T,Test.pvalue, cutoff,logi){
        
        a=t.test(Patients, conf.level=0.95)
        inta=round(a$estimate-a$conf.int[1],2)
        b=t.test(Témoins, conf.level=0.95)
        intb=round(b$estimate-b$conf.int[1],2)
        
       if(!logi){
           boxplot(Patients, Témoins,   main = paste("Boxplot Patient VS Témoins\n Moyenne et IC 95%"),names=c("Patients","Témoins"), xlab=paste("p-value de",Test.T,"=",round(as.numeric(Test.pvalue),5)),ylab = "intégrité")
       }
       else{
           boxplot(Patients, Témoins,   main = paste("Boxplot Patient VS Témoins\n Moyenne et IC 95%"),names=c("Patients","Témoins"), xlab=paste("p-value de",Test.T,"=",round(as.numeric(Test.pvalue),5)),ylab = "intégrité",log="y")
           
       }
       
       points(1, a$estimate, col = "green",pch = 3)
       points(1, a$conf.int[1], col = "blue",pch = "-")
       points(1, a$conf.int[2], col = "red",pch = "-")
       
       points(2, b$estimate, col = "green",pch = 3)
       points(2, b$conf.int[1], col = "blue",pch = "-")
       points(2, b$conf.int[2], col = "red",pch = "-")
       
       
       abline( h = cutoff, col = "lightgray", lty = 3)





        return()
    }

    
 
    
#####ROC
ROC<-function(Patients,Témoins)
{
    nbr.P=rep(1,length(Patients))
    nbr.T=rep(0,length(Témoins))
    
    roc2=roc(c(nbr.P,nbr.T),c(as.numeric(Patients),as.numeric(Témoins)))
    pred.best=coords(roc2, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
    "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
    "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))
    
    pred.all=coords(roc2, "all", ret=c("threshold", "specificity", "sensitivity", "accuracy",
    "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
    "1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))
    
    abline( v = pred.best[1], col = "lightgray", lty = 3)


roc2 <- plot.roc(c(nbr.P,nbr.T),c(as.numeric(Patients),as.numeric(Témoins)),  percent=TRUE, thresholds="best", print.thres="best",print.auc=TRUE, main=paste("ROC : AUC=",round(auc(roc2),3),"±",round((auc(roc2)[1]-ci.auc(roc2)[1]),2),"\n VPP=",round(pred.best[10],3),"VPN=",round(pred.best[9],3)))
    
    plot(pred.all[1,],pred.all[4,], type="l", ylab="Accuracy", xlab=paste("Threshold \n Se=",round(pred.best[3],3),"Sp=",round(pred.best[2],3)), main=paste("Accuracy=", round(pred.best[4],3),"\n cutoff=",round(pred.best[1],3)))
    abline( v = pred.best[1], col = "lightgray", lty = 3)

    max.acc = which(pred.all[4,]==max(pred.all[4,]))
    acc = pred.all[4,which(pred.all[4,]==max(pred.all[4,]))]
    cutoff = pred.all[1,which(pred.all[4,]==max(pred.all[4,]))]
    print(c(accuracy= acc, cutoff = cutoff))
    
    cutcut<-round(pred.best[1],3)
    return(cutcut)
}



cutoff=ROC(Patients,Témoins)
Student(Patients,Témoins,Test.T,Test.pvalue,cutoff,logi)


dev.print(device = pdf, file = gsub(" ","",paste("./figs/",DATASET,"/Synthese/synthese++.pdf")), bg="white")
}

else{
    Test=tests.SWW(Patients, Témoins, paired=TRUE)
    Test.T=Test$Test
    Test.pvalue=Test$res[3]
   
   boxplot(Patients, Témoins,   main = paste("Boxplot P1 VS P2 \n Moyenne et IC à 95%"),names=c("P1","P2"), xlab=paste("p-value de",Test.T,"=",round(as.numeric(Test.pvalue),5)),ylab = "intégrité",log="y")
   
   a=t.test(Patients, conf.level=0.95)
    inta=round(a$estimate-a$conf.int[1],2)
    b=t.test(Témoins, conf.level=0.95)
    intb=round(b$estimate-b$conf.int[1],2)
    points(1, a$estimate, col = "green",pch = 3)
    points(1, a$conf.int[1], col = "blue",pch = "-")
    points(1, a$conf.int[2], col = "red",pch = "-")
    
    points(2, b$estimate, col = "green",pch = 3)
    points(2, b$conf.int[1], col = "blue",pch = "-")
    points(2, b$conf.int[2], col = "red",pch = "-")
    
}



return(

)
}