#++++++++++++++++++++++++
# rquery.t.test
#+++++++++++++++++++++++
# Description : Performs one or two samples t-test
# x : a (non-empty) numeric vector of data values.
# y : an optional (non-empty) numeric vector of data values
# paired : if TRUE, paired t-test is performed
# graph : if TRUE, the distribution of the data is shown
  # for the inspection of normality
# ... : further arguments to be passed to the built-in t.test() R function
# 1. shapiro.test is used to check normality
# 2. F-test is performed to check equality of variances
# If the variances are different, then Welch t-test is used


AB<-function(valeurs,titre){
    x=1:length(valeurs);
    X=sample(x,length(valeurs),replace=TRUE)
    Y=valeurs
    
    regression=lm(Y~X);
    #On réalise une régréssion linéaire
    residus=rstudent(regression)
    #On calcule les residus
    plot(residus, type = "p", cex = 0.5, ylab = "Résidus studentisés ",  col = "springgreen2",  main=paste("Valeurs abbérantes \n ",deparse(substitute(titre))),ylim=c(min(residus)-1,max(residus+1)))
    #On represente les résidus dans un graphique
    abline(h=c(-2,0,2), col = "red")
    abline(h=c(0), col = "black")
    
}

tests.SWW<-function(x, y = NULL, paired = FALSE,
                        graph = TRUE, ...)
{
  # I. Preliminary test : normality and variance tests
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  var.equal = FALSE # by default
  
  # I.1 One sample t test
  if(is.null(y)){
    if(graph)
    shapiro.px<-normaTest(x, graph, 
                          hist.title="X - Histogram",
                          qq.title="X - Normal Q-Q Plot")
                          AB(x,"Patient")
                          
                          # Check for equality of variances
                          if(var.test(x,y)$p.value >= 0.05) var.equal=TRUE
                         
                         if(shapiro.px > 0.05){
                             res <- t.test(x, y, paired=paired, var.equal=var.equal, ...)
                             
                         }
                       
                         if(shapiro.px < 0.05){
                             res <- wilcox.test(x, y)

                          }
  }
  
  # I.2 Two samples t test
  if(!is.null(y)){
    
      # I.2.a unpaired t test
      if(!paired){
          if(graph)
          # normality test
          shapiro.px<-normaTest(x, graph, 
                                hist.title=paste(deparse(substitute(x)),"Histogramme"),
                                qq.title=paste(deparse(substitute(x)),"Normal Q-Q Plot"))
                                AB(x,"Patient")
          shapiro.py<-normaTest(y, graph,
                                hist.title=paste(deparse(substitute(y)),"Histogramme"),
                                qq.title=paste(deparse(substitute(y)),"Normal Q-Q Plot"))
                                AB(y,"Témoins")
                                
                                # Check for equality of variances
                                if(var.test(x,y)$p.value >= 0.05) var.equal=TRUE
     
     if(shapiro.px > 0.05 && shapiro.py > 0.05){
         res <- t.test(x, y, paired=paired, var.equal=var.equal, ...)
     }
     
     if(shapiro.px > 0.05 && shapiro.py > 0.05 && var.equal == TRUE){

      test="Student"}
     
     if(shapiro.px > 0.05 && shapiro.py > 0.05 && var.equal == FALSE){
         
         test="Welch"}
     
     
     
          if(shapiro.px < 0.05 | shapiro.py < 0.05){
              res <- wilcox.test(x, y)
                test="Wilcoxon"
            }
          
          
       
        } 
      
      # I.2.b Paired t-test
      else {
        if(graph)
        d = x-y 
        shapiro.pd<-normaTest(d, graph, 
                              hist.title="D - Histogram",
                              qq.title="D - Normal Q-Q Plot")
                              
                              # Check for equality of variances
                              if(var.test(x,y)$p.value >= 0.05) var.equal=TRUE
                              
                    if(shapiro.pd > 0.05 ){
                        res <- t.test(x, y, paired=paired, var.equal=var.equal, ...)

                    }
                    
                    if(shapiro.pd > 0.05 && var.equal == TRUE){
                        
                        test="Student"}
                    
                    if(shapiro.pd > 0.05 && var.equal == FALSE){
                        
                        test="Welch"}
                    
                    if(shapiro.pd < 0.05 ){
                        res <- wilcox.test(x, y)
                        test="Wilcoxon"

                              }
      } 
      
   }
  
  resultat <- list("Test" = test, "res" = res)
  

  
  # II. Student's t-test
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(resultat)
}
#+++++++++++++++++++++++
# Helper function
#+++++++++++++++++++++++
# Performs normality test using Shapiro Wilk's method
# The histogram and Q-Q plot of the data are plotted
# x : a (non-empty) numeric vector of data values.
# graph : possible values are TRUE or FALSE. If TRUE,
  # the histogram and the Q-Q plot of the data are displayed
# hist.title : title of the histogram
# qq.title : title of the Q-Q plot
normaTest<-function(x, graph=TRUE, 
                    hist.title="Histogram", 
                    qq.title="Normal Q-Q Plot",...)
  {  
  # Significance test
  #++++++++++++++++++++++
  shapiro.p<-signif(shapiro.test(x)$p.value,1) 
  
  if(graph){
    # Plot : Visual inspection
    #++++++++++++++++
    h<-hist(x, col="lightblue", main=hist.title, 
            xlab="Data values", ...)
    m<-round(mean(x),2)
    s<-round(sd(x),2)
    a=t.test(x, conf.level=0.95)
    int=round(a$estimate-a$conf.int[1],2)
    mtext(paste0("Mean : ", m, "±",int,"; SD : ", s),
          side=3, cex=0.8)
    # add normal curve
    xfit<-seq(min(x),max(x),length=40)
    yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
    yfit <- yfit*diff(h$mids[1:2])*length(x)
    lines(xfit, yfit, col="red", lwd=2)
    # qq plot
    qqnorm(x, pch=19, frame.plot=FALSE,main=qq.title)
    qqline(x)
    mtext(paste0("Shapiro-Wilk, p-val : ", shapiro.p),
          side=3, cex=0.8)
  }
  return(shapiro.p)
}