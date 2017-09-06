af <- read.csv("Asian_Fusion_Data.csv");

mydata <- sqldf("select record,Catkey,sex,NAR,BRR,VRR,LAR,OSR,BAR,SBA from af")

OUTNAR <- Boxplot(mydata$NAR)
listNAR <- cbind("NAR",mydata[OUTNAR,c("Catkey","NAR")])
names(listNAR)[1] <- "Var"
names(listNAR)[3] <- "Val"
    if (listNAR !=0) 
      {
      mydata[Boxplot(mydata$NAR),"NAR"] <- NA
      return()

      } 
    else 
      {
        Boxplot(mydata$NAR)
      }

Boxplot(mydata$NAR)
  
  
  
  
  
    if (print & !npar) {
      cat("Mean=", center, "\n", "SD=", spread, "\n")
    } else if (print & npar) {
      cat("Median=", center, "\n", "MAD=", spread, "\n")
    }
    result <- list(center=center,spread=spread)
    return(result)
 }