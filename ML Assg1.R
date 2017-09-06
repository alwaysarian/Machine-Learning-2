#Read the file into R
af <- read.csv("Asian_Fusion_Data.csv");

mydata <- sqldf("select record,Catkey,sex,NAR,BRR,VRR,LAR,OSR,BAR,SBA from af")

shapiro.test(mydata$NAR)

shapiro.test(mydata$BRR)

shapiro.test(mydata$VRR)

shapiro.test(mydata$LAR)


shapiro.test(mydata$OSR)

shapiro.test(mydata$BAR)

shapiro.test(mydata$SBA)

OUTNAR <- Boxplot(mydata$NAR)
listNAR <- cbind("NAR",mydata[OUTNAR,c("Catkey","NAR")])
names(listNAR)[1] <- "Var"
names(listNAR)[3] <- "Val"

mydata[Boxplot(mydata$NAR),"NAR"] <- NA
Boxplot(mydata$NAR)

OUTBRR <- Boxplot(mydata$BRR)
listBRR <- cbind("BRR",mydata[OUTBRR,c("Catkey","BRR")])
names(listBRR)[1] <- "Var"
names(listBRR)[3] <- "Val"

mydata[Boxplot(mydata$BRR),"BRR"] <- NA
Boxplot(mydata$BRR)

OUTBRR1 <- Boxplot(mydata$BRR)
listBRR1 <- cbind("BRR",mydata[OUTBRR1,c("Catkey","BRR")])

mydata[Boxplot(mydata$BRR),"BRR"] <- NA
Boxplot(mydata$BRR)

OUTBRR2 <- Boxplot(mydata$BRR)
listBRR2 <- cbind("BRR",mydata[OUTBRR2,c("Catkey","BRR")])

mydata[Boxplot(mydata$BRR),"BRR"] <- NA
Boxplot(mydata$BRR)





OUTVRR <- Boxplot(mydata$VRR)
listVRR <- cbind("VRR",mydata[OUTVRR,c("Catkey","VRR")])
names(listVRR)[1] <- "Var"
names(listVRR)[3] <- "Val"

mydata[Boxplot(mydata$VRR),"VRR"] <- NA
Boxplot(mydata$VRR)







OUTLAR <- Boxplot(mydata$LAR)
listLAR <- cbind("LAR",mydata[OUTLAR,c("Catkey","LAR")])
names(listLAR)[1] <- "Var"
names(listLAR)[3] <- "Val"

mydata[Boxplot(mydata$LAR),"LAR"] <- NA
Boxplot(mydata$LAR)



OUTOSR <- Boxplot(mydata$OSR)
listOSR <- cbind("OSR",mydata[OUTOSR,c("Catkey","OSR")])
names(listOSR)[1] <- "Var"
names(listOSR)[3] <- "Val"

mydata[Boxplot(mydata$OSR),"OSR"] <- NA
Boxplot(mydata$OSR)



OUTBAR <- Boxplot(mydata$BAR)
listBAR <- cbind("BAR",mydata[OUTBAR,c("Catkey","BAR")])
names(listBAR)[1] <- "Var"
names(listBAR)[3] <- "Val"

mydata[Boxplot(mydata$BAR),"BAR"] <- NA
Boxplot(mydata$BAR)



OUTSBA <- Boxplot(mydata$SBA)
listSBA <- cbind("SBA",mydata[OUTSBA,c("Catkey","SBA")])
names(listSBA)[1] <- "Var"
names(listSBA)[3] <- "Val"

mydata[Boxplot(mydata$SBA),"SBA"] <- NA
Boxplot(mydata$SBA)


names(listNAR)

Finallist <- rbind(listNAR,listBRR, listVRR, listLAR, listOSR, listBAR, listSBA)

mydata[Boxplot(mydata$NAR),"NAR"] <- NA
Boxplot(mydata$NAR)

plot(NAR,BRR,VRR,LAR,OSR,BAR,SBA)

shapiro.test(mydata$NAR)

shapiro.test(mydata$BRR)

shapiro.test(mydata$VRR)

shapiro.test(mydata$LAR)

shapiro.test(mydata$OSR)

shapiro.test(mydata$BAR)

shapiro.test(mydata$SBA)



outliers <- aq.plot(na.omit(mydata[c(4:10)]), alpha = 0.01)

mydata[outliers[[1]],] or 

na.omit(mydata[outliers$outliers,])

x <- vector(mode="numeric", length=0)
y<- c(1:length(mydata$Catkey))
for (i in y)
{
  mdist <- mahalanobis(na.omit(mydata[i,c(4:10)]),center=colMeans(na.omit(mydata[-(i),c(4:10)])),cov(na.omit(mydata[-(i),c(4:10)])) )
  val2 <- 1-pchisq(mdist,7)
    if (!is.null(val2) & val2 < 0.01)
    {
      x <- c(x, val2[i])
    }
  
    else
    {
      print("NO")
    }
  
  
}
  
