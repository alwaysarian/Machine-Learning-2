install.packages("sqldf")
library("sqldf")

install.packages("car")
library(car)


af <- read.csv("Asian_Fusion_Data.csv");


mydata <- sqldf("select record,Catkey,sex,NAR,BRR,VRR,LAR,OSR,BAR,SBA from af")
head(mydata)

#af <- na.omit(af)

IDBRR<-Boxplot(mydata$BRR, labels=mydata$Record, main = "Boxplot with record")
RemovedIDRR <- mydata[-which(mydata$Record %in% IDBRR),c("Record", "BRR")]


IDNAR<-Boxplot(mydata$NAR, labels=mydata$Record, main = "Boxplot with record")
#a <- mydata[-1,]
mydata[which(mydata$Record %in% IDBRR),]

     
          