af <- read.csv("C:\\Users\\pneelappa\\Downloads\\Asian_Fusion_Data.csv");
install.packages("sqldf")
mydata <- sqldf("select record,sex,NAR,BRR,VRR,LAR,OSR,BAR,SBA from af")

install.packages("car")
library(car)

IDBRR<-Boxplot(mydata$BRR, labels=mydata$Record, main = "Boxplot with record")
mydata[which(mydata$Record %in% IDBRR), ] 