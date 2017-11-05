af <- read.csv("Asian_Fusion_Data.csv", as.is = TRUE);

af <- af[,c(1:48)]

asian_dat <- na.omit(af)


for (i in 4:50)
{
  x <- mydata[,i]
  y <- colnames(asian_dat[i])
  OUT <- Boxplot(x)
  as_data <- cbind(asian_dat[OUT,c("Catkey",y)])
  print(as_data)
  asian_dat[Boxplot(x),y] <- NA
} 