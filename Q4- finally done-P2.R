#handle multivariate - Completed

af <- read.csv("Asian_Fusion_Data.csv", as.is = TRUE);

af <- af[,c(1:48)]

mydata <- na.omit(af)

{ 
  y<- c(1:length(mydata$Catkey))
  z <- mydata$Catkey
  for (i in y)
  {
    mdist <- mahalanobis(na.omit(mydata[i,c(4:48)]),center=colMeans(na.omit(mydata[-(i),c(4:48)])),cov(na.omit(mydata[-(i),c(4:48)])) )
    
    if (length(mdist) > 0)
    {
      val2 <- 1-pchisq(mdist,46)
      if(val2 < 0.01)
      { 
        x <- c(x, z[which(mydata$Record==i)])
      }

      val2 <- NA
    }
    
    else
    {
      #Do Nothin
    }
  }
  x
}


