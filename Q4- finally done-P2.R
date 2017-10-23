#handle multivariate - Completed

af <- read.csv("Asian_Fusion_Data.csv", as.is = TRUE);

af <- af[,-c(59,66,67,68,71,72,73,74,75,76,77,78,79,80,81,82,83,84,99,100,101,102,103)]

mydata <- na.omit(af)

{ 
  #x <- vector(mode="numeric", length=0)
  x <- c()
  y<- c(1:length(mydata$Catkey))
  z <- mydata$Catkey
  for (i in y)
  {
    mdist <- mahalanobis(na.omit(mydata[i,c(4:87)]),center=colMeans(na.omit(mydata[-(i),c(4:87)])),cov(na.omit(mydata[-(i),c(4:87)])) )
    
    if (length(mdist) > 0)
    {
      val2 <- 1-pchisq(mdist,84)
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


