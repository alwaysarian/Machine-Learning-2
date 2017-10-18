#library(car)

af <- read.csv("Asian_Fusion_Data.csv", as.is = TRUE);

af <- af[,-c(59,66,67,68,71,72,73,74,75,76,77,78,79,80,81,82,83,84,99,100,101,102,103)]

mydata <- na.omit(af)

finallist <- list()

for (i in 4:87)
{
  x <- mydata[,i]
  y <- colnames(mydata[i])
  OUT <- Boxplot(x)
  #list1 <- cbind(y,mydata[OUT,c("Catkey",y)])
  
  finallist[4:87] <- cbind(y,mydata[OUT,c("Catkey",y)])
  
  #finallist <- list1[]
  
  mydata[Boxplot(x),y] <- NA
  OUT <- Boxplot(x)
  names(list1)[1] <- "Var"
  names(list1)[3] <- "Val"

}

#handle multivariate

af <- read.csv("Asian_Fusion_Data.csv", as.is = TRUE);

af <- af[,-c(59,66,67,68,71,72,73,74,75,76,77,78,79,80,81,82,83,84,99,100,101,102,103)]

mydata <- na.omit(af)

myfunction <- function(ads)
{ 
  x <- vector(mode="numeric", length=0)
  y<- c(1:length(ads$Catkey))
  for (i in y)
  {
    mdist <- mahalanobis(na.omit(ads[i,c(4:87)]),center=colMeans(na.omit(ads[-(i),c(4:87)])),cov(na.omit(ads[-(i),c(4:87)])) )
    
    if (length(mdist) > 0)
    {
      val2 <- 1-pchisq(mdist,84)
      if(val2 < 0.01)
      { 
        x <- c(x, val2)
      }
      val2 <- NA
    }
    
    else
    {
      #Do Nothin
    }
  }
  print(x)
}

    






















x <- vector(mode="numeric", length=0)
y<- c(1:length(mydata$Catkey))
for (i in y)
 {
     mdist <- mahalanobis(na.omit(mydata[i,c(4:87)]),center=colMeans(na.omit(mydata[-(i),c(4:87)])),cov(na.omit(mydata[-(i),c(4:87)])) )
     
     if (length(mdist) > 0)
     {
         val2 <- 1-pchisq(mdist,7)
         if(val2 < 0.01)
         { 
             x <- c(x, val2)
         }
     }
     
     else
     {
         #Do Nothin
     }
 }






#junk code/reusable



>>>>>
  if(OUT == NULL)
  {
    #do nothing
  }
else
{
  
  list1 <- cbind(y,mydata[OUT,c("Catkey",y)])
  
  names(list1)[1] <- "Var"
  names(list1)[3] <- "Val"
  finallist[i] <- list1
  mydata[Boxplot(x),y] <- NA
}












OUT
list1
finallist


abc

finallist
  
l<-c()
i=1

while(i<100) {
  
  b<-i
  l<-c(l,b)
  i=i+1
}