#library(car)

af <- read.csv("Asian_Fusion_Data.csv", as.is = TRUE);

af <- af[,-c(59,66,67,68,71,72,73,74,75,76,77,78,79,80,81,82,83,84,99,100,101,102,103)]

mydata <- na.omit(af)

flist <- c("a", "c")

for (i in 4:87)
{
  #flist <- c("","")
  x <- mydata[,i]
  y <- colnames(mydata[i])
  OUT <- Boxplot(x)
  list1 <- cbind(mydata[OUT,c("Catkey",y)])
  a <- print(list1)
  a
  #flist <- rbind(flist, a)
  #print(list(y))
  
  mydata[Boxplot(x),y] <- NA
  #mydata <- na.omit(mydata)
  
  #names(list1)[1] <- "Var"
  
  #names(list1)[3] <- "Val"
  
  OUT <- Boxplot(x)
  
  if (is.null(OUT))
  {
    #Do Nothin
  }
  
  else
  {
    x <- mydata[,i]
    y <- colnames(mydata[i])
    OUT <- Boxplot(x)
    list1 <- cbind(mydata[OUT,c("Catkey",y)])
    a <- print(list1)
    a
    #flist <- rbind(flist, a)
    #print(list(y))
    
    mydata[Boxplot(x),y] <- NA

  }
}
flist  