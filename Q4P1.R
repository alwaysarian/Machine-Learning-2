af <- read.csv("Asian_Fusion_Data.csv", as.is = TRUE);

af <- af[,c(1:48)]

mydata <- na.omit(af)

flist <- c("a", "c")
df <- data.frame()

for (i in 4:48)
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
  #df <-  
  
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
#flist  



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

finaldata <- read.csv("finaldata.csv", as.is = TRUE)