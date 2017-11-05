Howells <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/Howells.csv', as.is = T);
Howells$PopSex <- as.factor(Howells$PopSex)
# many columns are Nas
Howells <- na.omit(Howells[,c(1,2,4,5:61,63,67:80)])
attach(Howells);
H4A <- Howells[which(Pop == 'NORSE' | Pop == 'BERG'),];
# Peruvian males
HP <- Howells[which(PopSex == 'PERUM'),];
# ALL males
HowMs <- Howells[which(Sex == 'M'),];
library(MASS)
HP11 <- HP[,c("HID","PopSex","GOL","NOL","BNL","BBH","XCB","ZYB","WNB","AUB","ZMB","NAR","DKB")]

hclust:
  # get distances using dist()
  # average = UPGMA
  HPcl <- hclust(dist(HP11[3:13], method = 'euclidean'), "average")
#default plot with x labeled
plot(HPcl, main = 'Peru males', xlab = 'GOL,NOL,BNL,BBH,XCB, ZYB, WNB, AUB, ZMB , NAR, DKB')
#display using record numbers, even bottom
plot(HPcl, hang = -1, main = "Howells Peru males") 