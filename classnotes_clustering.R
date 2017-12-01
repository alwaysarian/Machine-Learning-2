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


plot(HPcl, main = 'Peru', xlab = 'GOL,NOL,BNL,BBH,XCB, ZYB, WNB, AUB, ZMB , NAR, DKB')

plot(HPcl, hang = -1, main = "Howells Peru males") 


plot(HPcl, hang = -1, main = "Howells Peru males")
rect.hclust(HPcl,k=3)



# Trick: Assign ID label to each individual in dendrogram
dd <- as.dendrogram(HPcl)
labs <- HP11[order.dendrogram(dd),"HID"]
# see what is in the list
order.dendrogram(dd)

plot(HPcl, labels = labs, hang = -1, main = "Howells Bushman")
plot(dd)
str(dd)



#display using ID
plot(HPcl, labels = labs, hang = -1, main = "Howells Peru Ms")

plot(HPcl, labels = labs, hang = -1, main = "Howells Peru Ms")


HCM <- sqldf("select HID, PopSex, GOL,NOL,BNL,BBH,XCB, ZYB, WNB, AUB, ZMB , NAR, DKB FROM Howells where (PopSex = 'NORSEM' or
PopSex = 'BURIATM' or PopSex = 'NJAPANM' or PopSex = 'BUSHMANM' or PopSex = 'ZULUM' or PopSex = 'ARIKARAM' or PopSex =
             'MOKAPUM' ) " )

require(Morpho)
pVCVM <- covW(HCM[,3:13],HCM[,2])

HMs <- sqldf("select HID, sex, popsex, AUB, BBH, BNL, BPL, DKB, GOL, NAR, NLB,
NLH, NPH, OBH, WNB, XCB, ZMB, ZYB FROM HowMs ")
str(HMs)

# do k-means clustering, set k = 2
HMs.kcl <- kmeans(HMs[4:18],2)
names(HMs.kcl)

HMs.kcl

table(HMs$PopSex,HMs.kcl$cluster)