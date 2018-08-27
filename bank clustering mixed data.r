#loading the dataset
library(xlsx)
bank <- read.xlsx(file="BankCustomer.xls",sheetIndex=1,header=T)

#descriptive statistics
summary(bank)

#active variables for the analysis
bank.active <- bank[,1:7]

#ade4 package
library(ade4)

#FAMD (factorial analysis for mixed data) , we retain the first two factors
bank.afdm <- dudi.mix(bank.active,scannf=F,nf=2)

#factorial coordinates for the first 5 instances
print(head(bank.afdm$li,5))

#euclidian distance between pairs of instancesdistance euclidenne from the FAMD 2 factors
dist.afdm <- dist(bank.afdm$li[,1:2],method="euclidian")

#squared distance for Ward's méthod
#see http://en.wikipedia.org/wiki/Ward's_method
dist.afdm <- dist.afdm^2

#HAC from the distance matrix
bank.tree <- hclust(dist.afdm,method="ward")
plot(bank.tree)

#cutting in 3 groupes
bank.clusters <- cutree(bank.tree,k=3)
table(bank.clusters)

#graphical reprensentation of the instances and the groups
plot(bank.afdm$li[,1],bank.afdm$li[,2],col=c("red","yellow","green")[bank.clusters])

#mean of the SCORE according to the groups
print(aggregate(x=bank$score,by=list(bank.clusters),FUN=mean))

