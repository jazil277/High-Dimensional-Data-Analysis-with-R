Stocks<- read.csv("stocks.csv")
library(ggplot2)
library(dplyr)
library(tibble)

#Boxplot
ggplot(data = Stocks,mapping = aes(x=Market,y=Market.cap..intra.day.))+geom_boxplot()+
  labs(y="Market cap")
  
#Reordered boxplot
ggplot(data = Stocks,
       mapping = aes(x=reorder(Market,Market.cap..intra.day.,median,na.rm=TRUE),y=Market.cap..intra.day.))+geom_boxplot()+labs(y="Market cap")

  
#Flipped coordinates
ggplot(data = Stocks,
       mapping = aes(x=reorder(Market,Market.cap..intra.day.,median,na.rm=TRUE),y=Market.cap..intra.day.))+geom_boxplot()+labs(x="Financial Indices",y="Market.cap..intra.day.")+
  coord_flip()

summary(Stocks)


#Principal component analysis
head(Stocks)
summary(Stocks)
#Scale means to divide by the standard deviation
Stocks = Stocks[-c(31),] #To remove 31
na.omit(Stocks)%>%
  select_if(is.numeric)%>%
  prcomp(scale.=TRUE)->pca
pca
summary(pca)

plot(pca, type="l") #Shows the variances

biplot(pca,xlim=c(-0.4,0.4),cex=1)

#plotting using ggplot
str(pca)
pca$x
Stocks2 <- cbind(Stocks,pca$x[,1:2])

ggplot(Stocks2,aes(PC1,PC2,col=Market,fill=Market))



