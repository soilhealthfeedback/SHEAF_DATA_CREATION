#loading the library
library(semPlot)
library(lavaan)
library(clusterGeneration) #this is to generate a positive definite covariance matrix

#simulate some data
set.seed(1222)

sig<-genPositiveDefMat("onion",dim=5,eta=4)$Sigma #the covariance matrix

mus<-c(10,5,120,35,6) #the vector of the means

data<-as.data.frame(mvrnorm(100,mu=mus,Sigma=sig)) #the dataset

names(data)<-c("CO2","Temp","Nitro","Biom","Rich") #giving it some names

#building an SEM with a latent variable
m<-'Abiot =~ CO2 + Temp + Nitro
Biom ~ Abiot
Rich ~ Abiot + Biom'
m.fit<-sem(m,data)

#the plot
#basic version, the what arguments specify what should be plotted, here we choose to look at the standardized path coefficients
semPaths(m.fit,what="std",layout="circle")


#define the label that will go into the nodes
lbls<-c("CO2\nconcentration","Temperature","Nitrogen\ncontent","Plant\nbiomass","Plant\nrichness","Abiotic\nenvironment")

#define the groups
grps<-list(Abiotic=c("CO2","Temp","Nitro","Abiot"),Plant=c("Biom","Rich"))

#define the layout
ly<-matrix(c(-0.5,-0.5,0,-0.5,0.5,-0.5,0,0.5,-0.5,0.5,0,0),ncol=2,byrow=TRUE)

#new plot
semPaths(m.fit,what="std",layout=ly,residuals=FALSE,nCharNodes=0,groups=grps,color=c("brown","green"),nodeLabels=lbls,sizeMan=8,posCol=c("blue","red"),edge.label.cex=1.5,legend=FALSE)
text(0.9,0.9,labels="Some text about\nthe model or\nthe weather in Indonesia")




semPaths(m.fit,what="std",nodeLabels=letters[1:6],edgeLabels=1:12,edge.label.cex=1.5,fade=FALSE)
