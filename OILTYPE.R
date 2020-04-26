#**************************************************************************************
#* OILTYPE       DATA MINING                               *                                *
# *************************************************************************************

# Loading Required Packages

library(caret)  # This package contains the data set OilTypes
library(dplyr)  # This package is used for data handling
library(ggplot2)# This package helps to plot user defined graphs
library(xtable) # This package helps to import tables to latex format

# Loading the required data set
data(oil)

# Investigating the variable oilType
str(oilType)
table(oilType) # distribution of data in Oil Type

# Storing data to a new variable for further investigation
# Converted to charactter type so that row number access is possible
OT<-as.character(oilType)
# Outputing latex vrsion of table
xtable(table(OT), type = "latex", file = "filename2.tex")

# This section containts multiple run of sampling on oil type
# A dataframe is defined to store the distribution of sampled oil types
Results<-data.frame()

# 1000 times iteration to see the pattern
for (i in 1:1000){
  s<-sample(sample(1:length(OT), size = 60)) # to get random 60 numbers from 96
  b<-OT[s] # storing the sampled data to new data
  a<-table(b) # category wise breakup of sample
  Results[i,1]<-sum(b=='A')/60 # Proprtion of A in sample set
  Results[i,2]<-sum(b=='B')/60 # Proprtion of B in sample set
  Results[i,3]<-sum(b=='C')/60 # Proprtion of C in sample set
  Results[i,4]<-sum(b=='D')/60 # Proprtion of D in sample set
  Results[i,5]<-sum(b=='E')/60 # Proprtion of E in sample set
  Results[i,6]<-sum(b=='F')/60 # Proprtion of F in sample set
  Results[i,7]<-sum(b=='G')/60 # Proprtion of G in sample set
}
# Naming of the columns of results data frame
colnames(Results)<-c('A','B','C','D','E','F','G')

# The following section plots the distribution

# ploting the distribution of A oil prportion in samples
ggplot(Results, aes(x=A)) + 
  geom_histogram(binwidth = 0.05,color="white", fill=rgb(0.2,0.7,0.1,0.4))+
  geom_vline(data = OT, xintercept=sum(OT=='A')/96, alpha=0.5, size=2, linetype="longdash")+
  geom_vline(aes(xintercept = mean(Results$A)),col='red',size=2)



# ploting the distribution of B oil prportion in samples
ggplot(Results, aes(x=B)) + 
  geom_histogram(binwidth = 0.05,color="white", fill=rgb(0.8,.3,0.1,0.4))+
  geom_vline(data = OT, xintercept=sum(OT=='B')/96, alpha=0.5, size=2, linetype="longdash")+
  geom_vline(aes(xintercept = mean(Results$B)),col='red',size=2)

# ploting the distribution of C oil prportion in samples
ggplot(Results, aes(x=C)) + 
  geom_histogram(binwidth = 0.01,color="white", fill=rgb(0.8,.3,0.8,0.4))+
  geom_vline(data = OT, xintercept=sum(OT=='C')/96, alpha=0.5, size=2, linetype="longdash")+
  geom_vline(aes(xintercept = mean(Results$C)),col='red',size=2)


# ploting the distribution of D oil prportion in samples
ggplot(Results, aes(x=D)) + 
  geom_histogram(binwidth = 0.01,color="white", fill=rgb(0.1,.3,0.8,0.4))+
  geom_vline(data = OT, xintercept=sum(OT=='D')/96, alpha=0.5, size=2, linetype="longdash")+
  geom_vline(aes(xintercept = mean(Results$D)),col='red',size=2)

# ploting the distribution of E oil prportion in samples
ggplot(Results, aes(x=E)) + 
  geom_histogram(binwidth = 0.01,color="white", fill=rgb(0.9,.1,0.1,0.4))+
  geom_vline(data = OT, xintercept=sum(OT=='E')/96, alpha=0.5, size=2, linetype="longdash")+
  geom_vline(aes(xintercept = mean(Results$E)),col='red',size=2)


# ploting the distribution of F oil prportion in samples
ggplot(Results, aes(x=F)) + 
  geom_histogram(binwidth = 0.01,color="white", fill=rgb(0.5,0.5,0.5,0.4))+
  geom_vline(data = OT, xintercept=sum(OT=='F')/96, alpha=0.5, size=2, linetype="longdash")+
  geom_vline(aes(xintercept = mean(Results$F)),col='red',size=2)


# ploting the distribution of G oil prportion in samples
ggplot(Results, aes(x=G)) + 
  geom_histogram(binwidth = 0.01,color="white", fill=rgb(0.6,0.7,0.1,0.4))+
  geom_vline(data = OT, xintercept=sum(OT=='G')/96, alpha=0.5, size=2, linetype="longdash")+
  geom_vline(aes(xintercept = mean(Results$G)),col='red',size=2)

# using stratified sampling
train.index <- createDataPartition(OT, p = .59, list = FALSE)[,1]
train <- OT[ train.index]
test  <- OT[-train.index]
table(train)

xtable(table(train), type = "latex", file = "filename2.tex")

#******************************************Concluded***********************************
