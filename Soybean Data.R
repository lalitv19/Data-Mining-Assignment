# ********************************************************
#  Soybean data set analysis                                                       
# ********************************************************

# Loading the necessary package to get data Soybean
library(mlbench)
data(Soybean)
d<-Soybean # The data is stored into a dataframe d

# Loading other required packages
library(ggplot2)  # This package is required for creating user defined plots 
library(dplyr)    # This package is useful for easy data handling
library(reshape2) # This package helps to handle matrix form of data
library(corrplot) # This library hlps to plot correlation plot
library(Amelia)   # This package helps to study missing data 
library(mice)     # This package helps to study missing data and imputation
library(VIM)      # This package helps to variable relations
library(lattice)  # This package helps in plotting 


# SECTION 1

# The following section aims to plot the variance of alll categorical variables 
# in a single plot

x<-apply(d[,2:36], 2, table) # We create a list of table functions applied to all variables
                             # We have puposefully avoided table function on class variable

# It may be noted that we have a maximum of only 7 variables in all the variables
# The following function converts the list into a matrix
fin<-sapply(x, '[', seq(max(sapply(x,length))))

# We transpose the matrix to get row as the predictors and convert it to a data frame
fin<-data.frame(t(fin))

# We name the row as the predictor names and convert the xtable form to tidy form.
# This is for easy plotting of the data. 
fin2<-data.frame(rows = rownames(fin), stack(fin))

# There are few NA rows in the data. This is because of few variables not having all 7 values
# the following code removes these rows
fin2<-na.omit(fin2)

# The following code is to plot all the variables in a single plot to identify the distribution 
# and missing data
ggplot(fin2, aes(fill=ind, y=values, x=rows)) + 
  geom_bar( stat="identity")+scale_fill_brewer(palette = "Dark2")+
  geom_text(aes(label = values), size = 2, hjust = 0.5, vjust = 1, position ="stack")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))


# SECTION 2

# This section dels with missing data analysis
# We create a fuction to convert factors to character
asNum <- function(x) as.character(x)

# We use the above defined function to convert all factors to numeric by defining following
# function

factorsNum <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                   asNum))
# The user defined function is applied to our data set d
f <- factorsNum(d)

# We need to indentify where the data is missing
ma<-f
ma[!(ma=='') ]<-0 # 1 if data is available
ma[is.na(ma)]<-1 # 0 if data is missing

# The resulting data frame automatically converted it to factors. so now we convert it to
# numeric so that we can identify patterns
ma1<-data.frame(lapply(ma, function(x) as.numeric(x)))

# This gives the missing data of class as well. It is not of interest to us rather we
# like to know if missing data has relation to class. So we need to group by class.
# Thus we manually add class to ma1
ma2<- cbind(ma1[,-1],class=f$Class)

# Now we group by class and take sum of missing data in each class variable wise
ma3<-ma2%>%group_by(class)%>%summarise_all(list(sum))

# We convert the data to matrix form 
x2b<-as.matrix(ma3[-1])

# name the rows as class names
rownames(x2b)<-ma3$class

# we convert data from xtable to tidy verse form for easy plotting with ggplot
x2=melt(x2b)

# name the columns, the count decides color intensity
names(x2)=c("x","y","color")

# the x variable is automatically taken as factor. so we convert it to character
x2$x<-as.character(x2$x)
# The following code plots the matrix form to see missing data patern with class labels

ggplot(x2, aes(x = x, y = y))   +
  geom_raster(aes(fill=color)) + 
  scale_fill_gradient(low="grey90", high="orange") + 
  labs(x="Class Labels", y="predictors") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=-45, vjust=0.3, hjust=0.1),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))


# SECTION 3

# We plot correlation of missing data to see set of variables that disappear together
# we remove the variables Class and leaves as they have no missing data

cord<-ma2%>%select(-c('class','leaves'))
M<-cor(cord)
corrplot(M, method="circle")

# SECTION 4
# The following function plots the missing data map
missmap(f,col=c('blue','gray'),y.at=1,y.labels='',legend=TRUE)

# Missing data pattern with row type analysis is given by
nhanes_miss <- aggr(d, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(nhanes), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

# The following code is used to take data out of default plotter so that user defined graphs
# can be created
x<-md.pattern(d) 
x3<-data.frame(x[1:9,37])
x1=melt(x[1:9,1:36]) # converting to tidy form
names(x1)=c("x","y","color")
x1$x<-as.character(x1$x)

ggplot(x1, aes(x = x, y = y)) + 
  geom_raster(aes(fill=color)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="No of Obs", y="predictors") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

# SECTION 5
#In this section we try to identify if some variables with degenerate distribution can be 
# removed
d<-Soybean

# Removing degeneratevariables
d$mycelium<-NULL
d$sclerotia <-NULL
d$leaves<-NULL
d$lodging<-NULL
d$shiveling<-NULL
# As other missing data can influence class we can impute it
library(DMwR)
knnOutput <- knnImputation(d)

str(d)

#******************************************Concluded***********************************
