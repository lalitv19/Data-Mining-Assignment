#**************************************************************************************
# USARREST DATA 
# *************************************************************************************
# Load the given data
df<-read.csv('F:/MS BA/Data Mining/Assignment1/USArrests.csv')

# Set row name as city names
rownames(df)<-df$X

# Scaling on the data frame avoiding 
dsc = scale(df[-1])

# Loading packages
library(dendextend) # to plot dentograms
library(tidyverse)  # Many packages for data handling
library(tables)     # tabular way to cluster data

# Hierarchical clustering on unscaled data eucledian distance and method as complete
d1 = df [-1]%>% dist() %>% hclust( method="complete" ) %>% as.dendrogram()

# plotting the hierarchical clusters with k =3
d1 %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  plot(horiz=TRUE, axes=FALSE)
abline(v = 350, lty = 2)

# Hierarchical clustering on scaled data eucledian distance and method as complete
d2=dsc%>% dist() %>% hclust( method="complete" ) %>% as.dendrogram()

# plotting the hierarchical clusters with k =3
d2 %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  plot(horiz=TRUE, axes=FALSE)
abline(v = 350, lty = 2)
# Comparison of clustering models
d <- dendlist(
  d1 %>% 
    set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
    set("branches_lty", 1) %>%
    set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3),
  d2 %>% 
    set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
    set("branches_lty", 1) %>%
    set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3)
)

# Plot of compared models
tanglegram(d, 
           common_subtrees_color_lines = FALSE, highlight_distinct_edges  = TRUE, highlight_branches_lwd=FALSE, 
           margin_inner=7,
           lwd=2
)

# table comparing both models
table(cutree(d1, 3), cutree(d2, 3))

#Kmeans Clustering
# scaled data clustering with kmeans with k =3 and 20 restarts
cluster_three_scaled <- kmeans(as.data.frame(dsc),3,nstart=20)
# unscaled data clustering with kmeans with k =3 and 20 restarts
cluster_three <- kmeans(as.data.frame(df[-1]),3,nstart=20)
# table comparing both models
table(cluster_three$cluster, cluster_three_scaled$cluster)
# table comparing unscaled clustering with hclust and kmeans
table(cluster_three$cluster,cutree(d1, 3))
# table comparing scaled clustering with hclust and kmeans
table(cluster_three_scaled$cluster,cutree(d2, 3))

# add all the clusters to the base data
df_new<-cbind(df,KM_c=cluster_three$cluster,KM_cs=cluster_three_scaled$cluster,
              h_c=cutree(d1, 3),h_cs=cutree(d2, 3))

# make all the clustering variables as factors
df_new$KM_c<-base::as.factor(df_new$KM_c)
df_new$KM_cs<-base::as.factor(df_new$KM_cs)
df_new$h_c<-base::as.factor(df_new$h_c)
df_new$h_cs<-base::as.factor(df_new$h_cs)

# pre-profiling the data for cluster analysis
profile<-tabular(1+Murder+Assault+UrbanPop+Rape ~  mean +
                   (mean*KM_c)+(mean*KM_cs)+(mean*h_c)+(mean*h_cs),
                 data=df_new[-1])

profile1<-as.matrix(profile)
profile1<-data.frame(profile1)

# pre-profiling the data cluster numbers for cluster analysis
profile<-tabular(1~length+(length*KM_c)+(length*KM_cs)+(length*h_c)+(length*h_cs),
                 data=df_new[-1])
profile2<-as.matrix(profile)
profile2<-data.frame(profile2)

# downloading data for further analysis
write.csv(profile1, "profile1.csv")
write.csv(profile2, "profile2.csv")

#******************************************Concluded***********************************
