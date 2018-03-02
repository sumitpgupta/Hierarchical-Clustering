---
  title: "Hierarchial Clustering on Water treatment data"
author: "Sumit Gupta"
date: "December 22, 2017"
output:
  pdf_document: default
html_document: default
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Loading libraries:
  ```{r}
library(data.table)
library(ggplot2)
library(fpc)
```

```{r}
# Loading the dataset
water_data <- read.table("water-treatment.data.txt", header = F, sep=",", na.strings = c("?")) # inserting NA's in place of "?"
setDT(water_data)
# str(water_data)
```

```{r}
# Checking missing values
colSums(is.na(water_data))
```

```{r}
#impute missing values with median

for(i in colnames(water_data)[!(colnames(water_data) %in% c("V1"))])
  set(x = water_data,i = which(is.na(water_data[[i]])), j = i, value = median(water_data[[i]], na.rm = T))
```
```{r}
# Scaling the variables except V1
scaled_wd <- scale( water_data[,-c("V1"),with=F])
```

Now, our data is ready for clustering! 
  For hierarchical clustering, we'll first calculate a distance matrix based on Euclidean measure. Then using the hclust function, we can implement hierarchical clustering.

```{r}
# distance matrix
d <- dist(scaled_wd, method = "euclidean")

# Hierarchical Clustering
h_clust <- hclust(d, method = "ward") #clustering

```

```{r}


# Plotting the dendrogram
plot(h_clust, labels = water_data$V1)

# All the leaves at the bottom carry one observation each, which are then merged into similar values as they rise upward. 
# Now, how can you estimate the number of clusters? 
# Going by the logic of horizontal cut, 4 clusters are evident. 
rect.hclust(h_clust,k=4)

```


Lets see which observation is into which cluster:
```{r}

groups <- cutree(h_clust,k=4) 
groups
```
Let's visualize the clusters in a better way. However, visualizing this high dimensional data in one plot has its own challenges. A smart way could be, to visualize the cluster on principal components of this data. This way we would be able to capture most of information by reducing the data dimension.

To implement PCA, we'll use princomp base function. For our convenience, we'll take only the first two components.

```{r}
# pca 
pcmp <- princomp(scaled_wd)
pred_pc <- predict(pcmp, newdata = scaled_wd)[,1:2]
```

Now, we'll create a data frame having pc values and their corresponding clusters. Then, using ggplot2 we'll create the plot.

```{r}
comp_dt <- cbind(as.data.table(pred_pc),cluster = as.factor(groups), Labels = water_data$V1) # Combining the Principal components and Cluster

ggplot(comp_dt,aes(Comp.1,Comp.2))+ geom_point(aes(color = cluster),size=3)


```


Let's now proceed to k means clustering. We'll use the base function k means with 100 iterations to converge:
  
  
  ```{r}
#kmeans

kclust <- kmeans(scaled_wd,centers = 4,iter.max = 100)
```

You can check out the number of observations comprised by each cluster using kclust$size. Again, let's create a plot to understand this clustering better. The parameter.

```{r}
kclust$size
```
```{r}
# Lets visualize
ggplot(comp_dt,aes(Comp.1,Comp.2))+ geom_point(aes(color = as.factor(kclust$cluster)),size=3)

```

As seen above, both the techniques have partitioned the observations in same clusters. Is 4 the optimal number of clusters in k means ? Let's find out. 
To pick the best value of k, we'll use kmeansruns function from fpc package. This function is enabled with two distance metrics: Average silhouette width and Calinski-Harabasz. 

Let's try to use both the methods and check out the best k value:
  
  ```{r}
tunek <- kmeansruns(scaled_wd,krange = 1:10,criterion = "ch") 
tunek$bestk #3
tunekw <- kmeansruns(scaled_wd,krange = 1:10,criterion = "asw") 
tunekw$bestk #4

```



