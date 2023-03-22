# libraries
library(jpeg)
library(rasterImage)
library(imager)

# upload an image
landscape<-readJPEG("landscape.jpg")

# plot the raster image
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(landscape,0,0,1,1)

# inspect the dimensions of the object
dm1<-dim(landscape) 
dm1

# develop a data frame
# get the coordinates of pixels - RGB info in three columns
image_df<-data.frame(x=rep(1:dm1[2], each=dm1[1]),  y=rep(dm1[1]:1, dm1[2]), r.value=as.vector(landscape[,,1]),  g.value=as.vector(landscape[,,2]), b.value=as.vector(landscape[,,3]))
head(image_df)

# plot the image in RGB
plot(y~x, data=image_df, main="Desert beach landscape RGB", col=rgb(image_df[c("r.value", "g.value", "b.value")]), asp=1, pch=".")


# apply CLARA and get Silhouette 
library(cluster)


# empty vector to save results
n1<-c() 

# number of clusters to consider
for (i in 1:30) {
  cl<-clara(image_df[, c("r.value", "g.value", "b.value")], i)
  # saving silhouette to vector
  n1[i]<-cl$silinfo$avg.width
}

plot(n1, type='l', main="Optimal number of clusters", xlab="Number of clusters", ylab="Average silhouette", col="blue")
points(n1, pch=21, bg="navyblue")
abline(h=(1:30)*5/100, lty=3, col="grey50")

# Silhouette information, for 2 clusters
clara<-clara(image_df[,3:5], 2) 
plot(silhouette(clara))

# assign medoids (“average” RGB values) to each cluster id and convert RGB into colour
colours<-rgb(clara$medoids[clara$clustering, ])

# plot pixels in the new colours
plot(y~x, data=image_df, col=colours, pch=".", cex=2, asp=1, main="2 colours")

# Function to export the clustered image to jpeg
clustering_to_jpeg <- function(image_matrix, cluster_object, k, file_name) {
  
  # Determine the cluster for each pixel
  cluster_df <- cbind(image_matrix, cluster_object$clustering)
  
  # Function to swap number of cluster for its center
  medoids <- function(x){
    for (i in 1:k){
      if(x==i) return(cluster_object$medoids[i,])
    }
  }
  
  # Apply above function
  a <- t(apply(as.data.frame(cluster_df$`cluster_object$clustering`), 1, medoids))
  
  # Change dimension from single columns to matrix
  img_list <- list(matrix(a[,1], nrow=dim(landscape)[1], ncol=dim(landscape)[2]),
               matrix(a[,2], nrow=dim(landscape)[1], ncol=dim(landscape)[2]),
               matrix(a[,3], nrow=dim(landscape)[1], ncol=dim(landscape)[2]))
  
  # Transformation of data to jpeg package format
  img_list <- sapply(img_list, function(x) {compressed.img <- x}, simplify = 'array')
  
  # Save the image to a JPEG file
  writeJPEG(img_list, file_name)
}

clustering_to_jpeg(image_df, clara, 2, 'asd.jpg')

# how many original colours were on picture?
cols.org<-rgb(image_df[,3:5])
head(cols.org)
length(unique(cols.org))






