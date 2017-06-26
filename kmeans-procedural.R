setwd("E:/data/datasets/sample2 - kmeans/")
fileName <- "../book-machine learning/chapter 8/dogs.csv"
seperator <- ","
medianAndDeviation <- data.frame()
#### setting the number of k-means clusters
k <- 3
#### retrieve the file into a global variable
fileData <<- read.delim(fileName, 
	sep=seperator, head=T)

cnames <- c("breed", "height", "weight")
names(fileData) <- cnames

##############################
#### getting the median 
##############################
getMedian <- function(numCol){
	numCol <- sort(numCol)
	colLen <- length(numCol)

	if(colLen %% 2 == 1){
		numCol <- numCol[((length(numCol) + 1)/2)]
	}else{	
		v1 <- numCol[((length(numCol) + 1)/2)]
		v2 <- numCol[((length(numCol) + 1)/2)+1]
		numCol <- (v1 + v2) / 2.0
	}
	
}

################################
#### getting the asd
################################
getAbsoluteStandardDeviation <- function(numCol, median){
	sum = 0
	for (item in numCol){
		sum <- (sum + abs(item - median))
	}
	return(sum / length(numCol))
}

################################
#### Normalize a column
################################
normalizeColumn <- function(colNumber, set){
	col <- set[,colNumber]
	median <- getMedian(col)
	asd <- getAbsoluteStandardDeviation(col, median)
	medianAndDeviation <<- rbind(
			medianAndDeviation , cbind(median, asd))
	for (i in 1:length(col)){
		col[i] <- (col[i]-median)/asd
	}
	return(col)
}

#################################
#### calculate the manhattan distance
#################################
manhattanDistance <- function(vector1, vector2){
	dist <- data.frame()
	for (j in 1:nrow(vector2)){
			dist <- rbind(dist, c(j, sum(abs(vector1 - vector2[j,]))))
	
	}
	return(dist)
}






################################
#### Start the normalization ###
################################
##fileData <- fileData2
dataCols <- length(fileData)
numCols <- fileData[,-1]
for (i in 1:length(numCols)){
	numCols[,i] <- normalizeColumn(i,numCols)
}

fileData[,-1] <- numCols


################################
#### select random cluster centers
centroids <- fileData[sample(nrow(fileData), k), ]

#### which clusters do the data points belong to
#### default membership is "-1" for starters
membership <- vector()
for (i in 1:nrow(fileData)) membership <- c(membership, -1)


################################
################################
#### clustering
################################
#### keep track of how many points changed
pointsChanged <- 0
#### sum of squared error
sse <- 0
#### number of iterations made by the algorithm
#### to converge
iterationNumber <- 0

euclideanDistance <- function(x1, x2){
	return(sqrt(sum((fileData[x1,-1] - centroids[x2,-1])^2)))
	
}

################################
#### assigning specific point to a cluster
#### depending on the distance from the mean
################################
assignPointToCluster <- function(i){
	#### The minimum distance between the data point "i"
	#### that has been pursed and all centroids - initially too high
	#### but should be updated after distance between the two
	#### points has been calculated
	minDistance <- 999999
	clusterNum <- -1
	for (centroid in 1:k){
		dist <- euclideanDistance(i, centroid)
		if (dist < minDistance){
			minDistance = dist
			clusterNum = centroid
		}
	}
	if(clusterNum != membership[i]){
		pointsChanged <<- pointsChanged + 1
	}
	sse <<- sse + (minDistance^2)
	return(clusterNum)
}

################################
#### assigning points to a cluster
################################
assignPointsTocluster <- function(){
	pointsChanged <<- 0
	sse <<- 0
	for (i in 1:nrow(fileData)){
		membership[i] <<- assignPointToCluster(i)
	}
	
	return(membership)
}

################################
#### updating the centroids
################################
updateCentroids <- function(){
	#### number of members per cluster
	clusterMembers <- data.frame(table(membership))
	for (centroid in 1:nrow(centroids)){
		currentCentroidMembers <- data.frame()
		centroidFreq <- 
			clusterMembers[clusterMembers$membership == centroid, ]
		for (k in 1:nrow(fileData)){
			if(membership[k] == centroid){
				currentCentroidMembers <- 
					rbind(currentCentroidMembers, fileData[k,])
			}
		}
		for (name in 2:length(centroids)){
			centroids[centroid,][[name]] <<- 
				sum(currentCentroidMembers[[name]])/centroidFreq$Freq
		}
		
	}
	
}

### clusters
startClusters <- function(){
	assignPointsTocluster()
	done <- FALSE
	while (!done){
		iterationNumber <<- iterationNumber + 1
		updateCentroids()
		assignPointsTocluster()
		#### stop the 
		if ((pointsChanged / length(membership)) < 0.01){
			done = TRUE
		}
	}
	print(paste("Final SSE:", sse))
	print(paste("iterations", iterationNumber))
}
cls <- data.frame()
#### show the dogs in the 3 clusters
showClusterMembers <- function(){
	
	for (centroid in 1:nrow(centroids)){
		currentCentroidMembers <- data.frame()
		for (k in 1:nrow(fileData)){
			if(membership[k] == centroid){
				currentCentroidMembers <- 
					rbind(currentCentroidMembers, fileData[k,])
			}
		}
		print(paste("**** cluster ", centroid))
		print(currentCentroidMembers[1])
		cluster <- rep(centroid, nrow(currentCentroidMembers))
		currentCentroidMembers <- cbind(currentCentroidMembers, cluster)
		cls <<- rbind(cls, currentCentroidMembers)
		
	}

}

startClusters()
showClusterMembers()

cls$cluster <- as.factor(cls$cluster)

plot(cls$height, cls$weight, col=c("red","blue","green")[cls$cluster])


