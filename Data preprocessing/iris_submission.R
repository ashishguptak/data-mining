## Reading the iris dataset from file
iris_df<-read.csv(file.choose())

size<-dim(iris_df)
iris_row<-size[1]
iris_col<-size[2]

euclid_iris<-function()
{
  eu_mat <- matrix(0L,nrow = iris_row,ncol = 11,byrow = T)
  colnames(eu_mat) <- c("Transaction ID","1st","1-dist","2nd","2-dist","3rd","3-dist","4","4-dist","5","5-dist")
  
	for(i in 1:iris_row)
	{	
		mat <- matrix(,nrow=iris_row,ncol=2,byrow=T)
		colnames(mat) <- c("index","euclid distance")
		for(j in 1:iris_row)
		{
		  if(i!=j) 
		  {
		    k1 <- ((iris[[1]][[i]] - iris[[1]][[j]])^2) 
		    k2 <- ((iris[[2]][[i]] - iris[[2]][[j]])^2) 
		    k3 <- ((iris[[3]][[i]] - iris[[3]][[j]])^2) 
		    k4 <- ((iris[[4]][[i]] - iris[[4]][[j]])^2) 
		    
		    d <- (k1+k2+k3+k4)^0.5
		    d<-round(d,digits=3)
		    
		    mat[j,1] <- j
		    mat[j,2] <- d
		  }
		}
		y <- order(mat[,2])
		sortedMat <- mat[y,]
		
		## Now to save it in a result matrix
		eu_mat[i,1]<- i
		eu_mat[i,2]<- sortedMat[1,1]
		eu_mat[i,3]<- sortedMat[1,2]
		eu_mat[i,4]<- sortedMat[2,1]
		eu_mat[i,5]<- sortedMat[2,2]
		eu_mat[i,6]<- sortedMat[3,1]
		eu_mat[i,7]<- sortedMat[3,2]
		eu_mat[i,8]<- sortedMat[4,1]
		eu_mat[i,9]<- sortedMat[4,2]
		eu_mat[i,10]<- sortedMat[5,1]
		eu_mat[i,11]<- sortedMat[5,2]
	}
  return(eu_mat)
}

eu_mat <- euclid_iris()

print("Now showing Manhattan_iris")

Manhattan_iris<-function()
{

  man_mat <- matrix(0L,nrow = iris_row,ncol = 11,byrow = T)
  colnames(man_mat) <- c("Transaction ID","1st","1-dist","2nd","2-dist","3rd","3-dist","4","4-dist","5","5-dist")
  
	for(i in 1:iris_row)
	{	
		mat <- matrix(,nrow=iris_row,ncol=2,byrow=T)
		colnames(mat) <- c("index","Manhattan distance")
		for(j in 1:iris_row)
		{
		  if(i!=j)
		  {
		    k1 <- abs((iris[[1]][[i]] - iris[[1]][[j]])) 
		    k2 <- abs((iris[[2]][[i]] - iris[[2]][[j]])) 
		    k3 <- abs((iris[[3]][[i]] - iris[[3]][[j]])) 
		    k4 <- abs((iris[[4]][[i]] - iris[[4]][[j]])) 
		    
		    d <- (k1+k2+k3+k4)
		    
		    d<-round(d,digits=3)
		    
		    mat[j,1] <- j
		    mat[j,2] <- d
		  }
		}
		y <- order(mat[,2])
		sortedMat <- mat[y,]
		
		## Now to save it in a result matrix
		man_mat[i,1]<- i
		man_mat[i,2]<- sortedMat[1,1]
		man_mat[i,3]<- sortedMat[1,2]
		man_mat[i,4]<- sortedMat[2,1]
		man_mat[i,5]<- sortedMat[2,2]
		man_mat[i,6]<- sortedMat[3,1]
		man_mat[i,7]<- sortedMat[3,2]
		man_mat[i,8]<- sortedMat[4,1]
		man_mat[i,9]<- sortedMat[4,2]
		man_mat[i,10]<- sortedMat[5,1]
		man_mat[i,11]<- sortedMat[5,2]
	}
  
  return(man_mat)
}

man_mat <- Manhattan_iris()

#Load xls lib for exporting distances to csv files 
library(xlsx)
write.csv(eu_mat,file="iris_euclidean.csv")
write.csv(eu_mat,file="iris_manhattan.csv")