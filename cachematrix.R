## The 2 functions below can be used to create a special matrix whose inverse can be 
## cached as calcuating the inverse of a matrix is usually a costly computation

## The makeCacheMatrix function is used to create a special "cacheble" matrix that can cache its inverse
## This function by iteself cannot be used to calculate and cache the inverse of the matrix.
## This function in turn exposes other function to 
## 1) set and get the value of the matrix.
## 2) set and get the inverse value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
 	inv <- NULL
  	set <- function(y){
    	x <<- y
    	inv <<- NULL
  	}
  	get <- function() x
  	setinverse <- function(inverse) inv <<- inverse
  	getinverse <- function() inv
  	list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function is used to calculate the inverse of a matrix that gets created 
## using the function "makeCacheMatrix". 
## This function caches the inverse value of the matrix, that gets calculated using the R's inbuilt solve function,
## if its not already calculated.

cacheSolve <- function(x, ...) {    
	## Return a matrix that is the inverse of 'x'
  	inv <- x$getinverse()
  	if(!is.null(inv)){
    	message("Getting the cached Inverse")
    	return(inv)
  	}
  	mat<-x$get()
  	inv <- solve(mat)
  	x$setinverse(inv)
  	inv
}
