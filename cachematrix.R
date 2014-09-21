## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## set the value of the matrix
    	inv<- NULL
    	set <- function(y) 
	 {
     	  x <<- y
     	  inv <<- NULL
   	 }
   	## get the value of the matrix
   	get <- function() x
   
   	## set the inverse of the matrix
   	setinverse <- function(solve) inv <<- solve
   	getinverse <- function() inv
   
   	## get the inverse of the matrix
   	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   
   	## get the inverse of the matrix        
   	inv <- x$getinverse()
   
   	## check if there is the matrix   
   	if(!is.null(inv)) {
     	message("getting cached data")
     	return(inv)
   	}
   	## if not: get the inverse of the matrix   
   	data <- x$get()
   	inv <- solve(data, ...)
   	## set the inverse of the matrix 
   	x$setinverse(inv)
   	inv
}

##Test:
##> x <- rbind(c(4,3),c(3,2))
##> m <- makeCacheMatrix(x)
##> m$get()
##     [,1] [,2]
##[1,]    4    3
##[2,]    3    2
##> cacheSolve(m)
##     [,1] [,2]
##[1,]   -2    3
##[2,]    3   -4
##> cacheSolve(m)
##getting cached data
##     [,1] [,2]
##[1,]   -2    3
##[2,]    3   -4
##> 