## This script contains two functions to compute inverse of a matrix
## and use cached value to improve performance

## First function makeCacheMatrix takes matrix as an input argument 
## and returns a special matrix object that cache its inverse
## A special operator <<- is used below to assign the set values to parent variables x and i (i variable has inverse of matrix x)
## Variable i is set to NULL initially, and it is also set to NULL when set function is called to reset the matrix variable.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Second function cacheSolve takes special matrix object returned by above function as an input argument and returns the inverse matrix
## This function checks if inverse is already computed and skips the computation to improve performance if inverse is already available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
                
