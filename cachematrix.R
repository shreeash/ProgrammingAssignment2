## makeCacheMatrix creates a special matrix that can cache its inverse 
## with a list containing functions to perfom the following operations 

## set - sets the value of the matrix
## get - gets the value of the matrix
## setinverse - sets the value of the inverse of the matrix
## getinverse - gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve(x) computes the inverse of the special matrix
## from makeCacheMatrix(x) and returns the inverse of x 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    ## If the inverse has already been calculated (and the matrix has 
    ## not changed), then this function retrieves the inverse from the cache
    
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    ## Else the following function will compute the inverse and return the value
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}