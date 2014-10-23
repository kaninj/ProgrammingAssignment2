## The following two functions create a special object
## that stores a numeric matrix and caches its inverse matrix.


## 'makeCacheMatrix' creates a special vector containing functions
## to get/set the value of the matrix, 
## get/set the value of the inverse matrix.

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


## 'cacheSolve' returns the inverse matrix of a invertible matrix
## created by 'mekCacheMatrix'. It gets the inverse matrix stored
## in the special vector if it has already been calculated. 
## Otherwise, it computes the inverse matrix and stores it 
## in the special vector via 'setinverse' function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
