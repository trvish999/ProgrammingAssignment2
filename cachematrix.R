## This file has has functions to enable caching of matrix inverse computations

## This function creates a special "matrix" that's really a list with functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the matrix inverse
## 4. get the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of the matrix, reusing previously computed values
## if needed. x should be a list returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    cachedInverse <- x$getinverse()
    if (!is.null(cachedInverse)) {
        message("getting cached data")
        return(cachedInverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
