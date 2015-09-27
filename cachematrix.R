#Creates a special "matrix" object that can cache the inverse of matrix 'x'

#The object is a list containing 4 functions:
#1. set(): set the value of the input matrix
#2. get(): get the value of the input matrix
#3. setinverse(): set the value of the inverse matrix
#4. getinverse(): get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function () x
    setinverse <- function(z) inv <<- z
    getinverse <- function () inv
    list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

#Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    data <- x$get()
    # Handle singular matrix case
    if(class(try(solve(data, ...),silent=T))=="matrix") {
        newInv <- solve(data, ...)
        x$setinverse(newInv)
        # Return a matrix that is the inverse of 'x'
        return (newInv)
    }
    else message("Matrix Inverse Nonexistent")
}
