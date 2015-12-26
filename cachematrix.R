## Functions to cache time-consuming calculation of the inverse of a matrix.

## Creates a strutcture to store a matrix and its inverse. 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Fecthes a cached inversed matrix in a structure created by makeCacheMatrix(). 
## If its not set, calculates it with solve() and stores it in the structure.
cacheSolve <- function(x, ...) {
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
