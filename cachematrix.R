## The following functions create a matrix and calculate an inverse matrix to it. 

## makeCacheMatrix function creates a matrix with a prespecified entries. 

makeCacheMatrix <- function(x = matrix()) {
        Inverse <- NULL
        set <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) Inverse <<- solve
        getInverse <- function() Inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve function checks if an Inverse function was already calculated for the makeCacheMatrix matrix,
##and then returns the cached inverse function. If not, it calculates the Inverse function, 
##and caches it via makeCacheMatrix.

cacheSolve <- function(x=matrix(), ...) {
        Inverse <- x$getInverse()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setInverse(Inverse)
        Inverse
}

