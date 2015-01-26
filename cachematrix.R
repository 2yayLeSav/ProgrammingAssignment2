## A series of functions that caches the Inverse of a Matrix

## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        reverse <- NULL
        set <- function(y) {
                x <<- y
                reverse <<- NULL
        }
        get <- function() x
        setrev <- function(solve) reverse <<- solve
        getrev <- function() reverse
        list(set = set, get = get,
             setrev = setrev,
             getrev = getrev)
}

## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        reverse <- makeCacheMatrix(x)$getrev()
        if(!is.null(reverse)) {
                message("getting cached data")
                return(reverse)
        }
        data <- makeCacheMatrix(x)$get()
        reverse <- solve(data, ...)
        makeCacheMatrix(x)$setrev(reverse)
        reverse
}
