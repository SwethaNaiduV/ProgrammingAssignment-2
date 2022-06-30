## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Function: makeCacheMatrix - It usually makes a exceptional 'matrix' object. This 'matrix' object can cache the matrix object's inverse.

makeCacheMatrix <- function(x = matrix()) {
         d <- NULL
    set <- function(y) {
        x <<- y
        d <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) d <<- inverse
    getinverse <- function() d
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)


}


## Write a short comment describing this function

## Function: cacheSolve - It calculates the inverse of the exceptional 'matrix' object which the above makeCacheMatrix gives.
##If the matrix is same then the inverse is already calculated,in that case cacheSolve will have to recover the solution from the CacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        d <- x$getinverse()
    if(!is.null(d)) {
        message("getting cached data")
        return(d)
    }
    mat <- x$get()
    d <- solve(mat, ...)
    x$setinverse(d)
    d
}
