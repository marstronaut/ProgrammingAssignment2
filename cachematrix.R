## This is a set of functions that are intended to speed up matrix inversions
## for repeated use by caching calculated inversions and their matrices
## in a special object in an external environment.

## makeCacheMatrix will create a special object that will store matrices
## and their inverses. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set.inverse <- function(inverse) i <<- inverse
    get.inverse <- function() i
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
    
}


## cacheSolve will calculate the inverse of a supplied matrix 
## if there is no inverse cached already. The returned value is the
## inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get.inverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set.inverse(i)
    i
}
