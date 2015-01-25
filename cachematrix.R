## This is a set of functions that are intended to speed up matrix inversions
## for repeated use by caching calculated inversions and their matrices
## in a special object in an external environment.

## makeCacheMatrix will create a list of functions that will store the matrix, 
## retrieve the matrix, set the inverse, and get the inverse 
## from a special object in a function specific environment

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## store the provided matrix
    set <- function(y) {  
        x <<- y
        i <<- NULL
    }
    ## retrieve the matrix 
    get <- function() x  
    
    ## set the calculated inverse
    set.inverse <- function(inverse) i <<- inverse 
    
    # get the cached inverse
    get.inverse <- function() i 
    
    # return the list of functions with names for ease of use in cacheSolve
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)  
}


## cacheSolve will check for a cached inverse value for a supplied matrix 
## and if there is no inverse cached it will calculate and store it. 
## The inverse matrix is the returned value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Attempt to get cached value - if present, return that value
    i <- x$get.inverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Cached value wasn't present - retrieve matrix and calculte inverse
    data <- x$get()
    i <- solve(data, ...)
    
    ## Store the calculated inverse in the cache
    x$set.inverse(i)
    i
}
