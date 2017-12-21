## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix creates a object that containing a methods to
## sets the value of the matrix
## gets the value of the matrix
## sets the value of the inverted matrix
## gets the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set_matrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get_matrix <- function() x
    store_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set_matrix=set_matrix, get_matrix=get_matrix, store_inverse=store_inverse, get_inverse=get_inverse)
}


## Write a short comment describing this function
## this function returns the inverse of a matrix. 
## It first checks to see if the inverse has been computed and will return the inversion from cache and skip the calculation
## If not in cache, it computes the inverse pushes that to cache (calling setinverse)

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("Returning cached data")
        return(inv)
    }
    ##get the source data from the cached object
    data <- x$get_matrix()
    ##use solve to invert this
    inv <- solve(data)
    ##inv is the inverted matrix, which is pushed into the cache 'x' before returning
    x$store_inverse(inv)
    ##return that bad boy now
    inv    
}

