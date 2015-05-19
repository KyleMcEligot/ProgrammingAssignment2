## This file has two functions to support Caching the Inverse of a Matrix
##
##   makeCacheMatrix: creates a special "matrix" object that can cache its 
##                    inverse.
##
##   cacheSolve: computes the inverse of the special "matrix" returned by 
##               makeCacheMatrix above. 
##               If the inverse has already been calculated (and the matrix 
##               has not changed), then the cachesolve retrieves the inverse 
##               from the cache.

## ---------------------------------------------------------------------

## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
##      - set the value of the matrix
##      - get the value of the matrix
##      - set the value of the inverse
##      - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    getinverse <- function() {
        inv
    }
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)     # return a list of functions
}

## ---------------------------------------------------------------------

## cacheSolve calculates the inverse of the special "matrix" created with 
## makeCacheMatrix. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {                 # if the inverse was cached
        message("getting cached data")
        return(inv)                     # return the cached inverse
    }
    
    # the inverse was not cached
    
    data <- x$get()                     # obtain the matrix data
    
    # Calculate the inverse of the matrix
    # 
    # Note from the programming assignment: 
    #   Computing the inverse of a square matrix can be done with the 
    #   solve function in R. For example, if X is a square invertible 
    #   matrix, then solve(X) returns its inverse.
    #
    #   For this assignment, assume that the matrix supplied is always 
    #   invertible.
    #
    # For the assignment, given the assurance that the matrix is always 
    # inverible, the data will not be checked to be an inverable matrix
    # (a square matrix that is not singular (using det() to check for a 
    #  non-zero determinant)))
    inv <- solve(data, ...)             # calculate the inverse of the matrix
    x$setinverse(inv)                   # cache the inverse of the matrix
    inv                                 # return the inverse of the matrix
}
