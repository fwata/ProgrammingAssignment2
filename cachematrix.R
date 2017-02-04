# Coursera Data Science - R Programming
# Programming Assignment 2: Lexical Scoping

# makeCacheMatrix: this function creates a special "matrix" object
# that can cache its inverse.

# cacheSolve: this function computes the inverse of the special "matrix"
# returned by makeCacheMatrix. If the inverse has already been calculated
# (and the matrix has not changed), then the it retrieves the inverse from
# from the cache.

# For this assignment it's assumed that all the inputed matrixes are 
# invertible.


makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <- NULL
        }
        
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i 
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



cacheSolve <- function(x, ...) {
        
        i <- x$getinv()
        
        if (!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        
        i
}
