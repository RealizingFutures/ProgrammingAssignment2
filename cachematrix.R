## This file contains two functions that create a special object that stores
## a numeric matrix and caches its inverse using the solve() function.
## The function assumes that the matrix supplied is invertible.

## This function creates a special object that stores a numeric matrix
## and can also store the inverse matrix.
## Return a list of functions: set, get, setsolve, and getsolve.

makeCacheMatrix <- function(x = matrix()) {
        ## Return a list of functions
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)       
}


## This funtion checks to see if the inverse of a matrix is already
## cached and if not it calculates the inverse and caches it. 
## Return the inverse matrix of the first argument.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s  
}
