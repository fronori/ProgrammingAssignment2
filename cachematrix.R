## Functions to Cache the Inverse of a Matrix

## This makeCacheMatrix function creates a special "matrix", 
## which is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the balue of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y){
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) invmat <<- solve
        getsolve <- function() invmat
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This cacheSolve function calculates the inverse of the matrix created 
## with the above funcion.

cacheSolve <- function(x, ...) {
        invmat <- x$getsolve()
        if(!is.null(invmat)){
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setsolve(invmat)
        invmat
        ## Return a matrix that is the inverse of 'x'
}
