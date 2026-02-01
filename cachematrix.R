## Put comments here that give an overall description of what your
## functions do

## This functions creates a special matrix object that can then cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(a) {
        x <<- a
        z <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) z <<- inverse
        getInverse <- function() z 
        list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getInverse()
        if(!is.null(z)) {
        message("getting cached data")
        return(z)
        }

        data <- x$get()
        z <- solve(data,...)
        x$setInverse(z)
        z
        }
