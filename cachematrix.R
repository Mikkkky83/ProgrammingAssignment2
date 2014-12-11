## The following R script allows to calculate the inverse of a give matrix.
## The script calculates the inverse only the first time and stores the results
## in cache memory. If there is no change, the following times R is asked to
## provide the same inverse matrix the results is given from the cache without
## performing again the calculation, informing us with a short message.

## The following function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix, ... ) {
        invmatrix <- NULL
        set <- function(y) {
                x <<- y
                invmatrix <<- NULL  ## set the invmatrix to NULL
        }
        get <- function() {x} 
        setsolve <- function(solve) invmatrix <<- solve
        getsolve <- function() invmatrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The following function computes the inverse of the "matrix" above. 
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
        invmatrix <- x$getsolve()
        if(!is.null(invmatrix)) { 
                ## check if there an invmatrix has alredy been calculated
                message("getting cached matrix")
                return(invmatrix)
        }
        data <- x$get()
        invmatrix <- solve(data, ...)
        x$setsolve(invmatrix)
        invmatrix
}
