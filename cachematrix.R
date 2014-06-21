## These two functions together make it easy to avoid calculating the inverse
## of a matrix if it has already been done.



## makeCacheMatrix creates four functions and a default value for the inverse of
## a matrix x, called invx.

## The four functions allow for easy caching and retrieval of the values x, invx.

## each time set() is called it is assumed that the matrix has changed, so the
## value of invx is reset.

## The value of invx can only be made non-null by the function setinv()

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL     #the default value of invx
    
    #define the functions
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) invx <<- inverse
    getinv <- function() invx
    
    #return the functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve makes use of setinv() to solve x, then cache the inverse,
## reducing the number of times it needs to be calculated.

cacheSolve <- function(x, ...) {
    invx <- x$getinv()     #grab invx from the cache
                           #(the cache is getinv()'s parent environment)
    
    if(!is.null(invx)) {     #check if there is already a value for invx
                             #skip computation if there is
        message("getting cached data")
        return(invx)
    }
    
    #if invx is not yet calculated: calculate it, cache it, then return it
    data <- x$get()
    invx <- solve(data, ...)
    x$setinv(invx)
    invx
}
