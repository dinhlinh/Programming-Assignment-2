## Create a special "matrix", which is a list containing a function to
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse matrix
##   get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Calculate the inverse of the special "matrix" created with the above 
## makeCacheMatrix function.
## Test whether the inverse is already calculated, 
## if yes, stop computation and reuse from the cache.
## if no, calculate the inverse and set of value of the inverse matrix.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
