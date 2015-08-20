## Set of functions will create a cache of matrix inverse and
## run related calculations, if necessary.

## Create list to contain default matrix value, input matrix value
## (which may or may not be equivalent to default), default inverse
## value, and true stored inverse value of input. This list constitutes
## cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Calculate the inverse of the input matrix x, if the inv value in the cache
## is NULL. setinv begins as an undefined value, but this function will fill
## that value.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
