## Creates a special matrix with attributes
## x --> matrix
## inv --> inverse of the matrix (initially set to NULL)
## and functions
## set() --> to set x
## get() --> to get x
## setinv() --> to set inverse
## getinv() --> to get inverse

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


## Special type of function to calculate invers for cacheMatrix
## if the cacheMatrix already has the inverse calculate then it returns the inverse
## else the function calculates the inverse and then return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data....")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
