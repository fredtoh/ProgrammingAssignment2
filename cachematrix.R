## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix caches a matrix. It returns as a metadata object, which the cacheSolve function will use as an argument.
##  makeCacheMatrix also caches the inverse of the matrix via cacheSolve.
##     usage: Y <- makeCacheMatrix(X)
##     argument: X, a matrix
##     output: Y, a metadata object, which is an argument to cacheSolve

##  cacheSolve takes the metadata object of makeCacheMatrix and returns the inverse of a matrix X.
##  The matrix X must first be stored via makeCacheMatrix. (See makeCacheMatrix for instructions.)
##     usage: inverseX <- cacheSolve(Y)
##     argument: Y, a metadata object created in makeCacheMatrix, which has the cached data of the matrix, X.
##     output: inverseX, the inverse matrix of X

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
##  makeCacheMatrix caches a matrix, and its inverse via cacheSolve. It has four methods within. They are:
##      set: stores the matrix
##      get: returns the matrix
##      setInverse: stores the inverse of the matrix
##      getInverse: returns the inverse of the matrix
    m <- NULL
    set <- function(y){
        x <<-y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix) m <<- inverseMatrix
    getInverse <- function() m
    list(
        set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
##  cacheSolve returns the inverse of a matrix X. The matrix X must first be stored via makeCacheMatrix.
##  If its inverse is already stored in makeCacheMatrix, it returns the cached data.
##  Otherwise, cacheSolve will calculate its inverse, stores it and returns it.

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m        
}
