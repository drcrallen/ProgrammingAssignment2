## cachematrix.R - Wrap a matrix with a caching structure and allow
##                 matrix inverse calculations on that structure
## 
## General use case
##
##  wrappedMatrix <- makeCacheMatrix(matrixOfInterest)
##  cacheSolve(wrappedMatrix)
##
##  First wrap the matrix of interst in makeCacheMatrix
##  Then call the `cacheSolve' instead of `solve'
##

## makeCacheMatrix(matrixOfInterst)
##  matrixOfInterst = The matrix that is desired to be inverted and cached

makeCacheMatrix <- function(x = matrix()) {
    ## x is the matrix of interest
    ## m is the cached mean value
    m <- NULL
    
    ## getter / setter for x
    set <- function(y){
        x <<- y
        m <<- NULL ## clear cache
    }
    get <- function() x
    
    ## getter / setter for mean
    setmean <- function(mean) m<<-mean
    getmean <- function() m
    
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## cacheSolve(wrappedMatrix, ...)
##  Check the wrappped matrix for a cached inverse and return it
##  If no cached inverse is found, run the `solve' command
##  Extra arguments are transparently passed to `solve'
##
## ****WARNING: the cached value uses only the FIRST set of arguments passed.
## Any further calls to cacheSolve always assume the same arguments are passed.

cacheSolve <- function(x, ...) {
    
    m <- x$getmean()
    
    ## Check for cache
    if(!is.null(m)){
        message("Found cached value for mean")
        return(m)
    }
    
    data <- x$get()
    
    ## Do the actual inverse
    m <- solve(data, ...)
    
    ## cache for later
    x$setmean(m)
    m
}
