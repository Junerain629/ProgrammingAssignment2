## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix", whose inverse and itself can be cached
## It is really a list containing a function to 
## 1. set the matrix into cache
## 2. get the matrix from cache
## 3. set the inverse of the matrix into cache
## 4. get the inverse of the matrix from cache

makeCacheMatrix <- function(x = matrix()) {
    ## define the variable that stores the inverse of the input matrix "x"
    invmtrx <- NULL
    
    ## set the matrix into cache
    set <- function(y) {
        ## cache the input matrix
        x <<- y
        ## initiate the cache of the inverse of the input to be NULL
        invmtrx <<- NULL
    }
    
    ## get the matrix from cache
    get <- function() x
    
    ## set the inverse of the matrix into cache
    setinv <- function(matrixinverse) invmtrx <<- matrixinverse
    
    ## get the inverse of the matrix from cache
    getinv <- function() invmtrx
    
    ## finally, return the list of all the four cache-access functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse has already existed, the inverse of matrix will be obtained from cache and skip the computation.
## Othersise, it computes the inverse of the matrix and set the result into cache.

cacheSolve <- function(x, ...) {
    ## attempt to obtain the inverse of matrix "x" from cache
    inv_x <- x$getinv()
    
    ## check the availability of the cache
    if(!is.null(inv_x)) {
        ## print the message that we found the cached result
        message("getting the inverse of matrix from cache")
        ## return the value, exist the main function
        return(inv_x)
    }
    
    ## otherwise, the cache does not yet exist
    ## get the original matrix from cache
    data <- x$get()
    ## compute the inverse of the matrix
    inv_x <- solve(data)
    ## set the inverse of the matrix into cache
    x$setinv(inv_x)
    
    ## output the result of the inverse at the end
    inv_x
}
