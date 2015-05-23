## The following functions work together to compute the inverse of a user-defined matrix and then 
## cache that inverse so if called again it will not need to be re-computed.  Detailed analysis of the
## functions found below.

makeCacheMatrix <- function(x = numeric()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse){
                i <<- inverse
        } 
        getinverse <- function() {
                i
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}



## functions repeated below with explanitory comments:

makeCacheMatrix <- function(x = numeric()) {   ## x is input matrix defined using matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)
        i <- NULL   ## i is NULL object that will later be overwritten as inverse of input matrix
        set <- function(y) {   ## sub-function that allows the input matrix to be reset after function has been run using initial input (if desired)
                x <<- y
                i <<- NULL   ## this re-defines i <- NULL in the makeCacheMatrix() environment in the case that the input is changed using set()
        }   
        get <- function() {   ## sub-function that returns the input matrix (either defined as argument of makeCacheMatrix() or later as argument of set())
                x
        }   
        setinverse <- function(inverse){   ## sub-function that sets the cached data (parameter i) in the makeCacheMatrix() environment (understood to be the inverse of the input)
                i <<- inverse              ## note: running setinverse sub-function will NOT compute the inverse, it will simply set whatever argument is passed as the inverse
        }  
        getinverse <- function() {   ## sub-function that returns the cached data (understood to be the inverse of the input)
                i
        }  
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   ## creates a vector list of sub-functions that can later be accessed individually by sub-setting (with arguments)
}


cacheSolve <- function(x, ...) {   ## x is a parameter user-defined in the global environment as follows: x <- makeCacheMatrix("input matrix"). 
        i <- x$getinverse()   ## subsetting x by the sub-functions defined in makeCacheMatrix() allows those functions to be accessed individually so that i is defined as the cached inverse
        if(!is.null(i)) {   ## if i has already be defined and is therefore non-NULL, it will be returned from cache with a message
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()   
        i <- solve(data, ...)   ## if i is found to be NULL it will be defined as solve(matrix returned by x$get()), cached and returned
        x$setinverse(i)
        i
}