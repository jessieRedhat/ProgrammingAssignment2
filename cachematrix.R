## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache
#    its inverse.

makeCacheMatrix <- function(x = matrix()) { 
    invx <- NULL
    set <- function(y) { 
        # this is to change the value of the defined object
        x <<- y
        invx <<- NULL
}
get <- function() x # get matrix X
setInvrs <- function(invrs) invx <<- invrs # caching  occurs here
getInvrs <- function() invx  # reading cache occurs here
list(set = set, get = get,setInvrs = setInvrs ,getInvrs = getInvrs)
}
 
##  this function tries to 1) get inverse from Cache, 2) if not successful,calculates it 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invsx <- x$getInvrs()
    if(!is.null(invsx)) {
        message("getting cached data")
        return(invsx)
    }
    data <- x$get()
    invsx <- solve(data,...)
    x$setInvrs(invsx)
    invsx
} 