##
## cachematrix.R - Code to cache/restore an inverse of a matrix.
##


## makeCacheMatrix - create and return a matrix object that can cache
## it's inverse. Internally, this object is a list of 'member'
## functions that can set and retreive the value of the matrix, and
## it's inverse.
makeCacheMatrix <- function(x=matrix()) {

    ## The 'cached-inverse' member.
    cached.inv <- NULL


    ## Set the value of the matrix wrapped by this object.
    set.matrix <- function(new.matrix) {
        x <<- new.matrix
        cached.inv <<- NULL
    }


    ## Get matrix wrapped by this object.
    get.matrix <- function() {
        x
    }


    ## Get the inverse of the matrix wrapped by this object, computing
    ## it if necessary. As per the instructions in the assignment, we
    ## assume the matrix is always invertible. An error will be trown
    ## if it is singular.
    get.inverse <- function() {
        if (is.null(cached.inv)){
            ## No cached value. Compute the inverse.
            cached.inv <<- solve(x)
        }
        cached.inv
    }


    list(set.matrix=set.matrix,
         get.matrix=get.matrix,
         get.inverse=get.inverse)
}



##
## Return the inverse of a matrix object created using
## 'makeCacheMatrix', using the cache if possible to avoid unneeded
## computations
##
cacheSolve <- function(x, ...) {
    ## Functionality is already implemented by the get.inverse method of
    ## the matrix object, so just forward the request to it.
    x$get.inverse()
}
