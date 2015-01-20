## This file includes two functions that are used together
## to define a matrix type that caches its inverse value
## to reduce calculation time

## makeCacheMatrix initiates a new instance of a matrix
## and defines helper functions for get, set, getinv and setinv

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the value for the matrix inverse
    inv <- NULL
    
    # set allows the contents of the matrix to be
    # modified so the inverse is reset to null
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get returns the matrix
    get <- function(){
        x
    }
    
    # setinv sets the value of the matrix inverse
    setinv <- function(inverse){
        inv <<- inverse
    } 
    
    # getinv returns the value of the matrix inverse
    getinv <- function(){
        inv
    }
    
    #return list of functions for the matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve first looks at the matrix to see if
## the inverse has already been calculated
## If the inverse has already been calculated then
## that value is returned, otherwise the inverse is
## calculated and set within the matrix using setinv

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # get the existing value from the matrix itself
    inv <- x$getinv()
    
    # if the matrix has a cached version simply return that
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # The matrix does not have an inverse value cached
    # so one has to be calculated and cached
    
    # get the matrix
    data <- x$get()
    
    # call solve to calculate the matrix inverse
    inv <- solve(data, ...)
    
    # set the inverse value of the matrix and return it
    x$setinv(inv)
    inv
}
