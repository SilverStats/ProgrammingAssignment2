## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix sets up a list for caching inverses of matrices
## cacheSolve calculates the inverse when it needs to and returns the inverse
## if it's already been calculated

## Write a short comment describing this function

## makeCacheMatrix sets up a list with functions to set and get the inverse
## of a matrix, depending on whether or not you've already calculated it
##
## You can also set and get the matrix itself

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL ## need to recalculate the inverse for the new matrix
    }
    get <- function() x
    getinverse  <- function() inverse
    setinverse <- function(solve) inverse <<- solve
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## Returns the inverse of the matrix (which is assumed to be invertible)
## Only calculates it once for speed purposes, then stores that value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setinverse(inverse)
    inverse
    
}
