## ----------------------------------------------
## File name: cachematrix.R
## Author: David Lyons
## Description: R Programming - Assignment 2
## Date created: 2021-04-10
## Date last modified: 2021-04-10
## ----------------------------------------------

## Holds two cached variables
## - x is a square invertible matrix 
## - inv_mat is the inverse of x
## Provides four internal functions to allow access
## to cached variables or create new variables.
makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- matrix()
    set <- function(y = matrix()) {
        x <<- y
        inv_mat <<- matrix()
    }
    get <- function() x
    setinv <- function(new_inv) inv_mat <<- new_inv
    getinv <- function() inv_mat
    list (set = set,
          get = get,
          setinv = setinv,
          getinv = getinv)
}
## Returns the inverse of the matrix held in
## an instance of makeCacheMatrix. 
## This will either be cached or recalculated 
## for a new matrix 
cacheSolve <- function(x, ...) {
    inv_mat <- x$getinv()
    if (!is.na(inv_mat[1, 1])){
        message("returning cached inverse matrix")
        return(inv_mat)
    }
    message("generating inverse matrix")
    new_mat <- x$get()
    new_inv <- solve(new_mat, ...)
    x$setinv(new_inv)
    new_inv
}

