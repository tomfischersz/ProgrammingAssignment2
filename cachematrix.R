## JHU Coursera R Programming Assignment #2
## January 22, 2018

## author:  Thomas Fischer
## email:   tomfischer@qq.com   
## -----------------------------------------------------------------------------

## Create a special matrix object (list) with functionality to
## set and get a matrix, and furthermore compute the matrix's inverse
## as a cached object

## -----------------------------------------------------------------------------
## Example of Usage:

## Create myMatrix and initialize with a 2x2 square matrix
# myMatrix <- makeCacheMatrix(matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2))

## returns the matrix
# myMatrix$get()

## to set the cached inverse in the myMatrix environment
# cacheSolve(myMatrix)

## to get the cached inverse
# myMatrix$getInverse()

## -----------------------------------------------------------------------------

## Function to return a special matrix object with functions
## to set and get a matrix, set it's inverse and especially
## get it's cached inverse

makeCacheMatrix <- function(x = matrix()) {
    ## I will be the cached Inverse of our matrix
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) I <<- Inverse
    getInverse <- function() I
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## Function to actually populate and retrive the cached inverse of
## the makeCacheMatrix() object (which is the input for this function)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    I <- x$getInverse()
    if(!is.null(I)) {
        message("getting cached inverse matrix")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setInverse(I)
    I
}
