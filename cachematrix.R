## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function takes a matrix and creates a series of setters and getters
## for creating an inverse matrix.  This function doesn't actually create the inverse.
## The inverse gets created using the solve() function in the cacheSolve function.
## A list is returned providing access to the setter and getter.
makeCacheMatrix <- function(x = matrix()) {
    # Set the Matrix
    inverseMatrix <- NULL
    setMatrix <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    # Get the Matrix
    getMatrix <- function() { x }
    
    # Set the Inverse
    setInverse <- function(inverse) {
        inverseMatrix <<- inverse
    }
    
    # Get the Inverse
    getInverse <- function() { inverseMatrix }
    
    # Return a list of the stters and getters for access in cacheSolve()
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function


## This function uses the setter and getters from the makeCacheMatrix
## to create an inverse of the Matrix.  It checks if the inverse of the original 
## matrix has already been created.  If it has, it will return the inverse matrix.
## If it hasn't, it'll create the inverse, set it to the setInverse() function from 
## the makeCahceMatrix(), and then return the inverse of the original function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInverse()

    # Check if the inverse matrix has been created
    if(!is.null(invMatrix)) {
        return(invMatrix) # if it's already been created, return it
    }
    
    # If inverse hasn't been created, create it
    originalMatrix <- x$getMatrix()
    invMatrix <- solve(originalMatrix, ...)
    x$setInverse(invMatrix)
    return(invMatrix)
}

