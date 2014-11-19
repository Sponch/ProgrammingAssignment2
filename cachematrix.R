## cachematrix.R
##
## This file contains functions to cache inversed matrix.
## makeCacheMAtrix create a special kind of matrix with its value inversed too
## cacheSolve return the inversed matrix created using the function makeCacheMatrix.  


## This function create a special "matrix" that cache it, 
## and its inversed matrix, in another environment
makeCacheMatrix <- function(savedMatrix = matrix()) {
        inversedMatrix <- NULL
        set <- function(matrix) {
                savedMatrix <<- matrix
                inversedMatrix <<- NULL
        }
        get <- function() savedMatrix
        setInversedMatrix <- function(invMatrix) inversedMatrix <<- invMatrix
        getInversedMatrix <- function() inversedMatrix
        list(set = set, get = get,
             setInversedMatrix = setInversedMatrix,
             getInversedMatrix = getInversedMatrix)
}


## This function calculate the inverse of one matrix and put it in a cache. 
## If the matrix is cached, the function return this value, otherwise the inverse 
## of the matrix is calculated and returned;
##
## x: matrix calculated using the function makeCacheMatrix
##
cacheSolve <- function(x, ...) {
        inversedMatrix <- x$getInversedMatrix()
        if(!is.null(inversedMatrix)) {
                message("getting cached data")
                return(inversedMatrix)
        }else{
                message("inversing the matrix")
                matrix <- x$get()
                inversedMatrix <- solve(matrix, ...)
                x$setInversedMatrix(inversedMatrix)        
        }
        inversedMatrix
}

##It's a test to check if the matrix return from cached when calling the method
## cacheSolve at second time
##
## How to run:
##      library("Matrix")
##      source("cachematrix.R")
##      testCacheSolve()
testCacheSolve <- function(){
        
        m <- matrix(c(7,4,7,4,5,6,7,8,9), nrow=3, ncol=3)
        print(m)
        mcm <- makeCacheMatrix(m)
        cs <- cacheSolve(mcm)
        print(cs)
        cs <- cacheSolve(mcm)
        print(cs)
}