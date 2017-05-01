## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## define the argument with default mode of "matrix"
MakeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## Initialize Matrix Inverse variable
         ## create the set function to assign new value of matrix in parent environment
        set <- function(y) {
                x <<- y
                inv <<- NULL ## if there is a new matrix, reset inv to NULL
        }
         ## define the get fucntion - returns value of the matrix argument
        get <- function() x
        ## assigns value of inv in parent environment
        setInverse <- function(inverse) inv <<- inverse
        ## gets the value of inv where called
        getInverse <- function() inv
        ## you need this in order to refer to the functions with the $ operator
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
