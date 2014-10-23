## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a list of 4 functions, using a matrix variable x to cache the data and the variable inverse to cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 			inverse <- NULL
            set <- function(newSquareMatrix) {
                    x <<- newSquareMatrix
                    inverse <<- NULL
            }
            get <- function() x
            setInverse <- function(inv) inverse <<- inv
            getInverse <- function() inverse
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)

}


## Write a short comment describing this function

# Tries to retrieve the cached inverse matrix 
# If there is no cached inverse matrix yet, computes it with the solve() function, from the cached matrix
# and finally cached the result, for future calls.

# Accepts all possible parameters of solve(), after the first parameter

# Usage:
# First create the cache functions: v <- makeCacheMatrix()
# Then set the input matrix: v$set(aSquareMatrix)
# And compute the inverse: cacheSolve(v)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cachedInverse <- x$getInverse()
        ret <- matrix()
        if (!is.null(cachedInverse)) {
        	message("Got inverse matrix from cache!")
        	ret <- cachedInverse
        }
        else {
        	cachedMatrix <- x$get()
        	computedInverse <- solve(cachedMatrix, ...)
        	x$setInverse(computedInverse)
        	ret <- computedInverse
        }
        
        ret
}
