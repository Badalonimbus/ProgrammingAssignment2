###################################
# Caching the inverse of a matrix #
###################################


#
# Date: July 2014
#

# This script contains two functions that cache the inverse of a matrix. Keep
# in mind that matrix inversion is usually a costly computation and their may be
# some benefit to caching the inverse of a matrix rather than compute it 
# repeatedly.


# The first function, called as makeCacheMatrix, creates a special "matrix"
# object that can cache its inverse.  

makeCacheMatrix <- function(x = matrix()) {
        
        # First of all, we have to initilise the property of the inverse
        inver <- NULL
        
        # Now, we define a method to set the matrix
        set <- function(matrix) {
                x <<- matrix
                inver <<- NULL
        }
        
        # We define other method in order to get the matrix above.
        get <- function() {
                x
        }
        
        # Now, we have to set the matrix inverse
        setInverse <- function(inverse) {
                inver <<- inverse
        }
        
        # Similarly to previous steps, we write a method to get the matrix inverse.
        getInverse <- function() {
                inver
        }
        
        # Finally, we have to return a list of the methods used in this function.
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# This second function, called cacheSolve, computes the inverse of the special matrix
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cacheSolve should retrieve the inverse from
# the cache. 

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        
        # As we described above, if the inverse has already been calculated
        # we have to return it.
        if(!is.null(inver)) {
                message("getting cached data...")
                return(inver)
        }
    
        # Now, we have to get the matrix from our object.
        data <- x$get()
        
        # We compute the inverse by using multiplication of matrix
        inver <- solve(data) %*% data
        
        # We set the inverse to the object
        x$setInverse(inver)
        
        # Finally, we return the matrix
        inver
}
