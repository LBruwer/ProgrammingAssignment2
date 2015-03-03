# Solving the inverse of a matrix can be computationally expensive,
# specially for large matrixes. To avoid repeating the computation of
# the inverse of a matrix, the following functions are provided for 
# caching and recovering the inverse of a matrix, computing it just
# the first time it is used:


# A matrix and its inverse are cached in an object of type list with
# the following elements:
# - set: function to set the value of the matrix into the list
# - get: function to get the value of the matrix from the list
# - getinverse: function to get the inverse of the matrix (previously set)
# - setinverse: function to set the inverse of the matrix
# The function makeCacheMatrix encapsulaptes these elements and 
# returns the corresponding list.
# Arguments:
# - x: matrix that will be referenced by the list

makeCacheMatrix <- function(x = matrix()) {
    # m: the inverse of the matrix, initially NULL until it is solved
    m <- NULL

    # Local functions to set and get the matrix and its inverse if it
    # is cached or NULL otherwise
    set <- function(y) {
        # x: the matrix itself
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    
    # Building and returning the list so it can be used/updated later
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The function cacheSolve returns the inverse of a matrix. This matrix
# must be represented as the list representation created with the
# function makeCacheMatrix.
# Arguments: 
# - x: list representation of a matrix

cacheSolve <- function(x, ...) {
    # If the list representation already has the inverse of the matrix, 
    # return it
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Otherwise, compute the inverse and cache the result in the list
    # representation (setinverse) of the matrix for future references
    message("solving... not cached")
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


## to run
#  y <- rbind(c(1,-1),c(2,-1))
#  y1 <- makeCacheMatrix(y)
#  cacheSolve(y1)
#  returns : solving... not cached
#             [,1] [,2]
#        [1,]   -1    1
#        [2,]   -2    1

#  y1 <- makeCacheMatrix(y)
#  cacheSolve(y1)
#  returns : getting cached data
#             [,1] [,2]
#        [1,]   -1    1
#        [2,]   -2    1
