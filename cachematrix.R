## The following functions create a special matrix object that can 
## cache its inverse to optimize the computation time when 
## calculating the inverse of a matrix multiple times.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. It provides methods to 
## set and get the matrix and its inverse, allowing efficient retrieval of the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    # This function sets the value of the matrix and resets the cached inverse
    set <- function(y) {
        x <<- y  # Assign new matrix to x
        inv <<- NULL  # Reset inverse cache when matrix is changed
    }
    # This function retrieves the current matrix
    get <- function() x
    # This function sets the value of the inverse
    setinverse <- function(inverse) inv <<- inverse
    # This function retrieves the cached inverse
    getinverse <- function() inv
    # Return a list of the functions to access matrix and its inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. It checks if the inverse 
## has already been calculated; if so, it retrieves the cached inverse to avoid unnecessary computation.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # Retrieve cached inverse if it exists
    if(!is.null(inv)) {
        message("getting cached data")  # Inform user that cached data is being used
        return(inv)  # Return the cached inverse
    }
    data <- x$get()  # Get the matrix data
    inv <- solve(data, ...)  # Calculate the inverse
    x$setinverse(inv)  # Cache the computed inverse
    inv  # Return the computed inverse
}
