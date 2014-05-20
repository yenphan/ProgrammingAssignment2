## There are two functions that are used to create a special object that stores a numeric vector and cache's its mean
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
   ## 1. set the value of the matrix
   ## 2. get the value of the matrix
   ## 3. set the value of the inverse
   ## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # inverse will store the cached inverse matrix
    inverse <- NULL
    # Setter for the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    # If the inverse is already calculated, return it
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    inverse <- solve(data, ...)
    # Cache the inverse
    x$setinverse(inverse)
    # Return a matrix that is the inverse of 'x'
    inverse
}
