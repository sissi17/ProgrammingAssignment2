## edit a function to cache the inverse of a matrix

## fct creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## computes the inverse of a special matrix if no returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        ## Return a matrix that is the inverse of 'x'
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
