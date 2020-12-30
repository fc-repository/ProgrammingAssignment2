## Function makeCacheMatrix creates:
## list of functions used to cache inverse matrix 
## if function cacheSolve is called

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        # return a list of functions
        list(get = get,
             setinv = setinv,
             getinv = getinv)    
}


## Function cacheSolve check if a cached inverse of the matrix exists and 
## returns it
## OR 
## compute the inverse and store it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

