## These functions cache the inverse of a matrix

## The first function, makeVector creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been caculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Gets the inverse from the cache
        m <- x$getinverse()
        ## Checks to see if the inverse has already been calculated
        ## If so, it skips the computation
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Gets the matrix
        data <- x$get()
        ## Computing the inverse of a square matrix
        m <- solve(data, ...)%*%data
        ## sets the matrix of the inverse of 'x' in the cache via the setinverse function
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
