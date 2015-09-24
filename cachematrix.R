## Take a matrix and calculate the inverse.
## Then store the result.

## The makeCacheMatrix is list containing four functions:
##  1. set the value of the matrix (set)
##  2. get the value of the matrix (get)
##  3. set the value of the inverse (setinv)
##  4. get the value of the inverse (getinv)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Using the output of the above function, cacheSolve
## checks to see if the inverse has already been calculated.
## If so, it returns the inverse from the cache.
## Otherwise, it calculates the inverse and uses the setinv
## function to store the inverse in the cache.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinv(m)
        return(m)
}
