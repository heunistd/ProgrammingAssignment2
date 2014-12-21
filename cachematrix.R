## Functions that cache the inverse of a matrix

## Creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInvCacheM <- function(solve) m <<- solve
        getInvCacheM <- function() m
        list(set = set, get = get,
             setInvCacheM = setInvCacheM,
             getInvCacheM = getInvCacheM)
}


## Computes inverse of "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getInvCacheM()
        if(!is.null(m)) {
                message("getting inverse cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvCacheM(m)
        m
}
