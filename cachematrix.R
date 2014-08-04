## These functions (makeCacheMatrix and cacheSolve) inverse a matrix then cache the result allowing the cached result to be
## to be retrieved later if it is already calculated and if the input is unchanged

## The 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix())
{
       m <- NULL
       set <- function(y)
       {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setInverse <- function(Inverse) m <<- Inverse
       getInverse <- function() m
       list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}

## The 'cacheSolve' function computes the inverse of the provided matrix (returned by the 'makeCacheMatrix' function)
## If the inverse result is cached it will retrieve the inverse restult from the cache


cacheSolve <- function(x, ...)
{
       m <- x$getInverse()
       if(!is.null(m))
       {
              message("fetching cached result")
              return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setInverse(m)
       m
}
