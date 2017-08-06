## Matrix inversion can often be a costly computation.  The following
## saves the inverse of the matrix in the cache, so it can be retrieved
## without additional calculation time.

## makeCacheMatrix() accepts a single parameter x, which is a matrix
## 
## It then creates a list containing a function that:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() accepts at least 1 parameter x, which is an invertable matrix
##
## It then checks to see if the inverse has already been calculated.  If so, it returns
## the cached inverse; otherwise, it calculates the inverse and caches it via the setinverse
## from makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
     if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}
