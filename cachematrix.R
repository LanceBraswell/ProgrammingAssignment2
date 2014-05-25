## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix
## rather than computing it repeatedly. These functions wrap a
## supplied class matrix object for the purpose of operating
## on the matrix (e.g. finding the inverse) and caching the
## results.
##
## Note: The wrapped matrix is assumed to be always invertible.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse. Its argument should be an 
## assumed always invertible matrix class object. It returns
## a special list object that can be operated on with the cacheSolve
## function.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y = matrix()) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache. If the inverse
## of the matrix has not been calculated, it will find the inverse
## and cache the result. It returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'mtx'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

## Tests
# matequal <- function(x, y)
#     is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
# sm <- matrix(sample(1:25),nrow=5)
# ans.known <- solve(sm)
# cm <- makeCacheMatrix(sm)
# matequal(ans.known, cacheSolve(cm))
# cacheSolve(cm)
# cacheSolve(cm)
# cm$set(sm)
# matequal(ans.known, cacheSolve(cm))
# cacheSolve(cm)
# cacheSolve(cm)
