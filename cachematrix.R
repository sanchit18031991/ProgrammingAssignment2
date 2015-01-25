## Functions to cache and compute the inverse of a matrix


## Function to create a matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## Function to compute inverse of the matrix returned by above function and check cache if the inverse is same as the original matrix to read from the cache

cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}