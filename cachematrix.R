## x: invertible square matrix
## usage: cacheSolve(makeCacheMatrix(x))

##makeCacheMatrix: Returns list of below functions:
## set: to set the value of matrix
## get: to return the matrix
## setinv: to set the inverse of the input matrix. This is for caching.
## getinv: to return the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: gets the cached inverse using getinv() function.
## If cached inverse exists then uses it otherwise computes the inverse,
##  and caches it using the function setinv()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of input matrix
        ## here variable x refers to the list of function returned by makeCacheMatrix()
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)
        inv	
}
