## cachematrix.R These functions cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and it can be benefiial 
## to cache the inverse of a matrix rather than compute it every time.

## makeCacheMatrix receives a matrix and creates a special matrix that can
## catch it's inverse. The special matrix is really a list containing functions to:
## 1.- set the value of the matrix
## 2.- get the value of the matrix
## 3.- set the value of the inverse
## 4.- get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        ## x is a matrix that is invertible
        
        ## Returns a list of functions that allow to get/set the matrix and get/set the 
        ## inverse of the matrix. 
        
        i <- NULL
        ## set = updates the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## get = returns the matrix
        get <- function() x
        
        ## setinv = set the inverse
        setinv <- function(inv) i <<- inv
        
        ## getinv = returns the inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## x is a special matrix (list of functions)
        
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
