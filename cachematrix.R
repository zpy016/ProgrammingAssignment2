## The objective is to write a pair of functions that cache the inverse of a matrix.

## The following function creates a special "matrix" object that can cache its inverse.
## It is a list contains 4 objects.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv_matrix) inv <<- inv_matrix
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}



## Note: You may check this code using the following lines of codes.
# Source cachematrix.R first, then run the following codes
# > mat <- matrix(c(4,1,3,1),2,2)
# > cm<-makeCacheMatrix(mat)
# > cacheSolve(cm)
# You should get
#        [,1] [,2]
# [1,]    1   -3
# [2,]   -1    4
