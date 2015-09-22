# The function creates a special "Matrix" with list of next function
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function calculate inverse matrix, but fist of all it check,
# maybe inverse matris have been calculated. If True, return alredy
# caltulated matrix, othewise calculate inverse matrix and stored it
# to Global Env.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invM <- x$getinverse()
    if(!is.null(invM)) {
        message("Getting cached data")
        return(invM)
    }
    data <- x$get()
    invM <- solve(data)
    x$setinverse(invM)
    invM
}
