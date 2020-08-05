##  The solution to the R programming week 3 assignment.

## makeChacheMatrix creates an object X and stores the matrix data and its 
## inverse in the objext x. The matrix value in object x can be reset using the 
## set function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(invVal) inv <<- invVal
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The chacheSolve function will be able to call the matrix stored in 
## object x and compute the inverse matrix which will then be stored in turn in
## object x.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    # calls the function solve to calculate the inverse matrix
    invVal <- solve(data, ...) 
    x$setinv(invVal)
    invVal
}
