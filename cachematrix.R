
## This function takes a square, non-singular matrix (can check for singularity
## optionally) and internally caches a slot for its inverse using cached set
## and get functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(compinv) inv <<- compinv
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function takes a square, non-singular cached matrix and returns its inverse
## It uses a cached matrix type as input and either returns an already computed and cached
## inverse or computes (for the first time) and returns its inverse

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
##
## attempt to retrieve cached inverse
##
  inv <- x$getinv()
  if (!is.null(inv)) {
##
## inverse already computed and cached; will retrieve
##
    message("getting cached inverse matrix")
    return(inv)
  }
##
## inverse not computed and cached yet; compute inverse
##
  data <- x$get()
##
## assumes square-invertible matrix; can use test function <isInv> beforehand
##
##  if isInv(data) {
##     inv <- solve(data, ...)
##     x$setinv(inv)
##  }
##  else {
##     message ("matrix is singular or nearly [computationally] singular")
##  }
##
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
##
## This function returns a logical to determine is an input square matrix is non-singular
##
isInv <- function(m) class(try(solve(m),silent=T))=="matrix"
