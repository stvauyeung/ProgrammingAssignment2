## The below functions makeCacheMatrix() and cacheSolve() are used
## together to calculate and cache the inverse of a square matrix input.
## By assigning the output of makeCacheMatrix() to a variable,
## and then passing it to cacheSolve(), the inverse matrix will
## either by calculated by cacheSolve(), or retrieved
## from a cached value if one has been set.

## Example:
## > cacheMatrix = makeCacheMatrix(squareMatrix)
## > cacheSolve(cacheMatrix)
## returns inverse matrix of squareMatrix and caches value

## makeCacheMatrix(), provides a list of setter
## and getter functions that access cached values of the given input,
## and if available, access cached values of the inverse matrix of
## the given input.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverse) inverseMatrix <<- inverse
  getInverseMatrix <- function() inverseMatrix
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
}


## cacheSolve() accesses the list of functions provided
## by makeCacheMatrix().  If a cached value of the inverse matrix
## is available, it will return this value without re-calculating.
## Otherwise, the function calculates the inverse matrix of given
## input, and caches the value for later retrieval.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  matrix <- x$get()
  inverseMatrix <- solve(matrix)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}
