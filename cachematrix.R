## Together these functions store a matrix in the cache and then return its inverse
## using the solve() function. Use a matrix supplied by another function or create one.

## An example of the full run:
## > YourMatrix <- rbind(c(1,2),c(3,0))
## > cachedMatrix <- makeCacheMatrix(YourMatrix)
## > cacheSolve(cachedMatrix)
##      [,1]       [,2]
##  [1,]  0.0  0.3333333
##  [2,]  0.5 -0.1666667


## makeCacheMatrix caches a matrix through creating a list that sets and gets the value
## of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){ 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## cacheSolve takes the returned value from makeCacheMatrix and solves for the inverse

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix)
  x$setmatrix(m)
  m
}
