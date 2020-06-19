## The first function will create a matrix, find the inverse of that
## matrix and then place the inverse in the cache. Then the second function
## will then check to see if the inverse of the matrix has been cached and
## if it has will return that information, if not it will compute the inverse
## of the input matrix.

## This first function will create a matrix, and create a space in the cache
## for the data to be stored.

makeCacheMatrix <- function(x = matrix(2,2)) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## The second function will calculate the inverse of the matrix if it finds
## that is has not already been calculated and stored in the cache. If it finds
## that the inverse has been stored then it will retrieve the data.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setInverse(i)
  i
  }
