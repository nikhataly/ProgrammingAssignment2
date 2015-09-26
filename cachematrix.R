## The following functions allow a user to use retrieve the inverse of a matrix 
## for use when needed.  The value of the matrix and its inverse are stored in cache
## so that the user does not have to repeatedly calculate the value of the inverse,
## this can help to prevent recomputation in case of potentially time consuming operations

## The following function returns a list of four functions 
## that get and set the values of the original matrix and its inverse. 
## It stores value for the matrix.
## If the inverse has already been calculated, it will store the value 
## of the inverse as well, and if not it, its value will be NULL.

makeCacheVector <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## The following function will return the value of the inverse. If 
## it has already been calculated and stored in the above function,
## it will return that value through getinverse variable in the list, then it will call 
## the getinverse function in the list and give a message to indicate that("getting cached data"). 
## If the inverse value is null or has not been calculated, it will calculate it 
## and store it in the variable in the above function (using the setinverse 
## function) and then return that calculated value.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if (!is.null(i)) {
        message("getting cached data")
        return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
