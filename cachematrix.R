## The functions below help make calculating the inverse of a matrix
## become more efficient by storing the inverse of all matrices
## previously encountered.


## The function below takes a matrix as an input, 
## sets the value of the matrix and the inverse,
## and gets the value of the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x 
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function below takes the output from the previous function as its input. 
## It returns the inverse of the matrix by calculating it or 
## if the matrix has been previously encountered, by searching the stored inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# Demonstrate

dummy <- makeCacheMatrix(matrix(4:1, nrow = 2, ncol = 2, byrow = T))
dummy$get()
dummy$getinverse() # NULL for now
cacheSolve(dummy) # calculates the inverse
dummy$getinverse() # stores the inverse
cacheSolve(dummy) # prints "getting cached data"

