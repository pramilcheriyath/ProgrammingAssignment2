# This function creates a special “matrix” object that can cache its inverse
# create a matrix 'x'
makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL
  set <- function(y) {
    x <<- y     # settting the value of matrix
    i <<- NULL  # clearing cache
  }
  get <- function() x  # getting the value of matrix 
  setinverse <- function(inverse)  # setting the value of inverse
          i <<- inverse
  getinverse <- function() i      # getting the value of inverse
  # a list creaed from all the above
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  i <- x$getinverse() # getttng the value from inverse 
  if (!is.null(i)) {  # checking the value if it's null
    message("getting cached data")
    return(i)     # returning the value of the matrix if it is not null
  }
  data <- x$get() # getting te value of the matric from the above function
  i <- solve(data, ...) # matrix inverse
  x$setinverse(i)  # setting the value of inverse       
  i
}

