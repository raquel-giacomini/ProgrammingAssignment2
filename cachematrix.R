## These functions get a matrix and calculate its inverse, if the inverse has already been claculated it will get it from cache

## This function return a list containing functions to set and get a matrix, and also to set and get the inverse of this matrix
## while setting a matrix, this function checks if the object is really a matrix, other wise sets NULL and diplay a message

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      if (is.matrix(y))
      {
        x <<- y
      }
      else
      {
        x <<- NULL
        message("The variable must be a matrix.")
      }
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of a matrix, if it has already been calculated, it will get it from the cache
## This function also checks if it´s possible to calculate the inverse matrix according to the following rules:
## 1) to calculate the inverse matrix, the matrix must be a square matrix
## 2) must not be a singular matrix, in other words, the determinant of this matrix must be different of zero

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    if (nrow(data)!=ncol(data))
    {
      message("Not possible to compute the inverse of the matrix. It has to be a square matrix.")
      return(invisible(m))
      
    }
    try(m <- solve(data, ...), TRUE)
    if(!is.null(m))
    {
      x$setinverse(m)
      m
    }
    else
    {
      message("Not possible to compute the inverse of the matrix. It's a singular matrix. Determinant must not be 0.")
      return(invisible(m))
    }
}
