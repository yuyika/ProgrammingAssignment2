## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL    ## Initializes m
  set <- function(y) {   ## Set the value of the matrix
    x <<- y    ## Assigns value of y to x
    m <<- NULL  ## Assigns value of NULL to m
  }
  get <- function() x ## get the value of the matrix
  set_inv <- function(inverse) m <<- inverse
  get_inv <- function() m
  ## Return a list
  list(set = set, get = get, 
       set_inv = set_inv,
       get_inv = get_inv)
  
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv()   ## Recover the value stored in x$get_inv
  if(!is.null(m)) {
    message("getting cached data") ## If the cache was not empty, return m
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) ## Calculate inverse
  x$set_inv(m)   ## Return the inverse matrix
  m
}
