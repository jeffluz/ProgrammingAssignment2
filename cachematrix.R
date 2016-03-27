## The function cacheSolve receives a matrix and returns the inverse matrix
## The function makeCacheMatrix calculates the inverse matrix or recovers the cached calculation

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  # Storing the received matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Returning the stored matrix
  get <- function() x
  
  # Storing the received inverse matrix
  setmtx <- function(minv) m <<- minv
  
  # Returning the stored inverse matrix
  getmtx <- function() m
  
  list(set = set, get = get, setmtx = setmtx, getmtx = getmtx)
  
}

cacheSolve <- function(x, ...) {
  
  # Recovering the possible stored inverse matrix
  m <- x$getmtx()
  
  if(!is.null(m)) {
    # Returning stored inverse matrix
    return(m)
  }
  
  # Calculating inverse matrix because there is no previous one
  data <- x$get()
  m <- solve(data, ...)
  x$setmtx(m)
  m
  
}
