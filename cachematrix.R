
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


cacheSolve <- function(x, ...) {
  ## return's inverse of the original matrix input to makeCacheMatrix()
  inv = x$getinv()  
  if (!is.null(inv)){
    return(inv)
  }
  
  # calculates the inverse
  my_matrix.data = x$get()
  inv = solve(my_matrix.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  return(inv)
}


# run function calling makeCacheMatrix twice
run = function(my_matrix){
  
  temp_matrix = makeCacheMatrix(my_matrix)
  
  time.start = Sys.time()
  cacheSolve(temp_matrix)
  time.end = Sys.time()
  elapsed_time = time.end - time.start
  print(elapsed_time)
  
  time.start = Sys.time()
  cacheSolve(temp_matrix)
  time.end = Sys.time()
  elapsed_time = time.end - time.start
  print(elapsed_time)
}

# call run function passing it a 1000x1000 matrix of randon nummbers
run (matrix(rnorm(250000), nrow=500, ncol=500))
