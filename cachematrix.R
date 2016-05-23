## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Input : X matrix to be inverted  
  ## pre condition : X must be a square matrix
  ## Output: List passed input to the function:cacheSolve()
  
  
  ## inv:list to store the values 
  inv = NULL
  
  ## set function definition
  set <-function(y)
  {
    # <<- to assigning a value y to X
    x <<- y
    inv <<- NULL
  }
  ## get function definition
  get <- function() x
  
  ## setinv  function definition
  setinv <-function(inverse) 
  {
    inv <<- inverse 
  }
  ## getinv function dmat
  
  
  getinv <-function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## input: x - output from the function makeCacheMatrix
  ## output: inverse of the original matrix input 
  
  inv = x$getinv()
  
  # check whether the inverse is already calculated are not
  if (!is.null(inv)){
    # if calculated gets it from the cache  
    message("already calculated, retrived from cached data")
    return(inv)
  }
  
  # If not calculated compute the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # set the inverse to the cache.
  x$setinv(inv)
  
  return(inv)
  
}
