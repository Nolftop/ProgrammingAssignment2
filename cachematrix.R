## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can be cached



makeCacheMatrix <- function(x = matrix()) {
  invM <-  NULL
  set <-  function(y) {
    x <<- y
    invM <<- NULL
  }
  get <-  function() x
  setinv <-  function(inverse) invM <<- inverse 
  getinv <-  function() invM
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" if it didnt compute before.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM <-  x$getinv()
  if (!is.null(invM)){
    message("getting cached data")
    return(invM)
  }
  matr <-  x$get()
  invM <- solve(matr, ...) # There is a main difference from that vector function from Assigment discription :D. Am I doing this assigment right?
  x$setinv(invM)
  return(invM)
}
