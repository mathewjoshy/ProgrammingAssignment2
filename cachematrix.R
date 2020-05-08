## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeCacheMatrix creates a special "matrix".it does the following.
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  get <- function()x
  setinvmat <- function(inverse) inv <<- inverse
  getinvmat <- function()inv
  list(set= set, get= get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)

}


##This function gets the inverse of the matrix, first it checks if a inverse is calculated,
## else it calculates the same and sets its to the cache via the setinvmat function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvmat()
  if(!is.null(inv)){
    message("getting Cached data")
    return(inv)
  }
  mat <- x$get()
  inv <-solve(mat,...)
  x$setinvmat(inv)
  inv
}
