## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function that creates a special "matrix" object with caching

makeCacheMatrix <- function(mat = matrix()) {
inv_matrix <- NULL
  set <- function(new_matrix) {
    mat <<- new_matrix
    inv_matrix <<- NULL
}
  get <- function() mat
  
  set_inverse <- function(inverse) inv_matrix <<- inverse
  
  get_inverse <- function() inv_matrix
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function
## Function that computes the inverse of a cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv_matrix <- x$get_inverse()
  
  if (!is.null(inv_matrix)) {
    message("Getting cached inverse.")
    return(inv_matrix)
  } else {
    mat <- x$get()
    inv_matrix <- solve(mat)
    x$set_inverse(inv_matrix)
    return(inv_matrix)
  }
}
