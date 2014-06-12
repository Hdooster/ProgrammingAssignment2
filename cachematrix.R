makeCacheMatrix <- function(x = matrix()) {
  
  ##Everytime a new matrix is 'made' delete the inverse
  matrix_inverse <- NULL
  
  ##output the matrix
  getMatrix <- function() x
  
  ##set a new matrix and delete the old one's inverse
  setMatrix <- function(new_matrix) {
    matrix_org <<- new_matrix
    matrix_inverse <<- NULL
  }
  
  ##output the matrix's inverse
  getInverse <- function() matrix_inverse
  
  ##set the matrix's inverse outside of the scope of the following function (<<-)
  setInverse <- function(new_inverse) matrix_inverse <<- new_inverse
  
  ##this is the list
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ouputter_matrix_inverse <- x$getInverse()
  if(!is.null(outputter_matrix_inverse)) {
    message("grabbing matrix's inverse ")
    ##return the known inverse
    return(outputter_matrix_inverse)
  }
  ##if it needs to be calculated, go grab the matrix
  data <- x$getMatrix()
  message("solving for matrix's inverse ")
  
  #... and solve it's inverse
  outputter_matrix_inverse <- solve(data, ...)
  x$setInverse(outputter_matrix_inverse)
  outputter_matrix_inverse
}