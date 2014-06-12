makeCacheMatrix <- function(x = matrix()) {
  ##this function makes a list: a matrix and its inverse (IF that inverse has already been solved)
  
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
  
  ##set the matrix's inverse, outside of the scope of the following function (<<-)
  setInverse <- function(new_inverse) matrix_inverse <<- new_inverse
  
  ##this is the list
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ##this function takes the matrix 'x', and goes to check if we know its inverse already. 
  ##If we don't, we calculate it here and then store it with 'x' through use of the function above.
  
  ##Go check what's stored as the inverse of x
  ouputter_matrix_inverse <- x$getInverse()
  if(!is.null(outputter_matrix_inverse)) { ##if there's an inverse already calculated and stored, return it.
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
  ##now output it.
  outputter_matrix_inverse
}
