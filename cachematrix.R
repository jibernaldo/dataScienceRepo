 

## This function contructs an object that contains a matrix, its inverse, and
## the get and set methods associated with these two matrices 

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function( i ){
    inverse <<- i
  }
  
  getInverse <- function(){
    inverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function checks first if inverse matrix contained in cache matrix is not 
## null, and if this is the case returns its value; in other case, inverse matrix
## is caculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverseMatrix <- x$getInverse()
  
  if(is.null(inverseMatrix)) {
    matrix <- x$get()
    inverseMatrix <- solve(matrix)
  }
  
  x$setInverse(inverseMatrix)
  inverseMatrix
}


