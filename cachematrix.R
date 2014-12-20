## This R function is able to cache potentially time-consuming calculation of
##inverse of matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL  ##Create a place holder for matrix inverse
  
  ##used for debugging purpose
  set <- function(y){
    x <<- y
    mat_inv <<- NULL
  }
  
  ##store the matrix in get
  get <- function()x
  
  ##assign the inverse of matrix from cacheSolve function for the first time calculating inverse
  setinverse <- function(inverse) mat_inv <<- inverse
  
  ##store matrix inverse
  getinverse <- function() mat_inv
  
  ##create a list of internal functions(methods) so a calling function knows how to access these method
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )
  
}


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ##call getinverse method in the makeCacheMatrix
  mat_inv <- x$getinverse()
  
  ##if mat_inv is not empty,then return its stored value
  if(!is.null(mat_inv)){
    message("getting cached data")
    return(mat_inv)
  }
  
  ##when a matrix is passed through for the first time
  data <- x$get()  ##obtain the matrix from makeCacheMatrix
  mat_inv <- solve(data, ...) ##Calculate inverse of matrix
  x$setinverse(mat_inv) ##pass the matrix inverse to makeCacheMatrix
  mat_inv ## Return a matrix that is the inverse of 'x'
  
  
}
