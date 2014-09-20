## These two functions allow to calculated the inverse of a Matrix
## and cache it if it has been calculated before.


## makeCacheMatrix is a function that create a special Matrix  
#  input: an object Matrix
#  output: a list of 4 functions:
#     set: set the matrix
#     get: get the matrix
#     setinverse: set the cached matrix
#     getinverse: get the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  
  # set the initial matrix
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  
  # get the initial matrix
  get <- function() x
  
  # set the inverse of the matrix
  setinverse <- function(inverse) inver <<- inverse
  
  # get the inverse of the matrix
  getinverse <- function() inver
  
  # we return the list with the functions
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated, then the cachesolve should retrieve the inverse 
## from the cache.
#  input: an object Matrix x
#  output: a Matrix with the inverse of x


cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)){
    # the matrix is already cached so we return it
    message("getting cache matrix")
    return(inver)    
  }
  
  # we have to calcule the inverse and return it  
  mymatrix <- x$get()
  inver <- solve(mymatrix)
  
  # we cache the inverse for future use
  x$setinverse(inver)
  
  # Return a matrix that is the inverse of 'x'
  inver
}
