## These two functions allow to calculated the inverse of a Matrix
## and cache it if it has been calculated before.
## 

## makeCacheMatrix is a function that create a special Matrix from 
#  input: an object Matrix
#  output: a list of 4 functions:
#     set: set the matrix
#     get: get the matrix
#     setinverse: set the cached matrix
#     getinverse: get the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
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
#  output: the inverse of x


cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)){
    message("getting cache matrix")
    return(inver)
  }
  mymatrix <- x$get()
  inver <- solve(mymatrix)
  x$setinverse(inver)
  inver
        ## Return a matrix that is the inverse of 'x'
}
