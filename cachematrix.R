
## makeCacheMatrix takes a matrix as input and returns the inverse of that matrix. 
## If the matrix inverse has already been calculated, it will instead ask cachesolve 
## to check if the inverse is already cached.
## If it finds the inverse in cache it will run the solve function which is a computationally
## intensive operation and return the previously cached inverse.


makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) invers <<-inverse
  getinverse <- function() invers
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve returns the cached inverse created with
## the makeCacheMatrix function. If the cached inverse is saved, cacheSolve returns it
## with a message telling "getting data from inverse matrix"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invers <- x$getinverse()
  if (!is.null(invers)) {
    message("getting cached inverse matrix")
    return(invers)
  } else {
    invers <- solve(x$get())
    x$setinverse(invers)
    return(invers)
  }
}


## Test Cases

## amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
## cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
## amatrix$get()         # Returns matrix
## amatrix$getinverse()  # Returns matrix inverse