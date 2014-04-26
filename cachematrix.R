## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will create a special matrix which will cache
## its inverse in this object making use of the R scoping rules   

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
## This function will return the inverse of the special matrix created 
## by using the function makeCacheMatrix. first it will see it the inverse
## of the matrix is already cached and then return the same. If inverse is 
## not cached then it will calcuate the inverse and also cache using the 
## special matrix set functions. then it will return the calucated/cached
## inverse value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
             message("getting cached data")
             return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
  
}
