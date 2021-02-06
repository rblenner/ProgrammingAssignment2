# function to set / get the value of the matrix and to set / get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  #initialize the value of the inverse
  inv <- NULL
  #assign matrix properties to a different environment (cache)
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##setting / getting matrix inverse 
  get <- function() x
  #inverse of matrix
  setinverse <- function() inv <<- solve(x)
  #getting inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
 


## function to calculate the inverse of the matrix created by the function above
  # checks to see if the inverse has already been calculated, skipping the calculation if it has
  # sets the inverse in the cache

cacheSolve <- function(x, ...) {
  #return the inverse if already present, else, proceed to calculation
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  #compute inverse of the matrix
  inv <- solve(data, ...)
  x$getinverse(inv)
  #return the inverse
  inv
}



#brief test
funct <- makeCacheMatrix()
funct$set(matrix(1:4, 2))
funct$get()
funct$setinverse()
funct$getinverse()



