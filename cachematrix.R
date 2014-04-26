## This file contains my solution for the 
## R Programming peer assessment for Week 3

## The functions below are copied from the assignment instructions example functions
## Only the minimum has been changed in order to implement the new matrix capability

# Use these functions as follows:


# > myMatrix <- rbind(c(1, 2), c(3, 4)) 
# > myMatrix
# [,1] [,2]
# [1,]    1    2
# [2,]    3    4
# 
# > myCachedMatrix <- makeCacheMatrix(myMatrix)
# > cacheSolve(myCachedMatrix)
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# 
# > cacheSolve(myCachedMatrix)
# getting cached data
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5

makeCacheMatrix <- function(x = matrix()) {
  
  # This is a local variable that will store the inverse value
  inverse <- NULL
  
  # Define the set function. Make sure it clears out any prior
  # inverse values that may have been associated with the previous x value
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # get function just returns x, nothing special
  get <- function() x
  
  #makeVector called the variable below 'mean' but that is a little confusing
  #because 'mean' is also a function name.
  setinverse <- function(inverseMatrix) inverse <<- inverseMatrix 
  
  # just gets the inverse value, nothing special
  getinverse <- function() inverse
  
  # This makeCacheMatrix function returns an 'object' that is a list of the above functions
  # and each element in the list has a name to match the function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function must be passed an object created by the makeCacheMatrix function
## it will return the inverse of the matrix, caching the result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #this is where the actual solve() function is called
  x$setinverse(m)
  m
}
