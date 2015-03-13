
# makeCacheMatrix: as in example makevector
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  # inv will have the inverse matrix
  inv <- NULL
  
  # Set for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Get for the matrix
  get <- function() x
  
  # Set for the inverse
  setsolve <- function(inverse) inv <<- inverse
  # Get for the inverse
  getsolve <- function() inv
  
  # Return the matrix 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: tries to find the inverse amtrix in cache if not calc and store
cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  
  # If already calculated, returns it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse matrix
  x$setsolve(inv)
  
  # Return the inverse matrix
  inv
}


# Example usage:
# > test1 <- matrix(rnorm(4),2,2)       // Create a matrix test1
# > mat <- makeCacheMatrix(test1)       // Create our  matrix
# > mat$get()                           // Returns the matrix
#         [,1]       [,2]
#[1,] -0.7762872  0.1427712
#[2,]  1.0273139 -0.6707105
# > cacheSolve(mat)                     // Returns the inverse of matrix
#          [,1]       [,2]
#[1,] -1.793377 -0.3817482
#[2,] -2.746879 -2.0756724
# > cacheSolve(mat)                     // Call the 2nd time, so return
#getting cached data                    // the cached inverse
#        [,1]       [,2]                // "getting cached data" message
#[1,] -1.793377 -0.3817482
#[2,] -2.746879 -2.0756724