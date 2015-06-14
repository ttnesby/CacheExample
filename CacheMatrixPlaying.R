# Creation of matrix

set.seed(17)
x <- matrix(sample.int(15, size = 5*5, replace = TRUE), nrow = 5, ncol = 5)

solve(x)

makeMatrix <- function(x = matrix()) {
     
     matrix_inverse <- NULL
     
     set <- function(y) {
          x <<- y
          matrix_inverse <<- NULL
     }
     
     get <- function() x
     
     setinverse <- function(inverse) matrix_inverse <<- inverse
     
     getinverse <- function() matrix_inverse
     
     list(set = set, 
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

cacheinverse <- function(x, ...) {
     
     inverse <- x$getinverse()
     
     if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
     }
     data <- x$get()
     inverse <- solve(data, ...)
     x$setinverse(inverse)
     inverse
}

# Testing the makeMatrix
set.seed(17)
m <- makeMatrix(matrix(sample.int(15, size = 5*5, replace = TRUE), nrow = 5, ncol = 5))

m$get()
m$getinverse()
m$setinverse(solve(m$get()))
m$getinverse()

m$set(matrix(sample.int(15, size = 5*5, replace = TRUE), nrow = 5, ncol = 5))
m$get()
m$getinverse()

rm(m)

# Testing the cacheinverse

set.seed(23)
m <- makeMatrix(matrix(sample.int(15, size = 5*5, replace = TRUE), nrow = 5, ncol = 5))

cacheinverse(m) # 1st time - no cache message
cacheinverse(m) # 2nd time - cache message
cacheinverse(m) # 3rd time - cache message

m$set(matrix(sample.int(15, size = 7*7, replace = TRUE), nrow = 7, ncol = 7))
cacheinverse(m) # 1st time - no cache message
cacheinverse(m) # 2nd time - cache message
