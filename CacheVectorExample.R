# Kind of vector object with basic methods

makeVector <- function(x = numeric()) {
     
     m <- NULL
     
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     get <- function() x
     
     setmean <- function(mean) m <<- mean
     
     getmean <- function() m
     
     list(set = set, 
          get = get,
          setmean = setmean,
          getmean = getmean)
}

# Testing the func
v <- makeVector(1:20)

v$get()
v$setmean(mean(v$get()))
v$getmean()

v$set(1:5)
v$get()
v$getmean()
v$setmean(mean(v$get()))
v$getmean()

rm(v)

# Kind of layer 2 of the makeVector - 'automating' the mean stuff
cachemean <- function(x, ...) {
     
     m <- x$getmean()
     
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- mean(data, ...)
     x$setmean(m)
     m
}

# Testing the func

v <- makeVector(1:20)

v$setmean(mean(v$get()))

# Checking use of chache for already calc mean
cachemean(v)

# Setting new vector
v$set(1:5)
cachemean(v) # no mean, calc mean and returning
cachemean(v) # cached mean

rm(v)
