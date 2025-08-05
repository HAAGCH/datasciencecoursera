## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

#Ejemplo funcion vector ----

#makeVector <- function(x = numeric()) {
  #m <- NULL
  #set <- function(y) {
    #x <<- y
    #m <<- NULL
  #}
  #get <- function() x
  #setmean <- function(mean) m <<- mean
  #getmean <- function() m
  #list(set = set, get = get,
       #setmean = setmean,
       #getmean = getmean)
#}


#Ejemplo funcion media ----

#cachemean <- function(x, ...) {
  #m <- x$getmean()
  #if(!is.null(m)) {
    #message("getting cached data")
    #return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }


#Solucion ----

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#Test solución ----

B <- matrix(c(1,2,3,4),2,2)

B1 <- makeCacheMatrix(B)
cacheSolve(B1)

cacheSolve(B1)











