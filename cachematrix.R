## These functions for Week 3 of the R Programming Coursera Course
## practice the concept of Lexical Scoping
##by cahing the inverse of a matrix

## create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
      x <<-y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## compute the inverse of the special "matrix" returned by the makeCacheMatrix function
## if the inverse already exists, then this function will simply retrieve it

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
