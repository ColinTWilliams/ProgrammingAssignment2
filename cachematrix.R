## Functions for computing and storing the inverse of a matrix to memory

## returns a list of functions(set, get) to modify and retrieve the values of X and X-inverse from within the makeCacheMatrix environment
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      set <- function(y){
            x <<- y
      }
      
      get<- function() x
      
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      
      list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Check if x has the inverse matrix cached already.  If so, return it.  If not, compute and cache before returning
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if (!is.null(i)){
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data)
      x$setinverse(i)
      i
}
