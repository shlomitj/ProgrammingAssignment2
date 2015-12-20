## The following function creates a matrix object that can cache its inverse.

## The first function, makeCacheMatrix, include creating the matrix as well as assigning lexical values.
makeCacheMatrix <- function(x = matrix()) { ## created a matrix
  inver <- NULL  ## creates a container for the inverse values
  set <- function(y) { 
    x <<- y
    inver <<- NULL
  
  }
  get <- function() x   ## get will allow us to return the original values of the matrix
  setinverse <- function(solve) inver <<- solve 
  getinverse <- function() inver ## getinverse will allow us to eturn the matrix inverse values
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function, CacheSolve, computes, caches and return the inverse values of the matrix and 
## put it into inver container
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  if(!is.null(inver)) { ## allow us to check if statment. If the inver is NOT null the cached will be returned.
    ## if it is NULL, the inver will be computed
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...) ## allow to compute the inver values using solve() function
  x$setinverse(inver)
  inver
}
