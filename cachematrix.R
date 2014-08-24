##There are two functions "makeCacheMatrix" and "cacheSolve "
## makeCacheMatrix creates custom matrix type which can run  four functions
## set stores the matrix in cache, get recalls the matrix
## setInverse and getInverse do the same but for the inverse of the original matrix
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL #store matrix in cache
  }
  get <- function() x #get matrix
  setInverse <- function(solve) m<<- solve #set inverse matrix
  getInverse <- function() m #get inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) ## create list of functions
}
## cacheSolve take a custom matrix type created by the makeCacheMatrix function
## and calculates the inverse matrix of it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse() #query the x matrix's cache
  if(!is.null(m))
  {
    message("getting cached data") 
    return(m) # return the cache
  }
  data <- x$get() # get the matrix used by makeCacheMatrix function
  m <- solve(data, ...) # calculate the inverse of the matrix
  x$setInverse(m) # store the inverse matrix in cache using the makeCacheMatrix set function
}
