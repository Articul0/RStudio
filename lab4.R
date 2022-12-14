generateMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheInverse <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("получение закешированных данных")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  
  return(m)
}

testFunc <- function(m) {
  matrix = generateMatrix(m)

  start = Sys.time()
  cacheInverse(matrix)
  message('Без кеша:', Sys.time() - start)
  
  start = Sys.time()
  cacheInverse(matrix)
  message('Из кеша:', Sys.time() - start)
}