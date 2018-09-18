## DATA SCIENCE SPECIAIZATION: COURSE 2
## R Programming: Week 3 
## Programming Assignment 2: Quiz


#This function (makeCacheMatrix) creates a special "vector", which is a list containing a function to:
#1)Set the value of the matrix. 2)Get the value of the matrix. 
#3)Set the value of the inverse of the matrix. 4)Get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


#This function calculates the inverse of the special "vector" created in makeCacheMatrix.
#It first checks to see if the inverse has already been calculated.
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setmean function.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("Getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  return(m)
}


#Testing the function (1)
x<-matrix(c(3,7,1,9,6,0,18,5,2,10,5,2,7,17,16,3),4,4) #create matrix
matCache<-makeCacheMatrix(x) #call the "makeCacheMatrix" function on x
matCache #see results of the function
cacheSolve(matCache) #call the "cacheSolve" function on matCache
cacheSolve(matCache) #call the "cacheSolve" function on matCache again this should retrive the value from cache
x%*%cacheSolve(matCache) #Verify that the matrices are indeed inverses of one another

#Testing the function (2)
x<-matrix(1:4,2,2) #create matriz
matCache<-makeCacheMatrix(x) #call the "makeCacheMatrix" function on x
matCache #see results of the function
cacheSolve(matCache) #call the "cacheSolve" function on matCache
cacheSolve(matCache) #call the "cacheSolve" function on matCache again this should retrive the value from cache
x%*%cacheSolve(matCache) #Verify that the matrices are indeed inverses of one another

