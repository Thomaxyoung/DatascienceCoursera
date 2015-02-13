 ## 1. The makeCacheMatrix function returns a list of four functions:
 ##    get, set, setinvers, and getinvers.
 ## 2. The cacheSolve function use the list of functions returned by 
 ##    makeCacheMatrix to check weather the invers was cached and 
 ##    solves it if the cache dose not exist.
  
 ## makeCacheMatrix act as a function constructor
 
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- matrix()
    set <- function(y) {
      x <<- y
      invers <<- matrix()
     }
     get <- function() x
     setinvers <- function(invers) inv <<- invers
     getinvers <- function() inv
     list(set = set, get = get,
          setinvers = setinvers,
          getinvers = getinvers)
     }

     # >makeCacheMatrix()$set(matrix(1:4,2,2))
     # > makeCacheMatrix()$get()
     # [,1]
     # [1,]   NA
     # the code above will give nothing cause when calling
     # makeCacheMatrix()$get() the function iniciate the x=matrix()
     # to avoid this, it is better to use a func_list to store the functions
     # constructed
     
    
 cacheSolve <- function(x, ...) {
         
   
 inv <- x$getinvers()
   if(!is.na(inv[1,1])) {
     message("getting cached data")
     inv
   }
   data <- x$get()
   inv <- solve(data)
   x$setinvers(inv)
   inv
 }

## After sourced all code, try this:
##>func_list<-makeCacheMatrix(matrix(1:4,2,2))
##>func_list$get()
#                [,1] [,2]
#           [1,]    1    3
#           [2,]    2    4
## the makeCacheMatrix will construct all functions and initiate 
## the matrix.
##cacheSolve(funclist) twice, fist time the answer was calculated
##second time the cached answer was get.
# > cacheSolve(funclist)
#       [,1] [,2]
# [1,]   -3  2.5
# [2,]    2 -1.5
# > cacheSolve(funclist)
# getting cached data
#       [,1] [,2]
# [1,]   -3  2.5
# [2,]    2 -1.5


## If a bigger matrix was inputed, the differce of processing time was clear.