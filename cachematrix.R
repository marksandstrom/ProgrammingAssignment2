## Create a special matrix:
makeCacheMatrix <- function(M = matrix()) {
	InvM <- NULL
	set <- function(y) {
		M <<- y
		InvM <<- NULL
	}
	get <- function() M
	setInv <- function(solved) InvM <<- solved
	getInv <- function() InvM
	list(set=set,get=get,setInv=setInv,getInv=getInv)
}

## Return a matrix that is the inverse of its input 
## As input, use the output of makeCacheMatrix 
cacheSolve <- function(M, ...) {
      InvM <- M$getInv()
      if(!is.null(InvM)) {
      	message("getting cached data")
      	return(InvM)
      }
      data <- M$get()
	InvM = solve(data)
      M$setInv(InvM)
      InvM
}
