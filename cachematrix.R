# Create an object with new 
makeCacheMatrix <- function(m = matrix()) 
{
	inv <- NULL
	
	setMatrix <- function(new_m)
	{
		m <<- new_m
		inv <<- NULL
	}
	
	getMatrix <- function () m
	
	setInverse <- function(new_inv) inv <<- new_inv
	getInverse <- function()a < inv
	
	list(setMatrix = setMatrix,
		 getMatrix = getMatrix,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

cacheSolve <- function(m, ...) 
{
    ## Return a matrix that is the inverse of 'x'
	inv <- m$getInverse()
	if(!is.null(inv))
	{
		message("*** Inverse Matrix is from cache ***")
		return(inv)
	}
	temp_matrix <- m$getMatrix()
	inv <- solve(temp_matrix)
	m$setInverse(inv)
	inv
}
