# Create a list object as to be able to calculate 
# Inverse Matrix using cache
makeCacheMatrix <- function(m = matrix()) 
{
	inv <- NULL
	
	# Set new matrix object and make cached inverse NULL
	setMatrix <- function(new_m)
	{
		m <<- new_m
		inv <<- NULL
	}
	
	# get matrix object
	getMatrix <- function () m
	
	# Set Inverse matrix to a global variable as to use
	# it for inverse cache
	setInverse <- function(new_inv) inv <<- new_inv
	
	# get cached Inversed Matrix
	getInverse <- function() inv
	
	# return list with function
	list(setMatrix = setMatrix,
		 getMatrix = getMatrix,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

# This function should take as argument a list that be created
# using makeCacheMatrix
cacheSolve <- function(m, ...) 
{
    # First take cached Inversed Matrix
	inv <- m$getInverse()
	
	# if the cached Inversed matrix is not NULL then return it 
	# and take advantage from cache
	if(!is.null(inv))
	{
		message("*** Inverse Matrix is from cache ***")
		return(inv)
	}
	
	# otherwise get matrix
	temp_matrix <- m$getMatrix()
	
	# calculate inverse matrix
	inv <- solve(temp_matrix)
	
	# put new calculated inversed matrix to cache 
	# for future use if call this function again
	# and matrix do not cache
	m$setInverse(inv)
	inv
}
