##**************************************************************************
## Functions that compute the inverse for a specified matrix input and cache the
## result so that further calls use the cache instead of recalculating.
##**************************************************************************

## This function creates a matrix that has a cachable inverse,
## and includes subfunctions to get/set the matrix value, and its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL					# initialize m	
	## Set the matrix and clear the cache.
    set <- function(y) 
	{
		x <<- y					# overwrite the data 
		m <<- NULL				# reset m to NULL
    }	
    
	## Get the input matrix.
	get <- function() 			# get the input matrix
	{
		x
	}
	
	## Save the calculated matrix inverse value.
	setInverse <- function(z)	# set the inverse of matrix x to m
	{
		m <<- z					# overwrite the inverse data 
	}
	
	## Get the inverse of the cached matrix.
	getInverse <- function()	# get the inverse of matrix x
	{
		m
	}
    
	## Return the list of functions.
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function calculates the inverse of the argument matrix 'x'. If the inverse has 
## already been calculated (and the matrix has not been overwritten), then the function
## retrieves the inverse of 'x' from the cache.
cacheSolve <- function(x, ...) 
{
	m <- x$getInverse()    		# get the inverse of matrix 'x' from the cache

	if(!is.null(m)) 			# evaluate whether the cache is empty
	{   
		# return the cached value of 'x' inverse, if it exists
		message("getting data from the cache") 
		return(m)                
	}
	
	# if there is no cached value, calculate the inverse of x and return the result
	data <- x$get()             
	m <- solve(data, ...)   	# calculate the inverse of the matrix
	x$setInverse(m)             # save the inverse to cache
	m
}


