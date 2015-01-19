# Pair of functions that cache the inverse of a matrix.


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
	{
	i <- NULL
      set <- function(y) 
		{
            x <<- y
            i <<- NULL
        	}
    	get <- function() x
	setinverse <- function(inverse) m <<- inverse
    	getinverse <- function() i
	list(set = set, get = get,
           setinverse  = setinverse ,
           getinverse  = getinverse )
	}


# This function computes the inverse of the 
# special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) 
	{
      ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
      if(!is.null(i)) 
		{
            message("getting cached data")
            return(i)
        	}
	matrix <- x$get()
	i <- solve(matrix,...)
	x$setinverse(i)
	i
	}




# test 
# remove the '#' in the following lines and run for test
#m1=matrix(1:4,nrow=2,ncol=2)
#m1
#m2=makeCacheMatrix(m1)
#m2
#names(m2)
#m_inv=cacheSolve(m2)
#m1%*%m_inv
