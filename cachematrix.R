## makeCachematrix() is a function to create a matrix object.
## It also assigns a list of functions to it.
## cacheSolve() is a function used to get inverse of matrix.


## makeCachematrix() creates an object of type matrix.
## Functions such as fetching matrix,inverse of matrix and
## changing values of both matrix and inverse of matrix are defined.

makeCachematrix <- function(x = matrix())
{
        inv <- NULL				# Initializing Inverse of Matrix
        set <- function(y = matrix())	# Changing Matrix object
	  {
                if(!identical(x,y))		# Check to compare old and new matrix
		   {
		    message("Change matrix")
		    x <<- y
                inv <<- NULL
		   }
		    else
		   {
   		    message("Matrix unchanged")
		   }
        }
        get <- function() x			# Returning Matrix object
        setinv <- function(i = matrix()) inv <<- i	# Changing Inverse of matrix
        getinv <- function() inv		# Returning Inverse of Matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve() returns inverse of matrix object.
## This is done by either getting value in cache
## or calculating the inverse.

cacheSolve <- function(x,...) {
        inv <- x$getinv()		# Checking if Cache holds a value
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)		# Calculating inverse of a matrix
        x$setinv(inv)
        inv
}
