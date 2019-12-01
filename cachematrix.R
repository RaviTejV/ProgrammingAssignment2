## The first function, `makeCacheMatrix` creates a special "matrix", which is
## a list containing a functions to set and get the matrix and its inverse
## The second function, `cacheSolve` returns the matrix inverse from
## the cache (updating the cache if required, in the process)

## makeCacheMatrix takes in a matrix and returns a list of functions

makeCacheMatrix <- function(x = matrix()) {

		mat_inv <- NULL
		set <- function(y) {
				x <<- y
				mat_inv <<- NULL
		}

		get <- function() x
		setinv <- function(inv) mat_inv <<- inv
		getinv <- function() mat_inv
		list(set = set, get = get,
			setinv = setinv,
			getinv = getinv)
}


## cacheSolve takes the list created above (along with args pertaining
## to the solve() method) and returns the inverse of the matrix.
## Returns the cached data if available, else calculates the inv and
## returns the value after storing in cache

cacheSolve <- function(x, ...) {
		inv <- x$getinv()
		if(!is.null(inv)) {
			message("returning cached data")
			return(inv)
		}

		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		inv
}
