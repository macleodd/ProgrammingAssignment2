## Matrix inversion is usually a costly computation
## Therefore, there are benefits to caching the inverse of a matrix.
## The pair of functions below offer ability to cache the inverse of a matrix.


## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	# set/get matrix functions
	set <- function(m) {
		x <<- m
		im <<- NULL
	}
	get <- function() { x }

	# set/get inverse matrix functions
	setInverse <- function(m) { im <<- m }
	getInverse <- function() { im }

	# initializations
	im <- NULL

	# interface
	list(
		set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse
	)
}


## Retrieves the inverse of the special "matrix" from the cache, when present.
## Otherwise, it calculates the inverse of the special "matrix" and caches it.

cacheSolve <- function(x, ...) {
	im = x$getInverse()
	if (is.null(im)) {
		message("Inverse matrix not cached. Will calculate and cache it...")
		im <- solve(x$get(), ...)
		x$setInverse(im)
	}

	## Return a matrix that is the inverse of 'x'
	im
}
