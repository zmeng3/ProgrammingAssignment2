## Below are two functions that are used to create a special
## object that stores a matrix and caches its inverse matrix

## This function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
	set <- function(y) {			## set value of the matrix
		x <<- y
		inv <<-NULL
	}
	get <- function() x				## get value of the matrix
	setinverse <- function(inverse) inv <<- inverse			## set the value of the inverse matrix
	getinverse <- function() inv							## get the value of the inverse matrix
	list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## This function computes the inverse of the special "matrix"
## returned by the first function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv<-x$getinverse()
    if(!is.null(inv)){					## True if the inverse has been calculated
        message("getting cachd data")	## and can directly get from cache and return
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)				## calculate the inverse matrix
    x$setinverse(inv)					## store the inverse matrix in cache
    inv
}
