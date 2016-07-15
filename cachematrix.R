## This pair of functions together create a means of calculating and
## cacheing the inverse of a given matrix

## makeCacheMatrix takes a matrix as its argument
## it sets up setter and getter functions for the matrix and its inverse
## then returns a list containing functions to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        
	inverse <- NULL
    
        set <- function(y) {
                x <<- y
        	inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse
    )
}


## cacheSolve calculates the inverse of the special "matrix" created with the above
## function, reusing cached result if it's available

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    m <- x$get()
    inverse <- solve(m, ...)
    x$setinverse(inverse)
    inverse
}