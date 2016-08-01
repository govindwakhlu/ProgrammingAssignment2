## The idea here is to create a "special vector" makeCacheMatrix, which caches the value of the inverse of a matrix. 
## Ways to test:
## Step 1: test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2)) --- creates a 2X2 matrix
## Step 2: test_matrix$getinverse() --- will give you null since no cached inverse exists
## Step 3: cacheSolve(test_matrix) --- calculates the inverse, and caches it
## Step 4: cacheSolve(test_matrix) --- before displaying the inverse, it shows the message "getting cached inverse of matrix"

## This is the special vector that creates the functions to get/set the original matrix, as well as get/set the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	
	i <- NULL
	set <- function(y) {
		x <<- y
	    i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)

}


## This function is used to calculate the inverse of the matrix. If a cached matrix for that is present, it uses that, 
## otherwise it calculates it afresh and then caches it for the new matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
	    if(!is.null(i)) {
	            message("displaying cached inverse of matrix")
	            return(i)
	    }
	    new_matrix <- x$get()
	    i <- solve(new_matrix,...)
	    x$setinverse(i)
	    i
}
