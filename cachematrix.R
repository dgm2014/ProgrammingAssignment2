## The cacheSolve function works with the makeCacheMatrix function to retrieve the inverse of a matrix, passed into it. from the cache, if it was found.
## If not, the cacheSolve function calculates the inverse, stores it in the cache using the makeCacheMatrix function and then returns the inverse as its output.

## The makeCacheMatrix function creates a special 'Matrix' object and returns a list of functions that allows you to 
## pass a matrix into it, retrieve the matrix, calculate the inverse and return the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        
	#Special object 'm' will hold the inverse of a matrix and it is set to NULL
	m <- NULL
        
	#This function assigns the argument passed into it, to x, and sets 'm' to NULL cause a new matrix is passed into the function.
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
	#This function returns the matrix that was set using the set function above
	get <- function() x

	#This function overrides the previous Inverse output stored in 'm'
        setinverse <- function(inverse) m <<- inverse

	#This function returns the Inverse of a matrix stored in 'm'
	getinverse <- function() m

	#This line returns a list of functions
        list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function returns the inverse of matrix 'x'. It will check the cache to determine if the inverse was already calculated, in which case it returns the inverse from the cache
## and skips the computation. If not it will compute the inverse, set the value of the inverse in the cache and return the inverse of the matrix as output


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	#Check the cache for the inverse of the matrix
	m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	#Retrieves the matrix, calculates its inverse and returns it as output
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}