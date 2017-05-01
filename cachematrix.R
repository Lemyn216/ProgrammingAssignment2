## Data Science Specialization Track R Programming Course
## Programming Assignment 2- Lexical Scoping

## When running code for some computations, it may be very time-consuming 
## especially for loops.  Sometimes a computation may only need to be run 
## once and the result can be cached which will be recalled quickly and 
## efficiently for later use.

## Matrix Inversion is one such computation that is time consuming and 
## can benefit from being cached.

## First, we can create a special matrix that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) {
	## The pattern to follow is a list containing a function to
	## 1. set the matrix
	## 2. get the matrix
	## 3. set the inverse
	## 4. get the inverse

	inv <- NULL
	
	## set matrix function
	set <- function (y) {
		x <<- y
		inv <<- NULL
		
	}
	
	## get function
	get <- function() x
	
	## set inverse function
	setinv <- function(inverse) inv <<- inverse
	
	## get inverse function
	getinv <- function() inv
	
	# create list
	list(set=set, get=get, setinv=setinv, getinv=getinv)
	
}


## After creating a matrix and it's inverse, we can use the cacheSolve to 
## quickly retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get the current state of inverse and see if computed yet
        inv <- x$getinv()
        
        ## If inverse has been calculated, get it from the cache
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        
        ## If inverse has not been calculated, get matrix
        ## calculate the inverse for matrix
        ## set value of inverse in cache
        ## return value
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        return(inv)

}
