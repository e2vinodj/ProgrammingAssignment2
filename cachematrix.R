## Function makeCacheMatrix will perform the following functions
## 	1. set: Sets the value of the Matrix only when they are different (either values or DIM)
##             or the very first time.
## 	2. get: Gets the value of the Matrix.
## 	3. setinvMatrrix: Set the inverse of the Matrix (caching).
## 	4. getinvMatrix: Get the inverse of the Matrix.

makeCacheMatrix <- function(x = matrix()) {
	invX <- NULL
	set <- function(y) {
	        if  (isTRUE(all.equal(x,y)))  {
				message("No change in matrix structure and data")
			} 
			else {
				x <<- y
				invX <<- NULL
			}
        }
    get <- function() x
    setinvMatrrix <- function(solveX) invX <<- solveX
    getinvMatrix <- function() invX
    list(set = set, get = get,
        setinvMatrrix = setinvMatrrix,
        getinvMatrix = getinvMatrix)
}


## Function - cacheSolve - Returns the Inverse of a Matrix X. Uses caching to save processing time.
## if the inverse is already computed, then it returns the value from the Cache.
## If Not- Then it computes the Inverse of the Matrix. Perform Caching using setinvMatrrix Function.

cacheSolve <- function(x, ...) {
        
	invX <-x$getinvMatrix()
		
	if(!is.null(invX)) {
        message("getting cached matrix data")
        return(invX)
    }
    data <- x$get()
    invX <- solve(data)
    x$setinvMatrrix(invX)
    invX
		
}

