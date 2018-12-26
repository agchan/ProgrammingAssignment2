## makeCacheMatrix: create a special "matrix" that can cache its inverse
#setMat: set matrix with input matrix
#getMat: return the value of matrix 
#setInv: set inverse matrix if the value is available
#getInv: return the inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
    z <-NULL
    setMat <- function(y) {
        x <<- y
        z <- NULL #z is the invserse matrix
    }
    getMat <- function() x
    setInvMat <- function(inv) {
        z <<- inv
    }
    getInvMat <- function() z
    #return the special matrix object
    list(
        setMat=setMat,
        getMat=getMat,
        setInvMat=setInvMat,
        getInvMat=getInvMat
    )
}


## return the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInvMat()
        if(!is.null(inv)){
            message("getting cached inverse matrix")
            return(inv)
        }
        inv <- solve(x$getMat(),...)
        x$setInvMat(inv)
        inv
}
