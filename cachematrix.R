## cacheInv assigns the inverse of a matrix
## getInv fetches that inverted matrix value
## setMat the value of a matrix
## getMat retrieves the value of a matrix

## The function returns a list of 4 elements (set, get, setinv, getinv)
## each of which is a function

makeCacheMatrix <- function(x = matrix()) {
     cache <- NULL
     setMat <-function(matrixValue) {
          x <<- matrixValue
          cache <<- NULL
     }
     getMat <- function() x
     cacheInv <- function(solve) cache <<- solve
     getInv <- function() cache
     list(setMat = setMat,
          getMat = getMat,
          cacheInv = cacheInv,
          getInv = getInv)

}


##This function will evaluate if two matricies are identical.

matEqual <- function(mat1, mat2)
     is.matrix(mat1) && is.matrix(mat2) && dim(mat1) == dim(mat2) && all(mat1 == mat2)

## Return a matrix that is the inverse of 'x' but first check if the matrix has changed.
## If it has changed, compute its inverse, else, proceed to see if there is a cached value

cacheSolve <- function(y, ...) {
     mat1 <- y
     mat2 <- y$getMat()
     if (!matEqual(mat1, mat2)) {
          message("The matrix has changed")
          
 ##since the matrix changed, we must compute the inverse of the new matrix and return it.         
        
          inverse <- solve(mat1, ...)
          return(inverse)
     }
## Return a matrix that is the inverse of x using elements of the makeCacheMatrix function
     inverse <- y$getInv()
     if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
     }
     data <- x$getMat()
     inverse <- solve(data, ...)
     x$cacheInv(inverse)
     inverse
}
