## Cache function to cache the inverse of the matrix
## Usually generating single inverse of a matrix may result in faster computational time.
## Larger datasets involving large number of inverse calculations however will be slow.
## The functions written below creates a matrix and caches its inverse.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        matInverse <-NULL;
        setMatrix<-function(matrix){
                x<<-matrix;
                matInverse<<-NULL
                
        }
        getMatrix<-function() x
        setInverse<-function(matrixInverse) matInverse<<-matrixInverse;
        getInverse<-function() matInverse;
        list(getMatrix=getMatrix,setMatrix=setMatrix,setInverse=setInverse,getInverse=getInverse);
        
}


## This function takes the matrix generated above 
## and calculates its inverse if not calculated earlier 
## behaving like a cache memory of the inverse matrix.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        inverseMatrix=x$getInverse();
        #if the inverseMatrix is already generated then get the data
        #instead of again calculating it
        if (!is.null(inverseMatrix)){
                message("Getting cached data")
                return(inverseMatrix)
        }
        matrixGet<-x$getMatrix();
        inverseOfMatrix <- solve(matrixGet);
        x$setInverse(inverseOfMatrix);
        inverseOfMatrix;
}
