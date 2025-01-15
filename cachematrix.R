## Caching the inverse of the matrix
## This pair of functions cache the value of inverse of matrix 
## so that when we need it again, it can be looked up in the cache
## rather than recomputed

## creating a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
           i <- NULL
           
           set <- function(y){
              
               x <<- y
               i <<- NULL  # Reset the cache when the matrix is updated
           }
                    
           get <- function() x 
           
           setinverse <- function(inverse) i <<- inverse
           
           getinverse <- function() i
           
           list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)    
                    
}


## computes inverse or retrieves the inverse of special "matrix" 
## from the cache

cacheSolve <- function(x, ...) {
  
        ## check if inverse is already cached
  
        i <- x$getinverse()
          if(!is.null(i)) {
             message("getting cached data")
             return(i)
        
        }
       
       data <- x$get()
       
       ## compute if the inverse is not cached
       
         if (nrow(data) != ncol(data)) {
           stop("Matrix must be square.")
         }
         
         # Check if the matrix is invertible
         if (det(data) == 0) {
           stop("Matrix is not invertible.")
         }
         
         # Calculate the inverse
         i <- solve(data, ...)
         
         # Cache the inverse
         x$setinverse(i)
         
         
         return(i)
        
 }
  

