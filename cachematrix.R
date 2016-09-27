## Overall description:
## Matrix inversion takes a lot of computations,
## these functions work to compute & cache the inverse
## of a matrix and retrieve the result from cache
## whenever it is appropriate

##IMPORTANT ASSUMPTIONS:
## 1. Only matrix object will be supplied to makeCacheMatrix()
## 2. Matrix supplied is ALWAYS INVERTIBLE


## This function creates a special "matrix" object
## containing setter & getter for the matrix and
## to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL             #by default, no cached inverse
      set <- function(y = matrix()){ #set the matrix x
            x <<- y 
            inv <<- NULL 
            #whenver new matrix is supplied, 
            #its inverse is resetted to NULL
      }
      get <- function() x     #return matrix x
      setinverse <- function(inverse)
            inv <<- inverse   #cache the inverse
      getinverse <- function()
            inv               #retrieve the cached inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## This function computes the inverse of "matrix" returned
## by makeCacheMatrix function above. If inverse has
## been calculated, retrieve it from the cache
cacheSolve <- function(x, ...) {
        inv <- x$getinverse() #get cache
        if(!is.null(inv)){    #check cache if it exists
              message("getting cached inverse")
              return(inv)      
        }
        matrix <- x$get()     #get "matrix" to be inversed
        inv <- solve(matrix)  #calculate inverse
        x$setinverse(inv)     #set cache
        inv ## Return a matrix that is the inverse of matrix x
}
