## A pair of functions to cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL #initialize the inverse property
  set <- function(matrix) { #setting the matrix
    m <<- matrix
    i <<- NULL
  }
  
  get <- function() { #get the matrix
    m #return the matrix
  }
  
  SetInverse <- function(inverse){ #set the inverse of the matrix
    i <<- inverse 
  }
  GetInverse <- function(){ #return the inverse of the matrix
    i 
  }
  list(set = set, get = get, #return a list of the methods
       SetInverse = SetInverse,
       GetInverse = GetInverse)
}


## This function computes the inverse of the special matrix returned by the 
## function

cacheSolve <- function(x, ...) { #return the inverse of x
  m <- x$GetInverse()
  
  if (!is.null(m)){ #return the inverse if it is already set
    message("getting cached data")
    return(m)
  }
  
  data <-x$get() #get matrix
  
  m <- solve(data) %*% data #calculate inverse using matrix multiplication
  
  x$SetInverse(m) #set inverse to the object
  m #return the matrix
}
