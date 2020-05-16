##This is the program that gives the inverse of matrix by caching it rather than computing it ,,
##because computing makes program costly and we have to compute it repeatedly.

##This makeCacheMatrix will create a special function that will be cached after , 
##there are some other special function also inside it .

makeCacheMatrix <- function(a = matrix()) 
{
        x <- NULL
        set <- function(b) 
        {
                a <<- b
                x <<- NULL
        }
        get <- function() 
        {
        	a
        }
        setinverse <- function(inverse) 
        {
        	x <<- inverse
        }
        getinverse <- function() 
        {
        	x
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
##This function will return a matrix that is the inverse of given matrix, 
##the first time we compute is the computed output and the next time we get output is taken out from cache .
cacheSolve <- function(a, ...)
{
        x <- a$getinverse()
        if(!is.null(x)) 
        {
                message("The data is been cached , and getting it.")
                return(x)
        }
        data <- a$get()
        x<- solve(data, ...)
        a$setinverse(x)
        x
}
