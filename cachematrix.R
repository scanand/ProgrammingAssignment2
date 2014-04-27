##makeCacheMatrix can be viewed as unit having 4 functions, and also holds the inverse matrix results
##returns the list of functions
makeCacheMatrix <- function(x = matrix()) {

        #initialise the variable
        inversed.matrix <- NULL
        
        #Func 1 :setter
        set <- function(y) {
                x <<- y
                inversed.matrix <<- NULL
        }
        
        #Func 2 :getter returns x
        get <- function() x
        
        #Func 3 : caches the inverse results
        setinverse <- function(inverse) inversed.matrix <<- inverse
        
        #Func 4 : returns inversed matrix, if calculated otherwise null
        getinverse <- function() inversed.matrix
        
        #this is just a returning list of functions
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
        
}

##cacheSolve function returns the inverse of matrix, if result is already calculated and stored,
## returns the stored result, otherwise calculates and keeps in storage 
## Returns the inversed matrix
cacheSolve <- function(x, ...) {
        #is inverse already cached?
        inversed.matrix <- x$getinverse()

        if(!is.null(inversed.matrix)) {
                # cached instance found
                message("getting cached data")
                return(inversed.matrix)
        }
        # first time get original matrix
        data <- x$get()
        # get inversed matrix
        inversed.matrix <- solve(data)
        #set it back in the cache
        x$setinverse(inversed.matrix)
        #return value
        inversed.matrix
}