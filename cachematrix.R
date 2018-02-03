## R Programming week 3 Assignment.
## makeCacheMatrix - function helps storing matrix and its inverse.
## cacheSolve - retrieves inverse and calculates it if necessary.


## makeCacheMatrix initializes an object that stores a numeric matrix passed to it 
## as an argument (x), as well as an inverse (inv) of that matrix as initialized object variables.
## Object provides basic behaviour:
## - returns matrix stored by object(get) and replaces it with another matrix (set);
## - returns stored inverse matrix (getinverse) and sets it (setinverse).

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inver) inv <<- inver
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Function cacheSolve returns the inverse matrix stored in object passed to it as an argument.
## In case passed object has the inverse set to null cacheSolve will calculate it and use the set method of an object to store inverse for future use.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrixX <- x$get()
    inv <- solve(matrixX, ...)
    x$setinverse(inv)
    inv
}
