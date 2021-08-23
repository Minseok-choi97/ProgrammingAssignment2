## functions that cache inverse of matrix

## makeCacheMatrix function creates a "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y){
                x<<-y
                inverse<<-NULL
}
        get<-function() x
        setinverse<-function(solveMatrix) inverse<<-solveMatrix
        getinverse<-function() inverse
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}
## it computes the inverse of the matrix returned by above function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
}
        data<-x$get()
        inverse<-solve(data)
        x$setinverse(inverse)
        inverse
}
