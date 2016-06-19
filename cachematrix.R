##These two functions return an inversed matrix for input x
##adding this value into a cache list.

## Creates a cache list for input matrix x

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y){
                x<<-y
                s<<-NULL
        }
        get<-function() x
        setsolve<-function(sol) s<<-sol
        getsolve<-function() s
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## Finds inversed matrix for a matrix contained in a cache matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s<-x$getsolve()
        if (!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data<-x$get()
        s<-solve(data)
        x$setsolve(s)
        s
}
