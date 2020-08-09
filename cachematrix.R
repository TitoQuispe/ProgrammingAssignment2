## En esta asignación se hallará la inversa de una matriz y almacenarla en el caché.
## Para ello se creará dos funciones llamadas makeCacheMatrix y cacheSolve

## La función makeCacheMatrix creará un objeto "matrix" especial que puede almacenar su inversa en caché.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NUL
        set<-function(y) {
              x<<-y
              inv<<-NULL }
        get<-function() x
        setinv<-function(solve) inv<<-solve
        getinv<-function() inv
        list (set=set, get=get,
              setinv=setinv,
              getinv=getinv)
}


## La función cacheSolve calcula la inversa de la "matriz" especial devuelta por makeCacheMatrix anterior.
## Asimismo, cacheSolve recuperará la inversa de la memoria caché si es que esta ya se hubiera calculado.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv<-x$getinv()
          if(!is.null(inv) {
                message("getting catched data")
                return(inv)
                }
           mat<-x$get()
           inv<-solve(mat,...)
           x$setinv(inv)
           inv
}
