## Esta función recibe como argumento una matriz y devuelve la matriz inversa indirectamente
## a partir de lo que se guarda en la memoria cache

## guarda en la memoria lo necesario para calcular la matriz inversa

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(invs) inv<<-invs
  getinv<- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  

}


## Hace el cálculo de la matriz inversa a partir de la función anterior

cacheSolve <- function(x, ...) {
  cacheSolve<-function(x){
    inversa <- x$getinv()
    if(!is.null(inversa)) {
      message("getting cached data")
      return(inversa)
    }
    matriz <- x$get()
    inversa <- solve(matriz)
    x$setinv(inversa)
    inversa
  }
}
