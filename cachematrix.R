makeCacheMatrix <- function(mat){
  inv <- NULL;
  set <- function(matrics){
    mat <<- matrics;
    inv <<- NULL;
  }
  
  get <- function(){
    mat;
  }
  
  setinverse <- function(inverse){
    inv <<- inverse;
  }
  
  getinverse <<- function(){
    inv;
  }
  
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse);
}

cacheSolve <- function(mat, ...){
  inv <- mat$getinverse();
  if(!is.null(inv)){
    message("getting cached data!");
    return(inv);
  }
  
  data <- mat$get();
  inv <- solve(data);
  mat$setinverse(inv);
  inv;
}
