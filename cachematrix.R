# These functions serve to cache matrix inversion in order to save time.
# Due to the fact that matrix inversion can be a time consuming calculation,
# caching it should be beneficial instead of computing it repreatedly.
# The inverse matrix caching is done in two functions (two steps)

# The first function (makeCacheMatrix) creates a list that contains a function 
# to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse value of the matrix
# 4. get the inverse value of the matrix



makeCacheMatrix <- function(x = matrix()) {
      i<-NULL
      set<-function(y) {
        x<<-y
        i<<-NULL
      }
  get<-function() x
  setinv<- function(inverse) i<<-inverse
  getinv<-function () i
  list(set=set, get=get,setinv=setinv, getinv=getinv)
  
}


## This function (cacheSolve) serves to calculate the inverse of the matrix created with the 
# above function. However, this first checks to see if the inverse has already
# been computed. If so, then it just skips to displaying the value without the
#computation

cacheSolve <- function(x, ...) {
  #retrieve cached value      
  i <- x$getinv()
        #if cached value exists return
  if(!is.null(i)){
        message("retrieving cached data")
        return(i)
  }
        ## Return a matrix that is the inverse of 'x'
  data<-x$get()
  i<-solve(data)
  x$setinv(i)
  
  i
}

#TESTING

#x=rbind(c(1,-1/2), c(-1/2,1))
#i=makeCacheMatrix(x)
#i$get()


#cacheSolve(i)

#cacheSolve(i)

