makeCacheMatrix <- function(x = matrix()) { # x is initialized in the function argument
  inv <- NULL           #This initializes an object within this enviornment to be used later in the function
  set <- function(y) {  #defines the set function, where most of the magic happens.
    x <<- y             #assigns the input argument to the x object in the parent environment. 
    inv <<- NULL        #assign the value of NULL to the m object in the parent environment.
                        #If there is an already valid mean cached in m, whenever x is reset, the value of m cached in the memroy of the object
                        #is cleared, forcing subsequent calls to cachmean()
  }
  get <- function() x   #This forces R to retrieve it from the parent environment of MakeVector.
  setinverse <- function(solve) inv <<- solve 
                        #m is defined in the parent enviornment.
                        #we need to access it after setmean () completes, 
                        #the code uses the <<- to assign the input argument to the value of m in the parent envioron.
  getinverse <- function() inv 
                        #R takes advantage of lexical scoping to find the correct symbol m to retrieve its value.
  list(set = set, get = get, #other part of the magic. assigned each function as element within a list () and returns in parent env
       setinverse = setinverse,
       getinverse = getinverse)
                        #when function ends, it returns a fully formed object of type makeVector() to be used downstream R code.
                        #Naming the list elements is what allows us to use the $ form of the extract operator 
                        #to access the functions by  name rather than using the 
                        #[[ form of the extract operator, as in myVector[[2]](), to get the contents of the vector.
  cacheSolve <- function(x, ...) { #this function requires an input argument of type makeVector()
    inv <- x$getinverse()
    if(!is.null(inv)) { #!is.null. is.null returns TRUE if its argument is NULL and FALSE otherwise.
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set(inv)
    inv
  }
