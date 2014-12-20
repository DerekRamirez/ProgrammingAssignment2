## This creates a matrix and functions to get and set it's value.
## Note, x is passed by reference, so it's created in the larger
## environment by default once the function is called and can be called
## in other functions as long as it isn't defined locally within the
## function.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Coerce x into being a matrix.  Notice the superassignment operator, else I would
  ## just be creating a local matrix x that wouldn't work outside the function.
  if(!is.matrix(x)) x <<- as.matrix(x)
  
  ## This is what you put the inverse in later to see if it's been calculated,
  ## and to do anything with the inverse. The original matrix stays in x.
  inv <- NULL
  
  ##This sets the value of the matrix.  Notice the superassignment operator. Without
  ## it, you would be creating a local copy of x with the matrix that wouldn't
  ## be accesable outside the function. Same for inv.
  set <-function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## Returns the matrix value, nothing special.
  get <- function() x
  
  ## Sets the inverse of the matrix.  Notice it uses the superassignment operator.
  setInv <- function (i) inv <<- i
  
  ## Gives the inv of the matrix x, passed by reference in the function.
  getInv <- function() inv
  
  ## The tags and items for the makeCacheMatrix function.
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Using the matrix "object" from above, we can do the inverse, save it, and recover it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'.  Notice I didn't use the superassignment
  ## operator.  That's what setInv is for.
  inv <-x$getInv()
  
  ## If there's already an inverse stored, just retrieve it.
  if(!is.null(inv)){
    message("Getting cached inverse")
    return(inv)
  }
  
  ## If there isn't a stored inverse, then we need to make one.
  
  ##I use a local variable "mat" to keep the original matrix.
  mat <- x$get()
  
  ## Get the inverse, and locally store it to inv.
  inv <- solve(mat)
  
  ## Use the setInv function to superassign the inverse to the matrix "object".
  x$setInv(inv)
  
  ##Return out the matrix inverse. Not sure why I don't use "return" here.
  inv
  
}
