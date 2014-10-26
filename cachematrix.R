## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly
## The following functions implement a cacheing its Inverse Matrix object

## This function creates a special "matrix" object 
#  that can cache its inverse

# usage: M <- matrix(1:4, nrow=2, ncol=2); cM <- makeCacheMatrix(M)
#   cM$getinverse()
#   cM$set(matrix(..., nrow =  .., ncol = ..))
#   
makeCacheMatrix <- function(x = matrix()) {  # 
  matInv <- NULL                             # the cache store
  # the given sample makeVector could be 
  # simplified while keeping full 
  # funtionality by defining the list of 
  # functions needed immediately
  # instead of first defining functions 
  # and later collect them in a list
  # this approach if used here
  list(set = function(new_matr) {     # matrix set (sub)function
    matr <<- new_matr                 # put new matrix in the "permanent" store
    matInv <<- NULL },                # reset the Inverse matrix store
    get = function() { matr },        # (sub)function to pull out the matrix from the store
    setinverse =                      # (sub)function to set the Inverse store 
      function(calc_inverse) 
      { matInv <<- calc_inverse },
    getinverse = function()           # (sub)function to retrieve the Inverse from store
      { matInv }
  )
}


## This function computes the inverse of the special
#  "matrix" returned by `makeCacheMatrix` above. If the inverse has
#  already been calculated (and the matrix has not changed), then
#  `cacheSolve` should retrieve the inverse from the cache

#  it is assumed that the matrix supplied is always invertible

# usage: cacheSolve(cM)

cacheSolve <- function(x, ...) {      ## Return a matrix that is the inverse of 'x'
  matInv <- cache_mat$getinverse()    #  get the contents of the cache store in the matrix object
  if(!is.null(matInv)) {              #  check wether it can be used
    # message("getting cached data")
    return(matInv)
  }
  matr <- cache_mat$get()             #  if the Inverse has to be computed new, we first need the matrix
  matInv <- solve(matr, ...)          #  compute the Inverse
  cache_mat$setinverse(matInv)        #  store the Inverse in the CacheMatrix object
  matInv                              #  return the computed Inverse
}
