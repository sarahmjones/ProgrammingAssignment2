
  
makeCacheMatrix <- function(x = matrix()){  # input to be a matrix
  invmat <- NULL        #creating a matrix named invat - (re)set to Null every time the function 
                        #makeCacherMatrix is called 
  set <- function(z) {  ## creates a call to return the object z
    x <<- z             #saves the new matrix to x & sets it back to NULL
    invmat <<- NULL
  }
  get <- function() x  # the get function is a simple call to return the matrix x, the original matrix
  setinv <- function(solve) {invmat<<-solve}  ##set inv is the function to get the inverse of the matrix x
  getinv <- function() {invmat} ##another function to simply return a matrix - in this case the matric invmat - the inverse matrix
  list(set = set, get = get,   ##creates the object which is a list with the following objects: set, get, setinv & getinv.
       setinv = setinv,         ##if the functions are not listed here then it canno be accessed outside this function
       getinv = getinv)
 }

cacheSolve <- function(x, ...) {
  invmat <- x$getinv()          # gets the inverse of x matrix
  if(!is.null(invmat)) {        # as long as it is already cached (i.e. it is not NULL)
    message("getting cached data") ## if not null then will first return the message "getting cached data
    return(invmat)                 ## and then return the matrix invmat 
  }                                ## (the function to get the inverse - solve - is defined below)
  data <- x$get()               ## if the inverse is not already stored - then else - define data as original matrix using x$get
  invmat <- solve(data, ...)    ## take the inverse of matrix data
  x$setinv(invmat)              ## store the inverse in X - setinv
  invmat                        ## return the inverse matrix
}

###



A<-makeCacheMatrix(rbind(c(4,2), c(7,6)))
A$get()
A$getinv()
A$setinv()

cacheSolve(A)

