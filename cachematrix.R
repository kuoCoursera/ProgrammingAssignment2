## This script contains a pair of funtions that cache and compute the 
## inverse of a matrix


## ===========================================================
## Function: makeCacheMatrix()
## input: a matrix
## output: a list containing 4 functions
##
## This function creates a special "vector", which is a list containing 
## the following 4 functions/tasks:
## 1. set original matrix and initialize the inverse matrix (set to NULL)
##    by overrides the containing environment
## 2. get original matrix 
## 3. set inverse matrix, which overrides the containing environment
## 4. get inverse matrix
## 
makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL  
        set <-function(y){
                x <<-y
                inv <<-NULL
        }
        get <- function () x  # get the input/original matrix
        setInverse <- function (inverse) inv <<- inverse  ##cache the inverse
        getInverse <- function () inv 
        list(set=set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## ===========================================================
## Function: cacheSolve()
## input: a matrix
## output: inverse of the input matrix
##
## This function calculates the inverse of the matrix created with 
## makeCacheMatrix(). 
## If the inverse is already in the cache (previously computed), 
## then just return the cache data without the computation.  
## Otherwise, use solve() to calculate the inverse of the matrix
## and sets the inv in the cache via setInverse().
##
cacheSolve <- function(x, ...) { 
        #get the inverse of 'x' as inv
        inv <- x$getInverse()
        
        #if the inverse has already been calculated previously, then retrieve the inverse from the cache
        if(!is.null(inv)){
                message("getting cached data")
                return(inv) # returns inverse matrix from cache and skip the computation below
        }
        
        #otherwise, if no inverse found in cache, compute the inverse
        data<-x$get() 
        inv <-solve(data, ...)  ##compute the inverse of a square matrix
        x$setInverse(inv) #set the new inverse in the cache
        inv #returns inverse matrix from solve()
}

## ===========================================================
# ##Overall description of what both functions do
#
# ## ------------------------------------------------------------
# matrix1<-makeCacheMatrix(matrix(c(7,0,-3,2,3,4,1,-1,-2), 3, 3))  #a 3x3 matrix object 'matrix2' of type list
# ## At this point:
# ##   x$getInverse()  # there is no inverse yet, so inv return NULL
# ##   > NULL 
# ##   x$get() # return original matrix
# ##   >       [,1] [,2] [,3]
# ##     [1,]    7    2    1
# ##     [2,]    0    3   -1
# ##     [3,]   -3    4   -2
# 
# ## ------------------------------------------------------------
# cacheSolve(matrix1)  ## Run the function 1st time.
# ## In this function:
# ##   inv <- x$getInverse()  #Try to get the inverse from cache.  But there is no cache data yet, so inv returns NULL
# ##
# ##   Since is.null(inv)== TRUE, do these
# ##     data<-x$get() #get the original matrix
# ##     inv <-solve(data, ...) # compute the inverse matrix
# ##     x$setInverse(inv) #set/write to the new inverse
# ##     inv # return inverse matrix
# ## ------------------------------------------------------------
# cacheSolve(matrix1)  # computer the inverse of the same matrix m again
# ##   x$getInverse() # Again, try to get the inverse from cache,. In this case, there already a cache data which 
# ##                  # previously computed from step2, so returns inverse matrix
# ##
# ##   Since inv is already assigned, output:
# ##     message("getting cached data")  # print message
# ##     return(inv) # returns inverse matrix from cache
# ##
# 
# ## ------------------------------------------------------------
# matrix2<-makeCacheMatrix(matrix(c(7,-6,-2,2), 2, 2)) #a 2x2 matrix object 'matrix1' of type list
# 
# cacheSolve(matrix2)  # different object, so this function will compute 'inverse', store it and return it
# 
# cacheSolve(matrix2)  # 2nd access, inverse is cached so just get the stored one. (Omitted teh computation)

