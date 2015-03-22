## cachematrix will cache the mean of the inversion of a square matrix
## so means of inversions do not have to be recalculated in the future 

## makeCachMatrix calls cacheSolve to get the inverse and stores in a list

makeCacheMatrix <- function(x = matrix()) {
        checklist <<- list("matrix" = x)
        ix <<- cacheSolve()
        invlist <<- list("matrix" = x, "inverse"= ix)
        return(invlist)
}

## cacheSolve  calculates the inverse of invertible square matrix
## UNLESS inverse already calculated

cacheSolve <- function() {
        ## Check to see if invlist exists. If not, create one.
        if(!exists("invlist")){
                invlist <<- list("matrix" = matrix(0), "inverse" = matrix(0))
        }
        ## Return a matrix that is the inverse of 'x' if already calculated
        if(identical(checklist$matrix, invlist$matrix)){
                message("getting cached data")
                return(invlist$inverse)
        }
        
        ## Since not already calculated, return calculated inverse of 'x'
        else {
                message("calculating inverse")
                ix <- solve(checklist$matrix)
                return(ix)
        }
}
