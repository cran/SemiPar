########## S-function: tps.cov ##########

# Evaluates the thin plate spline
# covariance function for two dimensional
# smoothing/kriging.

# Last changed: 23 MAY 2001

tps.cov <- function(r,m=2,d=1)
{     
    r <- as.matrix(r)
    num.row <- nrow(r)
    num.col <- ncol(r)

    r <- as.vector(r)

    nzi <- (1:length(r))[r!=0]


    ans <- rep(0,length(r))

    if ((d+1)%%2!=0)    
       ans[nzi] <- r[nzi]^(2*m-d)*log(abs(r[nzi]))     # d is even
    else
        ans[nzi] <- r[nzi]^(2*m-d)


    if (num.col>1) ans <- matrix(ans,num.row,num.col)  # d is odd

    return(ans)
}

######### End of S-function tps.cov ########
