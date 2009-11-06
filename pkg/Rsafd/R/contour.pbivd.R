`contour.pbivd` <-
structure(function(dist, n = 50, xlim = NA, ylim = NA, ...)  standardGeneric("contour.pbivd"), 
generic = structure("contour.pbivd", package = ".GlobalEnv"), package = ".GlobalEnv", group = list(),
valueClass = character(0), signature = c("dist","n","xlim","ylim"), 
#default = <S4 object of class structure("MethodsList", package = "methods")>, 
skeleton = quote(function(dist, n = 50, xlim = NA, ylim = NA, ...) 
{
    divis <- seq(from = 0.001, to = 0.999, length = (n+2)) 
    divis <- divis[2:(n+1)]
    
    if ( (is.na(xlim[1])) | (length(xlim) != 2)) {  
      x <- evalFunc(divis,get(paste("q",dist@Xmarg,sep = "")),dist@param.Xmarg)
    } else {  x <- seq(from = xlim[1], to = xlim[2], length = n) }

    if (is.na(ylim[1]) | length(ylim) != 2) {
     y <- evalFunc(divis,get(paste("q",dist@Ymarg,sep = "")),dist@param.Ymarg)
    } else { y <- seq(from = ylim[1], to = ylim[2], length = n) }
 
    xmat <- rep(x, n )
    ymat <- rep(y, each = n )
    zmat <- pbivd(dist, xmat, ymat)
      
   val <- list(x = x, y = y, z = matrix(ncol = n, nrow = n, byrow = F, 
       data = zmat   ))
contour(x,y,val$z)
title("Contour Plot of the CDF", xlab="u",ylab="v")
     invisible(val)
}
(dist, n, xlim, ylim, ...)), class = structure("standardGeneric", package = "methods"))
