library(stats)

#########################################################
# QED                                                   #
# Queen???s Economics Department Working Paper No. 1307 #
# A fast fractional difference algorithm                #
# Andreas Noack Jensen                                  #
# Morten AYrregaard Nielsen                             #
# 4-2013                                                #      
#########################################################

fracdiff <- function (x , d )
{
  iT <- length ( x )
  np2 <- nextn (2* iT - 1 , 2)
  k <- 1:( iT -1)
  b <- c (1 , cumprod (( k - d - 1)/ k ))
  dx <- fft ( fft ( c (b , rep (0 , np2 - iT )))*
        fft ( c (x , rep (0 , np2 - iT ))) , inverse = T )/ np2 ;
  return ( Re ( dx [1: iT ]))
}


