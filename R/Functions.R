birth.pace <- function(Age, bx){
  B  <- sum(bx)  
  Bx <- cumsum(bx)
  lx <- B-Bx # two ways of calculating it
  nbx <- c(-diff(lx),0)
  
  nqx <- nbx / lx
  npx <- 1- nqx
  
  Lx <- c((lx[-1] +lx[-length(lx)])*0.5,lx[length(lx)]) # Wachter approx.
  
  Tx <- rev(cumsum(rev(Lx)))
  
  ex  <- Tx/lx 
  
  ex[1]
}

birth.shape <- function(Age, bx){
  B  <- sum(bx)  
  Bx <- cumsum(bx)
  lx <- B-Bx # two ways of calculating it
  nbx <- c(-diff(lx),0)
  
  nqx <- nbx / lx
  npx <- 1- nqx
  
  Lx <- c((lx[-1] +lx[-length(lx)])*0.5,lx[length(lx)]) # Wachter approx.
  
  Tx <- rev(cumsum(rev(Lx)))
  
  ex  <- Tx/lx 
  
  # Triplet
  hx <- bx/(B-Bx)
  fx <- bx/B
  Sx <- (B - Bx) / B
  
  # Variance, standard deviation and coefficient of variation
  
  #V <- rev( cumsum( rev(((Age -Age[1L] + 0.5 - ex[1L])^2)* fx))) # alpha has to be substracted
  #SD <- sqrt(V)
  #CV <-SD/ex 
  
  # e-dagger and entropy
  e.dag <- sum(ex*fx,na.rm = T)
  H <- e.dag / ex[1L]
  
  H
}
