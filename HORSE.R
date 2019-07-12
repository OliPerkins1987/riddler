

Player.A <- TRUE
Player.B <- FALSE
a.score  <- 0
b.score  <- 0

HORSE <- function(my.strat = 0.5, playerbstrat = 'random') {

  Player.A <- TRUE
  Player.B <- FALSE
  a.score  <- 0
  b.score  <- 0
  
  
while(a.score < 5 & b.score < 5) {
  
  
  if(Player.A == TRUE) {
  
  a.shot <- rbinom(1, 1, my.strat)

  b.shot <- rbinom(1, 1, my.strat)
    
  

  a.point <- ifelse(a.shot == 1 & b.shot == 0, 1, 0)
  
  a.score <- a.score + a.point
  
}

  if(Player.B == TRUE) {
    
    pct    <- runif(1)
    
    a.shot <- rbinom(1, 1, 0.5)
    
    b.shot <- rbinom(1, 1, 0.5)
    
    
    b.point <- ifelse(b.shot == 1 & a.shot == 0, 1, 0)
    
    b.score <- b.score + b.point
    
  }
  
  
  
  if(Player.A == TRUE) {
    
    Player.A <- ifelse(a.shot == 0, FALSE, TRUE)
    
    Player.B  <- ifelse(a.shot == 0, TRUE, FALSE)
    
  }
  
  if(Player.B == TRUE) {
    
    Player.B <- ifelse(b.shot == 0, FALSE, TRUE)
    
    Player.A <- ifelse(b.shot == 0, TRUE, FALSE)
    
    }
  
  
  #print(a.score, b.score)
  
  }
  
  return(list(a.score, b.score))
  
}


results.25 <- lapply(rep(0.25, times = 1000), function(x) {HORSE(x)})

sum(unlist(lapply(results.25, function(x) {which(x[[1]] == 5)}))) / 1000


results.50 <- lapply(rep(0.50, times= 1000), function(x) HORSE(x))  

sum(unlist(lapply(results.50, function(x) {which(x[[1]] == 5)}))) / 1000


results.99 <- lapply(rep(0.999, times= 1000), function(x) HORSE(x))  

sum(unlist(lapply(results.99, function(x) {which(x[[1]] == 5)}))) / 1000


