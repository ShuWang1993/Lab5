## Question 1:
# rounds - simulation times, times - the number of bets in each simulation.
game <- function(rounds, times) {
  win <- as.vector(rep(0, rounds))
  for (n in 1:rounds){
    count <- as.vector(rep(0, times))
    for (i in 1:times) {
      x <- sample(1:6, size = 1, replace = TRUE)
      if (x == 6) {
        count[i] = 1
      } else {
        count[i] = 0
      }
    }
    if (sum(count) >= 1) {
      win[n] = 1
    } else {
      win[n] = 0
    }
  }
  return(sum(win) / rounds)
} 

game(1000, 4)
game(10000, 4)

## Question 2:
VecMin <- function (vec) {
  element <- vec[1]
  for (i in 2:length(vec)) {
    if (vec[i] < element) {
      element <- vec[i]
    } 
  }
  index <- which(vec == element)
  result <- list(min_element = element, min_index = index)
  return(result)
}

vec <- c(1,4,2,0,5,0)
VecMin(vec)
