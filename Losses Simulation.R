library(Rlab)

getLoss <- function () {
  bernulli <- rbern(1, q)
  loss <- 0
  if (bernulli == 1) {
    # Dies
    loss <- s*v-premium
  } else {
    # Lives
    loss <- -premium
  }
  return(loss)
}

n <- 1000
iterations <- 1000000
losses <- c()
averages <- c()
for (i in 1:iterations) {
  portfolioLosses <- 0
  for (j in 1:n) {
    portfolioLosses <- c(portfolioLosses, getLoss())
  }
  totalLoss <- sum(portfolioLosses)
  losses <- c(losses, totalLoss)
  average <- mean(portfolioLosses)  
  averages <- c(averages, average)
}

summary(losses)
hist(losses)
mean(losses)
var(losses)

summary(averages)
hist(averages)
mean(averages)
var(averages)