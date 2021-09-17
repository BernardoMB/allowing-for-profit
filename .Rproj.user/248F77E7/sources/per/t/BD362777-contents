library(Rlab)
library(ggplot2)
library(wesanderson)
library(gridExtra)

options("scipen"=100, "digits"=4)

p <- 0.99 # survival probability
q <- 1-p # death probability
i <- 0.05 # interest rate
v <- 1/(1+i) # discount factor
s <- 1000 # sum insured
equivalencePrinciplePremium <- s*v*q
loading <- 0
premium <- equivalencePrinciplePremium + loading
n <- 10000 # number of policies in the porftolio
a <- 0.95 # confidence level for profit

# PDF normal aproximation
pdf.normal <- function(x, mean = 0, sd = 1) {
  dnorm(x, mean = mean, sd = sd)
}

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

# Colors
bw <- FALSE
if (bw) {
  colors <- c('black', 'black', 'black', 'black', 'black', 'black', 'black', 'black', 'black', 'black') 
} else {
  colors <- c('#EBF5FB', '#D6EAF8', '#D6EAF8', '#85C1E9', '#5DADE2', '#3498DB', '#2E86C1', '#2874A6', '#21618C', '#1B4F72')
}

# Create a plot
plot <- ggplot(mapping = aes(0)) +  labs(x = "Value of x", y = "F(x)")
# Insert axes to make it look nicer
x.axis <- geom_hline(yintercept = 0, size = 0.001)
y.axis <-geom_vline(xintercept = 0, size = 0.001)
# Get rid of the grey background
blankTheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
detailTheme <- theme_bw() + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 15))
# Create an empty graph
emptyGraph <- plot + x.axis + y.axis + detailTheme
# Plot empty graph
emptyGraph

# Ploting Agregate Loss density -------------------------------------------

# Ploting Agregate Loss density
title <- ggtitle('Agregate Loss density')
labels <- labs(x = 'x', y = 'f(x)')
graph <- emptyGraph + title + labels
for (j in 1:10) {n <- j*20
  prem <- (qnorm(a)*sqrt(n*(s^2*v^2*(q-q^2)))+n*s*v*q)/n
  mean <- n*(s*v*q-prem)
  variance <- n*(s^2*v^2*(q-q^2))
  standarDeviation <- sqrt(variance)
  if (bw) {
    col <- black
  } else {
    col <- wes_palette(10, name = 'GrandBudapest2', type = 'continuous')[j] 
  }
  totalLossDensity <- stat_function(fun = pdf.normal, args = c(mean, standarDeviation), color = col)
  line <- geom_vline(xintercept = mean, linetype = 6, color = col, size = 0.5 + j*0.1)  
  nLabel <- paste("n",n, sep = "=")
  premiumLabel <- paste("P",round(prem, digits = 3), sep = "=")
  anotation <- annotate(geom = "text",
           label = c(paste(nLabel,premiumLabel, sep = "  ")),
           x = c(mean),
           y = c(0.00075),
           angle = 90, 
           vjust = 1,
           hjust = 'left')
  graph <- graph + totalLossDensity + line + anotation
}
# Define limits on the graph
y.lim <- ylim(0, 0.001)
x.lim <- xlim(-4000, 700)
graph <- graph + x.lim + y.lim + x.axis + y.axis
graph

# Ploting Average Loss density --------------------------------------------

# Ploting Average Loss density
title <- ggtitle('Average Loss density')
labels <- labs(x = 'x', y = 'f(x)')
graph <- emptyGraph + title + labels
for (j in 1:5) {
  n <- j*40
  prem <- (qnorm(a)*sqrt(n*(s^2*v^2*(q-q^2)))+n*s*v*q)/n
  averageLossMean <- (s*v*q-prem)
  averageLossVariance <- (s^2*v^2*(q-q^2))/n
  averageLossStandarDeviation <- sqrt(averageLossVariance)
  if (bw) {
    col <- black
  } else {
    col <- wes_palette(5, name = 'GrandBudapest1', type = 'continuous')[j] 
  }
  averageLossDensity <- stat_function(fun = pdf.normal, args = c(averageLossMean, averageLossStandarDeviation), color = col, linetype = 1)
  line <- geom_vline(xintercept = averageLossMean, linetype = 6, color = col, size = 0.5 + j*0.2)
  nLabel <- paste("n",n, sep = "=")
  premiumLabel <- paste("P",round(prem, digits = 3), sep = "=")
  anotation <- annotate(geom = "text",
                        label = c(paste(nLabel,premiumLabel, sep = "  ")),
                        x = c(averageLossMean),
                        y = c(0.0005),
                        angle = 90, 
                        vjust = 1,
                        hjust = 'left')
  graph <- graph + averageLossDensity + line + anotation + y.lim + x.lim + title
}
# Define limits on the graph
y.lim <- ylim(0, 0.06)
x.lim <- xlim(-30, 0)
graph <- graph + x.lim + y.lim + x.axis + y.axis
graph

# Agregate Loss density ---------------------------------------------------

# Ploting Agregate Loss density
title <- ggtitle('Agregate Loss density')
labels <- labs(x = 'x', y = 'f(x)')
graph <- emptyGraph + title + labels
for (j in 1:10) {n <- j*20
  prem <- (qnorm(a)*sqrt(n*(s^2*v^2*(q-q^2)))+n*s*v*q)/n
  mean <- n*(s*v*q-prem)
  variance <- n*(s^2*v^2*(q-q^2))
  standarDeviation <- sqrt(variance)
  if (bw) {
    col <- 'black'
    linetype <- j
  } else {
    col <- wes_palette(10, name = 'GrandBudapest1', type = 'continuous')[j] 
    linetype <- 1
  }
  totalLossDensity <- stat_function(fun = pdf.normal, args = c(mean, standarDeviation), color = col, linetype = linetype, size = 1)
  line <- geom_vline(xintercept = mean, linetype = linetype, color = col)  
  nLabel <- paste("n",n, sep = "=")
  premiumLabel <- paste("P",round(prem, digits = 3), sep = "=")
  textLabel <- c(paste(nLabel,premiumLabel, sep = "  "))
  x0 <- 2000
  y0 <- 0.001
  annotation <- annotate('segment', size = 1, x = x0, xend = x0 + 400, y = y0 - 0.00005*j, yend = y0 - 0.00005*j, colour = col, linetype = linetype)
  text <- annotate('text', size = 3, x = x0 + 900, y = y0 - 0.00005*j, label = textLabel)
  graph <- graph + totalLossDensity + line + annotation + text
}
# Define limits on the graph
y.lim <- ylim(0, 0.001)
x.lim <- xlim(-4000, 4000)
graph <- graph + x.lim + y.lim + x.axis + y.axis
graph
p1 <- graph

# Average Loss density ----------------------------------------------------

# Ploting Average Loss density
title <- ggtitle('Average Loss density')
labels <- labs(x = 'x', y = 'f(x)')
graph <- emptyGraph + title + labels
for (j in 1:10) {
  n <- j*20
  prem <- (qnorm(a)*sqrt(n*(s^2*v^2*(q-q^2)))+n*s*v*q)/n
  averageLossMean <- (s*v*q-prem)
  averageLossVariance <- (s^2*v^2*(q-q^2))/n
  averageLossStandarDeviation <- sqrt(averageLossVariance)
  if (bw) {
    col <- 'black'
    linetype <- j
  } else {
    col <- wes_palette(10, name = 'GrandBudapest2', type = 'continuous')[j] 
    linetype <- 1
  }
  averageLossDensity <- stat_function(fun = pdf.normal, args = c(averageLossMean, averageLossStandarDeviation), color = col, linetype = linetype, size = 1)
  line <- geom_vline(xintercept = averageLossMean, linetype = linetype, color = col)
  nLabel <- paste("n",n, sep = "=")
  premiumLabel <- paste("P",round(prem, digits = 3), sep = "=")
  textLabel <- c(paste(nLabel,premiumLabel, sep = "  "))
  x0 <- 20
  y0 <- 0.06
  annotation <- annotate('segment', size = 1, x = x0, xend = x0 + 5, y = y0 - 0.003*j, yend = y0 - 0.003*j, colour = col, linetype = linetype)
  text <- annotate('text', size = 3, x = x0 + 10, y = y0 - 0.003*j, label = textLabel)
  graph <- graph + averageLossDensity + line + annotation + text + y.lim + x.lim + title
}
# Define limits on the graph
y.lim <- ylim(0, 0.06)
x.lim <- xlim(-40, 40)
graph <- graph + x.lim + y.lim + x.axis + y.axis
graph
p2 <- graph

# Agregate Loss density (equivalence premium) -----------------------------

# Ploting Agregate Loss density (equivalence premium)
title <- ggtitle('Agregate Loss density (equivalence principle)')
labels <- labs(x = 'x', y = 'f(x)')
graph <- emptyGraph + title + labels
for (j in 1:10) {
  n <- j*20
  prem <- s*v*q
  mean <- n*(s*v*q-prem)
  variance <- n*(s^2*v^2*(q-q^2))
  standarDeviation <- sqrt(variance)
  if (bw) {
    col <- 'black'
    linetype <- j
  } else {
    col <- wes_palette(10, name = 'GrandBudapest1', type = 'continuous')[j] 
    linetype <- 1
  }
  totalLossDensity <- stat_function(fun = pdf.normal, args = c(mean, standarDeviation), color = col, linetype = linetype, size = 1)
  nLabel <- paste("n",n, sep = "=")
  premiumLabel <- paste("P",round(prem, digits = 3), sep = "=")
  textLabel <- c(paste(nLabel,premiumLabel, sep = "  "))
  x0 <- 2000
  y0 <- 0.001
  annotation <- annotate('segment', size = 1, x = x0, xend = x0 + 400, y = y0 - 0.00005*j, yend = y0 - 0.00005*j, colour = col, linetype = linetype)
  text <- annotate('text', size = 3, x = x0 + 900, y = y0 - 0.00005*j, label = textLabel)
  graph <- graph + totalLossDensity + annotation + text
}
# Define limits on the graph
y.lim <- ylim(0, 0.001)
x.lim <- xlim(-4000, 4000)
graph <- graph + x.lim + y.lim + x.axis + y.axis
graph
p3 <- graph


# Average Loss density (equivalence premium) ----------------------

# Ploting Average Loss density (equivalence premium)
title <- ggtitle('Average Loss density (equivalence principle)')
labels <- labs(x = 'x', y = 'f(x)')
graph <- emptyGraph + title + labels
for (j in 1:10) {
  n <- j*20
  prem <- s*v*q
  mean <- s*v*q-prem
  variance <- (s^2*v^2*(q-q^2))/n
  standarDeviation <- sqrt(variance)
  if (bw) {
    col <- 'black'
    linetype <- j
  } else {
    col <- wes_palette(10, name = 'GrandBudapest2', type = 'continuous')[j] 
    linetype <- 1
  }
  totalLossDensity <- stat_function(fun = pdf.normal, args = c(mean, standarDeviation), color = col, linetype = linetype, size = 1)
  nLabel <- paste("n",n, sep = "=")
  premiumLabel <- paste("P",round(prem, digits = 3), sep = "=")
  textLabel <- c(paste(nLabel,premiumLabel, sep = "  "))
  x0 <- 20
  y0 <- 0.06
  annotation <- annotate('segment', size = 1, x = x0, xend = x0 + 5, y = y0 - 0.003*j, yend = y0 - 0.003*j, colour = col, linetype = linetype)
  text <- annotate('text', size = 3, x = x0 + 10, y = y0 - 0.003*j, label = textLabel)
  graph <- graph + totalLossDensity + annotation + text
}
# Define limits on the graph
y.lim <- ylim(0, 0.06)
x.lim <- xlim(-40, 40)
graph <- graph + x.lim + y.lim + x.axis + y.axis
graph
p4 <- graph

# Agregate Loss density (eq. principle + loading) -------------------------

# Ploting Agregate Loss density (equivalence premium + loading)
title <- ggtitle('Agregate Loss density (equivalence principle + loading of 10)')
labels <- labs(x = 'x', y = 'f(x)')
graph <- emptyGraph + title + labels
for (j in 1:10) {
  n <- j*20
  prem <- s*v*q + 10
  mean <- n*(s*v*q-prem)
  variance <- n*(s^2*v^2*(q-q^2))
  standarDeviation <- sqrt(variance)
  if (bw) {
    col <- 'black'
    linetype <- j
  } else {
    col <- wes_palette(10, name = 'GrandBudapest1', type = 'continuous')[j] 
    linetype <- 1
  }
  totalLossDensity <- stat_function(fun = pdf.normal, args = c(mean, standarDeviation), color = col, linetype = linetype, size = 1)
  line <- geom_vline(xintercept = mean, linetype = linetype, color = col)  
  nLabel <- paste("n",n, sep = "=")
  premiumLabel <- paste("P",round(prem, digits = 3), sep = "=")
  textLabel <- c(paste(nLabel,premiumLabel, sep = "  "))
  x0 <- 2000
  y0 <- 0.001
  annotation <- annotate('segment', size = 1, x = x0, xend = x0 + 400, y = y0 - 0.00005*j, yend = y0 - 0.00005*j, colour = col, linetype = linetype)
  text <- annotate('text', size = 3, x = x0 + 900, y = y0 - 0.00005*j, label = textLabel)
  graph <- graph + totalLossDensity + annotation + text + line
}
# Define limits on the graph
y.lim <- ylim(0, 0.001)
x.lim <- xlim(-4000, 4000)
graph <- graph + x.lim + y.lim + x.axis + y.axis
graph
p5 <- graph

# Average loss density (eq. principle + loading) --------------------------

# Ploting Average Loss density (equivalence premium)
title <- ggtitle('Average Loss density (equivalence principle + loading of 10)')
labels <- labs(x = 'x', y = 'f(x)')
graph <- emptyGraph + title + labels
for (j in 1:10) {
  n <- j*20
  prem <- s*v*q + 10
  mean <- s*v*q-prem
  variance <- (s^2*v^2*(q-q^2))/n
  standarDeviation <- sqrt(variance)
  if (bw) {
    col <- 'black'
    linetype <- j
  } else {
    col <- wes_palette(10, name = 'GrandBudapest2', type = 'continuous')[j] 
    linetype <- 1
  }
  totalLossDensity <- stat_function(fun = pdf.normal, args = c(mean, standarDeviation), color = col, linetype = linetype, size = 1)
  line <- geom_vline(xintercept = mean, linetype = linetype, color = col)  
  nLabel <- paste("n",n, sep = "=")
  premiumLabel <- paste("P",round(prem, digits = 3), sep = "=")
  textLabel <- c(paste(nLabel,premiumLabel, sep = "  "))
  x0 <- 20
  y0 <- 0.06
  annotation <- annotate('segment', size = 1, x = x0, xend = x0 + 5, y = y0 - 0.003*j, yend = y0 - 0.003*j, colour = col, linetype = linetype)
  text <- annotate('text', size = 3, x = x0 + 10, y = y0 - 0.003*j, label = textLabel)
  graph <- graph + totalLossDensity + annotation + text + line
}
# Define limits on the graph
y.lim <- ylim(0, 0.06)
x.lim <- xlim(-40, 40)
graph <- graph + x.lim + y.lim + x.axis + y.axis
graph
p6 <- graph

# Plots grid --------------------------------------------------------------

gridGraph <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2, layout_matrix = rbind(c(1,2), c(3,4), c(5,6)))
gridGraph
