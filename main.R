## @knitr main
invisible(lapply(c("ggplot2", "dplyr", "compiler", "foreach", "doParallel"), require, ch = T))

# Parameters
hs <- seq(0.1, 0.6, by=0.1)
St.amp <- seq( 0, 2, by = 0.01) # Amplitudes
St.pos <- seq(-1, 1, by = 0.01) # Positions

# Main logic
calcQ.getDist <- function(a1, x1, y1, m0.0, m0.1, m0.2, m0.3) {
  b1   <- 2 - a1
  m1.0 <- a1 + b1
  m1.1 <- a1 * x1 + b1 * y1
  m1.2 <- a1 * x1 ^ 2 + b1 * y1 ^ 2
  m1.3 <- a1 * x1 ^ 3 + b1 * y1 ^ 3
  values <- (m0.0 - m1.0)^2 + (m0.1 - m1.1)^2 + (m0.2 - m1.2)^2 + (m0.3 - m1.3)^2
  sqrt(min(values))
}
calcQ <- function(xs, ys, h) {
  a0 <- 1; b0 <- 1; x0 <- 0; y0 <- h
  m0 <- a0 * x0 ^ (0:3) + b0 * y0 ^ (0:3)
  m0.0 <- m0[1]; m0.1 <- m0[2]; m0.2 <- m0[3]; m0.3 <- m0[4]
  
  data <- dplyr::rowwise(expand.grid(x = xs, y = ys, h = h))
  dplyr::mutate(data, value = calcQ.getDist(St.amp, x, y, m0.0, m0.1, m0.2, m0.3))
}

# Build Q
invisible(enableJIT(1))
registerDoParallel(cores = detectCores()) 
Q.parts <- foreach(h = hs) %dopar% calcQ(St.pos, St.pos, h)
Q <- data.frame(do.call("rbind", Q.parts))
invisible(enableJIT(0))

# Plot Q
interval.titles <- c("[0;1e-6]", "(1e-6; h^3]", "(h^3; h^2]", "(h^2; h]", "(h, infinity)")
interval.colors <- rainbow(5)
interval.getIndex <- Vectorize(function(h, value) {
  if (value <= 1e-6) 1
  else if (value <= h^3)  2
  else if (value <= h^2)  3
  else if (value <= h)    4
  else                    5
})
df <- Q %>% mutate(interval = factor(interval.getIndex(h, value)))
ggplot(df, aes(x = x, y = y, fill = interval)) + geom_raster() + ggtitle("Prony maps") +
  scale_fill_manual(name = "Eps", values = interval.colors, labels = interval.titles) +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
  facet_wrap( ~ h, labeller = labeller(h = label_both))