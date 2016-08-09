## @knitr error-sets
invisible(lapply(c("ggplot2", "dplyr", "compiler", "foreach", "doParallel"), require, ch = T))

drawErrorSet <- function(hs, C = 0.5, sigma = 0) {
  # Parameters
  A <- 1 # Amplitude Sum
  step <- 0.001
  St.amp <- seq(sigma, A-sigma, by = step) # Amplitudes
  St.amp.full <- seq(0, A, by = step) # Amplitudes
  St.pos <- seq(-1, 1, by = step) # Positions
  
  # Main logic
  calcQ.getDist <- function(a1, x1, y1, m0.0, m0.1, m0.2, m0.3) {
    b1   <- A - a1
    m1.0 <- a1      + b1
    m1.1 <- a1 * x1 + b1 * y1
    m1.2 <- a1 * x1 ^ 2 + b1 * y1 ^ 2
    m1.3 <- a1 * x1 ^ 3 + b1 * y1 ^ 3
    min(pmax(abs(m0.0 - m1.0), abs(m0.1 - m1.1), abs(m0.2 - m1.2), abs(m0.3 - m1.3)))
  }
  calcQ <- function(xs, ys, h) {
    a0 <- A/2; b0 <- A/2; x0 <- -h; y0 <- h
    m0 <- a0 * x0 ^ (0:3) + b0 * y0 ^ (0:3)
    m0.0 <- m0[1]; m0.1 <- m0[2]; m0.2 <- m0[3]; m0.3 <- m0[4]
    
    data <- dplyr::rowwise(expand.grid(x = xs, y = ys, h = h))
    data <- dplyr::mutate(data, value = calcQ.getDist(St.amp, x, y, m0.0, m0.1, m0.2, m0.3))
    data <- dplyr::mutate(data, value.full = calcQ.getDist(St.amp.full, x, y, m0.0, m0.1, m0.2, m0.3))
    data
  }
  
  # Build Q
  invisible(enableJIT(1))
  registerDoParallel(cores = detectCores()) 
  Q.parts <- foreach(h = hs) %dopar% calcQ(St.pos, St.pos, h)
  Q <- data.frame(do.call("rbind", Q.parts))
  invisible(enableJIT(0))
  
  # Plot Q
  interval.titles <- c(
    expression(0), 
    expression(paste("(0;", C * h^2, "]")),
    expression(paste("(", C * h^2, ";", infinity, "]")))
  
  getType <- Vectorize(function(h, value, value.full) {
    if (value <= C * h^2) 1
    else if (value.full <= C * h^2)  2
    else 3
  })
  if (sigma < 1e-6)
    getType <- Vectorize(function(h, value, value.full) ifelse(value <= C * h^2, 2, 3))
  df <- Q %>% mutate(type = factor(getType(h, value, value.full)))
  
  p.x <- c(-hs, hs)
  p.y <- -p.x
  p <- data.frame(x = p.x, y = p.y, h = abs(p.x))
  
  ggplot(df, aes(x = x, y = y, fill = type)) + 
    geom_raster() + 
    ggtitle(bquote(paste("Error sets, ", sigma) == .(sigma))) +
    xlab(expression(bold(x[1]))) +
    ylab(expression(bold(x[2]))) +
    theme_bw() +
    scale_fill_manual(values = c("1" = "green", "2" = "lightblue", "3" = "transparent"), guide = F) +
    geom_point(data = p, aes(x = x, y = y), pch = 21, size = 1, fill="red") +
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) +
    geom_abline(slope = 1) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    theme(axis.title.y = element_text(angle=0)) +
    facet_grid(. ~ h, labeller = labeller(h = label_both)) + 
    coord_fixed()
}

drawErrorSet(c(0.1, 0.3, 0.5), C = 0.5, sigma = 0.3)