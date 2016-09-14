## @knitr error-sets
invisible(lapply(c("ggplot2", "dplyr", "compiler", "foreach", "doParallel"), require, ch = T))

drawErrorSet <- function(hs, C = 0.45, sigma = 0, specific = data.frame(x = c(), y = c(), h = c())) {
  # Parameters
  A <- 1 # Amplitude Sum
  step <- 0.01
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
  calcQ.getDist2 <- function(a1, x1, y1, m0.0, m0.1, m0.2, m0.3) {
    b1   <- A - a1
    m1.0 <- a1      + b1              - m0.0
    m1.1 <- a1 * x1 + b1 * y1         - m0.1
    m1.2 <- a1 * x1 ^ 2 + b1 * y1 ^ 2 - m0.2
    m1.3 <- a1 * x1 ^ 3 + b1 * y1 ^ 3 - m0.3
    value <- pmax(abs(m1.0), abs(m1.1), abs(m1.2), abs(m1.3))
    df <- data.frame(a = a1, b = b1, m0 = m1.0, m1 = m1.1, m2 = m1.2, m3 = m1.3, value = value)
    df[which.min(df$value),]
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
  calcQ2 <- function(data) {
    h <- data$h[1] # Only single h-valus is supported
    a0 <- A/2; b0 <- A/2; x0 <- -h; y0 <- h
    m0 <- a0 * x0 ^ (0:3) + b0 * y0 ^ (0:3)
    m0.0 <- m0[1]; m0.1 <- m0[2]; m0.2 <- m0[3]; m0.3 <- m0[4]
    
    # TODO: Rewrite
    origins <- apply(data, 1, function(row) calcQ.getDist2(St.amp.full, row[1], row[2], m0.0, m0.1, m0.2, m0.3))
    origins <- do.call("rbind", origins)
    result <- cbind(1:nrow(data), data, origins, C * data$h^2)
    colnames(result)[1] <- "index"
    colnames(result)[ncol(result)] <- "Ch^2"
    rownames(result) <- result$index
    result
  }
  
  # Specific
  if (nrow(specific) > 0) {
    specific.table <- calcQ2(specific)
    print(specific.table)
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
  
  hyp <- buildHyperbola(hs, h.as.factor = F)
  
  result <- ggplot(df, aes(x = x, y = y, fill = type)) + 
    geom_raster() + 
    ggtitle(bquote(paste("Error sets, ", sigma == .(sigma), ", ", C == .(C)))) +
    xlab(expression(bold(x[1]))) +
    ylab(expression(bold(x[2]))) +
    theme_bw() +
    scale_fill_manual(values = c("1" = "green", "2" = "lightblue", "3" = "transparent"), guide = F) +
    geom_line(data = hyp, aes(x = x1, y = x2, group = group), linetype = "dotted", inherit.aes = F) +
    geom_point(data = p, aes(x = x, y = y), pch = 21, size = 1, fill="red") +
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) +
    geom_abline(slope = 1) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    theme(axis.title.y = element_text(angle=0), panel.margin = unit(2, "lines")) +
    facet_grid(. ~ h, labeller = labeller(h = label_both)) + 
    coord_fixed()
 if (nrow(specific) > 0)
   result <- result +
     geom_point(data = specific.table, aes(x = x, y = y), pch = 21, size = 2, fill="purple") +
     geom_text(data = specific.table, aes(x = x, y = y, label = index), inherit.aes = F, hjust = 0, nudge_x = 0.05)
  result
}


## Tests
# drawErrorSet(c(0.1, 0.3, 0.5), sigma = 0)
p <- drawErrorSet(c(0.3), sigma = 0.4, specific = data.frame(
   x = c(-0.24, -0.02), 
   y = c(0.6, 0.79), 
   h = c(0.3)))
p