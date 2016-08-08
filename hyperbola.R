## @knitr hyperbola
invisible(lapply(c("ggplot2", "dplyr"), require, ch = T))

# draw x1 * x2 = sign * h^2
drawHyperbola <- function(hs, sg = -1) {
  
  len <- 1000
  d <- expand.grid(x1 = seq(-1, 1, length = len + 1)[-(len / 2 + 1)], h = hs) %>%
    mutate(x2 = sg * h^2 / x1) %>%
    mutate(group = factor(h * sign(x1))) %>%
    mutate(h = factor(h)) %>%
    filter(abs(x2) <= 1)
  
  p.x <- c(-hs, hs)
  p.y <- p.x * sg
  p <- data.frame(x = p.x, y = p.y)
  
  title <- if (sg == -1) expression(x[1] * x[2] == -h^2) else expression(x[1] * x[2] == h^2)
  
  ggplot() +
    geom_line(data = d, aes(x = x1, y = x2, group = group, colour = h, linetype = h), size = 2) +
    scale_colour_discrete(name = "h") +
    scale_linetype_discrete(name = "h") +
    geom_point(data = p, aes(x = x, y = y), pch = 21, size = 5, fill="orange") +
    geom_abline(slope = 1) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    ggtitle(title) +
    xlab(expression(bold(x[1]))) +
    ylab(expression(bold(x[2]))) +
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() +
    theme(legend.key.width = unit(2, "cm"), axis.title.y = element_text(angle=0)) +
    coord_fixed()
}