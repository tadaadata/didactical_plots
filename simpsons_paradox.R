# Simpsons paradox, in plots
library(dplyr)
library(ggplot2)
library(MASS)

# Via https://simplystatistics.org/2017/08/08/code-for-my-educational-gifs/
## simulate data
N     <- 100
Sigma <- matrix(c(1,0.75,0.75, 1), 2, 2)*1.5
means <- list(c(11,3), c(9,5), c(7,7), c(5,9), c(3,11))
dat   <- lapply(means, function(mu) mvrnorm(N, mu, Sigma))
dat   <- tbl_df(Reduce(rbind, dat)) %>%
  mutate(Z = as.character(rep(seq_along(means), each = N)))
names(dat) <- c("X", "Y", "Z")

## First plot
sim_p1 <- ggplot(dat, aes(X,Y)) +
  geom_point(size = 2, alpha = .8) +
  geom_smooth(method = lm, color = "red", se = FALSE)

## second plot
means <- tbl_df(Reduce(rbind, means)) %>%
  setNames(c("x","y")) %>%
  mutate(z = as.character(seq_along(means)))

corrs <- dat %>% group_by(Z) %>% summarize(cor = cor(X,Y)) %>% .$cor

sim_p2 <- ggplot(dat, aes(X, Y, color = Z)) +
  geom_point(size = 2, show.legend = FALSE, alpha = 0.8) +
  scale_color_brewer(palette = "Set1", guide = FALSE)

## third plot
sim_p3 <- sim_p2 +
  geom_smooth(method = lm, se = FALSE) +
  annotate("label", x = means$x, y = means$y, alpha = .6,
           label = paste("Z=", means$z), cex = 9) +
  ggtitle(paste("Pearson's r = ",  paste(signif(corrs, 2), collapse = ", ")))

sim_p1
sim_p2
sim_p3
