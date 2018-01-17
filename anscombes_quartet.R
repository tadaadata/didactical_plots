library(readr)
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(scales)


## original data ----
# taken from:
# https://en.wikipedia.org/wiki/Anscombe's_quartet#Data
anscombe <- read_delim("./data/ansc.txt",
                       "\t", escape_double = FALSE, col_names = FALSE,
                       trim_ws = TRUE) %>%
  rename(I.x   = X1, I.y   = X2,
         II.x  = X3, II.y  = X4,
         III.x = X5, III.y = X6,
         IV.x  = X7, IV.y  = X8)


# restructure data ----
n <- tibble(
  qrt = c(rep("I", 11), rep("II", 11), rep("III", 11), rep("IV", 11)),
  x   = c(anscombe$I.x, anscombe$II.x, anscombe$III.x, anscombe$IV.x),
  y   = c(anscombe$I.y, anscombe$II.y, anscombe$III.y, anscombe$IV.y)
)


## regression-models ----
m1 <- lm(I.y ~ I.x, anscombe) %>% tidy()
m2 <- lm(II.y ~ II.x, anscombe) %>% tidy()
m3 <- lm(III.y ~ III.x, anscombe) %>% tidy()
m4 <- lm(IV.y ~ IV.x, anscombe) %>% tidy()

# ...and coefficients
models <- rbind(m1, m2, m3, m4)
a <- models[c(1, 3, 5, 7), ]
b <- models[c(2, 4, 6, 8), ]


## summarise data & cleanup ----
fin <- n %>%
  group_by(qrt) %>%
  summarise(
    mean.x = mean(x),
    var.x  = var(x),
    mean.y = mean(y),
    var.y  = var(y),
    cor    = cor(x, y)
  ) %>%
  mutate(
    term = paste0("y = ", format(round(b$estimate, 3), nsmall = 3),
                  "x + ", format(round(a$estimate, 3), nsmall = 3)),
    r.sq = format(round(cor^2, 3), nsmall = 3)
  )

rm(m1, m2, m3, m4, models, a, b)


## plotting, at last ----
ggplot(n, aes(x, y)) +
  geom_smooth(method = lm, se = F) +
  geom_point(color = "orange", size = 2) +
  geom_label(data = fin, y = 12, x = 10, label = fin$term) +
  geom_label(data = fin, y = 4, x = 12, label = paste("R^2 == ", fin$r.sq), parse = T) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = "Anscombe's Quartet", x = NULL, y = NULL) +
  facet_wrap(~qrt, ncol = 2, scales = "free_x") +
  theme(panel.grid.minor = element_line(color = "white")) +
  theme_bw()
