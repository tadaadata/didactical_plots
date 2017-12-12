# I think I made this to replicate the results of a paper about CIs
# and not for actual lecturing, but meh, doesn't hurt to put it here

library(purrr)
library(dplyr)
library(ggplot2)

stuff <- 1:200 %>%
  map(~rnorm(2, 100, 15))

mu <- stuff %>% map_dbl(mean)
sd <- stuff %>% map_dbl(sd)

df <- data.frame(mu, sd) %>%
  transmute(
    mu = mu,
    hi = mu + 0.6744898 * sd / sqrt(2),
    lo = mu - 0.6744898 * sd / sqrt(2),
    hit = ifelse(hi < 100 | lo > 100, "no", "yes")
  ) %>%
  arrange(abs(mu - 100)) %>%
  # alternative: sort by range of CI
  # arrange(desc(hi - lo)) %>%
  mutate(
    n  = seq_len(length(mu))
  )

ggplot(df, aes(y = mu, x = n, ymin = lo, ymax = hi, color = hit)) +
  geom_hline(yintercept = 100, color = "blueviolet", linetype = "dashed") +
  # geom_point(size = .25, color = "black", alpha = .1) +
  geom_errorbar(size = .25) +
  labs(
    title = "200 randomly generated 50% Confidence Intervals",
    subtitle = "of N(100, 15) distributed samples of size 2",
    y = expression(mu), x = "Sample", color = "Contains true mean"
  ) +
  scale_colour_manual(values = c("#cc0000", "#00e600")) +
  coord_flip() +
  theme(legend.position = "bottom")

