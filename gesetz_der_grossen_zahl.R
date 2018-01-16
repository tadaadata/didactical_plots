library(purrr)
library(dplyr)
library(ggplot2)
library(scales)

map(seq_len(1200), function(x) {
  wurf <- sample(c(1:6), x, replace = T)

  length(wurf[wurf == 6]) / length(wurf)
}) %>%
  unlist() %>%
  data.frame(
    Wurfse = seq_along(.),
    Prob   = .
  ) %>%
  ggplot(aes(x = Wurfse, y = Prob)) +
    geom_point(size = .75, alpha = .3) +
    geom_hline(aes(yintercept = 1/6), size = .5, color = "red") +
    labs(title = "Das Gesetz der großen Zahl",
         subtitle = "Häufigkeit eines 6er-Wurfs bei 1200 Würfen",
         y = "rel. Häufigkeit", x = "Würfe") +
    annotate("label", x = 1000, y = .25, label = "erwartete Häufigkeit", alpha = .8,
             fill = "red", color = "white", size = 4, label.padding = unit(.35, "lines")) +
    scale_x_continuous(breaks = pretty_breaks())
