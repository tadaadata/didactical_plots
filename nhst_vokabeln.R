library(dplyr)
library(ggplot2)

nr <- tibble(
  x = seq(-3, 3, .01),
  y = dnorm(x)
)

mymarks <- tibble(
  y    = c(0, .075, 0, .025),
  yend = c(dnorm(-.5), .3, dnorm(-1.64), .15),
  x    = c(-.5, -1, -1.64, -2),
  xend = rep(.5, 4),
  Begriff = c("Testwert", "Signifikanzniveau", "kritischer Wert", "Testniveau")
)

cols <- c("#66c2ff", "#66c2ff", "#007acc", "#007acc")

ggplot(nr, aes(x, y)) +
  geom_line() +
  geom_ribbon(data = subset(nr, x <= -1.64),
              aes(ymax = y, ymin = 0), fill = "#007acc", alpha = .5) +
  geom_ribbon(data = subset(nr, x <= -.5),
              aes(ymax = y, ymin = 0), fill = "#007acc", alpha = .3) +
  geom_segment(data = mymarks, aes(xend = xend, y = yend, yend = yend), linetype = "dashed",
               color = cols) +
  geom_segment(data = mymarks, aes(xend = x, yend = yend), linetype = "dashed",
               color = cols) +
  geom_point(data = mymarks, aes(x = x, y = y), shape = c(4, 1, 4, 1), size = 3,
             color = cols) +
  geom_label(data = mymarks, aes(label = Begriff, x = .55, y = yend), color = "white",
             hjust = "outward", fill = cols, size = 6) +
  scale_y_continuous(labels = NULL) +
  labs(title = "Vokabular des Nullhypothesen-Signifikanztests (NHST)",
       subtitle = "am Beispiel der Standardnormalverteilung",
       x = "z-Wert", y = "Dichte") +
  hrbrthemes::theme_ipsum_tw(grid = FALSE)
