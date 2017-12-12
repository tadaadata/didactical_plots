library(ggplot2)

# label_norm shamelessly ripped off from Lukas Burk (jemus42)
label_norm <- function(x) {
  if (length(x) > 1) {
    return(sapply(x, label_norm))
  }
  if (x != 0) {
    if (abs(x) == 1) {
      return(paste0(sub("1", "", sign(x)), "σ"))
    } else {
      return(paste0(x, "σ"))
    }
  } else {
    return("µ")
  }
}


ggplot(data = NULL, aes(x = -3:3)) +
  stat_function(fun = dnorm) +
  geom_area(aes(x = seq(-3, -1.96, .01), y = dnorm(seq(-3, -1.96, .01))), fill = "red", alpha = .3) +
  geom_area(aes(x = seq(1.96, 3, .01), y = dnorm(seq(1.96, 3, .01))), fill = "red", alpha = .3) +
  geom_area(aes(x = seq(-1.96, -0.95, .01), y = dnorm(seq(-1.96, -0.95, .01))), fill = "yellow", alpha = .3) +
  geom_area(aes(x = seq(0.95, 1.96, .01), y = dnorm(seq(0.95, 1.96, .01))), fill = "yellow", alpha = .3) +
  geom_area(aes(x = seq(-0.95, 0.95, .01), y = dnorm(seq(-0.95, 0.95, .01))), fill = "green", alpha = .3) +
  scale_y_continuous(labels = NULL) +
  scale_x_continuous(
    breaks = c(-1.96, -0.95, 0, 0.95, 1.96),
    labels = c("sehr niedrig", "niedrig", "durchschnitt", "hoch", "sehr hoch"),
    sec.axis = sec_axis(~., breaks = seq(-3, 3, 1), labels = label_norm) # , labels = label_norm)
  ) +
  labs(
    title = "Die Normalverteilung", subtitle = "am Beispiel Persönlichkeit",
    x = "Extraversion", y = "rel. Häufigkeit"
  )
