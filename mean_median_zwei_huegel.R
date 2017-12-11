zwei <- c(rnorm(100, 30, 5), rnorm(100, 70, 15))

ggplot() +
  geom_density(aes(x = zwei), fill = "light gray", alpha = .2) +
  geom_vline(xintercept = median(zwei), color = "red", size = 1) +
  geom_vline(xintercept = mean(zwei), color = "green", size = 1) +
  geom_text(aes(x = 37, y = .005), label = "Median", color = "dark red", angle = 90, size = 8) +
  geom_text(aes(x = 53.5, y = .015), label = "Mittelwert", color = "dark green", angle = -90, size = 8) +
  scale_y_continuous(breaks = scales::pretty_breaks(), labels = NULL) +
  labs(x = "Merkmalsausprägung", y = "rel. Häufigkeit")
