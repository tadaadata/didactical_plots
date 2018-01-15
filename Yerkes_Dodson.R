library(ggplot2)

p <- .5
q <- seq_len(100)
y <- (1200 - p * (q - 50)^2) / 12
y_noise <-  y + rnorm(length(q), 10, 7.5)
y_min <- min(y_noise)
y_max <- max(y_noise)
rsq_lm <- round(cor(y_noise, q)^2, 3)
rsq_poly <- round(summary(lm(y_noise ~ poly(q, 2)))$r.squared, 3)

ggplot(NULL, aes(q, y_noise)) +
  geom_jitter(height = 0, width = 0, shape = 21, size = 2) +
  geom_smooth(method = lm, color = "#e60000", se = FALSE, linetype = "dashed") +
  geom_smooth(method = loess, color = "#00cc44", se = FALSE) +
  geom_label(aes(x = 37.5, y = 65), color = "#e60000", size = 5,
             label = paste("lm:\nR² =", rsq_lm)) +
  geom_label(aes(x = 70, y = 60), color = "#009933", size = 5,
             label = paste("loess:\nR² =", rsq_poly)) +
  scale_y_continuous(breaks = c(seq(y_min, y_max, (y_max - y_min) / 5)),
                     labels = c("niedrig", "", "", "", "","hoch")) +
  scale_x_continuous(breaks = c(seq(0, 100, 25)),
                     labels = c("niedrig", rep("", 3),"hoch")) +
  labs(
    title = "Das Yerkes-Dodson-Gesetz",
    x = "Stressniveau / Erregung", y = "Leistungsfähigkeit"
  ) +
  theme_classic()
