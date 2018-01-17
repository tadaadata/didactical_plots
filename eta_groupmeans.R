# Visualizing (kind of) how eta^2 works

library(ggplot2)

data <- data.frame(groups = c(rep("A", 50), rep("B", 50), rep("C", 50)),
                   y = c(rnorm(50, 10, 1.5), rnorm(50, 13, 1), rnorm(50, 11, 3)))

ggplot(data = data, aes(x = groups, y = y)) +
  geom_jitter(alpha = .5) +
  stat_summary(fun.ymax = mean, fun.ymin = mean,
               geom = "errorbar", color = "red", size = 1.5) +
  geom_hline(aes(yintercept = mean(y)), lty = "dashed") +
  labs(title = "Visualization of grand and group means",
       subtitle = "Across 3 categories",
       x = "Categories", y = "Response Variable") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
