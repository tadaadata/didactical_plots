library(ggplot2)
library(dplyr)

qm <- readRDS(url("https://data.tadaa-data.de/qm_survey_ss2017.rds"))

dunkel <- round(cor(qm$alter, qm$beziehungen), 2)
hell   <- round(cor(filter(qm, alter <= 28)$alter,
                    filter(qm, alter <= 28)$beziehungen), 2)

qm %>%
  mutate(
    alter_z = tadaatoolbox::z(alter),
    Modellierbarkeit = ifelse(alter_z < 2, "fitted", "Ausreißer")
  ) %>%
  ggplot(aes(x = alter, y = beziehungen, alpha = Modellierbarkeit,
             shape = Modellierbarkeit, color = Modellierbarkeit)) +
  geom_smooth(data = qm, aes(x = alter, y = beziehungen),
              inherit.aes = F, method = "lm", color = "cadetblue", se = F) +
  geom_jitter(width = .2, height = .3, size = 2) +
  geom_smooth(data = filter(qm, alter <= 28), aes(x = alter, y = beziehungen),
              inherit.aes = F, method = "lm", color = "#4dccff", se = F) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_alpha_manual(values = c(.8, .3)) +
  scale_shape_manual(values = c(13, 19)) +
  scale_color_manual(values = c("dark red", "dark gray")) +
  labs(title = "Modellierbarkeit des Zusammenhangs\nzw. Alter und Beziehungsanzahl",
       subtitle = paste0(
         "Regressionsgerade mit Ausreißern\n(dunkel; r = ", dunkel,
         ") und ohne Ausreißer (hell; r = ", hell, ")"),
       x = "Alter", y = "Beziehungen") +
  theme(legend.position = "top")
