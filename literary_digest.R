library(ggplot2)

litdig <- readRDS("./data/literary_digest.rds")

ggplot() +
  geom_polygon(data = litdig, aes(x = long, y = lat, group = group, fill = winner_ld,
                                 alpha = ifelse(AML_ld > FDR_ld, AML_p_ld, FDR_p_ld))) +
  geom_path(data = litdig, aes(x = long, y = lat, group = group),
            color = "black", size = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  scale_alpha_continuous(range = c(0.3, 0.9), labels = scales::percent_format()) +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  coord_map() +
  labs(title = "Das Literary Digest Umfrage-Desaster von 1936",
       subtitle = "Die Schätzung von Literary Digest",
       x = NULL, y = NULL, alpha = "Stimmanteil",
       fill = "Sieger", caption = "n = ca. 2.3 Mio") +
  hrbrthemes::theme_ipsum(axis = FALSE, ticks = FALSE)

ggplot() +
  geom_polygon(data = litdig, aes(x = long, y = lat, group = group, fill = winner_rl,
                                 alpha = ifelse(AML_rl > FDR_rl, AML_p_rl, FDR_p_rl))) +
  geom_path(data = litdig, aes(x = long, y = lat, group = group),
            color = "black", size = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  scale_alpha_continuous(range = c(0.3, 0.9), labels = scales::percent_format()) +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  coord_map() +
  labs(title = "Das Literary Digest Umfrage-Desaster von 1936",
       subtitle = "Der tatsächliche Wahlausgang",
       x = NULL, y = NULL, alpha = "Stimmanteil",
       fill = "Sieger", caption = "n = ca. 44.4 Mio") +
  hrbrthemes::theme_ipsum(axis = FALSE, ticks = FALSE)
