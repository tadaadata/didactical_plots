# Requires FreeSerif Font or alternative font compatible with dice glyphs
# See http://ftp.gnu.org/gnu/freefont/

library(ggplot2)

w6_1 <- sample(c(1:6), 2000, replace = T)
w6_2 <- sample(c(1:6), 2000, replace = T) + sample(c(1:6), 2000, replace = T)

# see stringi::stri_escape_unicode
d1 <- "\u2680"
d2 <- "\u2681"
d3 <- "\u2682"
d4 <- "\u2683"
d5 <- "\u2684"
d6 <- "\u2685"

dice <- c(
  "2"  = paste0(d1, d1),
  "3"  = paste0(d1, d2, "\n", d2, d1),
  "4"  = paste0(d2, d2, "\n", d1, d3, "\n", d3, d1),
  "5"  = paste0(d1, d4, "\n", d2, d3, "\n", d3, d2, "\n", d4, d1),
  "6"  = paste0(d1, d5, "\n", d2, d4, "\n", d3, d3, "\n", d4, d2, "\n", d5, d1),
  "7"  = paste0(d1, d6, "\n", d2, d5, "\n", d3, d4, "\n", d4, d3, "\n", d5, d2, "\n", d6, d1),
  "8"  = paste0(d2, d6, "\n", d3, d5, "\n", d4, d4, "\n", d5, d3, "\n", d6, d2),
  "9"  = paste0(d3, d6, "\n", d4, d5, "\n", d5, d4, "\n", d6, d3),
  "10" = paste0(d4, d6, "\n", d5, d5, "\n", d6, d4),
  "11" = paste0(d5, d6, "\n", d6, d5),
  "12" = paste0(d6, d6)
)

# Expected relative frequencies:
c(1:6,5:1) / sum(c(1:6,5:1))

ggplot(NULL, aes(x = w6_2)) +
  geom_histogram(bins = 11, color = "white", fill = "#1aadff", alpha = .5) +
  scale_x_continuous(breaks = 2:12, labels = dice) +
  labs(title = "Häufigkeitsverteilung von Ergebnissen mit 2 Würfeln",
       subtitle = "bei 2.000 Würfen",
       x = "Kombination (reihenweise)", y = "Anzahl") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = rel(2), family = "FreeSerif", lineheight = .5))

# Neat but unused bonus feature
diceglyph <- function(..., paste = TRUE) {
  sym <- c(
    "\u2680",
    "\u2681",
    "\u2682",
    "\u2683",
    "\u2684",
    "\u2685"
  )
  if (paste) {
    paste0(sym[c(...)], collapse = "")
  } else {
    sym[c(...)]
  }
}

# e.g. diceglyph(2, 3, 6, 2)
