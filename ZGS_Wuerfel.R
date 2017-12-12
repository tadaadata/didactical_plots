library(ggplot2)

w6_1 <- sample(c(1:6), 2000, replace = T)
w6_2 <- sample(c(1:6), 2000, replace = T) + sample(c(1:6), 2000, replace = T)

dice <- c(
  "2"  = "⚀⚀",
  "3"  = "⚀⚁\n⚁⚀",
  "4"  = "⚁⚁\n⚀⚂\n⚂⚀",
  "5"  = "⚀⚃\n⚁⚂\n⚂⚁\n⚃⚀",
  "6"  = "⚀⚄\n⚁⚃\n⚂⚂\n⚃⚁\n⚄⚀",
  "7"  = "⚀⚅\n⚁⚄\n⚂⚃\n⚃⚂\n⚄⚁\n⚅⚀",
  "8"  = "⚁⚅\n⚂⚄\n⚃⚃\n⚄⚂\n⚅⚁",
  "9"  = "⚂⚅\n⚃⚄\n⚄⚃\n⚅⚂",
  "10" = "⚃⚅\n⚄⚄\n⚅⚃",
  "11" = "⚄⚅\n⚅⚄",
  "12" = "⚅⚅"
)

ggplot(NULL, aes(x = w6_2)) +
  geom_histogram(bins = 11, color = "white", fill = "#1aadff", alpha = .5) +
  scale_x_continuous(breaks = 2:12, labels = dice) +
  labs(title = "Häufigkeitsverteilung von Ergebnissen mit 2 Würfeln",
       subtitle = "bei 2.000 Würfen", x = "Kombination (reihenweise)", y = "Anzahl")

# Note: Dice symbols don't show up in ggplot2 in my case, needs debugging for portability
