w6_1 <- sample(c(1:6), 2000, replace = T)
w6_2 <- sample(c(1:6), 2000, replace = T) + sample(c(1:6), 2000, replace = T)

zwei <- "⚀⚀"
drei <- "⚀⚁\n⚁⚀"
vier <- "⚁⚁\n⚀⚂\n⚂⚀"
funf <- "⚀⚃\n⚁⚂\n⚂⚁\n⚃⚀"
sech <- "⚀⚄\n⚁⚃\n⚂⚂\n⚃⚁\n⚄⚀"
sieb <- "⚀⚅\n⚁⚄\n⚂⚃\n⚃⚂\n⚄⚁\n⚅⚀"
acht <- "⚁⚅\n⚂⚄\n⚃⚃\n⚄⚂\n⚅⚁"
neun <- "⚂⚅\n⚃⚄\n⚄⚃\n⚅⚂"
zehn <- "⚃⚅\n⚄⚄\n⚅⚃"
elf  <- "⚄⚅\n⚅⚄"
zwol <- "⚅⚅"

dice <- c(zwei, drei, vier, funf, sech, sieb, acht, neun, zehn, elf, zwol)
rm(zwei, drei, vier, funf, sech, sieb, acht, neun, zehn, elf, zwol)


ggplot(NULL, aes(x = w6_2)) +
  geom_histogram(bins = 11, color = "white", fill = "#1aadff", alpha = .5) +
  scale_x_continuous(breaks = 2:12, labels = dice) +
  labs(title = "Häufigkeitsverteilung von Ergebnissen mit 2 Würfeln",
       subtitle = "bei 2.000 Würfen", x = "Kombination (reihenweise)", y = "Anzahl")
