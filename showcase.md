Shwocase
================
Tadaa!
1/16/2018

*η*<sup>2</sup>: Group & grand mean
-----------------------------------

``` r
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
```

![](showcase_files/figure-markdown_github/eta_groupmeans-1.png)

Yerkes-Dotson Gesetz
--------------------

...und nichtlineare Zusammenhänge im allgemeinen.

``` r
p <- .5
q <- seq_len(100)
y <- (1200 - p * (q - 50)^2) / 12
y_noise  <- y + rnorm(length(q), 10, 7.5)
y_min    <- min(y_noise)
y_max    <- max(y_noise)
rsq_lm   <- round(cor(y_noise, q)^2, 3)
rsq_poly <- round(summary(lm(y_noise ~ poly(q, 2)))$r.squared, 3)

ggplot(NULL, aes(q, y_noise)) +
  geom_jitter(height = 0, width = 0, shape = 21, size = 2) +
  geom_smooth(method = lm, color = "#e60000", se = FALSE, linetype = "dashed") +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), color = "#00cc44", se = FALSE) +
  geom_label(aes(x = 37.5, y = 65), color = "#e60000", size = 5,
             label = paste("linear:\nR² =", rsq_lm)) +
  geom_label(aes(x = 70, y = 60), color = "#009933", size = 5,
             label = paste("quadr.:\nR² =", rsq_poly)) +
  scale_y_continuous(breaks = c(seq(y_min, y_max, (y_max - y_min) / 5)),
                     labels = c("niedrig", rep("", 4), "hoch")) +
  scale_x_continuous(breaks = c(seq(0, 100, 25)),
                     labels = c("niedrig", rep("", 3),"hoch")) +
  labs(
    title = "Das Yerkes-Dodson-Gesetz",
    x = "Stressniveau / Erregung",
      y = "Leistungsfähigkeit"
  ) +
  theme_classic()
```

![](showcase_files/figure-markdown_github/yerkes_dotson-1.png)

Gesetz der Großen Zahl
----------------------

``` r
library(purrr)
library(dplyr)
library(ggplot2)
library(scales)

map_dbl(seq_len(1200), function(x) {
  wurf <- sample(c(1:6), x, replace = T)

  length(wurf[wurf == 6]) / length(wurf)
}) %>%
  data.frame(
    Wurfse = seq_along(.),
    Prob   = .
  ) %>%
  ggplot(aes(x = Wurfse, y = Prob)) +
    geom_point(size = .75, alpha = .3) +
    geom_hline(aes(yintercept = 1/6), size = .5, color = "red") +
    labs(title = "Das Gesetz der großen Zahl",
         subtitle = "Häufigkeit eines 6er-Wurfs bei 1200 Würfen",
         y = "rel. Häufigkeit", x = "Würfe") +
    annotate("label", x = 1000, y = .25, label = "erwartete Häufigkeit", alpha = .8,
             fill = "red", color = "white", size = 4, label.padding = unit(.35, "lines")) +
    scale_x_continuous(breaks = pretty_breaks())
```

![](showcase_files/figure-markdown_github/gesetz_der_grossen_zahl-1.png)

Mittelwert und Median
---------------------

Allgemeine Hügeligkeit als solche.

``` r
zwei <- c(rnorm(100, 30, 5), rnorm(100, 70, 15))

ggplot() +
  geom_density(aes(x = zwei), fill = "light gray", alpha = .2) +
  geom_vline(xintercept = median(zwei), color = "red", size = 1) +
  geom_vline(xintercept = mean(zwei), color = "green", size = 1) +
  geom_text(aes(x = 37, y = .005), label = "Median", 
            color = "dark red", angle = 90, size = 8) +
  geom_text(aes(x = 53.5, y = .015), label = "Mittelwert", 
            color = "dark green", angle = -90, size = 8) +
  scale_y_continuous(breaks = pretty_breaks(), labels = NULL) +
  labs(x = "Merkmalsausprägung", y = "rel. Häufigkeit")
```

![](showcase_files/figure-markdown_github/mean_median_huegel-1.png)

Normalverteilung: Extraversion
------------------------------

``` r
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
  geom_area(aes(x = seq(-3, -1.96, .01), 
                y = dnorm(seq(-3, -1.96, .01))), 
            fill = "red", alpha = .3) +
  geom_area(aes(x = seq(1.96, 3, .01), y = dnorm(seq(1.96, 3, .01))), 
            fill = "red", alpha = .3) +
  geom_area(aes(x = seq(-1.96, -0.95, .01), y = dnorm(seq(-1.96, -0.95, .01))), 
            fill = "yellow", alpha = .3) +
  geom_area(aes(x = seq(0.95, 1.96, .01), y = dnorm(seq(0.95, 1.96, .01))), 
            fill = "yellow", alpha = .3) +
  geom_area(aes(x = seq(-0.95, 0.95, .01), y = dnorm(seq(-0.95, 0.95, .01))), 
            fill = "green", alpha = .3) +
  scale_y_continuous(labels = NULL) +
  scale_x_continuous(
    breaks = c(-1.96, -0.95, 0, 0.95, 1.96),
    labels = c("sehr niedrig", "niedrig", "durchschnitt", "hoch", "sehr hoch"),
    sec.axis = sec_axis(~., breaks = seq(-3, 3, 1), labels = label_norm)
  ) +
  labs(
    title = "Die Normalverteilung", subtitle = "am Beispiel Persönlichkeit",
    x = "Extraversion", y = "rel. Häufigkeit"
  ) +
  theme_classic()
```

![](showcase_files/figure-markdown_github/nv_extraversion-1.png)

Regressionsgerade mit Ausreißern
--------------------------------

``` r
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
  labs(title = "Modellierbarkeit des Zusammenhangs zw. Alter und Beziehungsanzahl",
       subtitle = paste0(
         "Regressionsgerade mit Ausreißern (dunkel; r = ", dunkel,
         ") und ohne Ausreißer (hell; r = ", hell, ")"),
       x = "Alter", y = "Beziehungen")
```

![](showcase_files/figure-markdown_github/reg_ausreisser-1.png)

Zentraler Grenzwertsatz: Würfel
-------------------------------

``` r
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

rm(d1, d2, d3, d4, d5, d6)

# Expected relative frequencies:
c(1:6,5:1) / sum(c(1:6,5:1))
```

    ##  [1] 0.0277777777778 0.0555555555556 0.0833333333333 0.1111111111111
    ##  [5] 0.1388888888889 0.1666666666667 0.1388888888889 0.1111111111111
    ##  [9] 0.0833333333333 0.0555555555556 0.0277777777778

``` r
ggplot(NULL, aes(x = w6_2)) +
  geom_histogram(bins = 11, color = "white", fill = "#1aadff", alpha = .5) +
  scale_x_continuous(breaks = 2:12, labels = dice) +
  labs(title = "Häufigkeitsverteilung von Ergebnissen mit 2 Würfeln",
       subtitle = "bei 2.000 Würfen",
       x = "Kombination (reihenweise)", y = "Anzahl") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = rel(2), family = "FreeSerif", lineheight = .5))
```

![](showcase_files/figure-markdown_github/zgs_wuerfel-1.png)

CIs for days
------------

``` r
# I think I made this to replicate the results of a paper about CIs
# and not for actual lecturing, but meh, doesn't hurt to put it here

library(purrr)
library(dplyr)
library(ggplot2)

stuff <- 1:200 %>%
  map(~rnorm(2, 100, 15))

mu <- stuff %>% map_dbl(mean)
sd <- stuff %>% map_dbl(sd)

df <- data.frame(mu, sd) %>%
  transmute(
    mu = mu,
    hi = mu + 0.6744898 * sd / sqrt(2),
    lo = mu - 0.6744898 * sd / sqrt(2),
    hit = ifelse(hi < 100 | lo > 100, "no", "yes")
  ) %>%
  arrange(abs(mu - 100)) %>%
  # alternative: sort by range of CI
  # arrange(desc(hi - lo)) %>%
  mutate(
    n  = seq_len(length(mu))
  )

ggplot(df, aes(y = mu, x = n, ymin = lo, ymax = hi, color = hit)) +
  geom_hline(yintercept = 100, color = "blueviolet", linetype = "dashed") +
  # geom_point(size = .25, color = "black", alpha = .1) +
  geom_errorbar(size = .25) +
  labs(
    title = "200 randomly generated 50% Confidence Intervals",
    subtitle = "of N(100, 15) distributed samples of size 2",
    y = expression(mu), x = "Sample", color = "Contains true mean"
  ) +
  scale_colour_manual(values = c("#cc0000", "#00e600")) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "bottom")
```

![](showcase_files/figure-markdown_github/cis-1.png)
