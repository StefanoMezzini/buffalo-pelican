library('ggplot2') # for plotting

theme_set(theme_bw() +
            theme(panel.grid = element_blank(),
                  strip.background = element_blank()))
BREAKS <- seq(1800, 2020, by = 10)
BREAK.LABELS <- if_else(BREAKS %% 50 == 0, as.character(BREAKS), '')

W1 <- 3.23
W2 <- 6.86
