library('readr')   # for importing data
library('dplyr')   # for data wrangling funtions; e.g. mutate(), select(), %>%
library('tidyr')   # for data wrangling funcrions; e.g. expand_grid(), pivot_*
library('mgcv')    # for modeling
source('analysis/figure-theme.R') # ggplot theme and useful constants 

d <- read_csv('data/full-dataset.csv',
              col_types = cols(.default = 'd', lake = 'f')) %>%
  filter(!is.na(tp))

# plotting ####
ggplot(d, aes(year, tp)) +
  facet_grid(lake ~ ., scales = 'free_y') +
  geom_point(na.rm = TRUE)

# modelling ####
m.tp <- gam(tp ~ lake + s(year, k = 10, by = lake, bs = 'ad'),
            family = Gamma(link = 'log'),
            data = d,
            method = 'REML',
            weights = weight)

pred <-
  expand_grid(lake = levels(d$lake), year = seq(1800, 2020)) %>%
  mutate(predict(m.tp, tibble(lake = lake, year = year), se.fit = TRUE) %>%
           bind_cols(),
         mu = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit))

ggplot() +
  facet_grid(lake ~ ., labeller = label_parsed) +
  geom_point(aes(year, tp, alpha = weight), d, na.rm = TRUE) +
  geom_ribbon(aes(year, ymin = lwr, ymax = upr), pred, alpha = 0.25) +
  geom_line(aes(year, mu), pred) +
  labs(x = 'Year C.E.', y = 'Total phosphorus (mg P/g)') +
  scale_x_continuous(breaks = BREAKS, labels = BREAK.LABELS) +
  scale_alpha_continuous('Weight', range = c(0.2, 1), breaks = c(0.5, 1:3)) +
  theme(legend.position = 'top')

ggsave('figures/phosphorus.png', width = W1, height = 2, dpi = 300, scale = 2)
