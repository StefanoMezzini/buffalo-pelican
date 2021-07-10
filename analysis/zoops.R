library('readr')   # for importing data
library('dplyr')   # for data wrangling funtions; e.g. mutate(), select(), %>%
library('tidyr')   # for data wrangling funcrions; e.g. expand_grid(), pivot_*
library('ggplot2') # for plotting
library('mgcv')    # for modeling
theme_set(theme_bw())

d <- read_csv('data/full-dataset.csv',
              col_types = cols(.default = 'd', lake = 'f')) %>%
  filter(!is.na(zoop.ratio))

# plotting ####
ggplot(d, aes(year, zoop.ratio)) +
  facet_grid(lake ~ ., scales = 'free_y') +
  geom_point(na.rm = TRUE)

# modelling ####
m.zoops <- gam(zoop.ratio ~ lake + s(year, k = 20, by = lake, bs = 'ad'),
               family = betar(link = 'logit'),
               data = d,
               method = 'REML',
               weights = weight)

# inverse logit function
logit.inv <- function(x) {
  exp(x) / (1 + exp(x))
}

pred <-
  expand_grid(lake = levels(d$lake), year = seq(1800, 2020)) %>%
  filter((lake == 'Buffalo Pound' & year < 2010) | lake == 'Pelican') %>%
  mutate(predict(m.zoops, tibble(lake = lake, year = year), se.fit = TRUE) %>%
           bind_cols(),
         mu = logit.inv(fit),
         lwr = logit.inv(fit - 1.96 * se.fit),
         upr = logit.inv(fit + 1.96 * se.fit))

ggplot() +
  facet_grid(lake ~ .) +
  geom_point(aes(year, zoop.ratio, size = weight), d, alpha = 0.5, na.rm=TRUE) +
  geom_ribbon(aes(year, ymin = lwr, ymax = upr), pred, alpha = 0.25) +
  geom_line(aes(year, mu), pred) +
  coord_cartesian(ylim = c(0, 0.3)) +
  scale_size('Weight', range = range(d$weight), breaks = c(0.5, 1:3)) +
  labs(x = 'Year C.E.', y = expression(Fraction~of~italic(Daphnia~spp.))) +
  theme(legend.position = 'top')

ggsave('figures/zoop-ratio.png', width = 3.23, height = 2, dpi = 300, scale = 2)
