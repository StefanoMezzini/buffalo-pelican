library('readr')   # for importing data
library('dplyr')   # for data wrangling funtions; e.g. mutate(), select(), %>%
library('tidyr')   # for data wrangling funcrions; e.g. expand_grid(), pivot_*
library('ggplot2') # for plotting
library('mgcv')    # for modeling
theme_set(theme_bw())

params <- c('d15NAIR', 'd13CVPDB', 'mgN', 'mgC', 'c.n.ratio')

d <- read_csv('data/full-dataset.csv',
              col_types = cols(.default = 'd', lake = 'f'))
d.long <- pivot_longer(d, all_of(params),
                       names_to = 'parameter', values_to = 'value') %>%
  mutate(parameter = factor(parameter, levels = params))

# plotting ####
ggplot(d, aes(year, d15NAIR)) +
  facet_grid(lake ~ ., scales = 'free_y') +
  geom_point(na.rm = TRUE)
ggplot(d, aes(year, d13CVPDB)) +
  facet_grid(lake ~ ., scales = 'free_y') +
  geom_point(na.rm = TRUE)
ggplot(d, aes(year, mgN)) +
  facet_grid(lake ~ ., scales = 'free_y') +
  geom_point(na.rm = TRUE)
ggplot(d, aes(year, mgC)) +
  facet_grid(lake ~ ., scales = 'free_y') +
  geom_point(na.rm = TRUE)

# modelling ####
m.dn <- gam(d15NAIR ~ lake + s(year, k = 10, by = lake, bs = 'ad'),
            family = gaussian(),
            data = d,
            method = 'REML',
            weights = weight)
m.dc <- gam(d13CVPDB ~ lake + s(year, k = 10, by = lake, bs = 'ad'),
            family = gaussian(),
            data = d,
            method = 'REML',
            weights = weight)
m.mgn <- gam(mgN ~ lake + s(year, k = 20, by = lake, bs = 'ad'),
             family = Gamma(link = 'log'),
             data = d,
             method = 'REML',
             weights = weight)
m.mgc <- gam(mgC ~ lake + s(year, k = 15, by = lake, bs = 'ad'),
             family = Gamma(link = 'log'),
             data = d,
             method = 'REML',
             weights = weight)
m.cn <- gam(c.n.ratio ~ lake + s(year, k = 10, by = lake, bs = 'ad'),
            family = Gamma(link = 'log'),
            data = d,
            method = 'REML',
            weights = weight)

pred.fun <- function(param) {
  m <- case_when(param == 'd15NAIR' ~ list(m.dn),
                 param == 'd13CVPDB' ~ list(m.dc),
                 param == 'mgN' ~ list(m.mgn),
                 param == 'mgC' ~ list(m.mgc),
                 param == 'c.n.ratio' ~ list(m.cn))[[1]]
  inv.link <- if_else(param %in% c('d15NAIR', 'd13CVPDB'),
                      true = 'identity', false = 'exp') %>%
    get()
  
  expand_grid(lake = levels(d$lake), year = seq(1800, 2020)) %>%
    mutate(predict(m, tibble(lake = lake, year = year), se.fit = TRUE) %>%
             bind_cols(),
           mu = inv.link(fit),
           lwr = inv.link(fit - 1.96 * se.fit),
           upr = inv.link(fit + 1.96 * se.fit),
           parameter = factor(param, levels = params))
}

pred <- purrr::map_dfr(params, pred.fun)

ggplot() +
  facet_grid(parameter ~ lake, scales = 'free_y', switch = 'y') +
  geom_point(aes(year, value, size = weight), d.long, alpha = 0.5, na.rm=TRUE) +
  geom_ribbon(aes(year, ymin = lwr, ymax = upr), pred, alpha = 0.25) +
  geom_line(aes(year, mu), pred) +
  scale_size('Weight', range = range(d.long$weight), breaks = c(0.5, 1:3)) +
  labs(x = 'Year C.E.', y = NULL) +
  theme(legend.position = 'top')

ggsave('figures/isotopes.png', width = 3.23, height = 6, dpi = 300, scale = 2)
