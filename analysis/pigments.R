library('readr')   # for importing data
library('dplyr')   # for data wrangling funtions; e.g. mutate(), select(), %>%
library('tidyr')   # for data wrangling funcrions; e.g. expand_grid(), pivot_*
library('ggplot2') # for plotting
library('mgcv')    # for modeling
theme_set(theme_bw())

pigments <- c('Fuco', 'Diato', 'Allo', 'Pheo_B', 'Lut_Zea', 'Echine', 'Cantha',
              'Aphan', 'Pheo_A', 'b.car')

d <- read_csv('data/full-dataset.csv',
              col_types = cols(.default = 'd', lake = 'f'))
d.long <- pivot_longer(d, cols = all_of(pigments),
                       names_to = 'pigment', values_to = 'value') %>%
  mutate(parameter = factor(pigment, levels = sort(unique(pigment))))

# plotting ####
ggplot(d.long, aes(year, value)) +
  facet_grid(pigment ~ lake, scales = 'free_y') +
  geom_point(na.rm = TRUE, alpha = 0.5)

# modelling ####
fit.model <- function(pigm, K = 10, basis = 'ad') {
  if(filter(d.long, pigment == pigm)$lake %>% unique() %>% length() != 2)
    warning('Missing a lake')
  m <- gam(value ~ lake + s(year, k = K, by = lake, bs = basis),
           family = tw(link = 'log'),
           data = filter(d.long, pigment == pigm),
           method = 'REML',
           weights = weight)
  print(gratia::appraise(m, method = 'simulate'))
  m
}
pred.fun <- function(pigm) {
  m <- get(paste0('m.', tolower(pigm)))

  expand_grid(lake = levels(d$lake),
              year = seq(1800, 2016, length.out = 400)) %>%
    mutate(predict(m, tibble(lake = lake, year = year), se.fit = TRUE) %>%
             bind_cols(),
           mu = exp(fit),
           lwr = exp(fit - 1.96 * se.fit),
           upr = exp(fit + 1.96 * se.fit),
           pigment = factor(pigm, levels = pigments))
}

m.allo <- fit.model('Allo', K = 20)
m.aphan <- fit.model('Aphan', K = 10)
m.b.car <- fit.model('b.car', K = 10)
m.cantha <- fit.model('Cantha', K = 10)
# m.chl_a <- fit.model('Chl_a', K = 10)
# m.chl_ap <- fit.model('Chl_ap', K = 10)
# m.chl_b <- fit.model('Chl_b', K = 10)
# m.diadino <- fit.model('Diadino', K = 10)
m.diato <- fit.model('Diato', K = 10)
m.echine <- fit.model('Echine', K = 10)
m.fuco <- fit.model('Fuco', K = 10, basis = 'cr')
m.lut_zea <- fit.model('Lut_Zea', K = 10) # merge lutein?
# m.lutein <- fit.model('Lutein', K = 10)
# m.myxo <- fit.model('Myxo', K = 10)
m.pheo_b <- fit.model('Pheo_B', K = 10)
m.pheo_a <- fit.model('Pheo_A', K = 10)
# m.sed_A <- fit.model('Sed_A', K = 10)
# m.sed_B <- fit.model('Sed_B', K = 10)
# m.sed_C <- fit.model('Sed_C', K = 10)
# m.sudan <- fit.model('Sudan', K = 10)

pred <- purrr::map_dfr(pigments[-5], pred.fun) %>%
  
  # only temporary # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  mutate(upr = case_when(pigment == 'Aphan' & lake == 'Pelican' &
                           upr > 500 ~ NA_real_,
                         TRUE ~ upr))

filter(pred, pigment == 'Aphan') %>%
  ggplot() +
  facet_grid(lake ~ ., scales = 'free_y', switch = 'y') +
  geom_ribbon(aes(year, ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_line(aes(year, mu)) +
  coord_cartesian(ylim = c(0, 500))

ggplot() +
  facet_grid(pigment ~ lake, scales = 'free_y', switch = 'y') +
  geom_point(aes(year, value, size = weight), d.long, alpha = 0.5, na.rm=TRUE) +
  geom_ribbon(aes(year, ymin = lwr, ymax = upr), pred, alpha = 0.25) +
  geom_line(aes(year, mu), pred) +
  scale_size('Weight', range = range(d.long$weight), breaks = c(0.5, 1:3)) +
  labs(x = 'Year C.E.', y = NULL) +
  theme(legend.position = 'top')

ggsave('figures/pigments.png', width = 6.86, height = 10, dpi = 300, scale = 1)
