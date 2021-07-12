library('readr')   # for importing data
library('dplyr')   # for data wrangling funtions; e.g. mutate(), select(), %>%
library('tidyr')   # for data wrangling funcrions; e.g. expand_grid(), pivot_*
library('mgcv')    # for modeling
library('cowplot') # for ggplots in grids
source('analysis/figure-theme.R') # ggplot theme and useful constants 


PIGMENTS <- c('Fuco', 'Diato', 'Allo', 'Pheo_B', 'Lut_Zea', 'Echine', 'Cantha',
              'Aphan', 'b.car', 'Pheo_A')
PIGM.LABS <- c('Fuco', 'Diato', 'Allo', 'Pheo~B', 'Lut-Zea', 'Echine', 'Cantha',
               'Aphan', 'beta-car', 'Pheo~A')

d <- read_csv('data/full-dataset.csv',
              col_types = cols(.default = 'd', lake = 'f'))
d.long <- pivot_longer(d, cols = all_of(PIGMENTS),
                       names_to = 'pigment', values_to = 'value') %>%
  mutate(pigment = factor(pigment, levels = sort(unique(pigment))),
         pigm.lab = purrr::map_chr(pigment,
                                   function(x) PIGM.LABS[PIGMENTS == x]) %>%
           factor(levels = PIGM.LABS))

# plotting ####
ggplot(d.long, aes(year, value)) +
  facet_grid(pigm.lab ~ lake, scales = 'free_y', labeller = label_parsed) +
  geom_point(na.rm = TRUE, alpha = 0.5)

# modelling ####
fit.model <- function(pigm, K = 10, basis = 'ad', diagnostics = FALSE) {
  if(filter(d.long, pigment == pigm)$lake %>% unique() %>% length() != 2)
    warning('Missing a lake')
  m <- gam(value ~ lake + s(year, k = K, by = lake, bs = basis),
           family = tw(link = 'log'),
           data = filter(d.long, pigment == pigm),
           method = 'REML',
           weights = weight)
  if(diagnostics) print(gratia::appraise(m, method = 'simulate'))
  m
}
pred.fun <- function(pigm) {
  m <- get(paste0('m.', tolower(pigm)))

  expand_grid(lake = levels(d$lake),
              year = seq(1800, 2016, length.out = 400)) %>%
    filter(lake == 'Pelican' | year < 2010) %>%
    mutate(predict(m, tibble(lake = lake, year = year), se.fit = TRUE) %>%
             bind_cols(),
           mu = exp(fit),
           lwr = exp(fit - 1.96 * se.fit),
           upr = exp(fit + 1.96 * se.fit),
           pigment = factor(pigm, levels = PIGMENTS),
           pigm.lab = purrr::map_chr(pigment,
                                     function(x) PIGM.LABS[PIGMENTS == x]) %>%
             factor(levels = PIGM.LABS))
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
m.lut_zea <- fit.model('Lut_Zea', K = 10)
# m.myxo <- fit.model('Myxo', K = 10)
m.pheo_b <- fit.model('Pheo_B', K = 10)
m.pheo_a <- fit.model('Pheo_A', K = 10)
# m.sed_A <- fit.model('Sed_A', K = 10)
# m.sed_B <- fit.model('Sed_B', K = 10)
# m.sed_C <- fit.model('Sed_C', K = 10)
# m.sudan <- fit.model('Sudan', K = 10)

pred <- purrr::map_dfr(PIGMENTS, pred.fun) %>%
  
  # only temporary # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  mutate(upr = case_when(pigment == 'Aphan' & lake == 'Pelican' &
                           upr > 50 ~ 50,
                         TRUE ~ upr))

filter(pred, pigment == 'Aphan') %>%
  ggplot() +
  facet_grid(lake ~ ., scales = 'free_y') +
  geom_ribbon(aes(year, ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_line(aes(year, mu))

plot.preds <- function(l) {
  d.long <- filter(d.long, lake == l)
  pred <- filter(pred, lake == l)
  
  p <-
    ggplot() +
    facet_grid(pigm.lab ~ lake, scales = 'free_y', labeller = label_parsed) +
    geom_point(aes(year, value, alpha = weight), d.long, na.rm = TRUE) +
    geom_ribbon(aes(year, ymin = lwr, ymax = upr), pred, alpha = 0.25) +
    geom_line(aes(year, mu), pred) +
    scale_alpha_continuous('Weight', range = c(0.2, 1), breaks = c(0.5, 1:3)) +
    scale_x_continuous(breaks = BREAKS, labels = BREAK.LABELS) +
    labs(x = 'Year C.E.', y = NULL) +
    theme(legend.position = 'none')
  
  if(l == 'Buffalo~Pound') p + theme(strip.text.y = element_blank()) else p
}

plot_grid(get_legend(plot.preds('Pelican') + theme(legend.position = 'top')),
          plot_grid(plot.preds('Buffalo~Pound'),
                    plot.preds('Pelican'),
                    rel_widths = c(0.975, 1)),
          ncol = 1, rel_heights = c(0.05, 1))

ggsave('figures/pigments.png', width = W2, height = 10, dpi = 300, scale = 1)
