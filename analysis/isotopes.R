library('readr')   # for importing data
library('dplyr')   # for data wrangling funtions; e.g. mutate(), select(), %>%
library('tidyr')   # for data wrangling funcrions; e.g. expand_grid(), pivot_*
library('mgcv')    # for modeling
source('analysis/figure-theme.R') # ggplot theme and useful constants

PARAMS <- c('d15NAIR', 'd13CVPDB', 'pn', 'pc', 'c.n.ratio')
PARAM.LABS <- c('delta^15~N', 'delta^13~C', '\'\045\'~N', '\'\045\'~C', 'C:N')

d <- read_csv('data/full-dataset.csv',
              col_types = cols(.default = 'd', lake = 'f'))
d.long <-
  select(d, lake, mid.depth, year, weight, all_of(PARAMS)) %>%
  pivot_longer(all_of(PARAMS), names_to = 'parameter', values_to = 'value') %>%
  mutate(parameter = factor(parameter, levels = PARAMS),
         param.lab = case_when(parameter == 'd15NAIR' ~ 'delta^15~N',
                               parameter == 'd13CVPDB' ~ 'delta^13~C',
                               parameter == 'pn' ~ '\'\045\'~N',
                               parameter == 'pc' ~ '\'\045\'~C',
                               parameter == 'c.n.ratio' ~ 'C:N'))

# plotting ####
ggplot(d.long, aes(year, value)) +
  facet_grid(param.lab ~ lake, scales = 'free_y', labeller = label_parsed,
             switch = 'y') +
  geom_point(na.rm = TRUE) +
  labs(x = 'Year C.E.', y = NULL) +
  theme(strip.placement = 'outside')

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
m.pn <- gam(pn ~ lake + s(year, k = 10, by = lake, bs = 'ad'),
            family = betar(link = 'logit'),
            data = d,
            method = 'REML',
            weights = weight)
m.pc <- gam(pc ~ lake + s(year, k = 10, by = lake, bs = 'ad'),
            family = betar(link = 'logit'),
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
                 param == 'pn' ~ list(m.pn),
                 param == 'pc' ~ list(m.pc),
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
           param.lab = factor(PARAM.LABS[param == PARAMS], levels = PARAM.LABS))
}

pred <- purrr::map_dfr(PARAMS, pred.fun)

ggplot() +
  facet_grid(param.lab ~ lake, scales = 'free_y', switch = 'y',
             labeller = label_parsed) +
  geom_point(aes(year, value, alpha = weight), d.long, na.rm=TRUE) +
  geom_ribbon(aes(year, ymin = lwr, ymax = upr), pred, alpha = 0.25) +
  geom_line(aes(year, mu), pred) +
  scale_alpha_continuous('Weight', range = c(0.2, 1), breaks = c(0.5, 1:3)) +
  labs(x = 'Year C.E.', y = NULL) +
  theme(legend.position = 'top', strip.placement = 'outside')

ggsave('figures/isotopes.png', width = W1, height = 6, dpi = 300, scale = 2)
