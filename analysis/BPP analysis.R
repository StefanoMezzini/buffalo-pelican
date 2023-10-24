#### Main analyses for Buffalo Pound and Pelican lakes

## packages
library('readxl')  # for importing Excel files
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('scam')    # for shape-constrained (i.e. monotonic) additive models
library('readr')   # for importing files
library('mgcv')    # for modeling
library('ggplot2')

### Figure theme
setwd("c:/Users/caleg/Dropbox/Research/University of Regina PostDoc/Buffalo Pound and Pelican")
source('figure-theme.R')

### 1. Plotting water-level changes derived from satelitte images
wat_lev <- read.csv(file.choose(), header = TRUE) ## BPPwaterlevel2.csv

Lake_area_plot <-
  ggplot(wat_lev, aes(x=Year, y=LSA, group=Lake, color=Lake))+
  scale_color_manual(values=c("royalblue", "darkred"))+
  geom_line(linewidth=0.72)+
  scale_y_continuous(breaks = seq(from=0,to=31, by=5), limits=c(0,32))+
  ylab(bquote(Lake~surface~area~(km^2)))+
  xlab("Year C.E.")+
  theme_bw()+
  theme(legend.position = c(0.8, 0.2))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
Lake_area_plot

ggsave("BPP-lake-area2.pdf", Lake_area_plot, dpi=300, scale=1)



### 2. Sediment geochemistry, isotope, and pigment models and plotting
PARAMS <- c('d15NAIR', 'd13CVPDB', 'nBeta', 'cBeta', 'c.n.ratio', 'tp')
PARAM.LABS <- c('δ15N ‰', 'δ13C ‰', 'N content (0-1)', 'C content (0-1)', 'C:N Ratio', 'TP (mg P/g)')

d <- read_csv(file.choose(),
              col_types = cols(.default = 'd', lake = 'f')) ## BPPfuldata.csv
d.long <-
  select(d, lake, mid.depth, year, weight, all_of(PARAMS)) %>%
  filter(year >= 1940) %>% ## based on SCAM, see BPP-dating.R
  pivot_longer(all_of(PARAMS), names_to = 'parameter', values_to = 'value') %>%
  mutate(parameter = factor(parameter, levels = PARAMS),
         param.lab = case_when(parameter == 'd15NAIR' ~ 'δ15N ‰',
                               parameter == 'd13CVPDB' ~ 'δ13C ‰',
                               parameter == 'nBeta' ~ 'N content (0-1)',
                               parameter == 'cBeta' ~ 'C content (0-1)',
                               parameter == 'c.n.ratio' ~ 'C:N Ratio',
                               parameter == 'tp' ~ 'TP (mg P/g)') %>%
           factor(levels = c('C content (0-1)', 'δ13C ‰',
                    'N content (0-1)', 'δ15N ‰', 
                    'C:N Ratio', 'TP (mg P/g)')))
  

# plotting ####
ggplot(d.long, aes(year, value)) +
  facet_grid(param.lab ~ lake, scales = 'free_y', 
             switch = 'y') +
  geom_rect(data=subset(d.long, lake=="Pelican"), aes(xmin=1987.5, xmax=1990.5, ymin=-Inf, ymax=Inf), fill="grey", alpha = 0.2)+
  geom_rect(data=subset(d.long, lake=="Pelican"), aes(xmin=1972.5, xmax=1973.5, ymin=-Inf, ymax=Inf), fill="grey", alpha = 0.2)+
  geom_rect(data=subset(d.long, lake=="Pelican"), aes(xmin=1980.5, xmax=1981.5, ymin=-Inf, ymax=Inf), fill="grey", alpha = 0.2)+
  geom_point(na.rm = TRUE) +
  labs(x = 'Year C.E.', y = NULL) +
  theme(strip.placement = 'outside')


# modeling ####
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
m.pn <- gam(nBeta ~ lake + s(year, k = 10, by = lake, bs = 'ad'),
            family = betar(link="log"),
            data = d,
            method = 'REML',
            weights = weight)
m.pc <- gam(cBeta ~ lake + s(year, k = 10, by = lake, bs = 'ad'),
            family = betar(link="log"),
            data = d,
            method = 'REML',
            weights = weight)
m.cn <- gam(c.n.ratio ~ lake + s(year, k = 10, by = lake, bs = 'ad'),
            family = Gamma(link = 'log'),
            data = d,
            method = 'REML',
            weights = weight)
m.tp <- gam(tp ~ lake + s(year, k = 10, by = lake, bs = 'ad'),
            family = Gamma(link = 'log'),
            data = d,
            method = 'REML',
            weights = weight)

pred.fun <- function(param) {
  m <- case_when(param == 'd15NAIR' ~ list(m.dn),
                 param == 'd13CVPDB' ~ list(m.dc),
                 param == 'nBeta' ~ list(m.pn),
                 param == 'cBeta' ~ list(m.pc),
                 param == 'c.n.ratio' ~ list(m.cn),
                 param == 'tp' ~ list(m.tp))[[1]]
  inv.link <- if_else(param %in% c('d15NAIR', 'd13CVPDB'),
                      true = 'identity', false = 'exp') %>%
    get()
  
  expand_grid(lake = levels(d$lake), year = seq(1940, 2014)) %>% ### 2014 top of core
    mutate(predict(m, tibble(lake = lake, year = year), se.fit = TRUE) %>%
             bind_cols(),
           mu = inv.link(fit),
           lwr = inv.link(fit - 1.96 * se.fit),
           upr = inv.link(fit + 1.96 * se.fit),
           param.lab = factor(PARAM.LABS[param == PARAMS], levels = PARAM.LABS))
}

pred <- purrr::map_dfr(PARAMS, pred.fun)

## figure plot
ggplot() +
  facet_grid(param.lab ~ lake, scales = 'free_y', switch = 'y') +
  geom_rect(data=subset(d.long, lake=="Pelican"), aes(xmin=1987.5, xmax=1990.5, ymin=-Inf, ymax=Inf), fill="tomato", alpha = 0.2)+
  geom_rect(data=subset(d.long, lake=="Pelican"), aes(xmin=1972.5, xmax=1973.5, ymin=-Inf, ymax=Inf), fill="tomato", alpha = 0.2)+
  geom_rect(data=subset(d.long, lake=="Pelican"), aes(xmin=1980.5, xmax=1981.5, ymin=-Inf, ymax=Inf), fill="tomato", alpha = 0.2)+
  geom_point(aes(year, value), d.long, na.rm=TRUE) +
  xlim(1940,2020)+
  geom_ribbon(aes(year, ymin = lwr, ymax = upr), pred, alpha = 0.25)+ 
  geom_line(aes(year, mu), pred) +
  labs(x = 'Year C.E.', y = NULL) +
  theme(legend.position = 'top', strip.placement = 'outside', axis.title=element_text(size=14))+
  theme(strip.background = element_blank())

ggsave('geochem.pdf', dpi = 300, scale = 1)


### Pigments analyses

PIGMENTS <- c('Fuco', 'Diato', 'Allo', 'Pheo_B', 'Lut_Zea', 'Echine', 'Cantha',
              'Aphan', 'Pheo_A', 'b.car')
PIGM.LABS <- c('Fucoxanthin', 'Diatoxanthin', 'Alloxanthin', 'Pheophytin b', 'Lutein-zeaxanthin', 'Echinenone', 'Canthaxanthin',
               'Aphanizophyll', 'Pheophytin a', 'B-carotene')

d <- read_csv('data/full-dataset.csv',
              col_types = cols(.default = 'd', lake = 'f'))
d.long <-
  select(d, lake, mid.depth, year, weight, all_of(PIGMENTS)) %>%
  filter(year >= 1940) %>% 
  pivot_longer(all_of(PIGMENTS),
                       names_to = 'pigment', values_to = 'value') %>%
  mutate(pigment = factor(pigment, levels = sort(unique(pigment))),
         pigm.lab = purrr::map_chr(pigment,
                                   function(x) PIGM.LABS[PIGMENTS == x]) %>%
           factor(levels = PIGM.LABS))


# plotting ####
ggplot(d.long, aes(year, value)) +
  facet_grid(pigm.lab ~ lake, scales = 'free_y') +
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
              year = seq(1940, 2014, length.out = 400)) %>%
    filter(lake == 'Pelican' | year < 2014) %>%
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
m.diato <- fit.model('Diato', K = 10)
m.echine <- fit.model('Echine', K = 10)
m.fuco <- fit.model('Fuco', K = 10, basis = 'cr')
m.lut_zea <- fit.model('Lut_Zea', K = 10)
m.pheo_b <- fit.model('Pheo_B', K = 10)
m.pheo_a <- fit.model('Pheo_A', K = 10)

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
    facet_grid(pigm.lab ~ lake, scales='free_y') +
    xlim(1940, 2014)+
    geom_rect(aes(xmin=1987.5, xmax=1990.5, ymin=-Inf, ymax=Inf), d.long, fill="tomato", alpha = 0.02)+
    geom_rect(aes(xmin=1972.5, xmax=1973.5, ymin=-Inf, ymax=Inf), d.long, fill="tomato", alpha = 0.02)+
    geom_rect(aes(xmin=1980.5, xmax=1981.5, ymin=-Inf, ymax=Inf), d.long, fill="tomato", alpha = 0.02)+
    geom_point(aes(year, value), d.long, na.rm = TRUE) +
    geom_ribbon(aes(year, ymin = lwr, ymax = upr), pred, alpha=.3) +
    geom_line(aes(year, mu), pred) +
    xlab('Year C.E.')+
    ylab(bquote(Concentration~(nmol~g^-1))+
    theme(legend.position = 'none'))
  
  
  if(l == 'Buffalo~Pound') p + theme(strip.text.y = element_blank()) else p
}

p

library(cowplot)
plot.preds('Buffalo Pound')
plot.preds('Pelican')


plot_grid(plot_grid(plot.preds('Buffalo Pound'),
                    plot.preds('Pelican'),
                    rel_widths = c(0.975, 1)),
          ncol = 1, rel_heights = c(0.05, 1))

### this creates the base figure. Final labels added in Inkscape

