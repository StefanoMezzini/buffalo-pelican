library('readxl')  # for importing Excel files
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('scam')    # for shape-constrained (i.e. monotonic) additive models
library('readr')   # for importing files
library('purrr')   # figure mapping 

setwd("buffalo-pelican/data")
source('figure-theme.R')

# dating CI extends further
BREAKS <- seq(1900, 2020, by = 10)
BREAK.LABELS <- if_else(BREAKS %% 50 == 0, as.character(BREAKS), '')

read.data <- function(path) {
  l <- case_when(grepl('bp', path) ~ 'Buffalo~Pound',
                 grepl('pelican', path) ~ 'Pelican')
  
  read_xls(path, sheet = 'age calculation CRS B', range = 'A11:AC29') %>%
    slice(-(1:3)) %>% # remove first three rows
    transmute(lake = l,
              mid.depth = as.numeric(`Mid-depth`),
              pb = as.numeric(`Pb-210...6`),
              pb.se = as.numeric(`Pb-210...7`),
              cs = as.numeric(`Cs-137...12`),
              cs.se = as.numeric(`Cs-137...13`),
              year = as.numeric(DATE),
              year.se.uncorrel = as.numeric(`AGE...26`),
              year.se.correl = as.numeric(`AGE...27`)) %>%
    filter(!is.na(year)) %>%
    mutate(weight = 1 / year.se.correl,
           weight = weight / mean(weight),
           lwr.1se = year - year.se.correl,
           upr.1se = year + year.se.correl)
}

### 210Pb activities with errors and CRS age estimates
dates <-
  bind_rows(read.data('core-data-bp/Ge det Buffalo Pound Routine Core revised.xls'),
            read.data('core-data-pelican/Ge det Pelican Lake Core.xls')) %>%
  mutate(lake = factor(lake))

# modeling with SCAMs ####
m.age <- scam(year ~ lake + s(mid.depth, k = 12, bs = 'mpd', by = lake),
              data = dates, family = gaussian(), weights = weight)

new.ages <- expand_grid(mid.depth = seq(0, 30, length.out = 300),
                        lake = levels(dates$lake))
new.ages <- bind_cols(new.ages,
                      predict(m.age, newdata = new.ages, se.fit = TRUE)) %>%
  mutate(lwr = fit - se.fit,
         upr = fit + se.fit) %>%
  filter(fit > 1900)

### plotting
ggplot() +
  facet_grid(lake ~ ., labeller = label_parsed) +
  geom_errorbarh(aes(xmin = lwr.1se, xmax = upr.1se, y = mid.depth), dates,
                 color = 'blue') +
  geom_point(aes(year, y = mid.depth), dates, color = 'blue') +
  geom_vline(data=dates, aes(xintercept=1940), linetype=2) +
  geom_ribbon(aes(xmin = lwr, xmax = upr, y = mid.depth), new.ages, alpha=0.25) +
  geom_line(aes(x = fit, y = mid.depth), new.ages) +
  scale_y_reverse('Depth (cm)') +
  scale_x_continuous(breaks = BREAKS, labels = BREAK.LABELS) +
  coord_cartesian(xlim=c(1900,2020))+
  labs(x = 'Year C.E.')

ggsave('dating.pdf', dpi = 300, scale = 1)
