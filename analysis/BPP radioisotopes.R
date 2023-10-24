### Plotting radioisotopes from gamma dating Buffalo Pound and Pelican lakes
library('dplyr')  # for data wrangling
library('tidyr')  # for data tidying
library('ggplot2') # plots

### Buffalo Pound radiosotopes 
dates <- read.csv(file.choose(), header=TRUE)

date.tidy <-
  select(dates, Depth, pb, ra, cs) %>%
  pivot_longer(-'Depth', values_to = 'est') %>%
  left_join(dates %>%
              select(Depth, se.pb, se.ra, se.cs) %>%
              pivot_longer(-'Depth', values_to = 'se') %>%
              mutate(name = substr(name, nchar('se.') + 1, nchar(name))),
            by = c('Depth', 'name')) %>%
  mutate(lwr = (est - se),
         upr = (est + se),
         est = est)

### plotting
p.bprad <-
  ggplot(date.tidy)+
  geom_errorbarh(aes(xmin = lwr, xmax = upr, y = Depth,
                     color = factor(name, levels = c('pb','ra', 'cs'))),
                 height=0)+
  geom_point(aes(est, Depth,
                 shape = factor(name, levels = c('pb','ra', 'cs')),
                 color = factor(name, levels = c('pb', 'ra', 'cs')))) +
  ylab('Depth (cm)') +
  xlab(expression(paste(Activity~(Bq~kg^{-1}~dry~mass))))+
  scale_y_reverse()+
  scale_shape_manual(NULL, values = c(19, 1, 17)) +
  scale_color_brewer(NULL, type = 'qual', palette = 6) +
  ggtitle("Buffalo Pound")+
  theme(legend.position = c(0.7, 0.3),
        legend.text = element_blank(),
        legend.key.height = unit(0.3, 'in'),
        legend.spacing.x = unit(-0.5, 'in'))

p.bprad
## final touches in Inkscape again

######################################
## Pelican Lake radioisotopes
dates <- read.csv(file.choose(), header=TRUE)

date.tidy <-
  select(dates, Depth, pb, ra, cs) %>%
  pivot_longer(-'Depth', values_to = 'est') %>%
  left_join(dates %>%
              select(Depth, se.pb, se.ra, se.cs) %>%
              pivot_longer(-'Depth', values_to = 'se') %>%
              mutate(name = substr(name, nchar('se.') + 1, nchar(name))),
            by = c('Depth', 'name')) %>%
  mutate(lwr = (est - se),
         upr = (est + se),
         est = est)

### plotting
p.pelrad <-
  ggplot(date.tidy)+
  geom_errorbarh(aes(xmin = lwr, xmax = upr, y = Depth,
                     color = factor(name, levels = c('pb','ra', 'cs'))),
                 height=0)+
  geom_point(aes(est, Depth,
                 shape = factor(name, levels = c('pb','ra', 'cs')),
                 color = factor(name, levels = c('pb', 'ra', 'cs')))) +
  ylab('Depth (cm)') +
  xlab(expression(paste(Activity~(Bq~kg^{-1}~dry~mass))))+
  xlim(0,200)+
  scale_y_reverse()+
  scale_shape_manual(NULL, values = c(19, 1, 17)) +
  scale_color_brewer(NULL, type = 'qual', palette = 6) +
  ggtitle("Pelican")+
  theme(legend.position = c(0.75, 0.15),
        legend.text = element_blank(),
        legend.key.height = unit(0.3, 'in'),
        legend.spacing.x = unit(-0.5, 'in'))
p.pelrad

library(cowplot)
plot_grid(p.bprad, p.pelrad, ncol=1, nrow=2)
