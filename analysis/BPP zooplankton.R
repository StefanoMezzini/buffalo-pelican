### Zooplanton analysis and plotting
### Figure theme
setwd("buffalo-pelican/data")
source('figure-theme.R')

### Packages
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('mgcv')    # for modeling
library('readr')   # for importing files

### full data
d <- read_csv(file.choose(),
              col_types = cols(.default = 'd', lake = 'f')) %>%
  filter(!is.na(zoop.ratio)) %>%
  filter(year >= 1940)

# plotting ####
ggplot(d, aes(year, zoop.ratio)) +
  facet_grid(lake ~ ., scales = 'free_y') +
  geom_point(na.rm = TRUE)


# modeling 
m.zoops <- gam(zoop.ratio ~ lake + s(year, k = 12, by = lake, bs = 'ad'),
               family = tw(link = 'log'),
               data = d,
               method = 'REML',
               weights=weight)

gam.check(m.zoops)
summary(m.zoops)

# inverse logit function
logit.inv <- function(x) {
  exp(x) / (1 + exp(x))
}

pred.fun <- function(pigm) {
  m <- get(paste0('m.', tolower(pigm)))
  
  expand_grid(lake = levels(d$lake),
              year = seq(1940, 2014, length.out = 400)) %>%
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

pred <-
  expand_grid(lake = levels(d$lake), year = seq(1940, 2014)) %>%
  filter((lake == 'Buffalo Pound' & year < 2010) | lake == 'Pelican') %>%
  mutate(predict(m.zoops, tibble(lake = lake, year = year), se.fit = TRUE) %>%
           bind_cols(),
         mu = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit))

### plotting
zoop.p <- ggplot() +
  facet_grid(lake ~ ., scales="free_y") +
  geom_rect(data=subset(d, lake=="Pelican"), aes(xmin=1987.5, xmax=1990.5, ymin=-Inf, ymax=Inf), fill="tomato", alpha = 0.2)+
  geom_rect(data=subset(d, lake=="Pelican"), aes(xmin=1972.5, xmax=1973.5, ymin=-Inf, ymax=Inf), fill="tomato", alpha = 0.2)+
  geom_rect(data=subset(d, lake=="Pelican"), aes(xmin=1980.5, xmax=1981.5, ymin=-Inf, ymax=Inf), fill="tomato", alpha = 0.2)+
  geom_point(aes(year, zoop.ratio), d, na.rm=TRUE) +
  xlim(1940,2015) +
  geom_ribbon(aes(year, ymin = lwr, ymax = upr), pred, alpha = 0.25) + 
  geom_line(aes(year, mu), pred) +
  labs(x = 'Year C.E.', y = expression(Daphnia:Bosmina~ratio)) 

ggsave('clad-ratio.pdf', zoop.p, dpi = 300, scale = 1)


###############################################################################
## Plotting Cladocera stratigraphies
library(tidypaleo) ## for vertical graphing 

### Buffalo pound cladocera counts
BPclad <- read.csv(file.choose(), header = TRUE)

BPclad <-
  BPclad %>%
  mutate(taxon = fct_relevel(taxon, "Chydorus spp.", "Alona spp.",
                             "Bosmina spp.", "Daphnia spp."))

### basic paleo plot
bp_plot <-
  ggplot(BPclad, aes(x=conc, y=depth))+
  geom_col_segsh()+
  geom_lineh()+
  scale_y_reverse(breaks=seq(0, 16, by=4))+
  expand_limits(x=0)+
  facet_geochem_gridh(vars(taxon))+
  labs(x= "Remains g/dry mass", y = "Depth(cm)")+
  theme_bw()+
  theme(strip.background = element_blank())+
  theme(strip.text = element_text(face="italic"))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
bp_plot


## add age-depht model
bpage <- read.csv(file.choose(), header=TRUE)

bp_adm <- age_depth_model(
  bpage,
  depth = depth,
  age = year
)
### putting ages on second axis
bp_plot + 
  scale_y_depth_age(
    bp_adm,
    age_name = "Year (CE)",
    age_breaks = seq(1940, 2010, by=10))

ggsave("BPclad.pdf", dpi=300, scale=1)

###############################################################################
### same for Pelican Lake
Pclad <- read.csv(file.choose(), header = TRUE)

Pclad <-
  Pclad %>%
  mutate(taxon = fct_relevel(taxon, "Chydorus spp.", "Pleuroxus spp.", "Leydgia spp.", "Alona spp.",
                             "Paralona spp.", "Bosmina spp.", "Daphnia spp.", "Ceriodaphnia spp.",
                             "Diaphanosoma spp.", "Leptodora spp."))


p_plot <-
  ggplot(Pclad, aes(x=conc, y=depth))+
  geom_col_segsh()+
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=9, ymax=10), fill="tomato", alpha = 0.2)+
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=11, ymax=11.25), fill="tomato", alpha = 0.2)+
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=12.5, ymax=12.75), fill="tomato", alpha = 0.2)+
  geom_lineh()+
  scale_y_reverse()+
  facet_geochem_wraph(vars(taxon), ncol=5)+
  labs(x= "Remains g/dry mass", y = "Depth(cm)")+
  theme_bw()+
  theme(axis.text.x = element_text(size=8))+
  theme(strip.background = element_blank())+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
  theme(strip.text = element_text(face="italic"))
p_plot

PAge <- read.csv(file.choose(), header=TRUE)

P_adm <- age_depth_model(
  PAge,
  depth = depth,
  age = year
)

p_plot + 
  scale_y_depth_age(
    P_adm,
    age_name = "Year (CE)",
    age_breaks = seq(1940, 2010, by=10))

ggsave("Pelclad.pdf", dpi=300, scale=1)
