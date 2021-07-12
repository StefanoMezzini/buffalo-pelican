library('readxl')  # for importing Excel files
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('scam')    # for shape-constrained (i.e. monotonic) additive models
library('readr')   # for importing files
source('analysis/figure-theme.R')

# dating CI extends further
BREAKS <- seq(1800, 2020, by = 10)
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

dates <-
  bind_rows(read.data('data/core-data-bp/Ge det Buffalo Pound Routine Core revised.xls'),
            read.data('data/core-data-pelican/Ge det Pelican Lake Core.xls')) %>%
  mutate(lake = factor(lake))

# modeling ####
m.age <- scam(year ~ lake + s(mid.depth, k = 12, bs = 'mpd', by = lake),
              data = dates, family = gaussian(), weights = weight)

new.ages <- expand_grid(mid.depth = seq(0, 40, length.out = 400),
                        lake = levels(dates$lake))
new.ages <- bind_cols(new.ages,
                      predict(m.age, newdata = new.ages, se.fit = TRUE)) %>%
  mutate(lwr = fit - se.fit,
         upr = fit + se.fit) %>%
  filter(fit > 1800)

ggplot() +
  facet_grid(lake ~ ., labeller = label_parsed) +
  geom_errorbarh(aes(xmin = lwr.1se, xmax = upr.1se, y = mid.depth), dates,
                 color = 'red') +
  geom_point(aes(year, y = mid.depth), dates, color = 'red') +
  geom_ribbon(aes(xmin = lwr, xmax = upr, y = mid.depth), new.ages, alpha=0.3) +
  geom_line(aes(x = fit, y = mid.depth), new.ages) +
  scale_y_reverse('Mid depth (cm)') +
  scale_x_continuous(breaks = BREAKS, labels = BREAK.LABELS) +
  labs(x = 'Year C.E.', title = 'scam, mpd +/- 1 SE')

# add all data into a single spreadsheet with new dates ########################
# zoops
full <-
  bind_rows(read_csv('data/core-data-bp/bp-cladocera-transposed.csv') %>%
              group_by(mid.depth) %>% # year column removed bc it's incorrect
              summarize(wet.weight.processed.g = `Wet weight processed (g)`,
                        bosmina = sum(`Bosmina longirostris carapace`,
                                      `Bosmina HS`, na.rm = TRUE),
                        daphnia = sum(`Daphnia claw/PA/mandible`,
                                      `Daphnia ephippium`, na.rm = TRUE),
                        .groups = 'drop') %>%
              mutate(bosmina = bosmina / wet.weight.processed.g,
                     daphnia = daphnia / wet.weight.processed.g,
                     zoop.ratio = daphnia / (daphnia + bosmina),
                     lake = 'Buffalo~Pound'),
            read_xlsx('data/core-data-pelican/Pelican Lake Cladocera data.xlsx',
                      sheet = 'inidividuals per gram+depth') %>%
              rename(mid.depth = mid) %>%
              group_by(mid.depth) %>%
              summarize(bosmina = sum(`Bosmina spp`, `Bosmina longispina`,
                                      `Bosmina longirostris`,`Bosmina coregoni`,
                                      na.rm = TRUE),
                        daphnia = sum(`Daphnia spp`, `Daphnia pulex`,
                                      `Daphnia longispina`, na.rm = TRUE)) %>%
              mutate(zoop.ratio = daphnia / (daphnia + bosmina),
                     lake = 'Pelican')) %>%
  mutate(core = 'zoops') %>%
  # pigments and isotopes
  full_join(
    bind_rows(
      read_xls('data/core-data-bp/Buffalo Pound Isotopes results.xls') %>%
        rename(mid.depth = Depth) %>%
        select(-c(Port, Sample, `Sample Name #2`:`Wt.[mg]`)) %>%
        full_join(
          read_xls('data/core-data-bp/Buffalo Pound Pigment results.xls') %>%
            slice(-1) %>% # remove first row (empty)
            rename(mid.depth = `Sample / Depth`) %>%
            select(-`Replicate #`, -Date, -DOY, -`Lake/Stream`) %>%
            mutate(lake = 'Buffalo~Pound'),
          by = c('mid.depth')),
      # sediments in nmol pigment/g sediment
      read_xlsx(
        'data/core-data-pelican/Pelican Lake Pigment and Isotope Data.xlsx') %>%
        select(Depth, d15NAIR, d13CVPDB, mgN, mgC, `%N`, `%C`, `C/N`, Fuco,
               Sed_A, Sed_B, Sed_C, Aphan, Diadino, Myxo, Allo, Diato, Lutein,
               Cantha, Chl_b, Chl_a, Chl_ap, Echine, Phaeo_B, Pheo_A,`B-car`)%>%
        filter(!is.na(Depth)) %>%
        rename(mid.depth = Depth, Lut_Zea = Lutein) %>%
        mutate(lake = 'Pelican')) %>%
      mutate(core = 'isotopes'),
    by = c('mid.depth', 'lake', 'core')) %>%
  # total phosphorus
  full_join(
    bind_rows(read_xlsx('data/core-data-bp/Buffalo Pound TP results.xlsx',
                        range = 'D3:E29') %>%
                transmute(mid.depth = `Depth (cm)`,
                          tp = `mg P/g`,
                          lake = 'Buffalo~Pound'),
              read_xlsx('data/core-data-pelican/Pelican Lake TP results.xlsx',
                        range = 'D3:E24') %>%
                transmute(mid.depth = `Depth (cm)`,
                          tp = `mg P/g`,
                          lake = 'Pelican')) %>%
      mutate(core = 'tp'),
    by = c('mid.depth', 'lake', 'core')) %>%
  arrange(lake, core, mid.depth) %>%
  mutate(year = predict(m.age, tibble(mid.depth = mid.depth, lake = lake)),
         lake = factor(lake)) %>%
  filter(year > 1800) %>%
  # values from slices w more years have more info => larger weight
  mutate(`%N` = `%N` / 100,
         `%C` = `%C` / 100,
         diff = lag(year) - year,
         weight = if_else(lag(lake) == lake & lag(core) == core,
                          true = diff,
                          false = NA_real_,
                          missing = NA_real_)) %>%
  rename(c.n.ratio = `C/N`, b.car = `B-car`, Pheo_B = Phaeo_B, pn = `%N`,
         pc = `%C`)

# standardize weights
full <-
  full %>%
  group_by(lake, core) %>%
  summarize(mean.weight = mean(weight, na.rm = TRUE)) %>%
  right_join(full, by = c('lake', 'core')) %>%
  mutate(weight = weight / mean.weight,
         weight = if_else(is.na(weight), 0.4, weight)) %>%
  select(-mean.weight)

# estimates are ok
ggplot(full) +
  facet_grid(core ~ lake, labeller = label_parsed) +
  geom_point(aes(year, weight,
                 col = lake == lag(lake) & core == lag(core) &
                   year != year[1])) +
  scale_color_brewer(NULL, type = 'qual', palette = 6) +
  theme(legend.position = 'none')

# write full dataset to a csv file
#write.csv(select(full, -core), 'data/full-dataset.csv', row.names = FALSE)
