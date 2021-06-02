library('sp')      # for shapefiles
library('rgdal')   # for shapefiles
library('dplyr')   # for utility functions
library('ggplot2') # for plotting
library('ggmap')   # for plotting maps


# unprojected shapefiles subset from "canvec_250K_SK_Hydro_shp.zip" at
# https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/
lakes <- readOGR(dsn = 'data', 'selected-lakes') %>% fortify() %>% as_tibble()
rivers <- readOGR(dsn = 'data',
                  'selected-rivers-small') %>% fortify() %>% as_tibble()

ggplot(mapping = aes(long, lat, group = group)) +
  cowplot::theme_map() + # no axes, axis labels, etc.
  # mercator projection with some cropping
  coord_map(projection = 'mercator', xlim = c(-107, NA), ylim = c(NA, 51.25)) +
  # remove white space around the map (i.e. do not "expand" the plot)
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # add somewhat transparent lake filling and rivers, then add lake outline
  geom_polygon(data = lakes, fill = 'cornflowerblue', alpha = 0.4) +
  geom_path(data = rivers, color = 'cornflowerblue', alpha = 0.7) +
  geom_polygon(data = lakes, color = 'cornflowerblue', fill = 'transparent') +
  # add lake labels
  annotate('text', x = -106.4, y = 51.06, label = 'Diefenbaker\nLake') +
  annotate('text', x = -105.65, y = 50.59, label = 'Buffalo Pound\nLake') +
  annotate('text', x = -106.15, y = 50.52, label = 'Pelican Lake')

# save plot, scale factor decreases line with and label size
ggsave('figures/lake-map.png', width = 6, height = 4, scale = 1.5)
