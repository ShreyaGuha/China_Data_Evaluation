###Grouping###

##Early Laws##

#reading file
china_early <- fread("Early.csv")

# check out our province names
china_early$Province
china_early$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_early, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(china_early$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "viridis") +
  theme(legend.position = 'right',legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Initial years in which early policies targeting air pollution were implemented
  in various provinces in China")



##Coal Policies##

#read the file
coal_group <- fread("Coal_year.csv")

# check out our province names
coal_group$Province
coal_group$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, coal_group, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) +
  geom_sf( aes(fill = as.factor(coal_group$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "viridis") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
  labs( title = "Initial years in which policies targeting coal industries were implemented
  in various provinces in China")

