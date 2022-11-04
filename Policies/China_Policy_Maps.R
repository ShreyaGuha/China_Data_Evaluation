###Plotting spatial maps for policy implementations in China###

#Prerequisites#
#installing libraries
library( sf)
library( data.table)
library( ggplot2)
library( maptools)
library(viridis)
dir <- tempdir()


##For China spatial plots##

# download.file('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_CHN_1_sf.rds', 
#               file.path(dir, 'gadm36_CHN_1_sf.rds'))
# china_sf <- readRDS(file.path(dir, 'province.shp'))
# try this shapefile
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DBJ3BX

china_sf <-  st_read('province.shp') #for provinces
plot( china_sf)

# check out the province names
china_sf$NAME_PINGY

###Policies###

##1##
##Air Pollution Prevention and Control Law, 2000##

#reading file
china_2000_pol <- fread("China_2000.csv")

# check out our province names
china_2000_pol$Province
china_2000_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_2000_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(china_2000_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs( title = "Provinces which implemented
Air Pollution Prevention and Control
Law, 2000")


##2##
##Regulation on Prevention and Control of Air Pollution, 2005##

#reading file
china_2005_pol <- fread("Regulations_2005.csv")

# check out our province names
china_2005_pol$Province
china_2005_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_2005_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(china_2005_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs( title = "Provinces which implemented
  Regulation on Prevention and Control
  of Air Pollution, 2005")


##3##
##Measures for Implementation of Air Pollution Prevention and Control Law, 2008##

#reading file
china_2008_pol <- fread("Measures_2008.csv")

# check out our province names
china_2008_pol$Province
china_2008_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_2008_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(china_2008_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs( title = "Provinces with Implementation of 
  Air Pollution Prevention and Control Law, 2008")


##4##
##Emission trading Mechanism 2011-17##

#reading file
china_2011_pol <- fread("Emission.csv")

# check out our province names
china_2011_pol$Province
china_2011_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_2011_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(china_2011_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs( title = "Provinces which implemented 
  Emission Trading Mechanism, 2011-17")


##5##
##Air Pollution and Prevention Control Plan 2013-17##

#reading file
china_2013_pol <- fread("China_laws.csv")

# check out our province names
china_2013_pol$Province
china_2013_pol$Value

#merge the spatial and data
china_sf_dat <- merge( china_sf, china_2013_pol, by.x = "NAME_PINGY", by.y = "Province")

#make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(china_2013_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
  labs( title = "Provinces which implemented Air 
  Pollution and Prevention Control Plan, 2013-17")


##6##
##Beijing-Tianjin-Hebei Cooperative Development of Ecoenvironmental Protection Planning(2015-20)##

#reading file
bth_2015_pol <- fread("BTH_15.csv")

# check out our province names
bth_2015_pol$Province
bth_2015_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, bth_2015_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(bth_2015_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs( title = "Provinces which implemented 
  BTH Cooperative Development of 
  Ecoenvironmental Protection Planning, 2015-20")

##7##
##Special Action on Industrial Green Development in 2015##

#reading file
green_2015_pol <- fread("Green_15.csv")

# check out our province names
green_2015_pol$Province
green_2015_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, green_2015_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(green_2015_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs( title = "Provinces which implemented 
  Special Action on Industrial Green Development, 2015")


##8##
##Carbon trading Mechanism 2015##

#reading file
china_2015_pol <- fread("Carbon.csv")

# check out our province names
china_2015_pol$Province
china_2015_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_2015_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(china_2015_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs( title = "Provinces which implemented 
  Carbon Trading Mechanism, 2015")


##9##
##Electricity Consumption Policies 2015##

#reading file
elec_2015_pol <- fread("Elec_15.csv")

# check out our province names
elec_2015_pol$Province
elec_2015_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, elec_2015_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(elec_2015_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs( title = "Provinces which implemented 
  Electricity Consumption Policies, 2015")

##10##
##Work plan for Energy Conservation and Emission Reduction for 13th-Five-Year, 2016-2020##

#reading file
china_2016_pol <- fread("Work_16.csv")

# check out our province names
china_2016_pol$Province
china_2016_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_2016_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(china_2016_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs( title = "Provinces which implemented 
  Work plan for Energy Conservation and 
  Emission Reduction for 13th-Five-Year, 2016-2020")


##11##
##Energy Intensity Target, 2016##

#reading file
energy_2016_pol <- fread("Energy_16.csv")

# check out our province names
energy_2016_pol$Province
energy_2016_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, energy_2016_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(energy_2016_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  labs( title = "Provinces which implemented 
  Energy Intensity Target, 2016")


##12##
##Clean Electricity (Part of 13th 5-year plan) (2016-2020)##

#reading file
elec_2016_pol <- fread("Elec_16.csv")

# check out our province names
elec_2016_pol$Province
elec_2016_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, elec_2016_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(elec_2016_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  labs( title = "Provinces which implemented 
  Clean Electricity (Part of 13th 5-year plan)
       (2016-2020)")

##13##
##PV Plants (Part of 13th 5-year plan) (2016-2020)##

#reading file
elec_2016_pol <- fread("Elec_16.csv")

# check out our province names
elec_2016_pol$Province
elec_2016_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, elec_2016_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(elec_2016_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  labs( title = "Provinces which implemented 
  PV plants (Part of 13th 5-year plan)
        (2016-2020)")


##14##
##Power Emission Standard (Part of 13th 5-year Plan)(2016-2020)##

#reading file
elec_2016_pol <- fread("Elec_16.csv")

# check out our province names
elec_2016_pol$Province
elec_2016_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, elec_2016_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(elec_2016_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  labs( title = "Provinces which implemented 
  Power Emission Standard (Part of 13th 5-year Plan)
  (2016-2020)")


##15##
##Iron Emission Standard (2017)##

#reading file
china_2017_pol <- fread("Iron_17.csv")

# check out our province names
china_2017_pol$Province
china_2017_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_2017_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(china_2017_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
  labs( title = "Provinces which implemented 
  Iron Emission Standard (2017)")

##16##
##Clean Heating (2017)##

#reading file
heat_2017_pol <- fread("Heat_17.csv")

# check out our province names
heat_2017_pol$Province
heat_2017_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, heat_2017_pol, by.x = "NAME_PINGY", by.y = "Province")
# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(heat_2017_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
  labs( title = "Provinces which implemented 
  Clean Heating (2017)")

##17##
##NO2 target, 2017##

#reading file
NO2_2017_pol <- fread("NO2.csv")

# check out our province names
NO2_2017_pol$Province
NO2_2017_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, NO2_2017_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(NO2_2017_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
  labs( title = "Provinces which implemented 
  NO2 Target (2017)")


##18##
##SO2 target, 2017##

#reading file
NO2_2017_pol <- fread("NO2.csv")

# check out our province names
NO2_2017_pol$Province
NO2_2017_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, NO2_2017_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(NO2_2017_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
  labs( title = "Provinces which implemented 
  SO2 Target (2017)")


##19##
##Three Year Action Plan (2018-2020)##

#reading file
china_2018_pol <- fread("china_18.csv")

# check out our province names
china_2018_pol$Province
china_2018_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_2018_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(china_2018_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  labs( title = "Provinces which implemented
  Three Year Action Plan (2018-2020)")


##20##
##VOC monitoring (2018)##

#reading file
VOC_2018_pol <- fread("VOC_18.csv")

# check out our province names
VOC_2018_pol$Province
VOC_2018_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, VOC_2018_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(VOC_2018_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
  labs( title = "Provinces which implemented 
  VOC monitoring (2018)")


##21##
##Environmental Tax (2018)##

#reading file
tax_2018_pol <- fread("Tax_18.csv")

# check out our province names
tax_2018_pol$Province
tax_2018_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, tax_2018_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(tax_2018_pol$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
  labs( title = "Provinces which implemented 
  Environmental Tax (2018)")

##22##
##Vehicle Driving Restrictions (2018)##

#reading file
veh_2018_pol <- fread("Vehicle_18.csv")

# check out our province names
veh_2018_pol$Province
veh_2018_pol$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, veh_2018_pol, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(veh_2018_pol$Value))) +
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
  labs( title = "Provinces which implemented 
  Vehicle Driving Restriction (2018)")


###Grouping###

##Coal 2015##

#reading file
china_2015_coal <- fread("2015_coal.csv")

# check out our province names
china_2015_coal$Province
china_2015_coal$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_2015_coal, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(china_2015_coal$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
  labs( title = "Provinces with Implementation of 
  Laws targeting the coal sector after 2015")


##Coal 2016##

#reading file
china_2016_coal <- fread("2016_coal.csv")

# check out our province names
china_2016_coal$Province
china_2016_coal$Value

# merge the spatial and data
china_sf_dat <- merge( china_sf, china_2016_coal, by.x = "NAME_PINGY", by.y = "Province")

# make a plot
ggplot( china_sf) + 
  geom_sf( aes( fill = as.factor(china_2016_coal$Value))) + 
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = 'right', legend.title = element_blank(), rect = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
  labs( title = "Provinces with Implementation of 
  Laws targeting the coal sector after 2016")


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
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  labs( title = "Provinces with Implementation of Early 
        Laws to curb Air Pollution by 2008")


