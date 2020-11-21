#---
# title: "R Practical 8 - Agriculture"

# Tom Matthews (based on original script by Tom Pugh)
# 20.11.20
#---

# The aim of this practical is to allow you to explore how the climate
# mitigation policy that you have chosen will impact upon, and interact with,
# agriculture.

# This practical relies on the skills you developed during the previous practicals

# As a first step, load in the packages we will use today and set your working directory
# remember to can use install.packages("name") if you have not used the package before
library("ncdf4")
library("fields")
library("viridis")
library("maps")

#set the wd to the Data folder
setwd("/home/jovyan/Earth_Systems_Practical8/Data")

# There are obviously a wide range of crops grown worldwide which can make the
# modelling of agricultural dynamics quite complex. One way of simplifying this
# is to assume wheat is a proxy for all temperate crops and maize is a proxy for
# all tropical crops (a very rough approximation). Following this, in the data
# folder accompanying the practical there are only model projections and harvest
# data for these two crops. It is fine to make this assumption in your assessment.

# Model simulation data for maize has 'mai' in the file name
# (e.g. lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T0_W0_N200_A0_1deg_mean),
# while wheat has 'swh' in the name
# (e.g. lpjml_agmerra_fullharm_yield_swh_global_annual_1980_2010_C360_T0_W-30_N200_A0_1deg_mean)

# Before making calculations of crop production, we first need to know where
# cropland area is located. Estimates of cropland area can be taken from
# data on crop harvest areas at the global scale. For example, for maize, this can
# be found in the maize_AreaYieldProduction_1deg_harvarea.nc file. Lets load this
# file in and make a plot.

#load in harvested area for maize
harvest_maize <- nc_open('maize_AreaYieldProduction_1deg_harvarea.nc')

#have a look at the the nc file object
harvest_maize

#extract latitude, longitude and the data on harvest amount (in ha)
lat_har_maize <- ncvar_get(nc=harvest_maize, varid='lat')

lon_har_maize <- ncvar_get(nc=harvest_maize,varid='lon')

dat_har_maize <- ncvar_get(nc=harvest_maize, varid='maizeData')

nc_close(harvest_maize)

#first go at plotting these data
image.plot(dat_har_maize)

#as we have seen in previous weeks, the data are all upside down 
#so we need to flip them again.
dat_har_maize_flip <- dat_har_maize[,180:1]

lat_har_maize_flip <- lat_har_maize[180:1]

#lets try and plot it again
image.plot(lon_har_maize, lat_har_maize_flip, dat_har_maize_flip, 
           col=viridis(256), xlab="", ylab="", 
           main="Maize Harvested Area", 
           legend.lab="ha", legend.line=4, legend.mar=7)

map(database = 'world', add = T, lwd=1.5)

##The above is actual harvest data for maize (there is the equivalent data file
#for wheat in the data folder). Now, lets look at model simulations projections
#of potential yield in kg of dry matter (DM) per m2 for the same crop. Each
#simulation uses climate for the period 1980-2010. You are provided with the
#mean values for potential over that period from the LPJmL model. Sensitivity
#simulations have been conducted in which temperature, precipitation, nitrogen
#fertilization or atmospheric CO2 mixing ratio are perturbed by a fixed amount
#to give and indication of how climate changes may affect the yields.

#REMEMBER - the descriptions of all datasets are provided in the data summary document

##lets try looking at the model projection for Maize with very low N fertilization, with a temperature
#increase of 1 kelvin everywhere, but no other changes. We will follow the same workflow as for the 
#harvest area data above.

lpj_maize_T1 <- nc_open('lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T1_W0_N10_A0_1deg_mean.nc4')

lpj_maize_T1 

lat_lpj_maize_T1 <- ncvar_get(nc=lpj_maize_T1, varid='lat')

lon_lpj_maize_T1 <- ncvar_get(nc=lpj_maize_T1,varid='lon')

dat_lpj_maize_T1 <- ncvar_get(nc=lpj_maize_T1, varid='yield_mai')

nc_close(lpj_maize_T1)

lat_lpj_maize_T1_flip <- lat_lpj_maize_T1[180:1]

dat_lpj_maize_T1_flip <- dat_lpj_maize_T1[,180:1]

image.plot(lon_lpj_maize_T1, lat_lpj_maize_T1_flip, 
           dat_lpj_maize_T1_flip, 
           col=viridis(256), xlab="", ylab="", 
           main="Predicted yield (T1) - Maize", 
           legend.lab="kg DM m-2", 
           legend.line=4, legend.mar=7)

map(database = 'world', add = T, lwd=1.5)


##This gives us potential yield under a scenario of 1 kelvin temperature
#increase, given this climate scenario, but if we want to calculate yield change
#we need to compare this scenario to a baseline scenario with no climate change. So lets
#do that following the same work flow. Notice that the baseline dataset name is T0, W0, N10,C360
#this means it has the same low nitrogen fertilisation as above, but all other climate variables do not
#change from current conditions (see the data summary document for further information)

baseline <- nc_open('lpjml_agmerra_fullharm_yield_mai_global_annual_1980_2010_C360_T0_W0_N10_A0_1deg_mean.nc4')

baseline 

lat_baseline <- ncvar_get(nc=baseline, varid='lat')

lon_baseline <- ncvar_get(nc=baseline,varid='lon')

dat_baseline <- ncvar_get(nc=baseline, varid='yield_mai')

nc_close(baseline)

lat_baseline_flip <- lat_baseline[180:1]

dat_baseline_flip <- dat_baseline[,180:1]

image.plot(lon_baseline, lat_baseline_flip, 
           dat_baseline_flip, 
           col=viridis(256), xlab="", ylab="", 
           main="Predicted yield (T0) - Maize", 
           legend.lab="kg DM m-2", 
           legend.line=4, legend.mar=7)

map(database = 'world', add = T, lwd=1.5)


##So we have a projection of potential yield under 1 kelvin temperature change,
#and we have projected yield under a baseline scenario of no climate change. Then,
#to calculate yield change, we need to subtract the T0 (baseline) scenario from the
#T1 (temperature increase) scenario, and plot it

yield_change_maize_T1 <- dat_lpj_maize_T1_flip -
  dat_baseline_flip

image.plot(lon_baseline, lat_baseline_flip, 
           yield_change_maize_T1, 
           col=viridis(256), xlab="", ylab="", 
           main="Yield Change (T1) - Maize", 
           legend.lab="kg DM m-2", 
           legend.line=4, legend.mar=7)

map(database = 'world', add = T, lwd=1.5)


##We can see that these models project yield predictions over the whole
#world, without taking into account areas where the crop is 
#actually grown. Thus, as a final step, we need to multiply
#our yield change data by the harvest area data for this crop
#to give ourselves actual predicted change in maize production

#But before we do that we need to change the units. Our yield_change data is in
#units of kg of dry matter (DM) per m2. But our harvest data are in
#hectares(ha). To convert the yield change data to kg of DM per ha, we need to
#multiply it by 10,000. We should also then convert it ton of DM per ha as this
#is the unit commonly used in agronomy. We do this by dividing the value by 1000.

yield_change_maize_T1_kgha <- yield_change_maize_T1 * 10000 #convert to kg per ha

yield_change_maize_T1_tonha <- yield_change_maize_T1 / 1000 #convert to ton per ha

prod_change_maize_T1 <- yield_change_maize_T1_tonha * dat_har_maize_flip #multiply yield by harvest area

#To better contrast the areas with change from areas with no
#change lets change the colour pallete using code we learnt
#in previous practicals

library("RColorBrewer")

image.plot(lon_baseline, lat_baseline_flip, 
           prod_change_maize_T1, 
           col=rev(brewer.pal(9, "RdBu")), xlab="", ylab="", 
           main="Production Change (T1) - Maize", 
           legend.lab="ton DM ha-1", 
           legend.line=4, legend.mar=7)

#calculate mean production change for maize under this scenario,
#remembering to set na.rm to TRUE to remove the NAs in the object before
#calculating the mean
mean(prod_change_maize_T1, na.rm = TRUE)

#and the max
max(prod_change_maize_T1, na.rm = TRUE)

#try any other summary statistics you can think of.

#You now have a map of global predicted production change for maize under
#a particular climate change scenario - well done! However, this is just
#one particular climate change scenario (T1) under a very low Nitrogen 
#input scenario (N10). There are many other model scenarios for you to consider,
#and all can be found in the Data folder and using the Data summary word
#document for information on which scenarios the data files represent. 

#try and undertake the following activities by re-running the above 
#code but changing the files where necessary:

#1) Sticking with maize and the T1 climate scenario, but using the high
# Nitrogen fertilization scenario (T1, W0, C360, N200)

#2) Try the same but with the T4 climate scenario (T4, w0, C360, N200)

#3) Run one of the above cases but this time with wheat instead. Remember, for
#this you will need to change every nc file for the wheat equivalent,
#including the harvest data (wheat_AreaYieldProduction_1deg_harvarea.nc)

#In terms of the ASSESSMENT (i.e. the geoengineering scenario), you can link
#these different simulations and what climate change they represent (e.g.
#C360/T1/w0) to the physical climate changes you have already seen with G3
#HadGEM2-ES. So, you need to have a -look at the Data Summary document and select
#scenarios that match with what you observe in the climate change simulations.
#And if you are particularly interested in one region, e.g. because the change
#in climate there is very big, or you suspect it may have big consequences for a
#variable of interest, then try investigating that more thoroughly. You could
#use the code we went over in previous practicals to subset the maps and data to focus
#down on one region.

#---A final consideration---

#Each time you run one of these scenarios, you need to make sure to try
#and interpret what the results are showing, i.e. how will agricultural production 
#be affected by climate change and/or Nitrogen fertilization? The code, figures and
#results are tools and resources for you to use to help you interpret the science,
#they are not the end point of the process.


