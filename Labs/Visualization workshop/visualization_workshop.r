####### 

setwd("~/Documents/Berkeley Files/Class/Fall 2019/IEOR242/visualization_workshop/")

load("sim.rdata")

#------------------------
# Challenge 1 

data = data.frame(a = 6, b = 9, c = 10, d = 2)

# show me your best plot displaying that info.





# -----------------------
# Challenge 2 

# run this to get a sense of the data

head(currentGen)

ggplot(currentGen)+
  geom_point(aes(x= startYear, y = carbonOutput, color=type), alpha = .3)

data1 = addCap[,c("PV", "SolarThermal", "Wind")]
data1$year = as.numeric(rownames(data1))

lowerPV = 0
higherPV = data1$PV
lowerST = higherPV
higherST = lowerST + data1$SolarThermal
lowerWind = higherST
higherWind = lowerWind + data1$Wind

# how would you best tell a story with the data? 

ggplot()





#### Challenge 2 

# now play around with the plot -- mainly the geom_points() fnc, but you can add others 

usaMap=map_data("county")
usaMap$locCode=paste(usaMap$region,",",usaMap$subregion,sep="")

ggplot(data=usaMap, aes(x=long,y=lat))+
  geom_polygon(aes(group=group))+
  geom_point( aes()# fill this in)+
  theme(text=element_text(size = 24))+
  ggtitle("US Powerplant fleet in 2014")
  
  
