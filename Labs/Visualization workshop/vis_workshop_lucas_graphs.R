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
  geom_point(aes(x= startYear, y = carbonOutput, color=type), alpha = .3)+
  facet_wrap(~type)

data1 = addCap[,c("PV", "SolarThermal", "Wind")]
data1$year = as.numeric(rownames(data1))

lowerPV = 0
higherPV = data1$PV
lowerST = higherPV
higherST = lowerST + data1$SolarThermal
lowerWind = higherST
higherWind = lowerWind + data1$Wind

# how would you best tell a story with the data? 

g = ggplotGrob(
    ggplot()+
    geom_line(data=data1, aes(x=year, y = PV, color = "PV"))+
    geom_line(data=data1, aes(x=year, y = SolarThermal, color= "ST"))+
    geom_line(data=data1, aes(x=year, y = Wind, color = "Wind"))+
    theme_bw()+
    guides(fill=FALSE)
)

ggplot()+
  geom_ribbon(data=data1, aes(x=year, ymin = lowerPV, ymax = higherPV, fill = "PV"), alpha = .6)+
  geom_ribbon(data=data1, aes(x=year, ymin = lowerST, ymax = higherST, fill = "ST"), alpha = .6)+
  geom_ribbon(data=data1, aes(x=year, ymin = lowerWind, ymax = higherWind, fill = "Wind"), alpha = .6)+
  ggtitle("Our Chart!!")+
  theme_bw()+
  geom_vline(xintercept = 2016, color = "red", size = 2)+ 
  annotate("text", label = "Paris Accords", x= 2020, y = 50)+
  annotation_custom(grob = g, xmin = 2030, xmax = 2040, ymin = 100, ymax = 200)
  

#### Challenge 2 

# now play around with the plot -- mainly the geom_points() fnc, but you can add others 

require(maps)

usaMap=map_data("county")
usaMap$locCode=paste(usaMap$region,",",usaMap$subregion,sep="")


currentGen1 = currentGen[currentGen$type%in%c("Coal", "Nuclear", "PV", "Wind"),]

ggplot()+
  geom_polygon(data=usaMap, aes(x=long,y=lat, group=group))+
  geom_point(data = currentGen1, aes(x= long, y = lat, color = type, size = energy), alpha = .4)+# fill this in)+
  theme(text=element_text(size = 10))+
  ggtitle("US Powerplant fleet in 2014")

save.png()
  
  
