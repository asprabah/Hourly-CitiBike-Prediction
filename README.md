# Hourly CitiBike Prediction in Jersey City

Authors:  **Aravindh Siddharth Prabaharan**, **Arvind Ram Karthikeyan**

---

## Introduction
*Largest network in United States; imperative for transportation infrastructure.*
*Durable bikes locked in network of docking station  (Available 24/7).*
*Access available in Manhattan, Brooklyn, Queens and Jersey City.*
*May, 2019 bikes available to rent on Lyft Application.*

## Objective
To predict the checkout count of Citi Bikes at a given hour in Jersey City, thereby, determining the influence of weather on stations with high checkout frequency

---

## Sources
- The data source is from [CitiBike](https://www.citibikenyc.com/system-data)
- The code retrieves weather data from [Darksky API](https://darksky.net/dev)
- The link to use [Gmap](https://cloud.google.com/maps-platform/)
---

## Explanation of the Code

The code, `citibike.R`, begins by importing necessary R libraries:
```
library(dplyr)
library(tidyverse)
library(broom)
library(ggmap)
library(moments)
library(gbm)
library(rpart)
library(randomforest)
library(glmnet)
```
- *NOTE 1: The CitiBike data pulled can also be pulled using API in json format.*  
- *NOTE 2: The data may change over time and the results may not be same everytime.*

Day, month and Year are extracted
```
#extracting date,month,year,time
fulln_model$month = months(as.Date(fulln_model$date))
fulln_model$day = weekdays(as.Date(fulln_model$date))
tm1.lt <- as.POSIXlt(fulln$starttime)
fulln_model$hour=tm1.lt$hour
```


### Data Visualization:
#### Visualization of prominent Contributing Factors using Stacked Bar and Pie Chart
```
#---------------------GMAPS------------------------------------------------

library(ggmap)
ggmap::register_google(key = "YOUR KEY")

p <- ggmap(get_googlemap(maptype="terrain",zoom=11,center = c(lon = 74.0431, lat = 40.7178)))
p + geom_point(aes(x =Start_Lng , y =Start_Lat ),colour = 'red', incidents, alpha=0.25, size = 0.5) + 
  theme(legend.position="bottom")
p + geom_point(aes(x =Start_Lng , y =Start_Lat ),colour = 'red', i2rain, alpha=0.25, size = 0.5) + 
  theme(legend.position="bottom")
  
Finally, we visualize the data.  We save our plot as a `.jpeg` image:
```
The output from this code is shown below:
![Image of Plot](images/GMAPS.jpeg)
```
#---------------------Weather Data merge-------------------------------------------------
darksky=read.csv('G:/project/dark_sky.csv', header=TRUE)
fulln_model=fulln[,c("starttime","stoptime","start.station.id","end.station.id","date")]

#extracting date,month,year,time
fulln_model$month = months(as.Date(fulln_model$date))
fulln_model$day = weekdays(as.Date(fulln_model$date))
tm1.lt <- as.POSIXlt(fulln$starttime)
fulln_model$hour=tm1.lt$hour

#extracting date,month,year,time for weather
darksky$date = as.Date(darksky$time, format = "%Y-%m-%d")
tm2.lt <- as.POSIXlt(darksky$time)
darksky$hour=tm2.lt$hour

#remove unwanted columns weather
darksky=subset(darksky, select=-c(summary,icon,precipIntensity,precipProbability,precipType,precipAccumulation,                                ozone,uvIndex,windGust,windBearing,cloudCover,apparentTemperature))
darksky=darksky[!duplicated(darksky$time), ]

#Merging weather with model
darksky$dthr=paste(darksky$date,darksky$hour, sep="")
fulln_model$dthr=paste(fulln_model$date,fulln_model$hour, sep="")
fi=merge(fulln_model,darksky,by.x = "dthr",by.y = "dthr")
```
The merge of weather with CitiBike data is shown:

The output from this code is shown below:
![Image of Plot](images/merge.jpg)

#### Heatmap (Monthly vs Weekly)

```
heatmap_df1["Month"] = pd.Categorical(heatmap_df1["Month"], heatmap_df1.Month.unique()) 
plt.figure(figsize = (15, 9)) # Assigning figure size (length & breadth) for the plot
file_long = heatmap_df1.pivot("Weekday", "Month", "Crash_Count") # Assigning the column names for which the heatmap needs to be plotted
sns.heatmap(file_long, cmap = 'viridis', annot=True, fmt=".0f") # Plotting the map
plt.title("Heatmap of Crash Count in New York City (Monthly vs Weekly)", fontsize = 14); # Assigning title for the plot
plt.savefig('Heatmap1.jpg') # Saving the plot
```
The output from this code is shown below:
![Image of Plot](images/Heatmap1.jpg)

#### Heatmap (Weekly vs Hourly)
```
heatmap_df2["Weekday"] = pd.Categorical(heatmap_df2["Weekday"], heatmap_df2.Weekday.unique())
plt.figure(figsize = (20, 10))
file_long = heatmap_df2.pivot("Weekday", "Hour", "Crash_Count")
sns.heatmap(file_long, cmap = 'viridis', annot=True, fmt=".0f")
plt.title("Heatmap of Crash Count in New York City (Weekly vs Hourly)", fontsize = 14);
plt.savefig('Heatmap2.jpg')
```

The output from this code is shown below:
![Image of Plot](images/Heatmap2.jpg)

#### GMAPS Heatmap on NYC

```
#------------------------Visualizing using Gmaps-------------------------------

locations=pd.DataFrame(results_df[['latitude','longitude']])
locations[['latitude','longitude']] = locations[['latitude','longitude']].astype(float) #Latitude and Longitude data are stored as float

gmaps.configure(api_key='Key Here') #GMAPS API key is inserted
nyc_coordinates = (40.7128, -74.0060)
fig = gmaps.figure(center=nyc_coordinates, zoom_level=10.5) #Map co-ordinates along with zoom level is set
heatmap_layer=gmaps.heatmap_layer(locations) #heatmap layer is created using latitude,longitude
heatmap_layer.max_intensity = 200
heatmap_layer.point_radius = 15
fig.add_layer(heatmap_layer)
embed_minimal_html('Heatmap_layer.html', views=[fig]) #heatmap file is exported in save directory
```

The output from this code is shown below:
![Image of Plot](images/map.png)

---

## How to Run the Code using R Studio
*1. Click on File->Open*

*2. Choose directory where `citibike.R` is stored*

*3. Click on run or Ctrl+Enter*

*4. The Results are displayed in Global Environment(right) and the plots are shown (bottom right)*

---

## Suggestions
Spatial relation with neighboring stations can be considered.The predicted checkout frequency can be converted to a classification namely high, mid and low therevy incresing the prediction accuracy. Pricing of Citi Bikes can be taken into consideration for comparison. Special occasions or global situations may fluctuate the demand. Moreover, Efficient urban planning and better transportation infrastructure can balance supply demand.
