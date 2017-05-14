# Example of heartRacer
Clifford Anderson-Bergman  

The following is an example of creating a heart racer animation. 


```r
library(heartRacer)
library(gganimate)
library(fit)
```

Loading and prepping the data.


```r
# Reading in FIT files 
ride1 <- read.fit('bikeRideFiles/ride1.FIT')
ride2 <- read.fit('bikeRideFiles/ride2.FIT')

# heartRacer wants a list of records from FIT files

record_list <- list()
record_list$ride1 <- ride1$record
record_list$ride2 <- ride2$record
```

Making the plots. 


```r
# Creating animation
ani_plot <- heartRace(recordList = record_list,
                      zoom = 11, 
                      map_type = 'satellite')
```

```
## Warning: Ignoring unknown aesthetics: frame, cumulative
```

```r
# Plotting
animation <- gganimate(ani_plot, 
                       filename = "heartRacerAnimation.gif",
                       interval = 0.1)
```

![Example Plot](heartRacerAnimation.gif)
