# interACT

This R package records my study note of the interACT software, a simulation tool
of road crossing behaviour.  interACT implements two types of road users: a 
pedestrian and a driver in two countries, UK and Japan. 

![Crossing](https://github.com/yxlin/interACT/blob/master/figs/crossingscenarios.png).


## Getting Started

```
library(interACT)
timeStep   <- 1/30
endTime    <- 30 ## endTime    <- 2
ts <- seq(0, endTime, by = timeStep)

v <- new("Pedestrian")
ans <- SimulateCrossingScenario(v, ts, 50)
```


## Contributors
The package was written by [Yi-Shin Lin](mailto:yishinlin001@gmail.com).


## License
GPL3 

## Acknowledgments
interACT is copyrighted (2019) to Gustav Markkula, Jami Pekkanen.
The software implemented in MATLAB is part of interACT road crossing models - 
simulation software v1.1, abbreviated "interACT RCM" in associated documents.

interACT is released under the GNU General Public License along with interACT RCM. 



