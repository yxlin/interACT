# interACT

intreACt implements the interACT MATLAB software in object-oriented S4 classes 
in R. interACT is a simulation tool for investigating road-crossing behaviour, 
both for pedestrians and drivers, assuming  traffic scenarios in either UK or 
Japan. 

![Crossing](https://github.com/yxlin/interACT/blob/master/figs/crossingscenarios.png).

## Installation

```
devtools::install_github('yxlin/interACT')
```

## Getting Started

```
library(interACT)
timeStep   <- 1/30
endTime    <- 30 
ts <- seq(0, endTime, by = timeStep)

v   <- new("Pedestrian")
res <- SimulateCrossingScenario(v, ts, 50)
```
## Contributors
The package was written by [Yi-Shin Lin](mailto:yishinlin001@gmail.com).


## License
GPL3 

## Acknowledgments
interACT is copyrighted (2019) to Gustav Markkula and Jami Pekkanen.
The software implemented in MATLAB is part of interACT road crossing models - 
simulation software v1.1, abbreviated "interACT RCM" in associated documents.

interACT is released under the GNU General Public License along with interACT RCM. 
