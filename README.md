# interACT

This R package records my study note of the interACT software, a simulation tool
of road crossing behaviour.   I  

![Paradigm](/media/yslin/MERLIN/Documents/interACT/figs/crossingscenarios.png).

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
MIT License]

## Acknowledgments


