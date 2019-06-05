# install.packages("dodgr")
devtools::install_github("ATFutures/dodgr") # Development version

library(dodgr)

hampi <- hampi


graph <- weight_streetnet (hampi, wt_profile = "motorcar")


# teste ---------------------------------------------------------------------------------------
# https://atfutures.github.io/dodgr/articles/dodgr.html

bb <- osmdata::getbb ("york uk")
npts <- 1000
xy <- apply (bb, 1, function (i) min (i) + runif (npts) * diff (i))
bb; head (xy)

d <- dodgr_dists(from = xy, to = xy,
                  wt_profile = "foot", quiet = FALSE)


# teste ---------------------------------------------------------------------------------------

# A simple graph
graph <- data.frame (from = c ("A", "B", "B", "B", "C", "C", "D", "D"),
                     to = c ("B", "A", "C", "D", "B", "D", "C", "A"),
                     d = c (1, 2, 1, 3, 2, 1, 2, 1),
                     time = c(12, 10, 4, 1, 4, 2, 5, 4))
dodgr_dists (graph)
dodgr_times (graph, shortest = TRUE)

# A larger example from the included [hampi()] data.
graph <- weight_streetnet (hampi)
from <- sample (graph$from_id, size = 100)
to <- sample (graph$to_id, size = 50)
d <- dodgr_dists (graph, from = from, to = to)
# d is a 100-by-50 matrix of distances between `from` and `to`

## Not run: 
# a more complex street network example, thanks to @chrijo; see
# https://github.com/ATFutures/dodgr/issues/47

xy <- rbind (c (7.005994, 51.45774), # limbeckerplatz 1 essen germany
             c (7.012874, 51.45041)) # hauptbahnhof essen germany
xy <- data.frame (lon = xy [, 1], lat = xy [, 2])
essen <- dodgr_streetnet (pts = xy, expand = 0.2, quiet = FALSE)
graph <- weight_streetnet (essen, wt_profile = "foot")
d <- dodgr_dists (graph, from = xy, to = xy)
# First reason why this does not work is because the graph has multiple,
# disconnected components.
table (graph$component)
# reduce to largest connected component, which is always number 1
graph <- graph [which (graph$component == 1), ]
d <- dodgr_dists (graph, from = xy, to = xy)
# should work, but even then note that
table (essen$level)
# There are parts of the network on different building levels (because of
# shopping malls and the like). These may or may not be connected, so it may be
# necessary to filter out particular levels
index <- which (! (levs == "-1" | levs == "1")) # for example
library (sf) # needed for following sub-select operation
essen <- essen [index, ]
graph <- weight_streetnet (essen, wt_profile = "foot")
graph <- graph [which (graph$component == 1), ]
d <- dodgr_dists (graph, from = xy, to = xy)
