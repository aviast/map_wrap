library(dplyr)
library(ggplot2)

getCircle <- function(radius = 60, resolution = 360, xoffset = 320, yoffset = 0, xedge = 360) {
  azimuth <- seq(from = 1, to = resolution)
  
  radians <- (azimuth / resolution) * (2 * pi)
  
  circle <- data.frame(
    x = sin(radians) * radius + xoffset,
    y = cos(radians) * radius + yoffset,
    group = "circle",
    order = azimuth,
    stringsAsFactors = FALSE
  )
  
  circle[circle$x > xedge, "x"] <- circle[circle$x > xedge, "x"] - xedge
  
  return(circle)
}

##
## The problem
##

circle <- getCircle()

ggplot(circle, aes(x = x, y = y, group = group, order = order)) +
  geom_polygon() +
  scale_x_continuous(name = "Longitude", limits = c(0,360)) +
  scale_y_continuous(name = "Latitude", limits = c(-90,90)) +
  coord_fixed()

##
## The solution?
##
#
# What geometries cross the edge of the plot area?
#
# Note: need to interpolate points so that transformed shape is OK.

# Calculate the length of *every* segment. $$$
circle <- circle %>% mutate(
  len = sqrt((x - lag(x))^2 + (y - lag(y))^2)
)

# This method assumes an even number of crossings of the edge... how can this be odd?
stopifnot(length(which(circle$len > 300)) %% 2 == 0)

segments <- matrix(
  data = which(circle$len > 300),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(seq(length(which(circle$len > 300)) / 2), c("start","end"))
)

newcircle <- plyr::adply(segments, 1, function(seg) {
  # The original segment
  orig <- circle[seq(from = seg["start"], to = seg["end"] - 1), ]
  
  # A new straight path using the same y-values a the original segment
  # TODO - Don't use the existing y-values. Connect the extremes with
  # points spaced according to the average spacing calculated above.
  new <- mutate(orig, x = min(x))
  
  # Bind the two, creating a new closed shape
  tmp <- rbind(orig, new)
  
  # The new shape has a distinct group
  tmp$group <- paste0(tmp$group, "=", seg["start"], "-", seg["end"])
  
  return(tmp)
}, .id = NULL)

# Change the original shape to set replace the segment with a stright line.
# TODO - Don't use the existing y-values. Connect the extremes with
# points spaced according to the average spacing calculated above.
for (row in rownames(segments)) {
  # Change original circle
  circle[seq(from = segments[row, "start"], to = segments[row, "end"] - 1), "x"] <- max(circle[seq(from = segments[row, "start"] - 1, to = segments[row, "end"]), "x"])
}

# Bind the two shapes
circle <- rbind(circle, newcircle)

ggplot(circle, aes(x = x, y = y, group = group, order = order)) +
  geom_polygon() +
  scale_x_continuous(name = "Longitude", limits = c(0,360)) +
  scale_y_continuous(name = "Latitude", limits = c(-90,90)) +
  coord_fixed()
