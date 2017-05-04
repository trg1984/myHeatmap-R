# myHeatmap-R
Plot pretty heatmaps with R.

First Time
-----
Copy the files to your working directory (User ```getwd()``` to check what that is). Open R and go to your working directory (use ```setwd()``` to set it). Load example script with
```R
source('myHeatmap.example.R')
```
An example heatmap should be drawn. Open example in a text editor to see how to create custom color palettes and how to call myHeatmap in R.

Hello World
-------------
```R
m <- matrix(1:100, nrow=10)
myHeatmap(m)
```

General Usage
------------
Once the myHeatmap source is loaded by calling ```source('<path_to_file>/myHeatmap2.R')```, and the plottable data matrix ```m``` has been loaded, call ```myHeatmap(m)``` to draw the plot. The row and column captions are drawn from the data matrix's row and column names, using ```rownames(m)``` and  ```colnames(m)```.

Available Parameters and Their Defaults
---------------------------------------
* m # Data matrix.
* cellCaptions = NA,  # Captions for individual cells. Uses data values, if not set.
* main = "",          # Plot Caption (main).
* mi = NA,            # Color scale minimum.
* Ma = NA,            # Color scale maximum.
* offsetX = 0,        # Heatmap x offset on device.
* offsetY = 0,        # Heatmap y offset on device.
* newPlot = T,        # Toggle for new plot creation. If false, draws on old canvas.
* gap = 0.04,         # Size of gap between the cells.
* colorf = colorfunc, # Cell and plot
* axes = T,           # Toggle axes with ticks. Tick labels are taken from the row and column names of m.
* asp = 1,             # Set aspect ratio of the plot. Defaults to 1.
* las = 1,
* xlab = "",
* ylab = "",
* sub = "" 
