source("myHeatmap2.R");

myColorfunc = function(t, t1, t2, returnName = F, textColor = F) {
	if (is.na(t) || is.na(t1) || is.na(t2)) {
		return(
			rgb(
				red   = 0.5,
				green = 0.5,
				blue  = 0.5
			)
		);
	}
	else {
		if (returnName) return("myColorFunc"); # Return the name of the color function.
		if (textColor) { # TextColor
			if (t < 0.5) color <- "white" else color <- "black";
		}
		else { # Cell color
			color <- character(length(t1));
			color[is.na(t1)] <- rgb(0.5, 0.5, 0.5) # For NA values.
			
			# Otherwise
			color[!is.na(t1)] <- rgb(
				red   = min(0.2 * t, 1.0),
				green = min(0.6 * t, 1.0),
				blue  = min(1.1 * t, 1.0)
			);
		}
		return(color);
	}
}

m <- matrix(runif(25), nrow=5);
myHeatmap(m, colorf = myColorfunc);
