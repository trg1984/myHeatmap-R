xMul = function(x, usr, rangeF, offset) { # Fix for inner margins. There is a better way but I didn't find it.
	xM <- diff(usr[1:2]) / rangeF;
	return((x - offset) * xM + usr[1]); 
}
	
yMul = function(y, usr, rangeF, offset) { # Fix for inner margins.There is a better way but I didn't find it.
	yM <- diff(usr[3:4]) / rangeF;
	return((y - offset) * yM + usr[3]);
}

colorfunc = function(t, t1, t2, returnName = F, textColor = F) {
	if (returnName) return("colorfunc");
	if (textColor) return( {if (t < 0.5) "white" else "black"} )
	else {	
		color <- character(length(t));
		color[is.na(t)] <- rgb(0.5, 0.5, 0.5);
		color[!is.na(t)] <- rgb(
			red = t,
			green = t,
			blue = t
		);
		return(color);
	}
}

myColorfunc2 = function(t, t1, t2, returnName = F, textColor = F) {
	if (is.na(t) || is.na(t1) || is.na(t2)) {
		return(
			rgb(
				red   = 1,
				green = 1,
				blue  = 1
			)
		);
	}
	else {
		if (returnName) return("myColorFunc"); # Return the name of the color function.
		if (textColor) { # TextColor
			if (t > 0.5) color <- "white" else color <- "black";
		}
		else { # Cell color
			color <- character(length(t1));
			color[is.na(t1)] <- rgb(0.5, 0.5, 0.5) # For NA values.
			
			# Otherwise
			color[!is.na(t1)] <- rgb(
				red   = min(1 - t, 1.0),
				green = min(1 - t, 1.0),
				blue  = min(1 - t, 1.0)
			);
		}
		return(color);
	}
}

myColorfunc3 = function(t, t1, t2, returnName = F, textColor = F) {
	if (is.na(t) || is.na(t1) || is.na(t2)) {
		return(
			rgb(
				red   = 1,
				green = 1,
				blue  = 1
			)
		);
	}
	else {
		if (returnName) return("myColorFunc"); # Return the name of the color function.
		if (textColor) { # TextColor
			if (t > 0.5) color <- "white" else color <- "black";
		}
		else { # Cell color
			color <- character(length(t1));
			color[is.na(t1)] <- rgb(0.5, 0.5, 0.5) # For NA values.
			
			# Otherwise
			color[!is.na(t1)] <- rgb(
				red   = min(1 - t, 1.0),
				green = min(1 - t, 1.0),
				blue  = min(1 - t, 1.0)
			);
		}
		return(color);
	}
}

myColorfunc4 = function(t, t1, t2, returnName = F, textColor = F) {
	if (is.na(t) || is.na(t1) || is.na(t2)) {
		return(
			rgb(
				red   = 1,
				green = 1,
				blue  = 1
			)
		);
	}
	else {
		if (returnName) return("myColorFunc"); # Return the name of the color function.
		if (textColor) { # TextColor
			if (t > 0.5) color <- "white" else color <- "black";
		}
		else { # Cell color
			color <- character(length(t1));
			color[is.na(t1)] <- rgb(0.5, 0.5, 0.5) # For NA values.
			#34 49 63
			# Otherwise
			color[!is.na(t1)] <- rgb(
				red   = min((t) * 52/255 + (1 - t) * 255/255, 1.0),
				green = min((t) * 152/255 + (1 - t) * 255/255, 1.0),
				blue  = min((t) * 219/255 + (1 - t) * 255/255, 1.0)
			);
		}
		return(color);
	}
}

myHeatmap = function(
	m,                  # Data matrix.
	cellCaptions = NA,  # Captions for individual cells. Uses data values, if not set.
	main = "",          # Plot Caption (main).
	mi = NA,            # Color scale minimum.
	Ma = NA,            # Color scale maximum.
	offsetX = 0,        # Heatmap x offset on device.
	offsetY = 0,        # Heatmap y offset on device.
	newPlot = T,        # Toggle for new plot creation. If false, draws on old canvas.
	gap = 0.04,         # Size of gap between the cells.
	colorf = colorfunc, # Cell and plot
	axes = T,           # Toggle axes with ticks. Tick labels are taken from the row and column names of m.
	asp = 1,             # Set aspect ratio of the plot. Defaults to 1.
	las = 1,
	xlab = "",
	ylab = "",
	sub = ""
) {
	if (newPlot) # Create a new plot window.
		plot(
			offsetX + 1 + c(0, ncol(m)),
			offsetY + c(0, nrow(m)),
			type = "n",
			main = main,
			sub = sub,
			xlab = xlab,
			ylab = ylab,
			axes = F,
			asp = asp
		);

	# If minimum and maximum have not been set, set them to be minimum and maximum values of the matrix, m.
	if (is.na(mi)) mi <- min(m, na.rm = T);
	if (is.na(Ma)) Ma <- max(m, na.rm = T);
	
	if ((length(cellCaptions) == 1) && is.na(cellCaptions)) cellCaptions <- round(m, 2)
	else if (length(cellCaptions) == 1) cellCaptions <- matrix(cellCaptions, nrow = nrow(m), ncol = ncol(m));
	
	# Collect user-set parameters.
	usr <- par("usr");
	
	# Go through each cell.
	for (r in 1:nrow(m))
	for (c in 1:ncol(m)) {
		
		# These are for scaling the colors of individual cells.
		#    t : Normalizes individual cell value to range [0, 1].
		#
		# t1 and t2 are meant to be used in cases where values of
		# m are both positive and negative:
		#    t1 : Maps range [mi, 0] to [0, 1].
		#    t2 : Maps range [0, Ma] to [0, 1].
		# t1 and t2 should only be used if mi < 0 and Ma > 0. 
		t <- (m[r, c] - mi) / (Ma - mi);
		t2 <- max( m[r, c] / Ma, 0, na.rm = T);
		t1 <- max(-m[r, c] / -mi, 0, na.rm = T);
		
		# Fetch colors for cell and text.
		cellColor <- colorf(t, t1, t2);
		textColor <- colorf(t, t1, t2, textColor = T);
		
		rect(
			xleft = xMul(offsetX + c + gap, usr, ncol(m), 1),
			xright = xMul(offsetX + c + 1 - gap, usr, ncol(m), 1),
			ytop = yMul(offsetY + nrow(m) - r + 1 - gap, usr, nrow(m), 0),
			ybottom = yMul(offsetY + nrow(m) - r + gap, usr, nrow(m), 0),
			col = cellColor,
			border = cellColor
		);
		text(
			x = xMul(offsetX + c + 0.5, usr, ncol(m), 1),
			y = yMul(offsetY + nrow(m) - r + 0.5, usr, nrow(m), 0),
			labels = cellCaptions[r, c],
			col = textColor
		);
	}
	
	if (axes == T) {
		axis(side = 1, at = xMul(1:ncol(m) + 0.5, par('usr'), ncol(m), 1), labels = { if ( is.null(colnames(m)) ) 1:ncol(m) else colnames(m)}, las = las);
		axis(side = 2, at = yMul(1:nrow(m) + 0.5, par('usr'), nrow(m), 1), labels = { if ( is.null(rownames(m)) ) nrow(m):1 else rownames(m)[nrow(m):1]}, las = las);
	}
}
