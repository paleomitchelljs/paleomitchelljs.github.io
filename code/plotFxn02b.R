# summarize the day count
Days <- unique(beren2$age[Feeds])
ndays <- length(Days)

# create a set of custom colors
Alpha <- 0.75
divCols <- c(rgb(158/255,1/255,66/255, Alpha),rgb(213/255,62/255,79/255, Alpha),rgb(244/255,109/255,67/255, Alpha),rgb(253/255,174/255,97/255, Alpha),rgb(254/255,224/255,139/255, Alpha),rgb(255/255,255/255,191/255, Alpha),rgb(230/255,245/255,152/255, Alpha),rgb(171/255,221/255,164/255, Alpha),rgb(102/255,194/255,165/255, Alpha),rgb(50/255,136/255,189/255, Alpha),rgb(94/255,79/255,162/255, Alpha))
Pal <- colorRampPalette(divCols, interpolate = "spline", alpha = T)

# subset the custom colors
Cols <- Pal(ndays)
names(Cols) <- Days

# make a blank plot
plot(1, 1, xlim=c(7, 16), ylim=c(0, 20), type="n", xlab="time of day", ylab="total milk (oz)")

# add data to the plot
for (Day in Days)	{
	FeedsOnDay <- which(beren3$age[Feeds] == Day)
	Times <- beren3$start_hour[Feeds[FeedsOnDay]] + (beren3$start_minute[Feeds[FeedsOnDay]] / 60)
	Amounts <- cumsum(beren3$value[Feeds[FeedsOnDay]])
	points(Times, Amounts, col=Cols[as.character(Day)], type="b", pch=16)
}

# add a legend to the plot
dayVec <- c(min(Days), Days[floor(0.25*ndays)], median(Days), Days[ceiling(0.75*ndays)], max(Days))
legend("top", legend=dayVec, pch=21, pt.bg=Cols[as.character(dayVec)], horiz = T, bty = "n", pt.cex=2, title = "age in days")
