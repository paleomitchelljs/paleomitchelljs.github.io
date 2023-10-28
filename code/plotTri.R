plotTri <- function(Choice, Col='black')	{
	if (Choice == 1)	{
		x0=0
		x1=14.1
		y0=4.45
		upslope=6
		downslope=3.5
		Col=rgb(1,0,0,0.25)
	}
	else if (Choice == 2)	{
		x0=15.3
		x1=18.7
		y0=6
		upslope=6.6
		downslope=5.2
		Col=rgb(0,0,1,0.25)
	}
	else	{
		x0 = Choice[1]
		x1 = Choice[2]
		y0 = Choice[3]
		upslope = Choice[4]
		downslope = Choice[5]
	}
	polygon(c(x0, x1, x0, x1, x1), c(y0, upslope, y0, downslope, upslope), col=Col)
}

