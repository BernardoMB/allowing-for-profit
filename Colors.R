# Generate a plot of color names which R knows about.
#++++++++++++++++++++++++++++++++++++++++++++
# cl : a vector of colors to plots
# bg: background of the plot
# rot: text rotation angle
#usage=showCols(bg="gray33")
showCols <- function(cl=colors(), bg="grey", cex=0.75, rot=30) {
  m <- ceiling(sqrt(n <-length(cl)))
  length(cl) <- m*m
  cm <- matrix(cl, m)
  require("grid")
  grid.newpage()
  vp <- viewport(w=.92, h=.92)
  grid.rect(gp=gpar(fill=bg))
  grid.text(cm, x=col(cm)/m, y=rev(row(cm))/m, rot=rot,
            vp=vp, gp=gpar(cex=cex, col=cm))
}
showCols()

# The first sixty color names
showCols(bg="gray20",cl=colors()[1:60], rot=30, cex=0.9)
# Barplot using color names
barplot(c(2,5), col=c("chartreuse", "blue4"))

showCols(cl= colors(), bg="gray33", rot=30, cex=0.75)

# Barplot using hexadecimal color code
barplot(c(2,5), col=c("#009999", "#0000FF"))

# Using RColorBrewer palettes
install.packages("RColorBrewer")
library("RColorBrewer")

display.brewer.all()

# Get the first 3 colors of the Greens color palette
brewer.pal(3, "Greens")

# View a single RColorBrewer palette by specifying its name
display.brewer.pal(n = 8, name = 'RdBu')

# Hexadecimal color specification 
brewer.pal(n = 8, name = "RdBu")

# Barplot using RColorBrewer
barplot(c(2,5,7), col=brewer.pal(n = 3, name = "RdBu"))

# Using Wes Anderson color palettes
install.packages("wesanderson")
# Load
library(wesanderson)

wes_palettes

# simple barplot
barplot(seq(1,length(wes_palettes[1]), by=1), col=wes_palette(n=5, name="BottleRocket1"))

# simple barplot
barplot(1:6, col=wes_palette(n=3, name="GrandBudapest1"))

# A plot
library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = wes_palette(n=3, name="GrandBudapest1"))

pal <- wes_palette(21, name = "GrandBudapest1", type = "continuous")
image(volcano, col = pal)
