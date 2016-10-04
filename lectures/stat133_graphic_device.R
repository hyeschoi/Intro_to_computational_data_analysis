quartz() # open screen device
plot(1:10, 1:10, pch = 19, col=rainbow(10)) # plot something
dev.off() # close window after plotting

options("device")
dev.new()
dev.off()

# Vector format:
  pdf("dummy_plot.pdf") # open device
pie(rep(1, 10), col = rainbow(10)) # plot something
dev.off() # close device

# Raster format:
  png("dummy_plot.png") # open device
pie(rep(1, 10), col = rainbow(10)) # plot something
dev.off() # close device


# same resolution with different size
# 600px - 500px
png(file = "cars-600-500.png", width = 600, height = 500)
plot(mtcars[ ,c('hp', 'mpg')], type = "n",
     main = "Horsepower and Miles-per-gallon")
text(mtcars[ ,c('hp', 'mpg')], lab = rownames(mtcars))
dev.off()
# 400px - 350px
png(file = "cars-400-350.png", width = 400, height = 350)
plot(mtcars[ ,c('hp', 'mpg')], type = "n",
     main = "Horsepower and Miles-per-gallon")
text(mtcars[ ,c('hp', 'mpg')], lab = rownames(mtcars))
dev.off()


# resolution 72 PPI (pixels-per-inch)
png(file = "cars72.png", width = 600, height = 500, res = 72)
plot(mtcars[ ,c('hp', 'mpg')], type = "n",
     main = "Horsepower and Miles-per-gallon")
text(mtcars[ ,c('hp', 'mpg')], lab = rownames(mtcars))
dev.off()
# resolution 96 PPI (pixels-per-inch)
png(file = "cars96.png", width = 600, height = 500, res = 96)
plot(mtcars[ ,c('hp', 'mpg')], type = "n",
     main = "Horsepower and Miles-per-gallon")
text(mtcars[ ,c('hp', 'mpg')], lab = rownames(mtcars))
dev.off()


# googleVIs
install.packages("googleVis")
library(googleVis)
data(Fruits)

head(Fruits)
M <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
str(M)
print(M)
plot(M)
