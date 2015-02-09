setwd("~/Documents/Coursera/DSP")

########################################################################
# example of inner product multiplication
# based on Lecture 3.2 - Hilbert Spaces, properties and bases
# Coursera clas DSP (Digital Signal Processing)
# by: Angelo Nardone
########################################################################

# let's first create the data points for the interval -1 to 1
t <- seq(-1,1,by=0.01)


first_wave <- function(x, a=4) { sin(a*pi*x)}
second_wave <- function(x, a=5) { sin(a*pi*x)}

# let's plot the 2 waves
plot(first_wave(t),type="l",col="blue", cex=.5)
points(second_wave(t),type="l",col="green", cex=.5)

# now let's plot the inner product
points(first_wave(t)*second_wave(t), type="l",col="red", cex=5)

# Now, let's confirm if they are Ortogonal calculating the "integral"
product_wave <- function(x) {first_wave(x)*second_wave(x)}
integrate(product_wave,-1,1)

# you sould see here with the above values the following result:
# 7.154504e-17 with absolute error < 9.2e-15
# wich is virtually CERO (0), so they are ortogonal
