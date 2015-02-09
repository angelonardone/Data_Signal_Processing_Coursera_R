
########################################################################
# The Karplus-Strong Algorithm
# Coursera clas DSP (Digital Signal Processing)
# by: Angelo Nardone
########################################################################




# Let's explore the R code implementing the equation
# y[n]= \alpha y[n-M]+x[n],
#
# where x[n] is the input signal, M is the delay, and α is the decay. 
# We assume that the value of the delay M is equal to the length of the input signal x. 
# In words, it is like filling the delay buffer and then recursively going over it. 
# Additionally, we set the length of the output to be a multiple of the delay M (i.e., of the length of the input signal). 
# This is controlled by the argument D, so that the output length is D∗M.

ks_loop <- function(x,alpha,D)
{
    if (D <= 0) stop ("D has to be bigger than 0 (cero)")
    if (length(x)<=1) stop("x should be a secuence of data biger than 1")
    
    M <- length(x)
    size_y <- D * M
    y <- x
    
    for (i in M:size_y)
    {
        # we add the 1 because in R the vectors starts with the index 1
        y[i] <- alpha * y[i-M +1]
    }
    return(y)
}

plot_dsp_style <- function(x)
{
    plot(x,type="h")
    points(x,type="p")
    abline(h=mean(x),col="red")
}

x <-rnorm(100)
# plot random data
plot_dsp_style(x)
# apply ks_loop algorithm
ks <- ks_loop(x, .9, 10)
# plot the KS applied to the random generated data
plot_dsp_style(ks)


# Two points moving average
two_points_moving_average <- function(x)
{
    y <- c()
    for ( i in 2:length(x))
    {
        y[i] <- (x[i] + x[i-1] ) / 2
    }
    return(y)
}

n_points_moving_average <- function(x,n=2)
{
    y <- c()
    for ( i in n:length(x))
    {
        y[i] <- mean(x[i:i-n+1])
    }
    return(y)
}

plot_2_functions <- function(f)
{

    par(mfrow = c(2, 1))
    plot(f,type="h")
    points(f,type="p")
    abline(h=0,col="red")
    
    plot(two_points_moving_average(f),type="h")
    points(two_points_moving_average(f),type="p")
    abline(h=0,col="red")
}

plot_n_functions <- function(f)
{
    
    par(mfrow = c(2, 1))
    plot(f,type="h")
    points(f,type="p")
    abline(h=0,col="red")
    
    plot(n_points_moving_average(f,3),type="h")
    points(n_points_moving_average(f,3),type="p")
    abline(h=0,col="red")
}

t <- -2:10
f <- cos(pi/10*t)
plot_2_functions(f)
plot_n_functions(f)

f <- cos(pi*t)
plot_2_functions(f)
plot_n_functions(f)


# Example of opening chord of Hard day's night
# Notes D2, D3, F3, G3, F4, A4, C5, G5
F0 <- 440*c((-31/12)^2,(-19/12)^2, (-16/12)^2, (-14/12)^2, (-4/12)^2, 1, (3/12)^2, (10/12)^2)
gain <- c(1.2, 3.0, 1.0, 2.2, 1.0, 1.0, 1.0, 3.5)
duration <- 4
alpha <- 0.9785
Fs <- 48000
# Number of samples in the chord
nbsample_chord <- Fs * duration
# This is used to correct alpha later, so that all the notes decay together
# (with the same decay rate)
first_duration = ceiling(nbsample_chord/Fs/F0[0])

for (i in 1:lenth(F0))
{
    current_M <- Fs/F0[i]
    current_duration <- ceiling(nbsample_chord/current_M)
    # Correct current alpha so that all the notes decay together (with the
    # same decay rate)
    current_alpha <- (first_duration/current_duration)^alpha
    if (i == 1+1) {current_alpha = 0.8^current_alpha}
    
}


