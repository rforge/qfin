`eda.shape` <-
function(x)
{
    par(mfrow = c(2, 2))
    hist(x, main="Histogram", xlab="")
    boxplot(x, main ="Boxplot")
    iqd <- IQR(x)
    plot(density(x, width = 2 * iqd), ylab = "", 
        type = "l", main="Density")
    qqnorm(x)
    qqline(x)
    par(mfrow = c(1, 1))
}

