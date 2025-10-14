library('ISLR2')
#######################################################
par(mfrow = c(1,2))
#######################################################

dens_medv_default <- density(Boston$medv, na.rm = TRUE)
dens_medv_bw_small <- density(Boston$medv, bw = 0.5, na.rm = TRUE)
dens_medv_bw_large <- density(Boston$medv, bw = 2, na.rm = TRUE)

plot(dens_medv_default,
     main = "Kernel Density Estimate of Boston$medv\nwith different bandwidths",
     xlab = "Median House Value (medv)",
     ylab = "Density",
     lwd = 2,
     col = "black")
lines(dens_medv_bw_small, col = "red", lwd = 2)
lines(dens_medv_bw_large, col = "blue", lwd = 2) 
legend("topright",
       legend = c("bw Default", "bw = 0.5", "bw = 2"),
       col = c("black", "red", "blue"),
       lwd = 2) 

#######################################################
medv_chas0 <- Boston$medv[Boston$chas == 0] 
medv_chas1 <- Boston$medv[Boston$chas == 1]

dens_chas0 <- density(medv_chas0, na.rm = TRUE)
dens_chas1 <- density(medv_chas1, na.rm = TRUE)

x_range <- range(dens_chas0$x, dens_chas1$x, na.rm = TRUE)
y_range <- range(dens_chas0$y, dens_chas1$y, na.rm = TRUE)

plot(dens_chas0,
     main = "Kernel Density Estimate of Boston$medv by chas",
     xlab = "Median House Value (medv)",
     ylab = "Density",
     col = "darkorange",
     lwd = 2,
     xlim = x_range, 
     ylim = y_range) 

lines(dens_chas1, col = "blue", lwd = 2)

legend("topright",
       legend = c("chas = 0 (Not River Bound)", "chas = 1 (River Bound)"),
       col = c("darkorange", "blue"),
       lwd = 2) 

