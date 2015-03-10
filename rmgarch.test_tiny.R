#################################################################################
# GO-GARCH model
#################################################################################
# Fit Tests

require(rmgarch)
cluster=NULL

tic = Sys.time()
# Mean Specification Tests
# constant-mvnorm
data(dji30ret)
# note: apply(...) is the rowwise mean
# so this matrix is the union of two matrices, each one has
# the rowwise means of columns i:j
# here it is just a matrix with two columns
ex = as.matrix(cbind(apply(dji30ret[,4:8], 1, "mean"), apply(dji30ret[,12:20], 1, "mean")))
# Lag the external data
# in other words, shift rows down one and pad a 0 at the top.
# note this removes the dates
ex = rugarch:::.lagx(ex, n.lag = 1, pad = 0)
spec = gogarchspec(
        mean.model = list(model = c("constant", "AR", "VAR")[1], lag = 3), 
        variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
        distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
fit.1 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, 
        cluster = cluster)

# AR(2)
spec = gogarchspec(
        mean.model = list(model = c("constant", "AR", "VAR")[2], lag = 2), 
        variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
        distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
fit.2 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, A.init = fit.1@mfit$A, 
        cluster = cluster)

# AR(2)+EXREG
spec = gogarchspec(
        mean.model = list(model = c("constant", "AR", "VAR")[2], lag = 2, external.regressors = ex), 
        variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
        distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
# some convergence problems with normal solvers so use gosolnp
fit.3 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], solver = "gosolnp", 
        solver.control = list(trace=1), out.sample = 0, A.init = fit.2@mfit$A, 
        cluster = cluster)

stop()

# VAR(2)
spec = gogarchspec(
        mean.model = list(model = c("constant", "AR", "VAR")[3], lag = 2), 
        variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
        distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
fit.4 = gogarchfit(spec, data = dji30ret[,1:3,drop=FALSE], out.sample = 0, 
        cluster = cluster)

# VAR(2)+EXREG
spec = gogarchspec(
        mean.model = list(model = c("constant", "AR", "VAR")[3], lag = 2, external.regressors = ex), 
        variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
        distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
fit.5 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, A.init = fit.4@mfit$A, 
        cluster = cluster)

# VAR(2)+EXREG+Robust
spec = gogarchspec(
        mean.model = list(model = c("constant", "AR", "VAR")[3], lag = 2, external.regressors = ex, robust = TRUE), 
        variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
        distribution.model = c("mvnorm", "manig", "magh")[1], ica = c("fastica", "radical")[1])
fit.6 = gogarchfit(spec, data =  dji30ret[,1:3,drop=FALSE], out.sample = 0, A.init = fit.5@mfit$A, 
        cluster = cluster)

# Collect and Compare:
modellik = c(0, likelihood(fit.2)-likelihood(fit.1), likelihood(fit.3)-likelihood(fit.1), 
        likelihood(fit.4)-likelihood(fit.1), likelihood(fit.5)-likelihood(fit.1), likelihood(fit.6)-likelihood(fit.1))
postscript("test1a1.eps", width = 8, height = 5)
barplot(modellik, names.arg = c(paste("C[",round(likelihood(fit.1),0), "]", sep=""),"AR(2)", "ARX(2)", "VAR(2)", "VARX(2)", "robVARX(2)"),
        ylab = "Diff Log-Likelihood", xlab = "Model", col = "steelblue", main = "GOGARCH with different\nconditional mean models")
dev.off()

postscript("test1a2.eps", width = 10, height = 8)
rc = rcor(fit.1)
D = as.POSIXct(dimnames(rc)[[3]])
plot(xts::xts(rc[1,2,], D), ylim = c(-0.4, 1), 
        main = "GOGARCH Correlation [AA-AXP] under\ndifferent mean models", 
        lty = 2, ylab = "Correlation", xlab = "Time", minor.ticks=FALSE, 
        auto.grid=FALSE)
rc = rcor(fit.2)
lines(xts::xts(rc[1,2,], D), col = 2, lty = 2)
rc = rcor(fit.3)
lines(xts::xts(rc[1,2,], D), col = 3)
rc = rcor(fit.4)
lines(xts::xts(rc[1,2,], D), col = 4, lty = 2)
rc = rcor(fit.5)
lines(xts::xts(rc[1,2,], D), col = 5)
rc = rcor(fit.6)
lines(xts::xts(rc[1,2,], D), col = 6)
legend("bottomleft", legend = c("C", "AR(2)", "ARX(2)", "VAR(2)", "VARX(2)", "robVARX(2)"), 
        col = 1:6, lty = c(2,K2,1,2,1,1), cex = 0.8, bty = "n")
dev.off()

# illustrate how the external regressors change the correlations
postscript("test1a3.eps", width = 12, height = 20)
T = fit.1@model$modeldata$T
D = fit.1@model$modeldata$index[1:T]
par(mfrow = c(3,1))
plot(xts::xts(fit.1@model$modeldata$data[1:T,1], D), 
        main = "AA Returns vs Fit under \ndifferent mean models", 
        xlab = "Time", ylab = "R_t", minor.ticks = FALSE, auto.grid=FALSE)
# fitted method returns xts for fit/filter objects
lines(fitted(fit.6)[,1], lty = 2, col = 3, lwd = 0.5)
lines(fitted(fit.2)[,1], lty = 2, col = 2)
legend("topleft", legend = c("Actual", "robVARX(2)", "AR(2)"), 
        col = c(1,3,2), lty = c(1,2,2), bty ="n")

plot(xts::xts(fit.1@model$modeldata$data[1:T,2], D), 
        main = "AXP Returns vs Fit under \ndifferent mean models", 
        xlab = "Time", ylab = "R_t", minor.ticks = FALSE, auto.grid=FALSE)
lines(fitted(fit.6)[,2], lty = 2, col = 3, lwd = 0.5)
lines(fitted(fit.2)[,2], lty = 2, col = 2)
legend("topleft", legend = c("Actual", "robVARX(2)", "AR(2)"), 
        col = c(1,3,2), lty = c(1,2,2), bty ="n")

plot(xts::xts(fit.1@model$modeldata$data[1:T,3], D), type = "l", 
        main = "BA Returns vs Fit under \ndifferent mean models", 
        xlab = "Time", ylab = "R_t", minor.ticks = FALSE, auto.grid=FALSE)
lines(fitted(fit.6)[,3], lty = 2, col = 3, lwd = 0.5)
lines(fitted(fit.2)[,3], lty = 2, col = 2)
legend("topleft", legend = c("Actual", "robVARX(2)", "AR(2)"), 
        col = c(1,3,2), lty = c(1,2,2), bty ="n")
dev.off()

toc = Sys.time()-tic
cat("Elapsed:", toc, "\n")
return(toc)



