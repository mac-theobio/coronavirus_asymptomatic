library(emdbook)
library(ggplot2); theme_set(theme_bw())
library(tikzDevice)

r <- 1/7
G_s <- 8
G_a <- 8
kappa_s <- 0.5

z <- seq(0.01, 0.99, by=0.005)
relkappa <- exp(seq(log(0.5), log(2), by=0.01))

R0 <- apply2d(
  function(x, y) {
    1/((1-x) * (1 + kappa_s * r * G_s)^(-1/kappa_s) + x * (1 + kappa_s * y * r * G_a)^(-1/(kappa_s * y)))
  }, 
  z,
  relkappa
)

figdata <- data.frame(
  R0=c(R0),
  z=rep(z, length(relkappa)),
  relkappa=rep(relkappa, each=length(z))
)

g0 <- ggplot(figdata) +
  geom_raster(aes(relkappa, z, fill=R0)) +
  geom_contour(aes(relkappa, z, z=R0), col="white") +
  scale_x_log10("Relative asymptomatic generation-interval dispersion, $\\kappa_a/\\kappa_s$", expand=c(0, 0),
                breaks=c(0.5, 1, 2),
                limits=c(0.5, 2)) +
  scale_y_continuous("Intrinsic proportion of asymptomatic transmission, $z$", expand=c(0, 0),
                     limits=c(0, 1),
                     breaks=0:10/10) +
  scale_fill_gradientn("$\\mathcal{R}_0$", colors=c("black", "#8a0072", "#cf2661", "#f66d4e", "#ffb34a", "#f9f871", "#f5f3b5"),
                       limits=c(1.5, 6),
                       breaks=c(1.5, 2.5, 3.5, 4.5, 5.5)) +
  ggtitle("B. Basic reproduction number") +
  theme(
    legend.key.height = unit(1.8, "cm")
  )

tikz(file = "figheatmap_kappa_R0.tex", width = 5.5, height = 4.5, standAlone = T)
print(g0)
dev.off()
tools::texi2dvi('figheatmap_kappa_R0.tex', pdf = T, clean = T)
