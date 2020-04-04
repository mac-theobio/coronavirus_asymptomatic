library(emdbook)
library(ggplot2); theme_set(theme_bw())
library(tikzDevice)

r <- 1/7
G_s <- 8
kappa_s <- 0.5
kappa_a <- 0.5

z <- seq(0.01, 0.99, by=0.01)
relG <- exp(seq(log(0.5), log(2), by=0.01))

R0 <- apply2d(
  function(x, y) {
    1/((1-x) * (1 + kappa_s * r * G_s)^(-1/kappa_s) + x * (1 + kappa_a * r * y * G_s)^(-1/kappa_a))
  }, 
  z,
  relG
)

figdata <- data.frame(
  R0=c(R0),
  z=rep(z, length(relG)),
  relG=rep(relG, each=length(z))
)

zexample <- 0.5
relg1 <- 0.55
1/((1-zexample) * (1 + kappa_s * r * G_s)^(-1/kappa_s) + zexample * (1 + kappa_a * r * relg1 * G_s)^(-1/kappa_a))

relg2 <- 1.8
1/((1-zexample) * (1 + kappa_s * r * G_s)^(-1/kappa_s) + zexample * (1 + kappa_a * r * relg2 * G_s)^(-1/kappa_a))

g0 <- ggplot(figdata) +
  geom_raster(aes(relG, z, fill=R0)) +
  geom_contour(aes(relG, z, z=R0), col="white", bins=9) +
  geom_vline(xintercept=1, lty=2, col="white", lwd=0.8) +
  # geom_point(aes(x=1, y=0), size=8, col="black", stroke=1, shape=1) +
  annotate(geom="text", x=0.96, y=0.27, label="$\\textrm{Naive estimate},\\,\\mathcal{R}_0=2.5$", col="white", angle=90) +
  annotate(geom="text", x=1*sqrt(2), y=0.03, label="underestimated", col="white") +
  annotate(geom="text", x=1/sqrt(2), y=0.03, label="overestimated", col="white") +
  annotate(geom="point", x=0.55, y=0.5, col="white", shape=21, size=4, fill="white") +
  annotate(geom="text", x=0.57, y=0.46, label="$\\mathcal{R}_0=2.0$", col="white") +
  annotate(geom="point", x=1.8, y=0.5, col="white", shape=24, size=4, fill="white") +
  annotate(geom="text", x=1.78, y=0.46, label="$\\mathcal{R}_0=3.1$", col="white") +
  scale_x_log10("Relative mean asymptomatic generation interval, $G_a/G_s$", expand=c(0, 0),
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
    legend.key.height = unit(1.5, "cm")
  )

tikz(file = "figheatmap_R0.tex", width = 5, height = 4, standAlone = T)
print(g0)
dev.off()
tools::texi2dvi('figheatmap_R0.tex', pdf = T, clean = T)
