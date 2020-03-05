library(emdbook)
library(ggplot2); theme_set(theme_bw())

r <- 1/7
G_s <- 6
G_a <- 6
kappa_s <- 0.5

z <- seq(0.01, 0.99, by=0.005)
relkappa <- seq(0.5, 1.5, by=0.005)

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
  scale_x_continuous("Relative asymptomatic generation-interval dispersion", expand=c(0, 0),
                     breaks=3:7*2/10) +
  scale_y_continuous("Intrinsic proportion of asymptomatic transmission", expand=c(0, 0),
                     limits=c(0, 1),
                     breaks=0:10/10) +
  scale_fill_gradientn(colors=c("#2B4162", "#40E0D0", "#FF8C00", "#FF0080"),
                       limits=c(1.4, 2.8),
                       breaks=c(1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8)) +
  ggtitle("D. Basic reproduction number") +
  theme(
    legend.title = element_blank(),
    legend.key.height = unit(1.6, "cm")
  )

ggsave("figheatmap_kappa_R0.pdf", g0, width=5, height=4)
