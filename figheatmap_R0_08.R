library(emdbook)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))

r <- 1/7
G_s <- 7
kappa_s <- 0.8
kappa_a <- 0.8

z <- seq(0.01, 0.99, by=0.005)
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

g0 <- ggplot(figdata) +
  geom_raster(aes(relG, z, fill=R0)) +
  geom_contour(aes(relG, z, z=R0), col="white") +
  scale_x_log10(expression(paste("Relative mean asymptomatic generation interval, ", italic(G[a]/G[s]))), expand=c(0, 0),
                breaks=c(0.5, 1, 2),
                limits=c(0.5, 2)) +
  scale_y_continuous(expression(paste("Intrinsic proportion of asymptomatic transmission, ", italic(z))), expand=c(0, 0),
                     limits=c(0, 1),
                     breaks=0:10/10) +
  scale_fill_gradientn(colors=c("#2B4162", "#40E0D0", "#FF8C00", "#FF0080"),
                       limits=c(1.5, 3.5),
                       breaks=c(1.5, 2, 2.5, 3.0, 3.5)) +
  ggtitle("B. Basic reproduction number") +
  theme(
    legend.title = element_blank(),
    legend.key.height = unit(1.8, "cm")
  )

ggsave("figheatmap_R0_08.pdf", g0, width=5.3, height=4.3)