library(emdbook)
library(ggplot2); theme_set(theme_bw())

r <- 1/7
G_s <- 7
G_a <- 7
kappa_s <- 0.5

z <- seq(0.01, 0.99, by=0.005)
relkappa <- exp(seq(log(0.5), log(2), by=0.005))

qodds <- apply2d(
  "*", 
  z/(1-z),
  (1 + kappa_s * r * G_s)^(1/kappa_s)/(1 + kappa_s * relkappa * r * G_a)^(1/(kappa_s * relkappa))
)

qvals <- qodds/(1+qodds)

figdata <- data.frame(
  q=c(qvals),
  z=rep(z, length(relkappa)),
  relkappa=rep(relkappa, each=length(z))
)

g0 <- ggplot(figdata) +
  geom_raster(aes(relkappa, z, fill=q)) +
  geom_contour(aes(relkappa, z, z=q), col="white") +
  scale_x_log10("Relative asymptomatic generation-interval dispersion", expand=c(0, 0),
                     breaks=c(0.5, 1, 2)) +
  scale_y_continuous("Intrinsic proportion of asymptomatic transmission", expand=c(0, 0),
                     limits=c(0, 1),
                     breaks=0:10/10) +
  scale_fill_gradientn(colors=c("#2B4162", "#40E0D0", "#FF8C00", "#FF0080"),
                       limits=c(0, 1),
                       breaks=0:5*2/10) +
  ggtitle("C. Relevance of asymptomatic cases") +
  theme(
    legend.title = element_blank(),
    legend.key.height = unit(1.6, "cm")
  )

ggsave("figheatmap_kappa.pdf", g0, width=5, height=4)
