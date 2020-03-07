library(emdbook)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))

r <- 1/7
G_s <- 8
kappa_s <- 0.2
kappa_a <- 0.2

z <- seq(0.01, 0.99, by=0.005)
relG <- exp(seq(log(0.5), log(2), by=0.005))

qodds <- apply2d(
  "*", 
  z/(1-z),
  (1 + kappa_s * r * G_s)^(1/kappa_s)/(1 + kappa_a * r * relG * G_s)^(1/kappa_a)
)

qvals <- qodds/(1+qodds)

figdata <- data.frame(
  q=c(qvals),
  z=rep(z, length(relG)),
  relG=rep(relG, each=length(z))
)

g0 <- ggplot(figdata) +
  geom_raster(aes(relG, z, fill=q)) +
  geom_contour(aes(relG, z, z=q), col="white") +
  scale_x_log10(expression(paste("Relative mean asymptomatic generation interval, ", italic(G[a]/G[s]))), expand=c(0, 0),
                     breaks=c(0.5, 1, 2)) +
  scale_y_continuous(expression(paste("Intrinsic proportion of asymptomatic transmission, ", italic(z))), expand=c(0, 0),
                     limits=c(0, 1),
                     breaks=0:10/10) +
  scale_fill_gradientn(colors=c("black", "#8a0072", "#cf2661", "#f66d4e", "#ffb34a", "#f9f871", "#f5f3b5"),
                       limits=c(0, 1),
                       breaks=0:5*2/10) +
  ggtitle("A. Relevance of asymptomatic cases") +
  theme(
    legend.title = element_blank(),
    legend.key.height = unit(1.8, "cm")
  )

ggsave("figheatmap_03.pdf", g0, width=5.3, height=4.3)
