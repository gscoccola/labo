graficar <- data.frame(envios=r1$envios, ganancia=r1$ganancias, y2=r2$ganancias, y3=r3$ganancias, y4=r4$ganancias, y5=r5$ganancias)

pp<- ggplot(graficar, aes(envios)) + 
  geom_line(aes(y = ganancia, colour = "blue")) + 
  geom_line(aes(y = y2, colour = "red")) +
  geom_line(aes(y = y3, colour = "green")) +
  geom_line(aes(y = y4, colour = "yellow")) +
  geom_line(aes(y = y5, colour = "brown"))

ggsave(
  "summary.png",
  plot = pp,
  device = "png",
  path = "./allin",
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

pp
