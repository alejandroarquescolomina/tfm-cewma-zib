# =====================================================================
#  Figuras de los diseños óptimos seleccionados por el optimizador
#  (Capítulo 5). Muestran cómo varían el suavizado equivalente
#  lambda* = gammaX*/(gammaX*+gammaY*) y el límite de control K*
#  con la magnitud del cambio (delta) y el tamaño de muestra (n).
#  Requiere: ggplot2
# =====================================================================
library(ggplot2)

fac <- read.csv("datos/datos_factorial.csv")
dir.create("figuras", showWarnings = FALSE)

tema <- theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA))
coma <- function(v, d = 2) sub("\\.", ",", formatC(v, format = "f", digits = d))

# Suavizado equivalente del EWMA
fac$lambda <- fac$gx / (fac$gx + fac$gy)

# (a) lambda* frente a delta
fac$delta_f <- factor(fac$delta, levels = sort(unique(fac$delta)),
                      labels = coma(sort(unique(fac$delta)), 1))
p_lam <- ggplot(fac, aes(delta_f, lambda)) +
  geom_boxplot(fill = "grey85", outlier.size = 0.6, linewidth = 0.35) +
  scale_y_continuous(labels = coma) +
  labs(x = expression(delta), y = expression("Suavizado equivalente " * lambda^"*")) +
  tema
ggsave("figuras/fig_disenos_lambda.pdf", p_lam, device = cairo_pdf,
       width = 5, height = 3.6)

# (b) K* frente a n
fac$n_f <- factor(fac$n, levels = sort(unique(fac$n)))
p_k <- ggplot(fac, aes(n_f, UCL)) +
  geom_boxplot(fill = "grey85", outlier.size = 0.6, linewidth = 0.35) +
  labs(x = expression(n), y = expression("Límite de control " * K^"*")) +
  tema
ggsave("figuras/fig_disenos_K.pdf", p_k, device = cairo_pdf,
       width = 5, height = 3.6)

cat("Figuras de diseños óptimos generadas.\n")
