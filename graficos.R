# =====================================================================
#  Figuras de resultados del TFM  -  gráfico de control ZIB-CEWMA
#  Genera las figuras del Capítulo 4 a partir de los datos del
#  optimizador (diseño factorial) y de la comparación con el CUSUM.
#  Requiere: ggplot2
# =====================================================================
library(ggplot2)

# ---- Lectura de datos -------------------------------------------------
fac <- read.csv("datos/datos_factorial.csv")     # 1080 escenarios
cmp <- read.csv("datos/datos_comparacion.csv")   # CEWMA vs CUSUM (sin tau)

# Directorio de salida y tema común
dir.create("figuras", showWarnings = FALSE)
tema <- theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA),
        legend.position = "bottom")
ylab_pd <- expression("%" * Delta * "ARL")
# Punto -> coma decimal (para etiquetas de ejes y niveles)
coma <- function(v, d = 2) sub("\\.", ",", formatC(v, format = "f", digits = d))

# =====================================================================
#  1. Boxplots de %DeltaARL por nivel de cada factor
# =====================================================================
factores <- c("n", "p0", "phi0", "delta", "tau")
etiquetas <- c(n = "n", p0 = "p[0]", phi0 = "varphi[0]",
               delta = "delta", tau = "tau")

# Formato largo: (factor, nivel, desv)
largo <- do.call(rbind, lapply(factores, function(f) {
  data.frame(factor = f,
             nivel  = factor(fac[[f]], levels = sort(unique(fac[[f]]))),
             desv   = fac$desv)
}))
largo$factor <- factor(largo$factor, levels = factores,
                       labels = etiquetas[factores])
# Ordenar los niveles numéricamente (para que cada panel respete el orden)
ord_niv <- as.character(sort(unique(as.numeric(as.character(largo$nivel)))))
largo$nivel <- factor(as.character(largo$nivel), levels = ord_niv,
                      labels = sub("\\.", ",", ord_niv))

p_box <- ggplot(largo, aes(nivel, desv)) +
  geom_boxplot(fill = "grey85", outlier.size = 0.6, linewidth = 0.35) +
  facet_wrap(~factor, scales = "free_x", nrow = 2,
             labeller = label_parsed) +
  scale_y_continuous(labels = coma) +
  labs(x = "Nivel del factor", y = ylab_pd) + tema
ggsave("figuras/fig_boxplots_factores.pdf", p_box, device = cairo_pdf, width = 8, height = 5)

# =====================================================================
#  2. Gráfico de efectos principales (medias marginales con IC 95%)
# =====================================================================
agg_m <- aggregate(desv ~ factor + nivel, data = largo, FUN = mean)
agg_s <- aggregate(desv ~ factor + nivel, data = largo, FUN = sd)
agg_n <- aggregate(desv ~ factor + nivel, data = largo, FUN = length)
medias <- agg_m; names(medias)[3] <- "m"
medias$s <- agg_s$desv; medias$n <- agg_n$desv
medias$se <- medias$s / sqrt(medias$n)
tcrit <- qt(0.975, medias$n - 1)
medias$lo <- medias$m - tcrit * medias$se
medias$hi <- medias$m + tcrit * medias$se

p_efec <- ggplot(medias, aes(nivel, m, group = 1)) +
  geom_line(colour = "grey55") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.18,
                colour = "#D7191C", linewidth = 0.5) +
  geom_point(size = 1.8, colour = "#2C3E50") +
  facet_wrap(~factor, scales = "free_x", nrow = 2,
             labeller = label_parsed) +
  scale_y_continuous(labels = coma) +
  labs(x = "Nivel del factor", y = expression("Media de %" * Delta * "ARL")) +
  tema
ggsave("figuras/fig_efectos_principales.pdf", p_efec, device = cairo_pdf, width = 8, height = 5)

# =====================================================================
#  3. Gráficos de interacción
# =====================================================================
interaccion <- function(datos, x, grupo, xlab, glab) {
  ag <- aggregate(datos$desv,
                  by = list(x = datos[[x]], g = datos[[grupo]]),
                  FUN = mean)
  names(ag) <- c("x", "g", "desv")
  xv <- sort(unique(ag$x)); gv <- sort(unique(ag$g))
  ag$x <- factor(ag$x, levels = xv, labels = sub("\\.", ",", as.character(xv)))
  ag$g <- factor(ag$g, levels = gv, labels = sub("\\.", ",", as.character(gv)))
  ggplot(ag, aes(x, desv, colour = g, group = g)) +
    geom_line() + geom_point(size = 1.8) +
    scale_y_continuous(labels = coma) +
    labs(x = xlab, y = expression("Media de %" * Delta * "ARL"),
         colour = glab) + tema
}

# delta x phi0  (se espera solapamiento -> phi0 poco influyente)
p_dp <- interaccion(fac, "delta", "phi0",
                    expression(delta), expression(varphi[0]))
ggsave("figuras/fig_interaccion_delta_phi.pdf", p_dp, device = cairo_pdf, width = 6.5, height = 4.2)

# delta x tau
p_dt <- interaccion(fac, "delta", "tau",
                    expression(delta), expression(tau))
ggsave("figuras/fig_interaccion_delta_tau.pdf", p_dt, device = cairo_pdf, width = 6.5, height = 4.2)

# --- Resto de interacciones dos a dos (para el conjunto exhaustivo) ---
en <- expression(n); ep <- expression(p[0]); ef <- expression(varphi[0])
ed <- expression(delta); et <- expression(tau)
ints <- list(
  list("delta","n",   ed, en, "fig_int_delta_n"),
  list("delta","p0",  ed, ep, "fig_int_delta_p0"),
  list("tau","n",     et, en, "fig_int_tau_n"),
  list("tau","p0",    et, ep, "fig_int_tau_p0"),
  list("tau","phi0",  et, ef, "fig_int_tau_phi"),
  list("p0","n",      ep, en, "fig_int_p0_n"),
  list("phi0","n",    ef, en, "fig_int_phi_n"),
  list("phi0","p0",   ef, ep, "fig_int_phi_p0"))
for (it in ints) {
  pl <- interaccion(fac, it[[1]], it[[2]], it[[3]], it[[4]])
  ggsave(paste0("figuras/", it[[5]], ".pdf"), pl, device = cairo_pdf,
         width = 6.2, height = 4.0)
}

# =====================================================================
#  4. Comparación CEWMA vs CUSUM: %DeltaARL frente a delta
# =====================================================================
agc <- aggregate(desv ~ delta + grafico, data = cmp, FUN = mean)
dv <- sort(unique(agc$delta))
agc$delta <- factor(agc$delta, levels = dv, labels = sub("\\.", ",", as.character(dv)))
p_cmp <- ggplot(agc, aes(delta, desv, colour = grafico, group = grafico)) +
  geom_line() + geom_point(size = 1.8) +
  scale_y_continuous(labels = coma) +
  labs(x = expression(delta), y = ylab_pd, colour = "Gr\u00e1fico") + tema
ggsave("figuras/fig_comparacion_cusum.pdf", p_cmp, device = cairo_pdf, width = 6.5, height = 4.2)

# =====================================================================
#  4b. (Apéndice C) Comparación CEWMA vs CUSUM en rejilla por n y p0
#      Version ampliada de la sintesis anterior: una rejilla p0 x n,
#      promediando sobre phi0 (irrelevante). Hace visible el cruce.
# =====================================================================
agg_grid <- aggregate(desv ~ n + p0 + delta + grafico, data = cmp, FUN = mean)
ns <- sort(unique(agg_grid$n)); ps <- sort(unique(agg_grid$p0))
agg_grid$nlab <- factor(agg_grid$n, levels = ns, labels = paste0("n = ", ns))
agg_grid$plab <- factor(agg_grid$p0, levels = ps,
                        labels = paste0("p\u2080 = ", coma(ps, 2)))
p_grid <- ggplot(agg_grid, aes(delta, desv, colour = grafico, group = grafico)) +
  geom_line() + geom_point(size = 1.4) +
  facet_grid(plab ~ nlab) +
  scale_x_continuous(breaks = c(1.1, 1.3, 1.5, 1.7, 2.0),
                     labels = function(v) coma(v, 1)) +
  scale_y_continuous(labels = function(v) coma(v, 2)) +
  scale_colour_manual(values = c(CEWMA = "#0072B2", CUSUM = "#D55E00")) +
  labs(x = expression(delta), y = ylab_pd, colour = "Gr\u00e1fico") + tema
ggsave("figuras/fig_comparacion_grid.pdf", p_grid, device = cairo_pdf, width = 8.5, height = 6)

# =====================================================================
#  5. Validación teórico vs Monte Carlo: ARL exacto frente a simulado
#     (apoya la Seccion 4.1). El ARL exacto se toma del CSV factorial;
#     el simulado se estima promediando longitudes de racha del CEWMA.
# =====================================================================
# Longitud de racha de una realizacion del CEWMA (mecanismo entero)
rl_cewma <- function(n, p0, phi0, delta, tau, gx, gy, K) {
  B <- gy * floor(n * p0 * (1 - phi0)); t <- 0L
  pp <- p0 * delta; ph <- phi0 * tau
  repeat {
    t <- t + 1L
    X  <- if (runif(1) > ph) rbinom(1, n, pp) else 0L
    Yt <- (gx * X + B) %/% (gx + gy)
    if (Yt > K) return(t)
    B <- gy * Yt + (gx * X + B - (gx + gy) * Yt)
  }
}
sim_arl <- function(reps, ...) mean(vapply(seq_len(reps), function(i) rl_cewma(...), 0L))

# Conjunto de escenarios que cubre un rango amplio de ARL
base_val <- data.frame(n = c(100, 200, 500), p0 = c(0.05, 0.02, 0.01),
                       phi0 = c(0.7, 0.8, 0.9))
val <- data.frame()
set.seed(1)
for (dl in c(1.1, 1.2, 1.3, 1.5, 1.7, 2.0)) for (k in 1:3) {
  b <- base_val[k, ]
  r <- fac[fac$n == b$n & fac$p0 == b$p0 & fac$phi0 == b$phi0 &
           fac$delta == dl & fac$tau == 1, ]
  if (!nrow(r)) next
  r <- r[1, ]
  s1 <- sim_arl(4000, b$n, b$p0, b$phi0, dl, 1, r$gx, r$gy, r$UCL)  # fuera de control
  s0 <- sim_arl(2000, b$n, b$p0, b$phi0, 1,  1, r$gx, r$gy, r$UCL)  # bajo control
  val <- rbind(val,
    data.frame(exacto = r$ARL1, sim = s1, tipo = "arl1"),
    data.frame(exacto = r$ARL0, sim = s0, tipo = "arl0"))
}
val$tipo <- factor(val$tipo, levels = c("arl0", "arl1"))
lims <- range(c(val$exacto, val$sim))

p_val <- ggplot(val, aes(exacto, sim, colour = tipo)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(size = 2.3, alpha = 0.9) +
  scale_x_log10() + scale_y_log10() + coord_fixed(xlim = lims, ylim = lims) +
  scale_colour_manual(name = NULL, values = c(arl0 = "#2C3E50", arl1 = "#D7191C"),
    labels = c("bajo control (ARL\u2080)", "fuera de control (ARL\u2081)")) +
  labs(title = "Validaci\u00f3n: ARL exacto frente a ARL simulado",
       subtitle = "c\u00e1lculo por cadena de Markov vs. Monte Carlo (escala log-log)",
       x = "ARL exacto (cadena de Markov)", y = "ARL simulado (Monte Carlo)") +
  tema + theme(plot.title = element_text(face = "bold", hjust = 0.5),
               plot.subtitle = element_text(hjust = 0.5))
ggsave("figuras/fig_validacion_mc.pdf", p_val, device = cairo_pdf, width = 6.5, height = 5.2)

# =====================================================================
#  6. Coste computacional frente al tamaño de muestra n (escala log)
# =====================================================================
fac$nf <- factor(fac$n, levels = sort(unique(fac$n)))
p_cpu <- ggplot(fac, aes(nf, t)) +
  geom_boxplot(fill = "grey85", outlier.size = 0.5, linewidth = 0.35) +
  scale_y_log10() +
  labs(title = "Coste computacional frente al tama\u00f1o de muestra",
       x = "tama\u00f1o de muestra  n",
       y = "tiempo de CPU por escenario (s)") +
  tema + theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("figuras/fig_coste_cpu.pdf", p_cpu, device = cairo_pdf, width = 6, height = 4)

# =====================================================================
#  7. Validación gráfica del modelo ANOVA (residuos vs. ajustados y Q-Q)
#     Residuos del modelo factorial (efectos principales + dobles).
# =====================================================================
for (cc in c("n","p0","phi0","delta","tau")) fac[[paste0(cc,"Ff")]] <- factor(fac[[cc]])
m_aov <- lm(desv ~ (nFf + p0Ff + phi0Ff + deltaFf + tauFf)^2, data = fac)
diag <- data.frame(fitted = fitted(m_aov), resid = resid(m_aov))

p_res <- ggplot(diag, aes(fitted, resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(size = 0.7, alpha = 0.35, colour = "#2C3E50") +
  scale_x_continuous(labels = coma) + scale_y_continuous(labels = coma) +
  labs(x = "valores ajustados", y = "residuos") + tema
ggsave("figuras/fig_residuos_ajustados.pdf", p_res, device = cairo_pdf, width = 6, height = 4)

p_qq <- ggplot(diag, aes(sample = resid)) +
  stat_qq(size = 0.7, alpha = 0.35, colour = "#2C3E50") +
  stat_qq_line(colour = "#D7191C") +
  scale_x_continuous(labels = coma) + scale_y_continuous(labels = coma) +
  labs(x = "cuantiles te\u00f3ricos (normal)",
       y = "cuantiles muestrales (residuos)") + tema
ggsave("figuras/fig_qqplot.pdf", p_qq, device = cairo_pdf, width = 6, height = 4)

cat("Figuras generadas en figuras/\n")
