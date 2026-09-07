# =====================================================================
#  Figuras conceptuales del TFM (Capitulos 1, 2 y 3)
#  Genera, con ggplot2 y salida UTF-8 (cairo_pdf) y coma decimal:
#    * zib_pmf.pdf      : la ZIB como mixtura (motivacion, 1.1.2)
#    * zib_phi.pdf      : la ZIB al aumentar la inflacion phi (3.1)
#    * carta_control.pdf: anatomia de un grafico de control (2.4.1)
#    * pesos_ewma.pdf   : decaimiento de los pesos del EWMA (2.4.4)
#  Requiere: ggplot2
# =====================================================================
library(ggplot2)
dir.create("figuras", showWarnings = FALSE)

# Utilidad: punto -> coma decimal con 'd' decimales
coma <- function(v, d = 2) sub("\\.", ",", formatC(v, format = "f", digits = d))


# =====================================================================
#  1. Distribucion ZIB (Capitulos 1 y 3)
# =====================================================================
n <- 15; p <- 0.2
col_comp <- c(INFLT = "#F8766D", BINOM = "#00BFC4")
lab_comp <- c("Inflado en 0  (\u03c6)", "Binomial escalada  ((1\u2212\u03c6)\u00b7Bn)")

# Componentes de la mixtura (binomial escalada + inflado en 0) para un phi
componentes <- function(phi) {
  x <- 0:n
  rbind(
    data.frame(x = x, prob = (1 - phi) * dbinom(x, n, p), comp = "BINOM"),
    data.frame(x = x, prob = ifelse(x == 0, phi, 0),      comp = "INFLT")
  )
}

# --- (a) La ZIB como mixtura, caso phi = 0,5  -> zib_pmf.pdf ----------
d1 <- componentes(0.5)
d1 <- d1[d1$prob > 0, ]
d1$comp <- factor(d1$comp, levels = c("INFLT", "BINOM"))

p_mix <- ggplot(d1, aes(x, prob, fill = comp)) +
  geom_col(width = 0.7) +
  scale_x_continuous(breaks = 0:n) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, .25, .5, .75, 1),
                     labels = function(v) coma(v, 2)) +
  scale_fill_manual(name = "Componente", values = col_comp,
                    breaks = c("INFLT", "BINOM"), labels = lab_comp) +
  labs(title = "Distribuci\u00f3n binomial inflada en cero (ZIB)",
       subtitle = "n = 15;  p = 0,2;  \u03c6 = 0,5", x = "x", y = "probabilidad") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = "right")

ggsave("figuras/zib_pmf.pdf", p_mix, width = 7.2, height = 4.0, device = cairo_pdf)

# --- (b) La ZIB al aumentar phi (paneles)  -> zib_phi.pdf ------------
phis <- c(0, 0.25, 0.5, 0.75)
d2 <- do.call(rbind, lapply(phis, function(ph) { d <- componentes(ph); d$phi <- ph; d }))
d2 <- d2[d2$prob > 0, ]
d2$comp  <- factor(d2$comp, levels = c("INFLT", "BINOM"))
d2$panel <- factor(d2$phi, levels = phis, labels = paste0("\u03c6 = ", coma(phis, 2)))

p_phi <- ggplot(d2, aes(x, prob, fill = comp)) +
  geom_col(width = 0.7) +
  facet_wrap(~ panel, ncol = 2) +
  scale_x_continuous(breaks = seq(0, n, 3)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, .25, .5, .75, 1),
                     labels = function(v) coma(v, 2)) +
  scale_fill_manual(name = "Componente", values = col_comp,
                    breaks = c("INFLT", "BINOM"), labels = lab_comp) +
  labs(title = "Distribuci\u00f3n ZIB al aumentar la inflaci\u00f3n \u03c6",
       subtitle = "n = 15;  p = 0,2", x = "x", y = "probabilidad") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom")

ggsave("figuras/zib_phi.pdf", p_phi, width = 7.0, height = 5.0, device = cairo_pdf)


# =====================================================================
#  2. Anatomia de un grafico de control (2.4.1)  -> carta_control.pdf
# =====================================================================
CL <- 20; sig <- 3; L <- 3
UCL <- CL + L * sig; LCL <- CL - L * sig      # 29 y 11
t <- 1:20
w <- c(20, 23, 18, 21, 17, 22, 19.5, 24, 16, 21,
       22.5, 18, 20.5, 25, 19, 23, 21, 31, 22, 18)   # solo t=18 rebasa UCL
dc <- data.frame(t = t, w = w)
dc$estado <- factor(ifelse(w > UCL | w < LCL, "Fuera de control", "Bajo control"),
                    levels = c("Bajo control", "Fuera de control"))
xr <- 22.6

p_carta <- ggplot(dc, aes(t, w)) +
  geom_hline(yintercept = UCL, linetype = "dashed", colour = "grey30") +
  geom_hline(yintercept = LCL, linetype = "dashed", colour = "grey30") +
  geom_hline(yintercept = CL,  linetype = "solid",  colour = "grey55") +
  geom_line(colour = "grey55") +
  geom_point(aes(colour = estado, size = estado)) +
  annotate("text", x = xr, y = UCL, label = "UCL", hjust = 0, size = 3.4) +
  annotate("text", x = xr, y = CL,  label = "CL",  hjust = 0, size = 3.4) +
  annotate("text", x = xr, y = LCL, label = "LCL", hjust = 0, size = 3.4) +
  annotate("segment", x = 14.2, y = 32.6, xend = 17.7, yend = 31.2,
           colour = "#D7191C",
           arrow = arrow(length = unit(0.018, "npc"), type = "closed")) +
  annotate("text", x = 14.0, y = 33.0, hjust = 1, size = 3.3,
           colour = "#D7191C", label = "Se\u00f1al: fuera de control") +
  scale_colour_manual(name = NULL,
                      values = c("Bajo control" = "#2C3E50",
                                 "Fuera de control" = "#D7191C")) +
  scale_size_manual(guide = "none",
                    values = c("Bajo control" = 1.9, "Fuera de control" = 3.1)) +
  scale_x_continuous(breaks = seq(2, 20, 2), limits = c(1, 24)) +
  scale_y_continuous(limits = c(9, 34)) +
  labs(x = "n\u00famero de muestra  t", y = expression(italic(W)[italic(t)])) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("figuras/carta_control.pdf", p_carta, width = 7.0, height = 4.0, device = cairo_pdf)


# =====================================================================
#  3. Pesos del EWMA segun lambda (2.4.4)  -> pesos_ewma.pdf
#     Peso del retardo j:  w_j = lambda (1-lambda)^j
# =====================================================================
lambdas <- c(0.8, 0.2, 0.1)
J <- 0:9
de <- do.call(rbind, lapply(lambdas, function(l)
  data.frame(j = J, w = l * (1 - l)^J, lambda = l)))
de$lambda <- factor(de$lambda, levels = lambdas, labels = coma(lambdas, 1))

p_pesos <- ggplot(de, aes(factor(j), w, fill = lambda)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72,
           colour = "grey30", linewidth = 0.2) +
  scale_y_continuous(limits = c(0, 0.85), breaks = seq(0, 0.8, 0.2),
                     labels = function(v) coma(v, 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(name = expression(lambda),
                    values = c("#0072B2", "#D55E00", "#009E73")) +
  labs(title = "Decaimiento de los pesos del EWMA",
       subtitle = expression(w[j] == lambda * (1 - lambda)^j),
       x = "retardo  j  (muestras hacia atr\u00e1s)",
       y = expression("peso " * w[j])) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = "right")

ggsave("figuras/pesos_ewma.pdf", p_pesos, width = 7.0, height = 3.8, device = cairo_pdf)

# =====================================================================
#  4. Shewhart frente a grafico con memoria (2.4.3) -> shewhart_vs_ewma.pdf
#     Misma serie con un cambio pequeno y sostenido (a la baja desde la
#     muestra t0): el Shewhart 3-sigma no lo detecta; el EWMA si.
# =====================================================================
set.seed(6)
mu0 <- 34; sigma <- 3.4; Ls <- 3; lambda <- 0.2
Nsw <- 30; t0 <- 16; shift <- -1.2 * sigma

x <- rnorm(Nsw, mu0, sigma); x[t0:Nsw] <- x[t0:Nsw] + shift

# Grafico de Shewhart (medidas individuales): limites 3-sigma constantes
UCLs <- mu0 + Ls * sigma; LCLs <- mu0 - Ls * sigma

# Grafico EWMA: estadistico y limites (dependientes del tiempo)
z <- numeric(Nsw); prev <- mu0
for (tt in 1:Nsw) { z[tt] <- lambda * x[tt] + (1 - lambda) * prev; prev <- z[tt] }
hw   <- Ls * sigma * sqrt(lambda / (2 - lambda) * (1 - (1 - lambda)^(2 * (1:Nsw))))
UCLe <- mu0 + hw; LCLe <- mu0 - hw

pan <- c("Gr\u00e1fico de Shewhart", "Gr\u00e1fico EWMA  (\u03bb = 0,2)")
pts <- rbind(
  data.frame(t = 1:Nsw, stat = x, panel = pan[1], sig = x > UCLs | x < LCLs),
  data.frame(t = 1:Nsw, stat = z, panel = pan[2], sig = z > UCLe | z < LCLe)
)
pts$panel  <- factor(pts$panel, levels = pan)
pts$estado <- factor(ifelse(pts$sig, "sig", "ok"), levels = c("ok", "sig"))
lim <- rbind(
  data.frame(t = 1:Nsw, panel = pan[1], UCL = UCLs, LCL = LCLs, CL = mu0),
  data.frame(t = 1:Nsw, panel = pan[2], UCL = UCLe, LCL = LCLe, CL = mu0)
)
lim$panel <- factor(lim$panel, levels = pan)
lab <- data.frame(t = t0 - 0.5, stat = Inf, panel = factor(pan[1], levels = pan))

p_sw <- ggplot(pts, aes(t, stat)) +
  geom_vline(xintercept = t0 - 0.5, linetype = "dotted", colour = "grey45") +
  geom_line(data = lim, aes(t, UCL), linetype = "dashed", colour = "grey30") +
  geom_line(data = lim, aes(t, LCL), linetype = "dashed", colour = "grey30") +
  geom_line(data = lim, aes(t, CL),  colour = "grey60") +
  geom_line(colour = "grey50") +
  geom_point(aes(colour = estado, size = estado)) +
  geom_text(data = lab, aes(t, stat), label = "inicio del cambio",
            hjust = -0.04, vjust = 1.4, size = 3, colour = "grey45") +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  scale_colour_manual(name = NULL,
                      values = c(ok = "#2C3E50", sig = "#D7191C"),
                      labels = c("Sin se\u00f1al", "Se\u00f1al")) +
  scale_size_manual(guide = "none", values = c(ok = 1.7, sig = 2.9)) +
  scale_x_continuous(breaks = seq(0, Nsw, 5)) +
  labs(title = "Shewhart frente a gr\u00e1fico con memoria",
       subtitle = "misma serie con un cambio peque\u00f1o y sostenido desde la muestra 16",
       x = "n\u00famero de muestra  t", y = "valor") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom")

ggsave("figuras/shewhart_vs_ewma.pdf", p_sw, width = 7.0, height = 5.2,
       device = cairo_pdf)

# =====================================================================
#  5. El estadistico CEWMA en accion (3.2) -> cewma_accion.pdf
#     Realizacion entera de Y_t muestra a muestra con su limite K.
#     Proceso ZIB en control y, desde la muestra t0, con un cambio
#     delta=2 en la proporcion; el CEWMA lo detecta al superar K.
#     Diseno optimo real (n=100, p0=0,05, phi=0,7, delta=2): gx=9, gy=2, K=9.
#     Mecanismo entero:  Y_t = floor( (gx*X_t + B_{t-1}) / (gx+gy) ),
#                        R_t = (gx*X_t + B_{t-1}) - (gx+gy)*Y_t,
#                        B_t = gy*Y_t + R_t,  Y_0 = floor(E[X]),  senal si Y_t>K.
# =====================================================================
set.seed(14)
nn <- 100; p0 <- 0.05; phi <- 0.7; gx <- 9L; gy <- 2L; K <- 9L
delta <- 2; t0c <- 16; Nc <- 30
y0 <- floor((1 - phi) * nn * p0)
Y <- integer(Nc); Bprev <- gy * y0
for (t in 1:Nc) {
  pt <- if (t >= t0c) delta * p0 else p0
  X  <- if (runif(1) > phi) rbinom(1, nn, pt) else 0L   # observacion ZIB
  Yt <- (gx * X + Bprev) %/% (gx + gy)                  # division entera
  Y[t] <- Yt
  Rt    <- (gx * X + Bprev) - (gx + gy) * Yt
  Bprev <- gy * Yt + Rt
}
dcw <- data.frame(t = 1:Nc, Y = Y)
dcw$estado <- factor(ifelse(dcw$Y > K, "sig", "ok"), levels = c("ok", "sig"))

p_cewma <- ggplot(dcw, aes(t, Y)) +
  geom_vline(xintercept = t0c - 0.5, linetype = "dotted", colour = "grey45") +
  geom_hline(yintercept = K, linetype = "dashed", colour = "#D7191C") +
  geom_line(colour = "grey55") +
  geom_point(aes(colour = estado, size = estado)) +
  annotate("text", x = Nc, y = K, label = "K = 9", hjust = 1, vjust = -0.6,
           size = 3.2, colour = "#D7191C") +
  annotate("text", x = t0c - 0.35, y = max(Y), hjust = 0, vjust = 1, size = 3,
           colour = "grey45", label = " cambio: \u03b4 = 2") +
  scale_colour_manual(name = NULL, values = c(ok = "#2C3E50", sig = "#D7191C"),
                      labels = c("Y \u2264 K", "se\u00f1al (Y > K)")) +
  scale_size_manual(guide = "none", values = c(ok = 1.9, sig = 3.3)) +
  scale_x_continuous(breaks = seq(0, Nc, 5)) +
  scale_y_continuous(breaks = seq(0, max(Y) + 1, 2)) +
  labs(title = "El estad\u00edstico CEWMA en acci\u00f3n",
       subtitle = "proceso ZIB bajo control y, desde la muestra 16, con \u03b4 = 2",
       x = "n\u00famero de muestra  t", y = expression(Y[t])) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom")

ggsave("figuras/cewma_accion.pdf", p_cewma, width = 7.0, height = 4.0,
       device = cairo_pdf)

# =====================================================================
#  6. ARL0 en funcion del limite de control K (3.2.4) -> arl0_vs_K.pdf
#     Calibracion (cruce de 370,4) y monotonia de ARL0 en K, que
#     justifica la terminacion temprana ('break') del optimizador.
#     ARL0 exacto por cadena de Markov (mismo metodo que ARL_teorico.py):
#       Q[i,j] += pmf_ZIB(x)  con  Y=floor((gx*x+i)/(gx+gy)) <= K,
#       ARL0 = q' (I-Q)^{-1} 1,  q en el estado inicial gy*floor(E[X]).
# =====================================================================
zib_pmf <- function(n, p, phi) {
  pr <- (1 - phi) * dbinom(0:n, n, p); pr[1] <- pr[1] + phi; pr
}
calcul_Q <- function(n, p, phi, gx, gy, K) {
  pr <- zib_pmf(n, p, phi); bmax <- gx + gy * (K + 1) - 1; d <- bmax + 1
  Q <- matrix(0, d, d)
  for (i in 0:bmax) for (x in 0:n) {
    Y <- (gx * x + i) %/% (gx + gy)
    if (Y <= K) {
      R <- gx * x + i - (gx + gy) * Y; j <- gy * Y + R
      Q[i + 1, j + 1] <- Q[i + 1, j + 1] + pr[x + 1]
    }
  }
  Q
}
arl0_exact <- function(n, p0, phi0, gx, gy, K) {
  EY <- n * p0 * (1 - phi0); Q <- calcul_Q(n, p0, phi0, gx, gy, K)
  d <- nrow(Q); q <- numeric(d); q[gy * floor(EY) + 1] <- 1
  as.numeric(q %*% solve(diag(d) - Q, rep(1, d)))
}

ARL0min <- 370.4
Kseq <- 1:12
# ARL exacto general: Q con el proceso vigente (pp,php); q en el estado inicial en control
arl_exact <- function(n, p0, phi0, pp, php, gx, gy, K) {
  EY <- n * p0 * (1 - phi0); Q <- calcul_Q(n, pp, php, gx, gy, K)
  d <- nrow(Q); q <- numeric(d); q[gy * floor(EY) + 1] <- 1
  as.numeric(q %*% solve(diag(d) - Q, rep(1, d)))
}
gxk <- 9; gyk <- 2; nk <- 100; p0k <- 0.05; phi0k <- 0.7; deltak <- 1.5
A0 <- vapply(Kseq, function(K) arl_exact(nk, p0k, phi0k, p0k,          phi0k, gxk, gyk, K), 0)
A1 <- vapply(Kseq, function(K) arl_exact(nk, p0k, phi0k, deltak * p0k, phi0k, gxk, gyk, K), 0)
Kstar <- min(Kseq[A0 >= ARL0min])
cat("K* =", Kstar, "| ARL0(K*) =", round(A0[Kseq == Kstar], 1),
    "| ARL1(K*) =", round(A1[Kseq == Kstar], 1), "\n")
da <- rbind(data.frame(K = Kseq, ARL = A0, serie = "ARL0"),
            data.frame(K = Kseq, ARL = A1, serie = "ARL1"))
da$serie <- factor(da$serie, levels = c("ARL0", "ARL1"))

p_arl <- ggplot(da, aes(K, ARL, colour = serie)) +
  geom_hline(yintercept = ARL0min, linetype = "dashed", colour = "#D7191C") +
  geom_vline(xintercept = Kstar, linetype = "dotted", colour = "grey45") +
  geom_line() + geom_point(size = 2.4) +
  annotate("text", x = 1, y = ARL0min, label = "ARL\u2080 = 370,4", colour = "#D7191C",
           hjust = 0, vjust = -0.6, size = 3.2) +
  annotate("text", x = Kstar + 0.15, y = 1.4, label = "K* = 9", colour = "grey30",
           hjust = 0, size = 3.2) +
  scale_colour_manual(name = NULL, values = c(ARL0 = "#2C3E50", ARL1 = "#E67E22"),
                      labels = c("ARL\u2080", "ARL\u2081  (\u03b4 = 1,5)")) +
  scale_x_continuous(breaks = Kseq) +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 10000),
                labels = c("1", "10", "100", "1000", "10000")) +
  labs(title = "ARL frente al l\u00edmite de control K",
       subtitle = "escala logar\u00edtmica;  ZIB(100; 0,05; 0,7),  dise\u00f1o \u03b3X = 9, \u03b3Y = 2",
       x = "l\u00edmite de control  K", y = "ARL") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom")

ggsave("figuras/arl0_vs_K.pdf", p_arl, width = 7.0, height = 4.4, device = cairo_pdf)

cat("Figuras conceptuales generadas en figuras/:",
    "zib_pmf.pdf, zib_phi.pdf, carta_control.pdf, pesos_ewma.pdf,",
    "shewhart_vs_ewma.pdf, cewma_accion.pdf, arl0_vs_K.pdf\n")

# =====================================================================
#  7. Ejemplo numérico del Capítulo 6: el ZIB-CEWMA aplicado
#     Escenario ZIB(200, 0,05, 0,7); diseño gx=9, gy=11, K=11.
#     Cuatro fases: bajo control y tres deterioros (sube p, baja phi, ambos).
# =====================================================================
ne<-200; p0e<-0.05; phi0e<-0.7; gxe<-9L; gye<-11L; Ke<-11L
y0e<-floor(ne*p0e*(1-phi0e))
sim_fase <- function(seed, pp, ph, base, nmax) {
  set.seed(seed); B <- gye*y0e; X<-integer(0); Y<-integer(0); t<-0L; sig<-NA_integer_
  repeat {
    t <- t + 1L; ic <- t <= base
    pt <- if (ic) p0e else pp; pht <- if (ic) phi0e else ph
    x <- if (runif(1) > pht) rbinom(1, ne, pt) else 0L
    yt <- (gxe*x + B) %/% (gxe+gye); X[t]<-x; Y[t]<-yt
    if (is.na(sig) && yt > Ke) sig <- t
    B <- gye*yt + (gxe*x + B - (gxe+gye)*yt)
    if (!is.na(sig) || t >= nmax) break
  }
  data.frame(t=1:t, X=X, Y=Y, senal=(!is.na(sig) & (1:t)==sig))
}
cfg <- list(
  list("(a) Bajo control",        1, p0e,   phi0e, 999L, 30L),
  list("(b) FC1: aumento de p",   6, 0.075, 0.7,   10L,  60L),
  list("(c) FC2: descenso de \u03c6", 3, 0.05, 0.35, 10L, 60L),
  list("(d) FC3: ambos",          2, 0.075, 0.35,  10L,  60L))
dd <- data.frame(); shifts <- data.frame()
for (c in cfg) {
  df <- sim_fase(c[[2]], c[[3]], c[[4]], c[[5]], c[[6]]); df$fase <- c[[1]]
  dd <- rbind(dd, df)
  if (c[[5]] < 100) shifts <- rbind(shifts, data.frame(fase=c[[1]], x=c[[5]]+0.5))
}
lev <- sapply(cfg, `[[`, 1)
dd$fase <- factor(dd$fase, levels=lev); shifts$fase <- factor(shifts$fase, levels=lev)
dd$estado <- factor(ifelse(dd$senal,"sig","ok"), levels=c("ok","sig"))
write.csv(dd[, c("fase","t","X","Y","senal")], "datos/ejemplo_fases.csv",
          row.names=FALSE, fileEncoding="UTF-8")

p_ej <- ggplot(dd, aes(t, Y)) +
  geom_vline(data=shifts, aes(xintercept=x), linetype="dotted", colour="grey45") +
  geom_hline(yintercept=Ke, linetype="dashed", colour="#D7191C") +
  geom_line(colour="grey55") +
  geom_point(aes(colour=estado, size=estado)) +
  facet_wrap(~fase, ncol=1, scales="free_x") +
  scale_colour_manual(name=NULL, values=c(ok="#2C3E50", sig="#D7191C"),
                      labels=c("Y \u2264 K","se\u00f1al (Y > K)")) +
  scale_size_manual(guide="none", values=c(ok=1.6, sig=3)) +
  scale_y_continuous(breaks=seq(0,12,2)) +
  labs(title="El gr\u00e1fico ZIB-CEWMA aplicado (ZIB(200; 0,05; 0,7); K = 11)",
       x="n\u00famero de muestra  t", y=expression(Y[t])) +
  theme_bw(base_size=11) +
  theme(panel.grid.minor=element_blank(),
        strip.background=element_rect(fill="grey92", colour=NA),
        plot.title=element_text(face="bold", hjust=0.5), legend.position="bottom")
ggsave("figuras/ejemplo_cartas.pdf", p_ej, width=6.8, height=7.6, device=cairo_pdf)

# señales observadas por fase (para la tabla del texto)
for (l in lev) { s<-which(dd$senal[dd$fase==l]); cat(l, "-> senal en t =", if(length(s))s else NA, "\n") }
cat("ejemplo_cartas.pdf generado\n")
