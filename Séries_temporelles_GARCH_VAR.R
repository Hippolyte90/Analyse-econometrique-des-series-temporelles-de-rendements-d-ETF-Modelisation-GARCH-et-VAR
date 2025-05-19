# Installer les packages si nécessaire
if (!require("rmgarch")) install.packages("rmgarch")
if (!require("xts")) install.packages("xts")
if (!require("quantmod")) install.packages("quantmod")
if (!require("reshape2")) install.packages("reshape2")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("xtable")) install.packages("xtable")



library(xtable)
library(rmgarch)
library(xts)
library(quantmod)
library(reshape2)
library(tidyverse)
library(tidyverse)

################################################################################

# Charger les données CSV (adapter le chemin)
data <- read.csv("C:/Users/sodji/OneDrive/Bureau/Cours amu/Second semestre/Series temporelles/Projet/etf_log_returns.csv", stringsAsFactors = FALSE)
data$Date <- as.Date(data$Date)

# MGARCH entre CW8 et CI2
# Convertir en xts pour faciliter la manipulation temporelle 
returns1 <- xts(data[, c("CW8", "CI2")], order.by = as.Date(data$Date))

head(returns1)

# Spécification du modèle GARCH univarié pour chaque série
uspec <- ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                    variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                    distribution.model = "norm")

# Spécification du modèle BEKK multivarié
bekk_spec <- dccspec(uspec = multispec(replicate(2, uspec)), 
                     dccOrder = c(1,1), 
                     distribution = "mvnorm",
                     model = "BEKK")

# Estimation du modèle
bekk_fit <- dccfit(bekk_spec, data = returns1)
summary(bekk_fit)

# Afficher les paramètres par composante
show(bekk_fit)

# Afficher les résidus standardisés
residuals(bekk_fit, standardize = TRUE)

# Afficher les volatilités conditionnelles
sigma(bekk_fit)



################################################################################
# Les graphiques

# Extraire les volatilités conditionnelles
vols <- sigma(bekk_fit)
dates <- index(returns1)

# Préparer les données dans un data.frame
vols_df <- data.frame(Date = dates, CW8 = vols[,1], CI2 = vols[,2])

# Graphique volatilité conditionnelle CW8
p_CW8 <- ggplot(vols_df, aes(x = Date, y = CW8)) +
  geom_line(color = "blue", linewidth = 1) +
  theme_bw() +
  labs(title = "Volatilité conditionnelle CW8", x = "Date", y = "Volatilité") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    #axis.title = element_text(size = 14),
    axis.text = element_text(size = 10)
  )

# Graphique volatilité conditionnelle CI2
p_CI2 <- ggplot(vols_df, aes(x = Date, y = CI2)) +
  geom_line(color = "red", linewidth = 1) +
  theme_bw() +
  labs(title = "Volatilité conditionnelle CI2", x = "Date", y = "Volatilité") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    #axis.title = element_text(size = 14),
    axis.text = element_text(size = 10)
  )


# Variance conditionnelle pour CW8
vars <- sigma(bekk_fit)^2
vars_df <- data.frame(Date = dates, CW8 = vars[,1], CI2 = vars[,2])

p_var1 <- ggplot(vars_df, aes(x = Date, y = CW8)) +
  geom_line(color = "blue", linewidth = 1) +
  theme_bw() +
  labs(title = "Variance conditionnelle CW8", x = "Date", y = "Variance") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    #axis.title.x = element_blank(),
    axis.text = element_text(size = 10)
  )


# Variance conditionnelle pour CI2
p_var2 <- ggplot(vars_df, aes(x = Date, y = CI2)) +
  geom_line(color = "red", linewidth = 1) +
  theme_bw() +
  labs(title = "Variance conditionnelle CI2", x = "Date", y = "Variance") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    #axis.title.x = element_blank(),
    axis.text = element_text(size = 10)
  )

# Covariances conditionnelles
covs <- rcov(bekk_fit)
cov_df <- data.frame(Date = dates, Covariance = covs[1, 2, ])

p_cov <- ggplot(cov_df, aes(x = Date, y = Covariance)) +
  geom_line(color = "purple", linewidth = 1) +
  theme_bw() +
  labs(title = "Covariance conditionnelle", x = "Date", y = "Covariance") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    #axis.title.x = element_blank(),
    axis.text = element_text(size = 10)
  )

# Corrélations conditionnelles
cors <- rcor(bekk_fit)
cors_df <- data.frame(Date = dates, Correlation = cors[1, 2, ])

p_cor <- ggplot(cors_df, aes(x = Date, y = Correlation)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  theme_bw() +
  labs(title = "Corrélation conditionnelle", x = "Date", y = "Corrélation") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10)
  )

# Afficher les quatre graphiques empilés verticalement
grid.arrange(p_var1,p_var2,p_CW8, p_CI2, p_cov, p_cor, nrow = 3, heights = c(2, 2, 2))

################################################################################
# MGARCH entre CW8 et EPRE
# Convertir en xts pour faciliter la manipulation temporelle 
returns2 <- xts(data[, c("CW8", "EPRE")], order.by = as.Date(data$Date))

head(returns2)

# Spécification du modèle GARCH univarié pour chaque série
uspec <- ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                    variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                    distribution.model = "norm")

# Spécification du modèle BEKK multivarié
bekk_spec <- dccspec(uspec = multispec(replicate(2, uspec)), 
                     dccOrder = c(1,1), 
                     distribution = "mvnorm",
                     model = "BEKK")

# Estimation du modèle
bekk_fit <- dccfit(bekk_spec, data = returns2)
summary(bekk_fit)

# Afficher les paramètres par composante
show(bekk_fit)

# Afficher les résidus standardisés
residuals(bekk_fit, standardize = TRUE)

# Afficher les volatilités conditionnelles
sigma(bekk_fit)



################################################################################
# Les graphiques

# Extraire les volatilités conditionnelles
vols <- sigma(bekk_fit)
dates <- index(returns2)

# Préparer les données dans un data.frame
vols_df <- data.frame(Date = dates, CW8 = vols[,1], CI2 = vols[,2])

# Graphique volatilité conditionnelle CW8
p_CW8 <- ggplot(vols_df, aes(x = Date, y = CW8)) +
  geom_line(color = "blue", linewidth = 1) +
  theme_bw() +
  labs(title = "Volatilité conditionnelle CW8", x = "Date", y = "Volatilité") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    #axis.title = element_text(size = 14),
    axis.text = element_text(size = 10)
  )

# Graphique volatilité conditionnelle EPRE
p_EPRE <- ggplot(vols_df, aes(x = Date, y = EPRE)) +
  geom_line(color = "red", linewidth = 1) +
  theme_bw() +
  labs(title = "Volatilité conditionnelle EPRE", x = "Date", y = "Volatilité") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    #axis.title = element_text(size = 14),
    axis.text = element_text(size = 10)
  )


# Variance conditionnelle pour CW8
vars <- sigma(bekk_fit)^2
vars_df <- data.frame(Date = dates, CW8 = vars[,1], CI2 = vars[,2])

p_var1 <- ggplot(vars_df, aes(x = Date, y = CW8)) +
  geom_line(color = "blue", linewidth = 1) +
  theme_bw() +
  labs(title = "Variance conditionnelle CW8", x = "Date", y = "Variance") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    #axis.title.x = element_blank(),
    axis.text = element_text(size = 10)
  )


# Variance conditionnelle pour EPRE
p_var2 <- ggplot(vars_df, aes(x = Date, y = EPRE)) +
  geom_line(color = "red", linewidth = 1) +
  theme_bw() +
  labs(title = "Variance conditionnelle EPRE", x = "Date", y = "Variance") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    #axis.title.x = element_blank(),
    axis.text = element_text(size = 10)
  )

# Covariances conditionnelles
covs <- rcov(bekk_fit)
cov_df <- data.frame(Date = dates, Covariance = covs[1, 2, ])

p_cov <- ggplot(cov_df, aes(x = Date, y = Covariance)) +
  geom_line(color = "purple", linewidth = 1) +
  theme_bw() +
  labs(title = "Covariance conditionnelle", x = "Date", y = "Covariance") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    #axis.title.x = element_blank(),
    axis.text = element_text(size = 10)
  )

# Corrélations conditionnelles
cors <- rcor(bekk_fit)
cors_df <- data.frame(Date = dates, Correlation = cors[1, 2, ])

p_cor <- ggplot(cors_df, aes(x = Date, y = Correlation)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  theme_bw() +
  labs(title = "Corrélation conditionnelle", x = "Date", y = "Corrélation") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10)
  )

# Afficher les quatre graphiques empilés verticalement
grid.arrange(p_var1,p_var2,p_CW8, p_EPRE, p_cov, p_cor, nrow = 3, heights = c(2, 2, 2))
