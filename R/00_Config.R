# ---- LOAD PACKAGES ----
packages <- c(
  "tidyverse", "lme4", "lmerTest", "dplyr", "lubridate", "ggplot2",
  "fitdistrplus", "logspline", "diptest", "grid", "hrbrthemes", "broom",
  "gridExtra", "gtable", "ggpubr"
)

# Install missing packages and load all
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!pkg %in% installed) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)
}

# ---- LOAD DATA ----
netopyri_all <- read.csv2("Data/Inputs/netopyri_all.csv",
                          stringsAsFactors = FALSE, fileEncoding = "Windows-1250") %>%
  pivot_longer(cols = 2:ncol(.), names_to = "DRUH", values_to = "POCET")

# ---- MANETIN DATA ----
manetin.y <- read.csv2("Data/Inputs/manetin.csv")

# ---- BATS ----
bats <- read_csv2(
  "Data/Inputs/bats.csv",
  locale = locale(encoding = "Windows-1250")
) %>%
  dplyr::filter(
    year >= 2000
  )
bat_1318 <- subset(bats, year >= 2013 & year <= 2018)

ledce  <- subset(bat_1318, site == "Ledce")
fit_ledce <- lm(FF.juv ~ year, data = ledce)
summary(fit_ledce)
plot(FF.juv ~ year, data = ledce)

dolany <- subset(bat_1318, site == "Dolany")
fit_dolany <- lm(FF.juv ~ year, data = dolany)
summary(fit_dolany)
plot(FF.juv ~ year, data = dolany)

# ---- NETOPYRI 2023 ----
predslav <- read.csv2("Data/Inputs/predslav.csv")
dolany   <- read.csv2("Data/Inputs/dolany.csv")
ejpovice <- read.csv2("Data/Inputs/ejpovice.csv")
manetin  <- read.csv2("Data/Inputs/manetin.csv")
vseruby  <- read.csv2("Data/Inputs/vseruby.csv")
radnice  <- read.csv2("Data/Inputs/radnice.csv")
