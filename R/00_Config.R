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


# 2. LOAD CESKY KRAS ----
netopyri_all <- read.csv2("Data/Inputs/netopyri_all.csv",
                          stringsAsFactors = FALSE, fileEncoding = "Windows-1250")
netopyri_ma  <- read.csv2("Data/Inputs/netopyri_ma.csv",
                          stringsAsFactors = FALSE, fileEncoding = "Windows-1250")

netopyri_all <- netopyri_all %>% 
  pivot_longer(cols = 2:ncol(netopyri_all),
               names_to = "DRUH",
               values_to = "POCET")

# 3. CLIMATE DATA (TEPLOTA PRŮMĚR 15/01 - 15/02) ----
klimadata_read <- readr::read_csv2("https://raw.githubusercontent.com/manmatej/chmu-process/master/airTmean.csv")

klimadata <- klimadata_read %>%
  dplyr::select(date, P1PKLE01, P1DOBE01) %>%
  dplyr::rename(KLEME_T = P1PKLE01, DOBRE_T = P1DOBE01) %>%
  pivot_longer(cols = 2:3, names_to = "NAZEV", values_to = "TEPLOTA")

klima <- klimadata %>%
  mutate(ROK = lubridate::year(date),
         M = lubridate::month(date),
         D = lubridate::day(date)) %>%
  group_by(ROK, NAZEV) %>%
  arrange(M, D) %>%
  slice(15:46) %>%
  mutate(MEAN_T = mean(as.numeric(TEPLOTA))) %>%
  ungroup() %>%
  group_by(ROK, NAZEV) %>%
  summarise(MEAN_T = unique(MEAN_T))

klimat <- klima %>%
  filter(NAZEV == "KLEME_T") %>%
  dplyr::select(MEAN_T)

netopyri_ma_t <- netopyri_ma %>%
  left_join(klimat, by = "ROK") %>%
  mutate(OPEN = case_when(ROK < 2005 ~ "o", TRUE ~ "z"))

netopyri_all_t <- netopyri_all %>% left_join(klimat, by = "ROK")

# Filtered subsets
jj <- netopyri_all_t %>% filter(DRUH == "Paus")
netopyri_summary <- netopyri_all_t %>% filter(DRUH == "Mmyo", POCET > 1000) %>% mutate(FRACTION = POCET/MEAN_T)
