# ---- MANETÍN COLONY DISTRIBUTION & TESTS ----
# assume `manetin` has columns month, rok and pocet
descdist(log(manetin$pocet), discrete = FALSE)

# fit uniform distribution
fit_uniform <- fitdist(manetin$pocet, "unif")

# normality tests
shap_raw <- shapiro.test(manetin$pocet)
shap_log <- shapiro.test(log(manetin$pocet))

# dip‐test for multimodality
dip_raw <- dip.test(log(manetin$pocet))

# Kruskal–Wallis test across months
kruskal_month <- kruskal.test(pocet ~ month, data = manetin)

list(
  fit_uniform    = fit_uniform,
  shapiro_raw    = shap_raw,
  shapiro_log    = shap_log,
  dip_log        = dip_raw,
  kruskal_month  = kruskal_month
)


# ---- BATS TRENDS BY SITE ----
bat_models <- bats %>%
  dplyr::group_by(site) %>%
  dplyr::reframe(
    # fit the model and store the results via broom
    tidied = list(tidy(lm(FF.juv ~ year, data = cur_data()))),
    glanced = list(glance(lm(FF.juv ~ year, data = cur_data()))),
    .groups = "drop"
  ) %>%
  # unnest out the numbers we need
  dplyr::mutate(
    coef    = map_dbl(tidied, ~ filter(.x, term == "year") %>% pull(estimate)),
    p.value = map_dbl(tidied, ~ filter(.x, term == "year") %>% pull(p.value)),
    adj.R2  = map_dbl(glanced, "adj.r.squared")
  ) %>%
  dplyr::select(-c(tidied, glanced)) %>%
  # compute trend and format numbers
  dplyr::mutate(
    trend = case_when(
      p.value > 0.05 ~ "no trend",
      coef    >  0   ~ "positive",
      TRUE            ~ "negative"
    ),
    across(c(coef, p.value, adj.R2),
           ~ formatC(.x, digits = 3, format = "f"))
  )

# View or export
print(bat_models)

write.csv2(bat_models, "bat_trends_by_site.csv", row.names = FALSE)

# CESKY KRAS ----

# 1. GLM MODELS ----
mdau_mt <- glm(POCET ~ MEAN_T, data = filter(netopyri_ma_t, DRUH == "MyoDau"), family = poisson())
summary(mdau_mt)

mmyo_mt <- glm(POCET ~ MEAN_T, data = filter(netopyri_ma_t, DRUH == "MyoMyo"), family = poisson())
summary(mmyo_mt)

bbar_mt <- glm(POCET ~ MEAN_T, data = filter(netopyri_ma_t, DRUH == "BarBar"), family = poisson())
summary(bbar_mt)

bbar_at <- glm(POCET ~ MEAN_T, data = filter(netopyri_all_t, DRUH == "Bbar"), family = poisson())
summary(bbar_at)

bbar_ar <- glm(POCET ~ ROK, data = filter(netopyri_all_t, DRUH == "Bbar"), family = poisson())
summary(bbar_ar)

bbar_art <- glm(POCET ~ ROK + MEAN_T, data = filter(netopyri_all_t, DRUH == "Bbar"), family = poisson())
summary(bbar_art)

mmyo_at <- glm(POCET ~ MEAN_T, data = filter(netopyri_all_t, DRUH == "Mmyo"), family = poisson())
summary(mmyo_at)

mmyo_ar <- glm(POCET ~ c(ROK), data = filter(netopyri_all_t, DRUH == "Mmyo"), family = poisson())
summary(mmyo_ar)

rhip_at <- glm(POCET ~ MEAN_T, data = filter(netopyri_all_t, DRUH == "Rhip"), family = poisson())
summary(rhip_at)

rhip_ar <- glm(POCET ~ ROK, data = filter(netopyri_all_t, DRUH == "Rhip"), family = poisson())
summary(rhip_ar)

paus_at <- glm(POCET ~ MEAN_T, data = filter(netopyri_all_t, DRUH == "Paus"), family = poisson())
summary(paus_at)

paus_ar <- glm(POCET ~ ROK, data = filter(netopyri_all_t, DRUH == "Paus"), family = poisson())
summary(paus_ar)

paur_at <- glm(POCET ~ MEAN_T, data = filter(netopyri_all_t, DRUH == "Paur"), family = poisson())
summary(paur_at)

ppip_at <- glm(POCET ~ MEAN_T, data = filter(netopyri_all_t, DRUH == "Ppip"), family = poisson())
summary(ppip_at)

ppip_ar <- glm(POCET ~ ROK, data = filter(netopyri_all_t, DRUH == "Ppip"), family = poisson())
summary(ppip_ar)

# Mean counts
netopyri_all_t %>% group_by(DRUH) %>% summarise(MEAN_N = mean(POCET, na.rm = TRUE))
