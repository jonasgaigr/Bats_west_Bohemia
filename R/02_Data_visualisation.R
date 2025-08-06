

descdist(log(manetin$POCET), discrete = FALSE)

fit.uniform <- fitdist(manetin$POCET, "unif")
plot(fit.uniform)

plot(manetin$month, manetin$POCET)

plot(density(manetin$POCET))
shapiro.test(manetin$POCET)
shapiro.test(log(manetin$POCET))
plot(density(log10(manetin$POCET)))

dip.test(log(manetin$POCET))
kruskal.test(POCET~month, data = manetin)

align_legend <- function(p, hjust = 0.5)
{
  # extract legend
  g <- cowplot::plot_to_gtable(p)
  grobs <- g$grobs
  legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
  legend <- grobs[[legend_index]]
  
  # extract guides table
  guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")
  
  # there can be multiple guides within one legend box  
  for (gi in guides_index) {
    guides <- legend$grobs[[gi]]
    
    # add extra column for spacing
    # guides$width[5] is the extra spacing from the end of the legend text
    # to the end of the legend title. If we instead distribute it by `hjust:(1-hjust)` on
    # both sides, we get an aligned legend
    spacing <- guides$width[5]
    guides <- gtable::gtable_add_cols(guides, hjust*spacing, 1)
    guides$widths[6] <- (1-hjust)*spacing
    title_index <- guides$layout$name == "title"
    guides$layout$l[title_index] <- 2
    
    # reconstruct guides and write back
    legend$grobs[[gi]] <- guides
  }
  
  # reconstruct legend and write back
  g$grobs[[legend_index]] <- legend
  g
}

manetin_rok <- ggplot(manetin, aes(x=rok, y=POCET)) + 
  geom_point() +
  #ggtitle("V?voj po?tu jedinc? v man?t?nsk? kolonii v pr?b?hu roku") +
  xlab("\nrok") +
  ylab("počet samic a juvenilů\n") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    legend.title = element_text(),
    legend.position = c(1.1, 0.5),
    legend.text = element_text(size = 9),
    plot.margin = unit(c(1,3,1,1), units = , "cm")
  )
manetin_rok <- manetin_rok + theme(legend.title.align=0.5) 
manetin_rok
ggsave(manetin_rok, file = "manetin_rok.png", width = 8, height = 5)


p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Petal.Width)) + geom_point()
ggdraw(align_legend(p, hjust = 1))
ggdraw(align_legend(manetin_year, hjust = 0))
library(colorspace)

fit1.manetin <- glm()


manetin_year <- ggplot(manetin, aes(x=date, y=POCET, colour=rok)) + 
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ x + sin(2*pi*x/360)) +
  ggtitle("Vývoj počtu samic a mláďat v Manětín\n") +
  xlab("rok") +
  ylab("počet samic a mláďat\n") +
  labs("\nrok") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    legend.title = element_text(),
    legend.position = c(1.125, 0.5),
    legend.text = element_text(size = 9),
    plot.margin = unit(c(1,3,1,1), units = , "cm")
  )
manetin_year <- manetin_year + theme(legend.title.align=0.5) 
manetin_year


ggsave(manetin_year, file = "manetin_year.png", width = 8, height = 5)



fit2.manetin <- lm(POCET ~ as.numeric(date) + sin(2*pi*as.numeric(date)/360), 
                   data = manetin)
summary(fit2.manetin)
summary(lm(POCET ~ as.numeric(date), data = manetin))

manetin.y <- read.csv2("manetin.csv")
manetin.y
plot(density(na.omit(manetin.y$FF.juv)))
shapiro.test(na.omit(manetin.y$FF.juv))

ggplot(manetin.y, aes(x=year, y=FF.juv)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x) +
  xlab("rok") +
  ylab("po?et") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.13, 0.75)
  )

fit.manetin.y <- lm(year ~ FF.juv, data = manetin.y)
summary(fit.manetin.y)


bats <- read.csv2("bats.csv")
head(bats)
ggplot(bats, aes(x=year, y=FF.juv, colour = site, group = site)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  xlab("\nrok") +
  ylab("po?et\n") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "plain"),
    plot.margin = unit(c(1,1,1,1), units = , "cm"),
    panel.grid = element_blank(),
    legend.title = element_blank(),    
    legend.text = element_text(size = 9, face = "italic"),  
    legend.position = c(0.13, 0.68)
  )


manetin_means <- bats %>%
  filter(site == "Man?t?n")

man_mean_g <- ggplot(man_means, aes(x=year, y=FF.juv)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  xlab("\nrok") +
  ylab("po?et\n") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "plain"),
    plot.margin = unit(c(1,1,1,1), units = , "cm"),
    panel.grid = element_blank(),
    legend.title = element_blank(),    
    legend.text = element_text(size = 9, face = "italic"),  
    legend.position = c(0.13, 0.68)
  )
man_mean_g

man_means <- lm(FF.juv ~ year, data = manetin_means)
summary(man_means)

bat_models = bats %>%
  group_by(site) %>%
  do(coef = coef(lm(FF.juv ~ year, data = .))[2],
     p.value = summary(lm(FF.juv ~ year, data = .))$coefficients[2,4],
     R.sqrt = summary(lm(FF.juv ~ year, data = .))$adj.r.squared) %>%
  mutate(trend = ifelse(p.value > 0.05, "X", ifelse(coef >= 0, "P", "N")))

bat_models
grid.table(as.data.frame(bat_models))
grid.table(bats)
write.csv2(bat_models, "netopyri.csv")

shapiro.test(bats$FF.juv)
plot(density(na.omit(bats$FF.juv)))
ggplot(data = bats, aes(x = FF.juv)) + geom_histogram(bins = 10)


bat_1318 <- subset(bats, bats$year >= 2013 & bats$year <= 2018)
bat_1318
netopyri_2013_2018 <- (ggplot(bat_1318, aes(x=year, y=FF.juv, colour = site, group = site)) + 
                         geom_point() +
                         geom_smooth(method = "lm", formula = y ~ x, se = TRUE, aes(fill = site)) +
                         ggtitle("Vývoj sledovaných kolonií v období 2013-2018") +
                         xlab("\nrok") +
                         ylab("počet samic a mláďat\n") +
                         theme_bw() +
                         theme(
                           axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1), 
                           axis.text.y = element_text(size = 10),
                           axis.title = element_text(size = 12, face = "plain"),
                           plot.title = element_text(hjust = 0.5),
                           plot.margin = unit(c(1,1,1,1), units = , "cm"),
                           panel.grid = element_blank(),
                           legend.title = element_blank(),    
                           legend.text = element_text(size = 9, face = "italic")
                         )
)
netopyri_2013_2018
ggsave(netopyri_2013_2018, file = "netopyri_2013_2018.png", width = 8, height = 5, dpi = 600)


grid.arrange(bat_models1318)
grid.table(as.data.frame(bat_models1318))



ledce <- subset(bat_1318, bat_1318$site == "Ledce")
fit_ledce <- lm(ledce$FF.juv ~ ledce$year)
summary(fit_ledce)
plot(ledce$year, ledce$FF.juv)

dolany <- subset(bat_1318, bat_1318$site == "Dolany")
fit_dolany <- lm(dolany$FF.juv ~ dolany$year)
summary(fit_dolany)
plot(dolany$year, dolany$FF.juv)


bat_models1318 = bat_1318 %>%
  group_by(site) %>%
  do(coef = coef(lm(FF.juv ~ year, data = .))[2],
     p.value = summary(lm(FF.juv ~ year, data = .))$coefficients[2,4],
     R.sqrt = summary(lm(FF.juv ~ year, data = .))$adj.r.squared) %>%
  mutate(trend = ifelse(p.value > 0.05, "není", ifelse(coef >= 0, "positivní", "negativní")))

bat_models1318

bat_models1318 = bat_models1318 %>%
  rename(lokalita = site) %>%
  rename(koeficient = coef) %>%
  rename("p-value" = p.value) %>%
  rename("R^2" = R.sqrt)

bat_models1318 <- as.data.frame(lapply(bat_models1318, unlist), stringsAsFactors = F) 

bat_models1318$koeficient <- formatC(bat_models1318$koeficient, format="f", digits=3)
bat_models1318$p.value <- formatC(bat_models1318$p.value, format="f", digits=3)
bat_models1318$R.2 <- formatC(bat_models1318$R.2, format="f", digits=3)

library(gridExtra)
library(gtable)
library(grid)
library("ggpubr")

ggtexttable(bat_models1318)
grid.table(bat_models1318)

g <- tableGrob(bat_models1318, rows = NULL, theme = ttheme_minimal())
separators <- replicate(ncol(g) - 2,
                        segmentsGrob(x1 = unit(0, "npc"), gp=gpar(lty=2)),
                        simplify=FALSE)
## add vertical lines on the left side of columns (after 2nd)
g <- gtable::gtable_add_grob(g, grobs = separators,
                             t = 2, b = nrow(g), l = seq_len(ncol(g)-2)+2)
ggsave(grid.draw(g), "tabulka.png", width = 8, height = 5)

png("g.png",width = 8, height = 5) 
grid.draw(g) 
dev.off()



ggsave('g.png', plot = g, width=8,height=5,dpi=600)

# PLOT ----
# join the trend info into the original data
bats_plot <- bats %>%
  left_join(bat_models, by = "site")

ggplot(bats_plot, aes(x = year, y = FF.juv, color = trend)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ site, scales = "free_y") +
  scale_color_manual(
    values = c(
      "positive" = "#2ca02c",
      "negative" = "#d62728",
      "no trend" = "#7f7f7f"
    )
  ) +
  labs(
    title    = "Vývoj počtu samic a mláďat podle lokality",
    x        = "Rok",
    y        = "Počet FF.juv",
    color    = "Trend"
  ) +
  theme_bw() +
  theme(
    strip.text   = element_text(face = "bold"),
    plot.title   = element_text(hjust = 0.5)
  )



# NETOPYRI 2023 ----

predslav <- read.csv2("predslav.csv")
dolany <- read.csv2("dolany.csv")
ejpovice <- read.csv2("ejpovice.csv")
manetin <- read.csv2("manetin.csv")
vseruby <- read.csv2("vseruby.csv")
radnice <- read.csv2("radnice.csv")


manetin_year <- ggplot(manetin, aes(x=rok, y=pocet)) + 
  geom_point() +
  geom_smooth(method = "glm") +
  ggtitle("Vývoj počtu samic a mláďat v Manětíně\n") +
  xlab("\nrok") +
  ylab("počet samic a mláďat\n") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    legend.title = element_text(),
    legend.position = c(1.125, 0.5),
    legend.text = element_text(size = 9),
    plot.margin = unit(c(1,3,1,1), units = , "cm")
  )
manetin_year <- manetin_year + theme(legend.title.align=0.5) 
manetin_year
ggsave(manetin_year, file = "manetin.png", width = 8, height = 5)

# Dolany
dolany_year <- ggplot(dolany, aes(x=rok, y=FF_juv)) + 
  geom_point() +
  geom_smooth(method = "glm") +
  ggtitle("Vývoj počtu samic a mláďat v Dolanech\n") +
  xlab("\nrok") +
  ylab("počet samic a mláďat\n") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    legend.title = element_text(),
    legend.position = c(1.125, 0.5),
    legend.text = element_text(size = 9),
    plot.margin = unit(c(1,3,1,1), units = , "cm")
  )
dolany_year <- dolany_year + theme(legend.title.align=0.5) 
dolany_year
ggsave(dolany_year, file = "dolany.png", width = 8, height = 5)

# Ejpovice
ejpovice_year <- ggplot(ejpovice, aes(x=rok, y=pocet)) + 
  geom_point() +
  geom_smooth(method = "glm") +
  ggtitle("Vývoj počtu samic a mláďat v Ejpovicích\n") +
  xlab("\nrok") +
  ylab("počet samic a mláďat\n") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    legend.title = element_text(),
    legend.position = c(1.125, 0.5),
    legend.text = element_text(size = 9),
    plot.margin = unit(c(1,3,1,1), units = , "cm")
  )
ejpovice_year <- ejpovice_year + theme(legend.title.align=0.5) 
ejpovice_year
ggsave(ejpovice_year, file = "ejpovice.png", width = 8, height = 5)

# Předslav
predslav_year <- ggplot(predslav, aes(x=rok, y=FF_juv)) + 
  geom_point() +
  geom_smooth(method = "glm") +
  ggtitle("Vývoj počtu samic a mláďat v Předslavi\n") +
  xlab("\nrok") +
  ylab("počet samic a mláďat\n") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    legend.title = element_text(),
    legend.position = c(1.125, 0.5),
    legend.text = element_text(size = 9),
    plot.margin = unit(c(1,3,1,1), units = , "cm")
  )
predslav_year <- predslav_year + theme(legend.title.align=0.5) 
predslav_year
ggsave(predslav_year, file = "predslav.png", width = 8, height = 5)

# Radnice
radnice_year <- ggplot(radnice, aes(x=rok, y=FF_juv)) + 
  geom_point() +
  geom_smooth(method = "glm") +
  ggtitle("Vývoj počtu samic a mláďat v Radnici\n") +
  xlab("\nrok") +
  ylab("počet samic a mláďat\n") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    legend.title = element_text(),
    legend.position = c(1.125, 0.5),
    legend.text = element_text(size = 9),
    plot.margin = unit(c(1,3,1,1), units = , "cm")
  )
radnice_year <- radnice_year + theme(legend.title.align=0.5) 
radnice_year
ggsave(radnice_year, file = "radnice.png", width = 8, height = 5)

# Všeruby
vseruby_year <- ggplot(vseruby, aes(x=rok, y=FF_juv)) + 
  geom_point() +
  geom_smooth(method = "glm") +
  ggtitle("Vývoj počtu samic a mláďat ve Všerubech\n") +
  xlab("\nrok") +
  ylab("počet samic a mláďat\n") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    legend.title = element_text(),
    legend.position = c(1.125, 0.5),
    legend.text = element_text(size = 9),
    plot.margin = unit(c(1,3,1,1), units = , "cm")
  )
vseruby_year <- vseruby_year + theme(legend.title.align=0.5) 
vseruby_year
ggsave(vseruby_year, file = "vseruby.png", width = 8, height = 5)

