rm(list = ls())
options(scipen=999)
setwd("/Users/christianbaehr/Dropbox/Resources/Data/")


library(tidyverse)
library(formula.tools)
library(fixest)
library(modelsummary)
library(ggplot2)
library(marginaleffects)
library(mice)
library(panelView)
library(fect)


panel <- read.csv("Processed/panel_cy.csv", stringsAsFactors=F)
panel <- panel[which(!panel$iso3c %in% c("USA", "CAN", "CHN")) , ]
panel <- panel[!duplicated(panel[ , c("iso3c", "year")]) , ]
#arab_spring <- c("TUN", "EGY", "LBY", "SYR", "YEM", "BHR", "MAR", "JOR", "DZA", "OMN", "SDN")

panel$ln_gdp_percap_2015usd_l1 <- log(panel$gdp_percap_2015usd_l1)


panel <- panel %>%
  group_by(iso3c) %>%
  filter(year<2010) %>%
  summarize(mean_pca_pre2010 = mean(pca, na.rm=T),
            median_pca_pre2010 = median(pca, na.rm=T),
            mean_oilrevenuepc_pre2010 = mean(oil_revenue_pc, na.rm=T)) %>%
  merge(panel, all.y=T)
#hist(panel$mean_pca_pre2010[which(panel$year==2009)])

panel <- panel %>%
  group_by(iso3c) %>%
  filter(year==2009) %>%
  summarize(v2xpolyarchy_baseline = v2x_polyarchy,
            polity2_baseline = polity2) %>%
  merge(panel, by="iso3c", all.y=T)
#panel$v2x_polyarchy_baseline[which(panel$iso3c=="VEN")]
#panel$v2x_polyarchy[which(panel$iso3c=="VEN" & panel$year==2009)]


median_cutoff <- median(panel$mean_pca_pre2010[which(panel$year==2009)], na.rm=T)

## A higher PCA score corresponds to a heavier crude
panel$treatment_group <- (panel$mean_pca_pre2010 < median_cutoff) * 1
panel$treated <- panel$treatment_group * (panel$year >= 2010)

sum(panel$treatment_group==1 & panel$year==2009, na.rm=T) # number treated
sum(panel$treatment_group==0 & panel$year==2009, na.rm=T) # number control

summary(panel$mean_oilrevenuepc_pre2010[which(panel$treatment_group==1)])
summary(panel$mean_oilrevenuepc_pre2010[which(panel$treatment_group==0)])

unique(panel$cname[which(panel$treatment_group==1)])
unique(panel$cname[which(panel$treatment_group==0)])

panel$baseline_autocracy <- panel$polity2_baseline <= (-5)
panel$baseline_democracy <- panel$polity2_baseline >= (5)

out.fect <- fect(v2x_polyarchy ~ treated, data = panel, index = c("iso3c", "year"), method = "fe", force = "two-way",
                 na.rm=T, se=T, alpha=0.10)
plot(out.fect, type = "gap")

out.fect <- fect(v2x_polyarchy ~ treated, data = panel[which(panel$baseline_autocracy) , ], index = c("iso3c", "year"), method = "fe", force = "two-way",
                 na.rm=T, se=T, alpha=0.10)
plot(out.fect, type = "gap")
out.fect <- fect(v2x_polyarchy ~ treated, data = panel[which(panel$baseline_democracy) , ], index = c("iso3c", "year"), method = "fe", force = "two-way",
                 na.rm=T, se=T, alpha=0.10)
plot(out.fect, type = "gap")


#feols(v2x_polyarchy ~ treated | iso3c + year, data=panel, vcov=~iso3c)



out.fect <- fect(v2x_polyarchy ~ treated, data = panel[which(panel$baseline_autocracy) , ], index = c("iso3c", "year"), method = "fe", force = "two-way",
                 na.rm=T, se=T, alpha=0.05)
plot(out.fect, type = "gap")


out.fect <- fect(v2x_polyarchy ~ treated, data = panel[which(panel$baseline_democracy) , ], index = c("iso3c", "year"), method = "fe", force = "two-way",
                 na.rm=T, se=T, alpha=0.05)
plot(out.fect, type = "gap")

tt <- feols(v2x_polyarchy ~ i(year, treatment_group, 2010) + mean_oilrevenuepc_pre2010 + ln_gdp_percap_2015usd_l1 | year, data=panel, cluster=~iso3c)
iplot(tt, xlab = " ", ylab = "Effect on Y", main = " ", grid=F,
      ref.line.par=list(col="red", lty=2, lwd=2), ci.width=0, ci_level=0.95)

gdp_growth_pct_l1 + ln_gdp_percap_2015usd_l1 + 
  trade_pctgdp_l1 + pop_growth_pct_l1


tt <- feols(v2x_polyarchy ~ i(year, mean_oilrevenuepc_pre2010, 2010) | iso3c+year, data=panel, cluster=~iso3c)
iplot(tt, xlab = " ", ylab = "Effect on Y", main = " ", grid=F,
      ref.line.par=list(col="red", lty=2, lwd=2), ci.width=0, ci_level=0.95)





