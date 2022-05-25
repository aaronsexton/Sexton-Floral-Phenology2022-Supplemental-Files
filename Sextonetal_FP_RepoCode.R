

# Urbanization and plant diversity influence different aspects of floral phenology
# DOI: coming soon...
# Author: Aaron Sexton

### packages----
library(dplyr)
library(tidyverse)
library(vegan)
library(lme4)
library(ggplot2)
library(MuMIn)
library(RColorBrewer)
library(ggpubr)

# Running models----

# Master file - fp_master_2022
fp_master <- read.csv("../Data/FP/fp_master_2022.csv")
View(fp_master)

# Split by 2019 and 2018
fp_19 <- dplyr::filter(fp_master, Year == "2019")
fp_18 <- dplyr::filter(fp_master, Year == "2018")

# Split by seasonality
spring_19 <- dplyr::filter(fp_19, Seasonality == "Spring")
summer_19 <- dplyr::filter(fp_19, Seasonality == "Summer")
fall_19 <- dplyr::filter(fp_19, Seasonality == "Fall")
spring_18 <- dplyr::filter(fp_18, Seasonality == "Spring")
summer_18 <- dplyr::filter(fp_18, Seasonality == "Summer")
fall_18 <- dplyr::filter(fp_18, Seasonality == "Fall")


# Run models & Make dfs
options(na.action = "na.fail")

##  .Initiation----
# 2019 dfs
fp_19_init <- fp_19 %>% drop_na(Initiation)
spring_19_init <- dplyr::filter(fp_19_init, Seasonality == "Spring")
summer_19_init <- dplyr::filter(fp_19_init, Seasonality == "Summer")
fall_19_init <- dplyr::filter(fp_19_init, Seasonality == "Fall")

# 2018 dfs
fp_18_init <- fp_18 %>% drop_na(Initiation)
spring_18_init <- dplyr::filter(fp_18_init, Seasonality == "Spring")
summer_18_init <- dplyr::filter(fp_18_init, Seasonality == "Summer")
fall_18_init <- dplyr::filter(fp_18_init, Seasonality == "Fall")

# Initiation Full 2019
init19 <- lm(Initiation ~ Urbanization + Size + Richness, data = fp_19_init)
mi9 <- dredge(init19)
i9 <- model.avg(mi9, subset = delta < 4)
i9coefs <- summary(i9)$coefmat.full[,1]
i9ses <- summary(i9)$coefmat.full[,2]
vars <- c("Richness", "Urbanization","Size")
i9res <- data.frame("Vars" = vars, "Beta" = i9coefs[match(vars,names(i9coefs))],
                    "SE" = i9ses[match(vars,names(i9ses))])
i9res

# Initiation Spring 2019
init19_spring <- lm(Initiation ~ Urbanization + Size + Richness, 
                    data = spring_19_init)
mi9s <- dredge(init19_spring)
i9s <- model.avg(mi9s, subset = delta < 4)
i9scoefs <- summary(i9s)$coefmat.full[,1]
i9sses <- summary(i9s)$coefmat.full[,2]
i9sres <- data.frame("Vars" = vars, "Beta" = i9scoefs[match(vars,names(i9scoefs))],
                     "SE" = i9sses[match(vars,names(i9sses))])
i9sres

# Initiation Summer 2019
init19_summer <- lm(Initiation ~ Urbanization + Size + Richness, 
                    data = summer_19_init)
mi9su <- dredge(init19_summer)
i9su <- model.avg(mi9su, subset = delta < 4)
i9sucoefs <- summary(i9su)$coefmat.full[,1]
i9suses <- summary(i9su)$coefmat.full[,2]
i9sures <- data.frame("Vars" = vars, "Beta" = i9sucoefs[match(vars,names(i9sucoefs))],
                      "SE" = i9suses[match(vars,names(i9suses))])
i9sures

# Initiation Fall 2019
init19_fall <- lm(Initiation ~ Urbanization + Size + Richness, 
                  data = fall_19_init)
mi9f <- dredge(init19_fall)
i9f <- model.avg(mi9f, subset = delta < 4)
i9fcoefs <- summary(i9f)$coefmat.full[,1]
i9fses <- summary(i9f)$coefmat.full[,2]
i9fres <- data.frame("Vars" = vars, "Beta" = i9fcoefs[match(vars,names(i9fcoefs))],
                     "SE" = i9fses[match(vars,names(i9fses))])
i9fres

# --

# Initiation Full 2018
init18 <- lm(Initiation ~ Urbanization + Size + Richness, data = fp_18_init)
mi8 <- dredge(init18)
i8 <- model.avg(mi8, subset = delta < 4)
summary(i8)
i8coefs <- summary(i8)$coefmat.full[,1]
i8ses <- summary(i8)$coefmat.full[,2]
vars <- c("Richness", "Urbanization","Size")
i8res <- data.frame("Vars" = vars, "Beta" = i8coefs[match(vars,names(i8coefs))],
                    "SE" = i8ses[match(vars,names(i8ses))])
i8res

# Initiation Spring 2018
init18_spring <- lm(Initiation ~ Urbanization + Size + Richness, 
                    data = spring_18_init)
mi8s <- dredge(init18_spring)
i8s <- model.avg(mi8s, subset = delta < 4)
i8scoefs <- summary(i8s)$coefmat.full[,1]
i8sses <- summary(i8s)$coefmat.full[,2]
i8sres <- data.frame("Vars" = vars, "Beta" = i8scoefs[match(vars,names(i8scoefs))],
                     "SE" = i8sses[match(vars,names(i8sses))])
i8sres

# Initiation Summer 2018
init18_summer <- lm(Initiation ~ Urbanization + Size + Richness, 
                    data = summer_18_init)
mi8su <- dredge(init18_summer)
i8su <- model.avg(mi8su, subset = delta < 4)
i8sucoefs <- summary(i8su)$coefmat.full[,1]
i8suses <- summary(i8su)$coefmat.full[,2]
i8sures <- data.frame("Vars" = vars, "Beta" = i8sucoefs[match(vars,names(i8sucoefs))],
                      "SE" = i8suses[match(vars,names(i8suses))])
i8sures

# Initiation Fall 2018
init18_fall <- lm(Initiation ~ Urbanization + Size + Richness, 
                  data = fall_18_init)
mi8f <- dredge(init18_fall)
i8f <- model.avg(mi8f, subset = delta < 4)
i8fcoefs <- summary(i8f)$coefmat.full[,1]
i8fses <- summary(i8f)$coefmat.full[,2]
i8fres <- data.frame("Vars" = vars, "Beta" = i8fcoefs[match(vars,names(i8fcoefs))],
                     "SE" = i8fses[match(vars,names(i8fses))])
i8fres


# --



##  .Duration ----

# 2019 dfs
fp_19_dur <- fp_19 %>% drop_na(Duration)
spring_19_dur <- dplyr::filter(fp_19_dur, Seasonality == "Spring")
summer_19_dur <- dplyr::filter(fp_19_dur, Seasonality == "Summer")
fall_19_dur <- dplyr::filter(fp_19_dur, Seasonality == "Fall")

# 2018 dfs
fp_18_dur <- fp_18 %>% drop_na(Duration)
spring_18_dur <- dplyr::filter(fp_18_dur, Seasonality == "Spring")
summer_18_dur <- dplyr::filter(fp_18_dur, Seasonality == "Summer")
fall_18_dur <- dplyr::filter(fp_18_dur, Seasonality == "Fall")

# Duration Full 2019
dur19 <- lm(Duration ~ Urbanization + Size + Richness, data = fp_19_dur)
md9 <- dredge(dur19)
d9 <- model.avg(md9, subset = delta < 4)
summary(d9)
d9coefs <- summary(d9)$coefmat.full[,1]
d9ses <- summary(d9)$coefmat.full[,2]
d9res <- data.frame("Vars" = vars, "Beta" = d9coefs[match(vars,names(d9coefs))],
                    "SE" = d9ses[match(vars,names(d9ses))])
d9res

# Duration Spring 2019
dur19_spring <- lm(Duration ~ Urbanization + Size + Richness, 
                   data = spring_19_dur)
md9s <- dredge(dur19_spring)
d9s <- model.avg(md9s, subset = delta < 4)
d9scoefs <- summary(d9s)$coefmat.full[,1]
d9sses <- summary(d9s)$coefmat.full[,2]
d9sres <- data.frame("Vars" = vars, "Beta" = d9scoefs[match(vars,names(d9scoefs))],
                     "SE" = d9sses[match(vars,names(d9sses))])
d9sres

# Duration Summer 2019
dur19_summer <- lm(Duration ~ Urbanization + Size + Richness, 
                   data = summer_19_dur)
md9su <- dredge(dur19_summer)
d9su <- model.avg(md9su, subset = delta < 4)
d9sucoefs <- summary(d9su)$coefmat.full[,1]
d9suses <- summary(d9su)$coefmat.full[,2]
d9sures <- data.frame("Vars" = vars, "Beta" = d9sucoefs[match(vars,names(d9sucoefs))],
                      "SE" = d9suses[match(vars,names(d9suses))])
d9sures

# Duration Fall 2019
dur19_fall <- lm(Duration ~ Urbanization + Size + Richness, 
                 data = fall_19_dur)
md9f <- dredge(dur19_fall)
d9f <- model.avg(md9f, subset = delta < 4)
d9fcoefs <- summary(d9f)$coefmat.full[,1]
d9fses <- summary(d9f)$coefmat.full[,2]
d9fres <- data.frame("Vars" = vars, "Beta" = d9fcoefs[match(vars,names(d9fcoefs))],
                     "SE" = d9fses[match(vars,names(d9fses))])
d9fres

# --

# Duration Full 2018
dur18 <- lm(Duration ~ Urbanization + Size + Richness, data = fp_18_dur)
md8 <- dredge(dur18)
d8 <- model.avg(md8, subset = delta < 4)
summary(d8)
d8coefs <- summary(d8)$coefmat.full[,1]
d8ses <- summary(d8)$coefmat.full[,2]
d8res <- data.frame("Vars" = vars, "Beta" = d8coefs[match(vars,names(d8coefs))],
                    "SE" = d8ses[match(vars,names(d8ses))])
d8res

# Duration Spring 2018
dur18_spring <- lm(Duration ~ Urbanization + Size + Richness, 
                   data = spring_18_dur)
md8s <- dredge(dur18_spring)
d8s <- model.avg(md8s, subset = delta < 4)
d8scoefs <- summary(d8s)$coefmat.full[,1]
d8sses <- summary(d8s)$coefmat.full[,2]
d8sres <- data.frame("Vars" = vars, "Beta" = d8scoefs[match(vars,names(d8scoefs))],
                     "SE" = d8sses[match(vars,names(d8sses))])
d8sres

# Duration Summer 2018
dur18_summer <- lm(Duration ~ Urbanization + Size + Richness, 
                   data = summer_18_dur)
md8su <- dredge(dur18_summer)
d8su <- model.avg(md8su, subset = delta < 4)
d8sucoefs <- summary(d8su)$coefmat.full[,1]
d8suses <- summary(d8su)$coefmat.full[,2]
d8sures <- data.frame("Vars" = vars, "Beta" = d8sucoefs[match(vars,names(d8sucoefs))],
                      "SE" = d8suses[match(vars,names(d8suses))])
d8sures

# Duration Fall 2018
dur18_fall <- lm(Duration ~ Urbanization + Size + Richness, 
                 data = fall_18_dur)
md8f <- dredge(dur18_fall)
d8f <- model.avg(md8f, subset = delta < 4)
d8fcoefs <- summary(d8f)$coefmat.full[,1]
d8fses <- summary(d8f)$coefmat.full[,2]
d8fres <- data.frame("Vars" = vars, "Beta" = d8fcoefs[match(vars,names(d8fcoefs))],
                     "SE" = d8fses[match(vars,names(d8fses))])
d8fres



# --




##  .Peak ----

# 2019 dfs
fp_19_pk <- fp_19 %>% drop_na(Peak)
spring_19_pk <- dplyr::filter(fp_19_pk, Seasonality == "Spring")
summer_19_pk <- dplyr::filter(fp_19_pk, Seasonality == "Summer")
fall_19_pk <- dplyr::filter(fp_19_pk, Seasonality == "Fall")

# 2018 dfs
fp_18_pk <- fp_18 %>% drop_na(Peak)
spring_18_pk <- dplyr::filter(fp_18_pk, Seasonality == "Spring")
summer_18_pk<- dplyr::filter(fp_18_pk, Seasonality == "Summer")
fall_18_pk <- dplyr::filter(fp_18_pk, Seasonality == "Fall")

# Peak Full 2019
pk19 <- lm(Peak ~ Urbanization + Size + Richness, data = fp_19_pk)
mp9 <- dredge(pk19)
p9 <- model.avg(mp9, subset = delta < 4)
summary(p9)
p9coefs <- summary(p9)$coefmat.full[,1]
p9ses <- summary(p9)$coefmat.full[,2]
p9res <- data.frame("Vars" = vars, "Beta" = p9coefs[match(vars,names(p9coefs))],
                    "SE" = p9ses[match(vars,names(p9ses))])
p9res

# Peak Spring 2019
pk19_spring <- lm(Peak ~ Urbanization + Size + Richness, 
                  data = spring_19_pk)
mp9s <- dredge(pk19_spring)
p9s <- model.avg(mp9s, subset = delta < 4)
p9scoefs <- summary(p9s)$coefmat.full[,1]
p9sses <- summary(p9s)$coefmat.full[,2]
p9sres <- data.frame("Vars" = vars, "Beta" = p9scoefs[match(vars,names(p9scoefs))],
                     "SE" = p9sses[match(vars,names(p9sses))])
p9sres

# Peak Summer 2019
pk19_summer <- lm(Peak ~ Urbanization + Size + Richness, 
                  data = summer_19_pk)
mp9su <- dredge(pk19_summer)
p9su <- model.avg(mp9su, subset = delta < 4)
p9sucoefs <- summary(p9su)$coefmat.full[,1]
p9suses <- summary(p9su)$coefmat.full[,2]
p9sures <- data.frame("Vars" = vars, "Beta" = p9sucoefs[match(vars,names(p9sucoefs))],
                      "SE" = p9suses[match(vars,names(p9suses))])
p9sures

# Peak Fall 2019
pk19_fall <- lm(Peak ~ Urbanization + Size + Richness, 
                data = fall_19_pk)
mp9f <- dredge(pk19_fall)
p9f <- model.avg(mp9f, subset = delta < 4)
p9fcoefs <- summary(p9f)$coefmat.full[,1]
p9fses <- summary(p9f)$coefmat.full[,2]
p9fres <- data.frame("Vars" = vars, "Beta" = p9fcoefs[match(vars,names(p9fcoefs))],
                     "SE" = p9fses[match(vars,names(p9fses))])
p9fres

# --
# Peak Full 2018
pk18 <- lm(Peak ~ Urbanization + Size + Richness, data = fp_18_pk)
mp8 <- dredge(pk18)
p8 <- model.avg(mp8, subset = delta < 4)
summary(p8)
p8coefs <- summary(p8)$coefmat.full[,1]
p8ses <- summary(p8)$coefmat.full[,2]
p8res <- data.frame("Vars" = vars, "Beta" = p8coefs[match(vars,names(p8coefs))],
                    "SE" = p8ses[match(vars,names(p8ses))])
p8res

# Peak Spring 2018
pk18_spring <- lm(Peak ~ Urbanization + Size + Richness, 
                  data = spring_18_pk)
mp8s <- dredge(pk18_spring)
p8s <- model.avg(mp8s, subset = delta < 4)
p8scoefs <- summary(p8s)$coefmat.full[,1]
p8sses <- summary(p8s)$coefmat.full[,2]
p8sres <- data.frame("Vars" = vars, "Beta" = p8scoefs[match(vars,names(p8scoefs))],
                     "SE" = p8sses[match(vars,names(p8sses))])
p8sres

# Peak Summer 2018
pk18_summer <- lm(Peak ~ Urbanization + Size + Richness, 
                  data = summer_18_pk)
mp8su <- dredge(pk18_summer)
p8su <- model.avg(mp8su, subset = delta < 4)
p8sucoefs <- summary(p8su)$coefmat.full[,1]
p8suses <- summary(p8su)$coefmat.full[,2]
p8sures <- data.frame("Vars" = vars, "Beta" = p8sucoefs[match(vars,names(p8sucoefs))],
                      "SE" = p8suses[match(vars,names(p8suses))])
p8sures

# Peak Fall 2018
pk18_fall <- lm(Peak ~ Urbanization + Size + Richness, 
                data = fall_18_pk)
mp8f <- dredge(pk18_fall)
p8f <- model.avg(mp8f, subset = delta < 4)
p8fcoefs <- summary(p8f)$coefmat.full[,1]
p8fses <- summary(p8f)$coefmat.full[,2]
p8fres <- data.frame("Vars" = vars, "Beta" = p8fcoefs[match(vars,names(p8fcoefs))],
                     "SE" = p8fses[match(vars,names(p8fses))])
p8fres



# Plotting ----

# Results dfs
i9res
i9sres
i9sures
i9fres
d9res
d9sres
d9sures
d9fres
p9res
p9sres
p9sures
p9fres
i8res
i8sres
i8sures
i8fres
d8res
d8sres
d8sures
d8fres
p8res
p8sres
p8sures
p8fres

# Combined into one df 
map <- read.csv("../Data/FP/map_final_2.csv")
View(map)
fulltry <- ggplot(data = map, aes(x = Response, y = Importance)) +
  geom_point(aes(size = Significance, color = Season, shape = Predictors), position = "jitter") +
  theme_classic() +
  coord_flip() +
  scale_color_brewer(palette="Dark2") +
  geom_hline(aes(), yintercept = 0, linetype = 3) +
  xlab("")  +
  geom_linerange(aes(ymin = Importance - SE, ymax = Importance + SE),
                 width = 0.1, cex = 0.3, color = "honeydew4", position = "jitter") +
  scale_size(breaks = waiver())
fulltry


# Too messy

# Lets break it up
map$sznresp <- paste(map$Season,map$Response)

aicma19 <- dplyr::filter(map, Year == "2019")
aic19urban <- dplyr::filter(aicma19, Predictors == "Urban ")
aic19div <- dplyr::filter(aicma19, Predictors == "Richness ")
aic19size <- dplyr::filter(aicma19, Predictors == "Size ")
aicma18 <- dplyr::filter(map, Year == "2018")
aic18urban <- dplyr::filter(aicma18, Predictors == "Urban ")
aic18div <- dplyr::filter(aicma18, Predictors == "Richness ")
aic18size <- dplyr::filter(aicma18, Predictors == "Size ")

# Re-order the response column to show in the order we want
aic19urban$sznresp <- factor(aic19urban$sznresp, 
                             levels = c("Full Duration", "Fall Duration",
                                        "Summer Duration", "Spring Duration",
                                        "Full Peak", "Fall Peak",
                                        "Summer Peak", "Spring Peak",
                                        "Full Initiation", "Fall Initiation",
                                        "Summer Initiation", "Spring Initiation"))

# ggplot it
# Urban 2019
urban19gg <- ggplot(data = aic19urban, aes(x = sznresp, y = Importance)) +
  geom_point(aes(size = Significance, color = Response)) +
  theme_classic() +
  coord_flip()

ramp <- c("#2ca25f","#2ca25f","#2ca25f","#2ca25f",
          "#d95f0e","#d95f0e", "#d95f0e","#d95f0e",
          "#756bb1", "#756bb1","#756bb1","#756bb1")

urban19gg <- urban19gg + scale_color_brewer(palette="Dark2") +
  geom_hline(aes(), yintercept = 0, linetype = 3) +
  xlab("") + ylab("Urbanization") +
  geom_linerange(aes(ymin = Importance - SE, ymax = Importance + SE),
                 width = 0.1, cex = 0.3, color = ramp) +
  scale_size(breaks = waiver()) + scale_fill_discrete(limits = c("Initiation",
                                                                 "Peak",
                                                                 "Duration"))+
  theme(axis.text.y = element_text(size = 12)) +
  scale_x_discrete(labels = c("Spring Initiation" = "Spring", "Summer Initiation" = "Summer",
                              "Fall Initiation" = "Fall", "Full Initiation" = "Full",
                              "Spring Peak" = "Spring", "Summer Peak" = "Summer",
                              "Fall Peak" = "Fall", "Full Peak" = "Full",
                              "Spring Duration" = "Spring", "Summer Duration" = "Summer",
                              "Fall Duration" = "Fall", "Full Duration" = "Full")) +
  scale_size(guide = FALSE) +
  theme(axis.title.x = element_text(size = 22, face = "bold"))
urban19gg
# Looks good, lets now plot them all together

# Diversity 2019
View(aic19div)
aic19div$sznresp <- factor(aic19div$sznresp, 
                           levels = c("Full Duration", "Fall Duration",
                                      "Summer Duration", "Spring Duration",
                                      "Full Peak", "Fall Peak",
                                      "Summer Peak", "Spring Peak",
                                      "Full Initiation", "Fall Initiation",
                                      "Summer Initiation", "Spring Initiation"))


div19gg <- ggplot(data = aic19div, aes(x = sznresp, y = Importance)) +
  geom_point(aes(size = Significance, color = Response)) +
  theme_classic() +
  coord_flip() + 
  scale_color_brewer(palette="Dark2") +
  geom_hline(aes(), yintercept = 0, linetype = 3) +
  xlab("") + ylab("Richness") +
  geom_linerange(aes(ymin = Importance - SE, ymax = Importance + SE),
                 width = 0.1, cex = 0.3, color = ramp) +
  scale_size(breaks = waiver()) + scale_fill_discrete(limits = c("Initiation",
                                                                 "Peak",
                                                                 "Duration"))+
  theme(axis.text.y = element_blank()) +
  theme(axis.title.x = element_text(size = 22, face = "bold"))
div19gg

figure19 <- ggarrange(urban19gg, div19gg, labels = c("2019", ""), ncol = 2, nrow = 1,
                      common.legend = TRUE)
figure19

# Size 2019
View(aic19size)
aic19size$sznresp <- factor(aic19size$sznresp, 
                            levels = c("Full Duration", "Fall Duration",
                                       "Summer Duration", "Spring Duration",
                                       "Full Peak", "Fall Peak",
                                       "Summer Peak", "Spring Peak",
                                       "Full Initiation", "Fall Initiation",
                                       "Summer Initiation", "Spring Initiation"))


size19gg <- ggplot(data = aic19size, aes(x = sznresp, y = Importance)) +
  geom_point(aes(color = Response)) +
  theme_classic() +
  coord_flip() + 
  scale_color_brewer(palette="Dark2") +
  geom_hline(aes(), yintercept = 0, linetype = 3) +
  xlab("") + ylab("Size") +
  geom_linerange(aes(ymin = Importance - SE, ymax = Importance + SE),
                 width = 0.1, cex = 0.3, color = ramp) +
  scale_size(breaks = waiver()) + scale_fill_discrete(limits = c("Initiation",
                                                                 "Peak",
                                                                 "Duration"))+
  theme(axis.text.y = element_blank()) +
  theme(axis.title.x = element_text(size = 22, face = "bold"))
size19gg

figure19 <- ggarrange(urban19gg, div19gg, size19gg, labels = c("2019", "", ""), ncol = 3, nrow = 1,
                      common.legend = TRUE)
figure19


# Urban 2018
View(aic18urban)
aic18urban$sznresp <- factor(aic18urban$sznresp, 
                             levels = c("Full Duration", "Fall Duration",
                                        "Summer Duration", "Spring Duration",
                                        "Full Peak", "Fall Peak",
                                        "Summer Peak", "Spring Peak",
                                        "Full Initiation", "Fall Initiation",
                                        "Summer Initiation", "Spring Initiation"))


urban18gg <- ggplot(data = aic18urban, aes(x = sznresp, y = Importance)) +
  geom_point(aes(size = Significance, color = Response)) +
  theme_classic() +
  coord_flip() + 
  scale_color_brewer(palette="Dark2") +
  geom_hline(aes(), yintercept = 0, linetype = 3) +
  xlab("") + ylab("Urbanization") +
  geom_linerange(aes(ymin = Importance - SE, ymax = Importance + SE),
                 width = 0.1, cex = 0.3, color = ramp) +
  scale_size(breaks = waiver()) + scale_fill_discrete(limits = c("Initiation",
                                                                 "Peak",
                                                                 "Duration")) +
  theme(axis.text.y = element_text(size = 12)) +
  scale_x_discrete(labels = c("Spring Initiation" = "Spring", "Summer Initiation" = "Summer",
                              "Fall Initiation" = "Fall", "Full Initiation" = "Full",
                              "Spring Peak" = "Spring", "Summer Peak" = "Summer",
                              "Fall Peak" = "Fall", "Full Peak" = "Full",
                              "Spring Duration" = "Spring", "Summer Duration" = "Summer",
                              "Fall Duration" = "Fall", "Full Duration" = "Full")) +
  theme(axis.title.x = element_text(size = 22, face = "bold"))
urban18gg

# Diversity 2018
View(aic18div)
aic18div$sznresp <- factor(aic18div$sznresp, 
                           levels = c("Full Duration", "Fall Duration",
                                      "Summer Duration", "Spring Duration",
                                      "Full Peak", "Fall Peak",
                                      "Summer Peak", "Spring Peak",
                                      "Full Initiation", "Fall Initiation",
                                      "Summer Initiation", "Spring Initiation"))


div18gg <- ggplot(data = aic18div, aes(x = sznresp, y = Importance)) +
  geom_point(aes(size = Significance, color = Response)) +
  theme_classic() +
  coord_flip() + 
  scale_color_brewer(palette="Dark2") +
  geom_hline(aes(), yintercept = 0, linetype = 3) +
  xlab("") + ylab("Richness") +
  geom_linerange(aes(ymin = Importance - SE, ymax = Importance + SE),
                 width = 0.1, cex = 0.3, color = ramp) +
  scale_size(breaks = waiver()) + scale_fill_discrete(limits = c("Initiation",
                                                                 "Peak",
                                                                 "Duration"))+
  theme(axis.text.y = element_blank()) +
  theme(axis.title.x = element_text(size = 22, face = "bold"))
div18gg

# Size 2018
View(aic18size)
aic18size$sznresp <- factor(aic18size$sznresp, 
                            levels = c("Full Duration", "Fall Duration",
                                       "Summer Duration", "Spring Duration",
                                       "Full Peak", "Fall Peak",
                                       "Summer Peak", "Spring Peak",
                                       "Full Initiation", "Fall Initiation",
                                       "Summer Initiation", "Spring Initiation"))


size18gg <- ggplot(data = aic18size, aes(x = sznresp, y = Importance)) +
  geom_point(aes(color = Response)) +
  theme_classic() +
  coord_flip() + 
  scale_color_brewer(palette="Dark2") +
  geom_hline(aes(), yintercept = 0, linetype = 3) +
  xlab("") + ylab("Size") +
  geom_linerange(aes(ymin = Importance - SE, ymax = Importance + SE),
                 width = 0.1, cex = 0.3, color = ramp) +
  scale_size(breaks = waiver()) + scale_fill_discrete(limits = c("Initiation",
                                                                 "Peak",
                                                                 "Duration")) +
  theme(axis.text.y = element_blank()) +
  theme(axis.title.x = element_text(size = 22, face = "bold"))
size18gg


# Put them all together

figure <- ggarrange(urban18gg, div18gg, size18gg, urban19gg, div19gg, size19gg,
                    labels = c("2018", "", "", "2019", "", ""),
                    ncol = 3, nrow = 2, common.legend = TRUE, 
                    font.label = list(size = 20, face = "bold"),
                    vjust = 0.95, hjust = -6,
                    legend = "right")

# Looks great!
# legend a little sloppy, so can clean up by using just one of the graphs
# then paste it in

div18gg + guides(colour = guide_legend(override.aes = list(size=5))) +
  theme(legend.text = element_text(size = 18)) + theme(legend.title = element_text(size = 18, face = "bold"))






