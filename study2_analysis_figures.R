### Study 2 in "Few science and engineering instructors come out to students, despite potential benefits"
###Audit study of instructor coming out

library(tidyr)
library(dplyr)
library(ggplot2)
library(car)
library(nnet)
library(reghelper)
library(forcats)

my_data <- read.csv("study2_deid_data.csv")

#filter out students who checked reveal lgbtq but did not see lgbtq video
my_data <- my_data[-which(my_data$checked_lgbtq == 1 & my_data$trt_grp == 0),]
#filter out students who did not check reveal lgbtq but saw lgbtq video
my_data <- my_data[-which(is.na(my_data$checked_lgbtq) & my_data$trt_grp == 1),]

#set reference groups
my_data$religion3 <- factor(my_data$religion3, 
                            levels = c("nonreligious", "otherreligion",
                                       "christian"))
my_data$religion3 <- relevel(my_data$religion3, ref = "nonreligious")

### creating df of all regression results - main effects ----

main_mod_out <- do.call(rbind, lapply(c("hireability_mean", "competence_mean", 
                                        "likeability_mean", "homophily_mean",
                                        "rapport_mean", "approachability_mean"),
                                      function(x){
                                        mod_out <- as.data.frame(summary(
                                          lm(formula = as.formula(
                                            paste0(x, "~ trt_grp + gender3 + religion3 + lgbtq2")),
                                            data = my_data))$coefficients[,-3])
                                        colnames(mod_out) <- c("est", "se", "pval")
                                        mod_out$outcome <- x
                                        mod_out$predictor <- rownames(mod_out)
                                        rownames(mod_out) <- NULL
                                        std_mod_out <- as.data.frame(beta(
                                          lm(formula = as.formula(
                                            paste0(x, "~ trt_grp + gender3 + religion3 + lgbtq2")),
                                            data = my_data))$coefficients[,-3])
                                        colnames(std_mod_out) <- c("std_est", "std_se", "std_pval")
                                        std_mod_out$outcomeb <- x
                                        std_mod_out$predictorb <- rownames(std_mod_out)
                                        rownames(std_mod_out) <- NULL
                                        combo <- cbind(mod_out, std_mod_out)
                                        #combo <- combo %>% select(outcome, predictor, everything())
                                        return(combo)
                                      }))

### creating df of all regression results - interactive effects -----

int_mod_out <- do.call(rbind, lapply(c("hireability_mean", "competence_mean", 
                                       "likeability_mean", "homophily_mean",
                                       "rapport_mean", "approachability_mean"),
                                     function(x){
                                       mod_out <- as.data.frame(summary(
                                         lm(formula = as.formula(
                                           paste0(x, "~ trt_grp*gender3 + 
                                                   trt_grp*religion3 + trt_grp*lgbtq2")),
                                           data = my_data))$coefficients[,-3])
                                       colnames(mod_out) <- c("est", "se", "pval")
                                       mod_out$outcome <- x
                                       mod_out$predictor <- rownames(mod_out)
                                       rownames(mod_out) <- NULL
                                       std_mod_out <- as.data.frame(beta(
                                         lm(formula = as.formula(
                                           paste0(x, "~ trt_grp*gender3 + 
                                                   trt_grp*religion3 + trt_grp*lgbtq2")),
                                           data = my_data))$coefficients[,-3])
                                       colnames(std_mod_out) <- c("std_est", "std_se", "std_pval")
                                       std_mod_out$outcomeb <- x
                                       std_mod_out$predictorb <- rownames(std_mod_out)
                                       rownames(std_mod_out) <- NULL
                                       combo <- cbind(mod_out, std_mod_out)
                                       combo <- combo %>% select(outcome, predictor, everything())
                                       return(combo)
                                     }))

### creating df of disaggregated means and sd for each outcome ----

#gender, religion, lgbtq

gender_outcome_fxn <- function(x){
  tmp.data <- my_data[my_data$gender3 == "man",]
  tmp <- data.frame(mean(tmp.data[[x]], na.rm = T))
  colnames(tmp) <- c("man_mean")
  tmp$man_sd <- sd(tmp.data[[x]], na.rm = T)
  tmp$outcome <- x
  tmp.data2 <- my_data[my_data$gender3 == "not_man",]
  tmp2 <- data.frame(mean(tmp.data2[[x]], na.rm = T))
  colnames(tmp2) <- c("not.man_mean")
  tmp2$not.man_sd <- sd(tmp.data2[[x]], na.rm = T)
  tmp2$outcome <- x
  comb <- merge(tmp, tmp2, by = "outcome")
  return(comb)
}

disagg_gender <- do.call(rbind, lapply(c("hireability_mean", "competence_mean", 
                                         "likeability_mean", "homophily_mean",
                                         "rapport_mean", "approachability_mean"),
                                       gender_outcome_fxn))


religion_outcome_fxn <- function(x){
  tmp.data <- my_data[my_data$religion3 == "christian",]
  tmp <- data.frame(mean(tmp.data[[x]], na.rm = T))
  colnames(tmp) <- c("christian_mean")
  tmp$christian_sd <- sd(tmp.data[[x]], na.rm = T)
  tmp$outcome <- x
  tmp.data2 <- my_data[my_data$religion3 == "otherreligion",]
  tmp2 <- data.frame(mean(tmp.data2[[x]], na.rm = T))
  colnames(tmp2) <- c("otherreligion_mean")
  tmp2$otherreligion_sd <- sd(tmp.data2[[x]], na.rm = T)
  tmp2$outcome <- x
  comb <- merge(tmp, tmp2, by = "outcome")
  tmp.data3 <- my_data[my_data$religion3 == "nonreligious",]
  tmp3 <- data.frame(mean(tmp.data3[[x]], na.rm = T))
  colnames(tmp3) <- c("nonreligious_mean")
  tmp3$nonreligious_sd <- sd(tmp.data3[[x]], na.rm = T)
  tmp3$outcome <- x
  comb2 <- merge(comb, tmp3, by = "outcome")
  return(comb2)
}

disagg_religion <- do.call(rbind, lapply(c("hireability_mean", "competence_mean", 
                                           "likeability_mean", "homophily_mean",
                                           "rapport_mean", "approachability_mean"),
                                         religion_outcome_fxn))


lgbtq_outcome_fxn <- function(x){
  tmp.data <- my_data[my_data$lgbtq2 == "no",]
  tmp <- data.frame(mean(tmp.data[[x]], na.rm = T))
  colnames(tmp) <- c("nonlgbtq_mean")
  tmp$nonlgbtq_sd <- sd(tmp.data[[x]], na.rm = T)
  tmp$outcome <- x
  tmp.data2 <- my_data[my_data$lgbtq2 == "yes",]
  tmp2 <- data.frame(mean(tmp.data2[[x]], na.rm = T))
  colnames(tmp2) <- c("lgbtq_mean")
  tmp2$lgbtq_sd <- sd(tmp.data2[[x]], na.rm = T)
  tmp2$outcome <- x
  comb <- merge(tmp, tmp2, by = "outcome")
  return(comb)
}

disagg_lgbtq <- do.call(rbind, lapply(c("hireability_mean", "competence_mean", 
                                        "likeability_mean", "homophily_mean",
                                        "rapport_mean", "approachability_mean"),
                                      lgbtq_outcome_fxn))
### creating df for regression class perceptions - main effects ----

class_main_mod_out <- do.call(rbind, lapply(c("cohesion_sob_mean", "cohesion_fom_mean", 
                                              "class_comfort_mean"),
                                            function(x){
                                              mod_out <- as.data.frame(summary(
                                                lm(formula = as.formula(
                                                  paste0(x, "~ trt_grp + gender3 + religion3 + lgbtq2")),
                                                  data = my_data))$coefficients[,-3])
                                              colnames(mod_out) <- c("est", "se", "pval")
                                              mod_out$outcome <- x
                                              mod_out$predictor <- rownames(mod_out)
                                              rownames(mod_out) <- NULL
                                              std_mod_out <- as.data.frame(beta(
                                                lm(formula = as.formula(
                                                  paste0(x, "~ trt_grp + gender3 + religion3 + lgbtq2")),
                                                  data = my_data))$coefficients[,-3])
                                              colnames(std_mod_out) <- c("std_est", "std_se", "std_pval")
                                              std_mod_out$outcomeb <- x
                                              std_mod_out$predictorb <- rownames(std_mod_out)
                                              rownames(std_mod_out) <- NULL
                                              combo <- cbind(mod_out, std_mod_out)
                                              combo <- combo %>% select(outcome, predictor, everything())
                                              return(combo)
                                            }))


### creating df for regression class perceptions - interactive effects -----

class_int_mod_out <- do.call(rbind, lapply(c("cohesion_sob_mean", "cohesion_fom_mean", 
                                             "class_comfort_mean"),
                                           function(x){
                                             mod_out <- as.data.frame(summary(
                                               lm(formula = as.formula(
                                                 paste0(x, "~ trt_grp*gender3 + 
                                                   trt_grp*religion3 + trt_grp*lgbtq2")),
                                                 data = my_data))$coefficients[,-3])
                                             colnames(mod_out) <- c("est", "se", "pval")
                                             mod_out$outcome <- x
                                             mod_out$predictor <- rownames(mod_out)
                                             rownames(mod_out) <- NULL
                                             std_mod_out <- as.data.frame(beta(
                                               lm(formula = as.formula(
                                                 paste0(x, "~ trt_grp*gender3 + 
                                                   trt_grp*religion3 + trt_grp*lgbtq2")),
                                                 data = my_data))$coefficients[,-3])
                                             colnames(std_mod_out) <- c("std_est", "std_se", "std_pval")
                                             std_mod_out$outcomeb <- x
                                             std_mod_out$predictorb <- rownames(std_mod_out)
                                             rownames(std_mod_out) <- NULL
                                             combo <- cbind(mod_out, std_mod_out)
                                             combo <- combo %>% select(outcome, predictor, everything())
                                             return(combo)
                                           }))
### multinom regression for neg, neu, pos -----
my_data$impact.clean <- NA
my_data[my_data$impact.overall == "Would be perceived as neutral by undergraduate students in the class.",]$impact.clean <- "neutral"
my_data[my_data$impact.overall == "Would be perceived negatively by undergraduate students in the class.",]$impact.clean <- "negative"
my_data[my_data$impact.overall == "Would be perceived positively by undergraduate students in the class.",]$impact.clean <- "positive"

my_data$impact.clean <- factor(my_data$impact.clean, 
                               levels = c("negative", "neutral",
                                          "positive"))
my_data$impact.clean <- relevel(my_data$impact.clean, ref = "neutral")

summary(ordinal::clm(impact.clean ~ trt_grp + gender3 + religion3 + lgbtq2, data = my_data))

multinom_impact <- multinom(impact.clean ~ trt_grp + gender3 + religion3 + lgbtq2,
                            data = my_data,
                            na.action = na.omit)

z1 <-summary(multinom_impact)$coefficients/summary(multinom_impact)$standard.errors
p1 <- (1 - pnorm(abs(z1), 0, 1))*2
p1<0.05
summary(multinom_impact)

impact_moddf <- as.data.frame(cbind(
  t(as.data.frame(summary(multinom(impact.clean ~ trt_grp + gender3 + religion3 + lgbtq2,
                                   data = my_data,
                                   na.action = na.omit))$coefficients)),
  t(as.data.frame(summary(multinom(impact.clean ~ trt_grp + gender3 + religion3 + lgbtq2,
                                   data = my_data,
                                   na.action = na.omit))$standard.errors))))

colnames(impact_moddf) <- c("negative_est", "positive_est", "negative_se", "positive_se")
impact_moddf$predictor <- rownames(impact_moddf)
rownames(impact_moddf) <- NULL


impact_moddf$negative_z <-impact_moddf$negative_est/impact_moddf$negative_se
impact_moddf$negative_p <- (1 - pnorm(abs(impact_moddf$negative_z), 0, 1))*2
impact_moddf$negative_or <- exp(impact_moddf$negative_est)

impact_moddf$positive_z <-impact_moddf$positive_est/impact_moddf$positive_se
impact_moddf$positive_p <- (1 - pnorm(abs(impact_moddf$positive_z), 0, 1))*2
impact_moddf$positive_or <- exp(impact_moddf$positive_est)

impact_moddf <- impact_moddf %>% dplyr::select(predictor, negative_est, negative_se, negative_or,
                                               negative_z, negative_p, positive_est, positive_se,
                                               positive_or, positive_z, positive_p)

### ordinal regression for appropriate ----
my_data$appropriate.likert <- factor(my_data$appropriate.likert, 
                                     levels = c("Strongly disagree", "Disagree",
                                                "Somewhat disagree", "Somewhat agree",
                                                "Agree", "Strongly agree"))

summary(ordinal::clm(appropriate.likert ~ trt_grp + gender3 + religion3 + lgbtq2, data = my_data))

appropriate_moddf <- as.data.frame(summary(ordinal::clm(appropriate.likert ~ trt_grp + gender3 + religion3 + lgbtq2, data = my_data))$coefficients[,-3])
colnames(appropriate_moddf) <- c("est", "se", "pval")
appropriate_moddf$predictor <- rownames(appropriate_moddf)
rownames(appropriate_moddf) <- NULL
appropriate_moddf <- appropriate_moddf %>% filter(predictor != "Strongly disagree|Disagree" &
                                                    predictor != "Disagree|Somewhat disagree" &
                                                    predictor != "Somewhat disagree|Somewhat agree" &
                                                    predictor != "Somewhat agree|Agree" &
                                                    predictor != "Agree|Strongly agree")
appropriate_moddf$or <- exp(appropriate_moddf$est)

### figures for manuscript ----

figa <- main_mod_out %>%
  dplyr::select(outcome, predictor, std_est, std_se) %>%
  filter(predictor == "trt_grp") %>%
  mutate(outcome = fct_relevel(outcome, "approachability_mean",
                               "rapport_mean",
                               "homophily_mean",
                               "likeability_mean",
                               "competence_mean",
                               "hireability_mean")) %>%
  ggplot(aes(y = outcome)) +
  geom_point(aes(x = std_est), size = 5, color = "slategray") +
  geom_point(aes(x = 0.1066921, y = "rapport_mean"), shape = "*", size = 5, color = "black", position = position_nudge(y = .25)) +
  geom_vline(aes(xintercept = 0), linetype="dashed", linewidth=.7, 
             color = "grey15") + 
  geom_errorbarh(aes(xmin = std_est - 1.96*std_se, xmax = std_est + 1.96*std_se), 
                 position=ggstance::position_dodgev(height=0.5),
                 linewidth = 1.5, color = "slategray") +  
  labs(x = "Standardized beta ± 95% CI", y = "", title = "A.") + 
  scale_y_discrete(labels = c("Approachability^",
                              "Student-instructor\nrapport",
                              "Attitude homophily",
                              "Likeability",
                              "Competence",
                              "Hireability")) +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.title.x = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.text.x = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        plot.title = element_text(family="Helvetica", color = "black", size = 12, face = "bold", hjust = -.2, vjust = -1))

figa

ggsave(figa, file = "~/Desktop/figa.pdf", device = "pdf", units = "in", width = 8, height = 6)
ggsave(appr_int, file = "~/Desktop/appr_int.pdf", device = "pdf", units = "in", width = 3, height = 3)

#interactive models
mylist <- list(trt_grp=seq(0,1,by=0.01),lgbtq2=c("no", "yes"))


#B. Hireability
hireability_mod <- lm(hireability_mean ~ trt_grp*gender3 + trt_grp*religion3 + trt_grp*lgbtq2, data = my_data)
hire_mod_dat <- emmeans::emmip(hireability_mod,lgbtq2~trt_grp,at=mylist, CIs=TRUE, plotit=FALSE)

hire_int <- hire_mod_dat %>%
  ggplot(aes(x=trt_grp,y=yvar, color=lgbtq2)) + 
  geom_line() +
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=lgbtq2), alpha=0.4) +
  scale_x_continuous(breaks = c(0,1), labels = c("control","reveal     ")) +
  scale_y_continuous(limits = c(1, 7), breaks = seq(from = 1, to = 7, by = 1)) +
  scale_fill_manual(values= c("gray4", "slategray1")) +
  scale_color_manual(values= c("gray4", "slategray1")) +
  labs(x="", y="", title = "B. Hireability", color="LGBTQ+", fill="LGBTQ+") + 
  theme_classic() +
  theme(axis.text.x = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.title = element_blank(),
        title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        legend.text =  element_text(family="Helvetica", color = "black", size = 12),
        legend.title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"))

hire_int

#C. Competence
competence_mod <- lm(competence_mean ~ trt_grp*gender3 + trt_grp*religion3 + trt_grp*lgbtq2, data = my_data)
comp_mod_dat <- emmeans::emmip(competence_mod,lgbtq2~trt_grp,at=mylist, CIs=TRUE, plotit=FALSE)

comp_int <- comp_mod_dat %>%
  ggplot(aes(x=trt_grp,y=yvar, color=lgbtq2)) + 
  geom_line() +
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=lgbtq2), alpha=0.4) +
  scale_x_continuous(breaks = c(0,1), labels = c("control","reveal     ")) +
  scale_y_continuous(limits = c(1, 7), breaks = seq(from = 1, to = 7, by = 1)) +
  scale_fill_manual(values= c("gray4", "slategray1")) +
  scale_color_manual(values= c("gray4", "slategray1")) +
  labs(x="", y="", title = "C. Competence") + 
  theme_classic() +
  theme(axis.text.x = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.title = element_blank(),
        title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        legend.position = "none")

comp_int

#D. Likeability
likeability_mod <- lm(likeability_mean ~ trt_grp*gender3 + trt_grp*religion3 + trt_grp*lgbtq2, data = my_data)
like_mod_dat <- emmeans::emmip(likeability_mod,lgbtq2~trt_grp,at=mylist, CIs=TRUE, plotit=FALSE)

like_int <- like_mod_dat %>%
  ggplot(aes(x=trt_grp,y=yvar, color=lgbtq2)) + 
  geom_line() +
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=lgbtq2), alpha=0.4) +
  scale_x_continuous(breaks = c(0,1), labels = c("control","reveal     ")) +
  scale_y_continuous(limits = c(1, 7), breaks = seq(from = 1, to = 7, by = 1)) +
  scale_fill_manual(values= c("gray4", "slategray1")) +
  scale_color_manual(values= c("gray4", "slategray1")) +
  labs(x="", y="", title = "D. Likeability") + 
  theme_classic() +
  theme(axis.text.x = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.title = element_blank(),
        title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        legend.position = "none")

like_int

#E. Attitude homophily
homophily_mod <- lm(homophily_mean ~ trt_grp*gender3 + trt_grp*religion3 + trt_grp*lgbtq2, data = my_data)
homo_mod_dat <- emmeans::emmip(homophily_mod,lgbtq2~trt_grp,at=mylist, CIs=TRUE, plotit=FALSE)

homophily_int <- homo_mod_dat |>
  ggplot(aes(x=trt_grp,y=yvar, color=lgbtq2)) + 
  geom_line() +
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=lgbtq2), alpha=0.4) +
  geom_point(aes(x = .5, y = 5.5), shape = "*", size = 5, color = "black") + 
  scale_x_continuous(breaks = c(0,1), labels = c("control","reveal")) +
  scale_y_continuous(limits = c(1, 7), breaks = seq(from = 1, to = 7, by = 1)) +
  scale_fill_manual(values= c("gray4", "slategray1")) + #"olivedrab1", "olivedrab4"
  scale_color_manual(values= c("gray4", "slategray1")) + #"paleturquoise2", "paleturquoise4"
  labs(x="", y="", title = "E. Attitude homophily") + 
  theme_classic() +
  theme(axis.text.x = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "none")

homophily_int

#F. Student-instructor rapport
rapport_mod <- lm(rapport_mean ~ trt_grp*gender3 + trt_grp*religion3 + trt_grp*lgbtq2, data = my_data)
rap_mod_dat <- emmeans::emmip(rapport_mod,lgbtq2~trt_grp,at=mylist, CIs=TRUE, plotit=FALSE)

rap_int <- rap_mod_dat %>%
  ggplot(aes(x=trt_grp,y=yvar, color=lgbtq2)) + 
  geom_line() +
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=lgbtq2), alpha=0.4) +
  scale_x_continuous(breaks = c(0,1), labels = c("control","reveal     ")) +
  scale_y_continuous(limits = c(1, 5), breaks = seq(from = 1, to = 5, by = 1)) +
  scale_fill_manual(values= c("gray4", "slategray1")) +
  scale_color_manual(values= c("gray4", "slategray1")) +
  labs(x="", y="", title = "F. Student-instructor rapport") + 
  theme_classic() +
  theme(axis.text.x = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.title = element_blank(),
        title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        legend.position = "none")

rap_int

#G. Approachability^
approach_mod <- lm(approachability_mean ~ trt_grp*gender3 + trt_grp*religion3 + trt_grp*lgbtq2, data = my_data)
appr_mod_dat <- emmeans::emmip(approach_mod,lgbtq2~trt_grp,at=mylist, CIs=TRUE, plotit=FALSE)

appr_int <- appr_mod_dat %>%
  ggplot(aes(x=trt_grp,y=yvar, color=lgbtq2)) + 
  geom_line() +
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=lgbtq2), alpha=0.4) +
  scale_x_continuous(breaks = c(0,1), labels = c("control","reveal     ")) +
  scale_y_continuous(limits = c(1, 7), breaks = seq(from = 1, to = 7, by = 1)) +
  scale_fill_manual(values= c("gray4", "slategray1")) +
  scale_color_manual(values= c("gray4", "slategray1")) +
  labs(x="", y="", title = "G. Approachability^") + 
  theme_classic() +
  theme(axis.text.x = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        axis.title = element_blank(),
        title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        legend.position = "none")

appr_int

### checking assumptions ----
#vif for multicollinearity - all good
model <- lm(hireability_mean ~ trt_grp + gender3 + religion3 + lgbtq2, data = my_data)
vif(model)

intmod <- lm(hireability_mean ~ trt_grp*gender3 + trt_grp*religion3 + trt_grp*lgbtq2, data = my_data)
vif(intmod)


#linear reg; plot each: hireability, competence, likeability, homophily, rapport, approachability, cohesion-sob, cohesion-fom, comfort
model <- lm(class_comfort_mean ~ trt_grp + gender3 + religion3 + lgbtq2, data = my_data)
plot(model)

#multinom logistic reg
residualPlot(multinom(impact.clean ~ trt_grp + gender3 + religion3,
                      data = my_data,
                      na.action = na.omit))
cooks.distance(multinom(impact.clean ~ trt_grp + gender3 + religion3,
                        data = my_data,
                        na.action = na.omit))

#different way to check proportional odds assumption (ucla site)
library(Hmisc)
sf <- function(y){
  c('Y>1' = qlogis(mean(y>=1)),
    'Y>2' = qlogis(mean(y>=2)),
    'Y>3' = qlogis(mean(y>=3)),
    'Y>4' = qlogis(mean(y>=4)),
    'Y>5' = qlogis(mean(y>=5)),
    'Y>6' = qlogis(mean(y>=6)))
}

(s <- with(my_data, summary(as.numeric(appropriate.likert) ~ trt_grp + gender3 + religion3 + lgbtq2, fun = sf)))

glm(I(as.numeric(appropriate.likert) >= 2) ~ trt_grp, family = "binomial", data = my_data)
glm(I(as.numeric(appropriate.likert) >= 3) ~ trt_grp, family = "binomial", data = my_data)
glm(I(as.numeric(appropriate.likert) >= 4) ~ trt_grp, family = "binomial", data = my_data)
glm(I(as.numeric(appropriate.likert) >= 5) ~ trt_grp, family = "binomial", data = my_data)
glm(I(as.numeric(appropriate.likert) >= 6) ~ trt_grp, family = "binomial", data = my_data)

plot(s, which = 1:6, pch = 1:6, xlab = 'logit', main = '', xlim = range(s[,6:7]))
