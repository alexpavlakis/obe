library(dplyr)
library(ggplot2)
library(stringr)
library(haven)
library(rstan)
library(gridExtra)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Read in data
setwd("/Users/alexpavlakis/Desktop/Data")
rct <- read_sav("BayesianModeling.sav")

# Redo ID to be 1:66
rct$Id <- c(1:66)

obe <- rct %>%
  dplyr::select(c(names(rct)[str_detect(names(rct), "OBE")])) %>%
  reshape(varying = names(rct)[str_detect(names(rct), "OBE")][1:6],
          timevar = "time", idvar = "id", 
          direction = "long", sep = ".")

# fix ID naming
obe$Id <- obe$id

# Combine with demographic data
all_data <- merge(select(obe, c(Id, OBE, time)), 
                  select(rct, c(Id, Tx, Dx, Age.0, 
                                     Sex, Ethnicity, Race)),
                  by = "Id",
                  all.x = TRUE)

# Get rid of dropouts
all_data <- all_data[complete.cases(all_data),]
all_data <- all_data[all_data$OBE >= 0,]

# Reorder by time
all_data <- all_data[order(all_data$time), ]

par(mfrow = c(3, 2),
    mar = c(4, 1, 2, 1),
    oma = c(1, 1, 1, 1),
    cex.main = 1.5)
for(i in c(0, 4, 8, 12, 24, 36)) {
  hist(all_data$OBE[all_data$time == i
                    & all_data$Tx == 0],
       freq = FALSE,
       breaks = 10,
       yaxt = 'n',
       xlim = c(0, 100),
       ylim = c(0, 0.2),
       xlab = ifelse(i == 36, "OBE", " "),
       ylab = " ",
       col = rgb(233/255, 150/255, 122/255, 0.7),
       border = "darksalmon",
       main = paste("Week", i))
  hist(all_data$OBE[all_data$time == i
                    & all_data$Tx == 1],
       freq = FALSE,
       breaks = 10,
       yaxt = 'n',
       xlim = c(0, 100),
       ylim = c(0, 0.2),
       xlab = ifelse(i == 36, "OBE", " "),
       ylab = " ",
       col = rgb(142/255, 229/255, 238/255, 0.7),
       border = "cadetblue2",
       main = paste("Week", i),
       add = TRUE)
  if(i == 0) {
    legend(x = 55, y = .10, 
           col = c("darksalmon", "cadetblue2"),
           legend = c("CBT-GSH", "CBT-GSH + Noom"),
           pch = c(15, 15),
           bty = 'n')
  }
}



# Build histograms with density lines

# Create table of margin locations and labels for plot
margin_table <- data.frame(pos = seq(0.1, 1, 0.16),
                           label = c(36, 24, 12, 8, 4, 0))

par(mfrow = c(6, 2),
    mar = c(1, 0, 1, 2),
    cex.main = 1.5,
    oma = c(1, 6.5, 2, 2))
for(i in c(0, 4, 8, 12, 24, 36)) {
  hist(all_data$OBE[all_data$time == i
                    & all_data$Tx == 0],
       freq = FALSE,
       breaks = 10,
       yaxt = 'n',
       xlim = c(0, 100),
       xlab = ifelse(i == 36, "OBE", " "),
       ylab = " ",
       col = rgb(233/255, 150/255, 122/255, 0.7),
       border = "darksalmon",
       main = ifelse(i == 0, "CBT-GSH", " "))
  hist(all_data$OBE[all_data$time == i
                    & all_data$Tx == 1],
       freq = FALSE,
       breaks = 10,
       yaxt = 'n',
       xlim = c(0, 100),
       xlab = ifelse(i == 36, "OBE", " "),
       ylab = " ",
       col = rgb(142/255, 229/255, 238/255, 0.7),
       border = "cadetblue2",
       main = ifelse(i == 0, "CBT-GSH + Noom", " "))
}
# Add margin labels
for(i in 1:nrow(margin_table)) {
  mtext(paste("Week", margin_table$label[i]),  
        side = 2,
        line = 0,
        at = margin_table$pos[i],
        outer = TRUE,
        cex = 1) 
}


# Look at actual paths
par(mfrow = c(1, 2),
    mar = c(3, 3, 2, 1),
    mgp = c(2, 0.7, 0),
    oma = c(1, 1, 1, 1),
    tck = -0.01,
    las = 1)
plot(all_data$time[all_data$Tx == 0
                   & all_data$Id == 1],
     all_data$OBE[all_data$Tx == 0
                  & all_data$Id == 1],
     type = "l",
     bty = 'n',
     axes = FALSE,
     xaxs = "i",
     yaxs = "i",
     ylim = c(-1, 100),
     xlim = c(-1, 37),
     xlab = "Week",
     ylab = "OBE",
     main = "CBT-GSH",
     col = "darkgrey")
axis(1, at = c(0, 4, 8, 12, 24, 36),
     col = "grey20",
     lwd.ticks = 0)
axis(2, at = seq(0, 100, 20),
     col = "grey20",
     lwd.ticks = 0)
abline(h = -1,v = -1, col = "grey20",
       lwd = 2.5)
for(i in 1:66) {
  lines(all_data$time[all_data$Tx == 0
                      & all_data$Id == i],
        all_data$OBE[all_data$Tx == 0
                     & all_data$Id == i],
        col = "darkgrey",
        cex = 0.8)
  points(all_data$time[all_data$Tx == 0
                       & all_data$Id == i],
         all_data$OBE[all_data$Tx == 0
                      & all_data$Id == i],
         pch = 16,
         col = "darkgrey")
}

plot(all_data$time[all_data$Tx == 1
                   & all_data$Id == 1],
     all_data$OBE[all_data$Tx == 1
                  & all_data$Id == 1],
     type = "l",
     bty = 'n',
     axes = FALSE,
     xaxs = "i",
     yaxs = "i",
     ylim = c(-1, 100),
     xlim = c(-1, 37),
     xlab = "Week",
     ylab = " ",
     main = "CBT-GSH + Noom",
     col = "darkgrey")
axis(1, at = c(0, 4, 8, 12, 24, 36),
     col = "grey20",
     lwd.ticks = 0)
axis(2, at = seq(0, 100, 20),
     col = "grey20",
     lwd.ticks = 0)
abline(h = -1,v = -1, col = "grey20",
       lwd = 2.5)
for(i in 1:66) {
  lines(all_data$time[all_data$Tx == 1
                      & all_data$Id == i],
        all_data$OBE[all_data$Tx == 1
                     & all_data$Id == i],
        col = "darkgrey",
        cex = 0.8)
  points(all_data$time[all_data$Tx == 1
                       & all_data$Id == i],
         all_data$OBE[all_data$Tx == 1
                      & all_data$Id == i],
         pch = 16,
         col = "darkgrey")
}

# Simple model 
OBE <- all_data$OBE
person_id <- match(all_data$Id, unique(all_data$Id))
time_id <- match(all_data$time, unique(all_data$time))
tmt <- all_data$Tx
sex <- all_data$Sex
age <- all_data$Age.0
black <- ifelse(all_data$Race == "Black or", 1, 0)
other_race <- ifelse(all_data$Race != "Black or"
                     & all_data$Race != "White", 1, 0)
hisp <- ifelse(all_data$Ethnicity == "Yes", 1, 0)
bn <- all_data$Dx
N <- length(OBE)
N_time <- max(time_id)
N_ppl <- max(person_id)

model_code <- "
data{
int N;
int N_time;
int N_ppl;
int OBE[N];
vector[N] tmt;
int person_id[N];
int time_id[N];
int sex[N];
int age[N];
int black[N];
int other_race[N];
int hisp[N];
int bn[N];
}
parameters{
real alpha[N_ppl];
real alpha_sex;
real alpha_age;
real alpha_black;
real alpha_other;
real alpha_hisp;
real alpha_bn;
real beta[N_time];
real gamma[N_time];
real mu_alpha;
real<lower = 0> tau_alpha;
real mu_gamma;
real<lower = 0> tau_gamma;
real<lower = 0> sigma;
}
model{
for(i in 1:N) {
OBE[i] ~ normal(alpha[person_id[i]] + alpha_sex*sex[i] 
+ alpha_black*black[i] + alpha_other*other_race[i]
+ alpha_hisp*hisp[i] + alpha_bn*bn[i]
+ beta[time_id[i]] + alpha_age*age[i]
+ gamma[time_id[i]]*tmt[i], sigma);
}
alpha ~ normal(mu_alpha, tau_alpha);
mu_alpha ~ normal(5, 5);
alpha_black ~ normal(0, 2);
alpha_sex ~ normal(0, 2);
alpha_other ~ normal(0, 2);
alpha_hisp ~ normal(0, 2);
alpha_bn ~ normal(0, 2);
tau_alpha ~ cauchy(0, 30);
gamma ~ normal(mu_gamma, tau_gamma);
mu_gamma ~ normal(0, 1);
tau_gamma ~ cauchy(0, 30);
sigma ~ cauchy(0, 30);
}
generated quantities{
real OBE_pred[N];
for(i in 1:N) {
OBE_pred[i] = normal_rng(alpha[person_id[i]] + alpha_sex*sex[i] 
+ alpha_black*black[i] + alpha_other*other_race[i]
+ alpha_hisp*hisp[i] + alpha_bn*bn[i]
+ beta[time_id[i]] + alpha_age*age[i]
+ gamma[time_id[i]]*tmt[i], sigma);
}
}"

model_code_pois <- "
data{
  int N;
  int N_time;
  int N_ppl;
  int OBE[N];
  vector[N] tmt;
  int person_id[N];
  int time_id[N];
  int sex[N];
  int age[N];
  int black[N];
  int other_race[N];
  int hisp[N];
  int bn[N];
}
parameters{
  real alpha[N_ppl];
  real mu_alpha;
  real<lower = 0> tau_alpha;
  real alpha_sex;
  real alpha_age;
  real alpha_black;
  real alpha_other;
  real alpha_hisp;
  real alpha_bn;
  real beta[N_time];
  real gamma[N_time];
  real mu_gamma;
  real<lower = 0> tau_gamma;
}
model{
  for(i in 1:N) {
    OBE[i] ~ poisson_log(alpha[person_id[i]] + alpha_sex*sex[i] 
                         + alpha_black*black[i] + alpha_other*other_race[i]
                         + alpha_hisp*hisp[i] + alpha_bn*bn[i]
                         + beta[time_id[i]] + alpha_age*age[i]
                         + gamma[time_id[i]]*tmt[i]);
  }
  alpha ~ normal(mu_alpha, tau_alpha);
  mu_alpha ~ normal(5, 2);
  tau_alpha ~ cauchy(0, 50);
  alpha_black ~ normal(0, 1);
  alpha_sex ~ normal(0, 1);
  alpha_other ~ normal(0, 1);
  alpha_hisp ~ normal(0, 1);
  alpha_bn ~ normal(0, 1);
  gamma ~ normal(mu_gamma, tau_gamma);
  mu_gamma ~ normal(0, 2);
  tau_gamma ~ cauchy(0, 30);
}
generated quantities{
  real OBE_pred[N];
  for(i in 1:N) {
    OBE_pred[i] = poisson_log_rng(alpha[person_id[i]] + alpha_sex*sex[i] 
                                  + alpha_black*black[i] + alpha_other*other_race[i]
                                  + alpha_hisp*hisp[i] + alpha_bn*bn[i]
                                  + beta[time_id[i]] + alpha_age*age[i]
                                  + gamma[time_id[i]]*tmt[i]);
  }
}"
# Fit model
#fit1 <- stan(model_code = model_code,
#             chains = 4, iter = 2000)
#

fit2 <- stan(model_code = model_code_pois,
             chains = 4, iter = 2000)

# Look at results
print(fit2, pars = c("alpha", "beta", "gamma",
                     "alpha_black", "alpha_other",
                     "alpha_sex", "alpha_age",
                     "alpha_hisp", "alpha_bn",
                     "mu_gamma", "tau_gamma",
                     "mu_alpha","tau_alpha"))

# Attach predicted values to data frame
all_data$OBE_pred <- apply(extract(fit2)$OBE_pred, 2, median)
all_data$se_OBE_pred <- apply(rstan::extract(fit2)$OBE_pred, 2, sd)

# PPC
par(mfrow = c(1, 2),
    mar = c(3,3,2,1),
    oma = c(1, 1, 1, 1),
    mgp = c(2, 0.7, 0),
    tck = -0.01,
    las = 1)
# PPC FOR NO NOOM
plot(all_data$OBE_pred[all_data$Tx == 0], all_data$OBE[all_data$Tx == 0],
     pch = 16,
     cex = 0.7,
     xlim = c(-2, 100),
     ylim = c(-2, 100),
     main = "CBT-GSH ",
     xlab = "Simulated OBE",
     ylab = "OBE",
     bty = 'n',
     xaxs = 'i',
     yaxs = "i")
abline(0, 1)
arrows(all_data$OBE_pred[all_data$Tx == 0], all_data$OBE[all_data$Tx == 0],
       all_data$OBE_pred[all_data$Tx == 0] + 0.6*all_data$se_OBE_pred[all_data$Tx == 0],
       all_data$OBE[all_data$Tx == 0],
       length = 0)
arrows(all_data$OBE_pred[all_data$Tx == 0], all_data$OBE[all_data$Tx == 0],
       all_data$OBE_pred[all_data$Tx == 0] - 0.6*all_data$se_OBE_pred[all_data$Tx == 0],
       all_data$OBE[all_data$Tx == 0],
       length = 0)

# PPC FOR NOOM
plot(all_data$OBE_pred[all_data$Tx == 1], all_data$OBE[all_data$Tx == 1],
     pch = 16,
     cex = 0.7,
     xlim = c(-2, 100),
     ylim = c(-2, 100),
     main = "CBT-GSH + Noom",
     xlab = "Simulated OBE",
     ylab = "OBE",
     bty = 'n',
     xaxs = 'i',
     yaxs = 'i')
abline(0, 1)
arrows(all_data$OBE_pred[all_data$Tx == 1], all_data$OBE[all_data$Tx == 1],
       all_data$OBE_pred[all_data$Tx == 1] + 0.6*all_data$se_OBE_pred[all_data$Tx == 1],
       all_data$OBE[all_data$Tx == 1],
       length = 0)
arrows(all_data$OBE_pred[all_data$Tx == 1], all_data$OBE[all_data$Tx == 1],
       all_data$OBE_pred[all_data$Tx == 1] - 0.6*all_data$se_OBE_pred[all_data$Tx == 1],
       all_data$OBE[all_data$Tx == 1],
       length = 0)

# Look at treatment effect over time
treatment_effects <- data.frame(effect = apply(extract(fit2)$gamma, 2, median),
                                se_effect = apply(rstan::extract(fit2)$gamma, 2, sd),
                                week = c(0, 4, 8, 12, 24, 36))

# Average among groups
fig_2 <- all_data %>% 
  group_by(Tx, time) %>%
  summarise(mean_obe = mean(OBE),
            se_obe = sd(OBE)/sqrt(n()),
            mean_obe_pred = mean(OBE_pred),
            se_obe_pred = sd(OBE_pred)/sqrt(n()))

# Look at actual paths
par(mfrow = c(1, 2),
    mar = c(3,3,2,1),
    mgp = c(2, 0.7, 0),
    tck = -0.01,
    las = 1)
plot(fig_2$time[fig_2$Tx == 0],
     fig_2$mean_obe_pred[fig_2$Tx == 0],
     type = "l",
     bty = 'n',
     axes = FALSE,
     xaxs = "i",
     yaxs = "i",
     ylim = c(-1, 100),
     xlim = c(-1, 37),
     xlab = "Week",
     ylab = "OBE",
     main = "CBT-GSH")
axis(1, at = c(0, 4, 8, 12, 24, 36),
     col = "grey20",
     lwd.ticks = 0)
axis(2, at = seq(0, 100, 20),
     col = "grey20",
     lwd.ticks = 0)
for(i in 1:66) {
  lines(all_data$time[all_data$Tx == 0
                      & all_data$Id == i],
        all_data$OBE[all_data$Tx == 0
                     & all_data$Id == i],
        col = "grey",
        cex = 0.8)
  points(all_data$time[all_data$Tx == 0
                       & all_data$Id == i],
         all_data$OBE[all_data$Tx == 0
                      & all_data$Id == i],
         pch = 16,
         col = "grey")
}
lines(fig_2$time[fig_2$Tx == 0],
      fig_2$mean_obe_pred[fig_2$Tx == 0],
      lwd = 1.5)
abline(h = -1,v = -1, col = "grey20",
       lwd = 2.5)
points(fig_2$time[fig_2$Tx == 0],
       fig_2$mean_obe_pred[fig_2$Tx == 0],
       pch = 16,
       cex = 1.2)

plot(fig_2$time[fig_2$Tx == 1],
     fig_2$mean_obe_pred[fig_2$Tx == 1],
     type = "l",
     bty = 'n',
     axes = FALSE,
     xaxs = "i",
     yaxs = "i",
     ylim = c(-1, 100),
     xlim = c(-1, 37),
     xlab = "Week",
     ylab = " ",
     main = "CBT-GSH + Noom")
axis(1, at = c(0, 4, 8, 12, 24, 36),
     col = "grey20",
     lwd.ticks = 0)
axis(2, at = seq(0, 100, 20),
     col = "grey20",
     lwd.ticks = 0)
for(i in 1:66) {
  lines(all_data$time[all_data$Tx == 1
                      & all_data$Id == i],
        all_data$OBE[all_data$Tx == 1
                     & all_data$Id == i],
        col = "grey",
        cex = 0.8)
  points(all_data$time[all_data$Tx == 1
                       & all_data$Id == i],
         all_data$OBE[all_data$Tx == 1
                      & all_data$Id == i],
         pch = 16,
         col = "grey")
}
lines(fig_2$time[fig_2$Tx == 1],
      fig_2$mean_obe_pred[fig_2$Tx == 1],
      lwd = 1.5)
points(fig_2$time[fig_2$Tx == 1],
       fig_2$mean_obe_pred[fig_2$Tx == 1],
       pch = 16,
       cex = 1.2)
abline(h = -1,v = -1, col = "grey20",
       lwd = 2.5)
legend(x = 24, y = 40,
       bty = 'n',
       col = c("grey", "black"),
       legend = c("Data", "Model"),
       pch = 16,
       lty = 1)


# GGPLOT2
p1 <- ggplot(treatment_effects, aes(week, effect)) + 
  geom_hline(yintercept = 0,
             lty = 1,
             col = "grey") +
  geom_vline(aes(xintercept = 12),
             col = "darkgrey",
             lty = 2) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = effect - 0.67*se_effect,
                    ymax = effect + 0.67*se_effect),
                width = 0,
                size = 1.0) +
  scale_y_continuous("Noom Effect",
                     limits = c(-2, 2),
                     breaks = seq(-2, 2, 1)) +
  scale_x_continuous("Week",
                     limits = c(-1, 37),
                     breaks = c(0, 4, 8, 12, 24, 36)) +
  #ggtitle("Noom Application Augments CBT-GSH") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black",
                                 size = 11),
        axis.title = element_text(colour = "black",
                                  size = 16,
                                  face = "bold"),
        plot.title = element_text(size = 20, 
                                  face = "bold"))

p2 <- ggplot(fig_2[fig_2$Tx == 0,], aes(time, mean_obe_pred)) + 
  geom_vline(aes(xintercept = 12),
             col = "darkgrey",
             lty = 2) +
  geom_line(col = "darksalmon",
            size = 1.2) +
  geom_errorbar(data = fig_2[fig_2$Tx == 0,],
                aes(ymin = mean_obe_pred - 1.96*se_obe_pred,
                    ymax = mean_obe_pred + 1.96*se_obe_pred),
                col = "darksalmon",
                size = 1.2,
                width = 0.5) +
  geom_line(data = fig_2[fig_2$Tx == 1,], 
            aes(time, mean_obe_pred),
            col = "cadetblue2",
            size = 1.2) +
  geom_errorbar(data = fig_2[fig_2$Tx == 1,],
                aes(ymin = mean_obe_pred - 1.96*se_obe_pred,
                    ymax = mean_obe_pred + 1.96*se_obe_pred),
                col = "cadetblue2",
                size = 1.2,
                width = 0.5) +
  geom_point(data = fig_2[fig_2$Tx == 0,], 
             aes(time, mean_obe_pred),
             col = "darksalmon") +
  geom_point(data = fig_2[fig_2$Tx == 1,], 
             aes(time, mean_obe_pred),
             col = "cadetblue2") +
  scale_y_continuous("OBE",
                     limits = c(0, 17),
                     breaks = seq(0, 15, 5)) +
  scale_x_continuous(" ",
                     limits = c(-1, 37),
                     breaks = c(0, 4, 8, 12, 24, 36)) +
  annotate("text",
           x = 36, y = 0.4,
           label = "CBT-GSH",
           col = "darksalmon") +
  annotate("text",
           x = 36, y = 4,
           label = "CBT+GSH + Noom",
           col = "cadetblue2") +
  ggtitle("Noom Application Augments CBT-GSH") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black",
                                 size = 11),
        axis.title = element_text(colour = "black",
                                  size = 16,
                                  face = "bold"),
        plot.title = element_text(size = 20, 
                                  face = "bold"))

grid.arrange(p2, p1)

# Outcome table
out_data <- tidy(fit1)

tab <- out_data %>% 
  filter(str_detect(term, "gamma")) 

tab$estimate <- round(tab$estimate, 2)
tab$std.error <- round(tab$std.error, 2)

# THEIR THEME
theme(axis.line = element_line(colour = "darkblue",
                               size = 1.2),
      axis.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 16, face = "bold"),
      plot.title = element_text(size = 20, face = "bold"))



ses <- 1.96

ggplot(fig_2[fig_2$Tx == 0,], aes(time, mean_obe_pred)) + 
  geom_ribbon(data = fig_2[fig_2$Tx == 1,],
              aes(ymin = mean_obe_pred - ses*se_obe_pred,
                  ymax = mean_obe_pred + ses*se_obe_pred),
              fill = "cadetblue2",
              alpha = 0.5) +
  geom_line(data = fig_2[fig_2$Tx == 1,], 
            aes(time, mean_obe_pred),
            col = "cadetblue",
            size = 1.2) +
  geom_point(data = fig_2[fig_2$Tx == 1,], 
             aes(time, mean_obe_pred),
             col = "cadetblue") +
  geom_ribbon(data = fig_2[fig_2$Tx == 0,],
              aes(ymin = mean_obe_pred - ses*se_obe_pred,
                  ymax = mean_obe_pred + ses*se_obe_pred),
              fill = "darksalmon",
              alpha = 0.5) +
  geom_vline(aes(xintercept = 12),
             col = "darkgrey",
             lty = 2) +
  geom_line(col = "darksalmon",
            size = 1.2) +
  geom_point(data = fig_2[fig_2$Tx == 0,], 
             aes(time, mean_obe_pred),
             col = "darksalmon") +
  scale_y_continuous("OBE",
                     limits = c(0, 17),
                     breaks = seq(0, 15, 5)) +
  scale_x_continuous(" ",
                     limits = c(-1, 37),
                     breaks = c(0, 4, 8, 12, 24, 36)) +
  annotate("text",
           x = 36, y = 5,
           label = "CBT+GSH",
           col = "darksalmon") +
  annotate("text",
           x = 36, y = 0.5,
           label = "CBT+GSH + Noom",
           col = "cadetblue2") +
  ggtitle("Noom Application Augments CBT-GSH") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black",
                                 size = 11),
        axis.title = element_text(colour = "black",
                                  size = 16,
                                  face = "bold"),
        plot.title = element_text(size = 20, 
                                  face = "bold"))



# Exploratory data analysis

# Create table of margin locations and labels for plot
margin_table <- data.frame(pos = seq(0.1, 1, 0.16),
                           label = c(36, 24, 12, 8, 4, 0))

# Build histograms with density lines
par(mfrow = c(6, 2),
    mar = c(1, 0, 1, 2),
    cex.main = 1.5,
    oma = c(1, 6.5, 2, 2))
for(i in c(0, 4, 8, 12, 24, 36)) {
  hist(all_data$OBE[all_data$time == i
                    & all_data$Tx == 0],
       freq = FALSE,
       breaks = 10,
       yaxt = 'n',
       xlim = c(0, 100),
       ylim = c(0, max(density(all_data$OBE_pred[all_data$time == i
                                                 & all_data$Tx == 0])$y)),
       xlab = ifelse(i == 36, "OBE", " "),
       ylab = " ",
       col = rgb(233/255, 150/255, 122/255, 0.7),
       border = "darksalmon",
       main = ifelse(i == 0, "CBT-GSH", " "))
  lines(density(all_data$OBE_pred[all_data$time == i
                                  & all_data$Tx == 0]),
        col = "red")
  hist(all_data$OBE[all_data$time == i
                    & all_data$Tx == 1],
       freq = FALSE,
       breaks = 10,
       yaxt = 'n',
       xlim = c(0, 100),
       ylim = c(0, max(density(all_data$OBE_pred[all_data$time == i
                                                 & all_data$Tx == 1])$y)),
       xlab = ifelse(i == 36, "OBE", " "),
       ylab = " ",
       col = rgb(142/255, 229/255, 238/255, 0.7),
       border = "cadetblue2",
       main = ifelse(i == 0, "CBT-GSH + Noom", " "))
  lines(density(all_data$OBE_pred[all_data$time == i
                                  & all_data$Tx == 1]),
        col = "blue")
}
# Add margin labels
for(i in 1:nrow(margin_table)) {
  mtext(paste("Week", margin_table$label[i]),  
        side = 2,
        line = 0,
        at = margin_table$pos[i],
        outer = TRUE,
        cex = 1) 
}


#################################
# Look at actual paths
par(mfrow = c(2, 2),
    mar = c(3, 2, 1, 0),
    mgp = c(2, 0.7, 0),
    oma = c(1, 4.5, 1, 1),
    tck = -0.01,
    las = 1,
    cex.main = 1.5)
plot(fig_2$time[fig_2$Tx == 0],
     fig_2$mean_obe_pred[fig_2$Tx == 0],
     type = "l",
     bty = 'n',
     axes = FALSE,
     xaxs = "i",
     yaxs = "i",
     ylim = c(-1, 100),
     xlim = c(-1, 37),
     xlab = " ",
     ylab = "OBE",
     col = "white",
     main = "CBT-GSH")
axis(1, at = c(0, 4, 8, 12, 24, 36),
     col = "grey20",
     lwd.ticks = 0)
axis(2, at = seq(0, 100, 20),
     col = "grey20",
     lwd.ticks = 0)
for(i in 1:66) {
  lines(all_data$time[all_data$Tx == 0
                      & all_data$Id == i],
        all_data$OBE[all_data$Tx == 0
                     & all_data$Id == i],
        col = "grey",
        cex = 0.8)
  points(all_data$time[all_data$Tx == 0
                       & all_data$Id == i],
         all_data$OBE[all_data$Tx == 0
                      & all_data$Id == i],
         pch = 16,
         col = "grey")
}
lines(fig_2$time[fig_2$Tx == 0],
      fig_2$mean_obe[fig_2$Tx == 0],
      lwd = 1.5)
abline(h = -1,v = -1, col = "grey20",
       lwd = 2.5)
points(fig_2$time[fig_2$Tx == 0],
       fig_2$mean_obe[fig_2$Tx == 0],
       pch = 16,
       cex = 1.2)

plot(fig_2$time[fig_2$Tx == 1],
     fig_2$mean_obe_pred[fig_2$Tx == 1],
     type = "l",
     bty = 'n',
     axes = FALSE,
     xaxs = "i",
     yaxs = "i",
     ylim = c(-1, 100),
     xlim = c(-1, 37),
     xlab = " ",
     ylab = " ",
     col = "white",
     main = "CBT-GSH + Noom")
axis(1, at = c(0, 4, 8, 12, 24, 36),
     col = "grey20",
     lwd.ticks = 0)
for(i in 1:66) {
  lines(all_data$time[all_data$Tx == 1
                      & all_data$Id == i],
        all_data$OBE[all_data$Tx == 1
                     & all_data$Id == i],
        col = "grey",
        cex = 0.8)
  points(all_data$time[all_data$Tx == 1
                       & all_data$Id == i],
         all_data$OBE[all_data$Tx == 1
                      & all_data$Id == i],
         pch = 16,
         col = "grey")
}
lines(fig_2$time[fig_2$Tx == 1],
      fig_2$mean_obe[fig_2$Tx == 1],
      lwd = 1.5)
points(fig_2$time[fig_2$Tx == 1],
       fig_2$mean_obe[fig_2$Tx == 1],
       pch = 16,
       cex = 1.2)
abline(h = -1,v = -1, col = "grey20",
       lwd = 2.5)

#### NOW PREDS
plot(fig_2$time[fig_2$Tx == 0],
     fig_2$mean_obe_pred[fig_2$Tx == 0],
     type = "l",
     bty = 'n',
     axes = FALSE,
     xaxs = "i",
     yaxs = "i",
     ylim = c(-1, 100),
     xlim = c(-1, 37),
     xlab = "Week",
     ylab = "OBE",
     col = "white",
     main = " ")
axis(1, at = c(0, 4, 8, 12, 24, 36),
     col = "grey20",
     lwd.ticks = 0)
axis(2, at = seq(0, 100, 20),
     col = "grey20",
     lwd.ticks = 0)
for(i in 1:66) {
  lines(all_data$time[all_data$Tx == 0
                      & all_data$Id == i],
        all_data$OBE_pred[all_data$Tx == 0
                          & all_data$Id == i],
        col = "darkgrey",
        cex = 0.8)
  points(all_data$time[all_data$Tx == 0
                       & all_data$Id == i],
         all_data$OBE_pred[all_data$Tx == 0
                           & all_data$Id == i],
         pch = 16,
         col = "darkgrey")
}
lines(fig_2$time[fig_2$Tx == 0],
      fig_2$mean_obe_pred[fig_2$Tx == 0],
      lwd = 1.5)
abline(h = -1,v = -1, col = "grey20",
       lwd = 2.5)
points(fig_2$time[fig_2$Tx == 0],
       fig_2$mean_obe_pred[fig_2$Tx == 0],
       pch = 16,
       cex = 1.2)

plot(fig_2$time[fig_2$Tx == 1],
     fig_2$mean_obe_pred[fig_2$Tx == 1],
     type = "l",
     bty = 'n',
     axes = FALSE,
     xaxs = "i",
     yaxs = "i",
     ylim = c(-1, 100),
     xlim = c(-1, 37),
     xlab = "Week",
     ylab = " ",
     col = "white",
     main = " ")
axis(1, at = c(0, 4, 8, 12, 24, 36),
     col = "grey20",
     lwd.ticks = 0)
for(i in 1:66) {
  lines(all_data$time[all_data$Tx == 1
                      & all_data$Id == i],
        all_data$OBE_pred[all_data$Tx == 1
                          & all_data$Id == i],
        col = "darkgrey",
        cex = 0.8)
  points(all_data$time[all_data$Tx == 1
                       & all_data$Id == i],
         all_data$OBE_pred[all_data$Tx == 1
                           & all_data$Id == i],
         pch = 16,
         col = "darkgrey")
}
lines(fig_2$time[fig_2$Tx == 1],
      fig_2$mean_obe_pred[fig_2$Tx == 1],
      lwd = 1.5)
points(fig_2$time[fig_2$Tx == 1],
       fig_2$mean_obe_pred[fig_2$Tx == 1],
       pch = 16,
       cex = 1.2)
abline(h = -1,v = -1, col = "grey20",
       lwd = 2.5)
mtext("Data",  
      side = 2,
      line = 0,
      at = 0.75,
      outer = TRUE,
      cex = 1.5) 
mtext("Model",  
      side = 2,
      line = 0,
      at = 0.25,
      outer = TRUE,
      cex = 1.5) 
legend(x = 24, y = 40,
       bty = 'n',
       col = c("grey", "black"),
       legend = c("Individuals", "Mean"),
       pch = 16,
       lty = 1)

# Put results in a table
xtable(summary(fit2, pars = c("gamma", "mu_gamma", "tau_gamma"))$summary)



  
  

