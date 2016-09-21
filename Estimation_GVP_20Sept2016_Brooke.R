rm(list = ls())

library(tidyr)
library(ggplot2)
library(dplyr)
library(viridis)
library(deSolve)
library(knitcitations)
library(purrr)
library(animation)
library(ReporteRsjars)
library(ReporteRs)
library(poweRlaw)
library(grid)


# Set working directory - change 
setwd("~/Documents/EIDR")


# Read data from csv files in folder
eidr <- read.csv(file = "eidr.csv", header = T, sep = ",")
eidr_pandemics <- read.csv(file= "pandemics.csv", header =T, sep= ",")
eidr2 <- read.csv2(file = "eidr_with_pandemics.csv", header = T, sep = ",")


# Use only numeric values from eidr$Number.of.Deaths
eidr2$Number.of.Deaths2 <- parse_numeric(eidr2$Number.of.Deaths)
Number.of.Deaths3 <- eidr2$Number.of.Deaths2[!is.na(eidr2$Number.of.Deaths2)]
Number.of.Deaths4 <- Number.of.Deaths3[Number.of.Deaths3!=0]

# SMU = Number.of.Deaths4/World.population
m_nofd = displ$new(Number.of.Deaths4)
est_nofd = estimate_xmin(m_nofd, pars=seq(1, 2, 0.01))
est_nofd = estimate_xmin(m_nofd)

m_nofd$setXmin(est_nofd)

# Plot fitness of Power Law distribution
plot(m_nofd)
lines(m_nofd)

# Set parameters
beta = 6.9e9   # base damage value
gamma = .0124  # convex damage function
years = 100    # length of simulation
reps = 1000    # number of trials
dt = 1 #year
delta = 0.05 #discount rate
wtp = 638e3 # statistical value of each death from WTP=$64 for a 1/10000 risk reduction of death
SMU = 1/10000
world_pop = 7.35e9
years_data = 2013 - 1940
mean_events = 106/years_data


# Draw a number of events (per year) from a Poisson distribution with mean 2
Z1 = matrix(rpois(reps*(years+1),2), reps, years+1)
# Max possible deaths in 1 event - 4 sizes
max_pandemic_size = 110*SMU * world_pop #include pandemics that could kill 1.1% of population
max_pandemic_size2 = 8*SMU * world_pop  #include pandemics that could kill 0.08% of population
max_pandemic_size3 = 3*SMU * world_pop  #include pandemics that could kill 0.03% of population 
max_pandemic_size4 = 220*SMU * world_pop #include pandemics that could kill 2.2% of population

power_samples = dist_rand(m_nofd, 10*sum(Z1))
power_samples = power_samples[power_samples < max_pandemic_size]
power_samples2 = power_samples[power_samples < max_pandemic_size2]
power_samples3 = power_samples[power_samples < max_pandemic_size3]
power_samples4 = power_samples[power_samples < max_pandemic_size4]

deaths_per_year <- function(events) {
  sum(base::sample(power_samples, events))
}
deaths_per_year2 <- function(events) {
  sum(base::sample(power_samples2, events))
}
deaths_per_year3 <- function(events) {
  sum(base::sample(power_samples3, events))
}
deaths_per_year4 <- function(events) {
  sum(base::sample(power_samples4, events))
}

#Matrices with total number of deaths for all events in a year (it takes some time to run this part, ~15 min )
D1 = plyr::aaply(Z1, c(1,2), deaths_per_year, .progress = "time")
D2 = plyr::aaply(Z1, c(1,2), deaths_per_year2, .progress = "time")
D3 = plyr::aaply(Z1, c(1,2), deaths_per_year3, .progress = "time")
D4 = plyr::aaply(Z1, c(1,2), deaths_per_year4, .progress = "time")


wtp_D1 = wtp*D1 # multiply each number of deaths per year by the value of each life
wtp_D2 = wtp*D2 # multiply each number of deaths per year by the value of each life
wtp_D3 = wtp*D3 # multiply each number of deaths per year by the value of each life
wtp_D4 = wtp*D4 # multiply each number of deaths per year by the value of each life

# Equations for damage function - linear
damage_fn <- function(Z) {
  wtp *(Z)
}

lives_fn <- function(Z) {
    
}

Z_all <-list(SMU_110=D1, SMU_8=D2, SMU_3=D3, SMU_220=D4) %>%
{  map2(., names(.), function(Z, method_name) {
  Z %>%
    as.data.frame() %>%
    set_colnames(0:(ncol(Z) - 1)) %>%
    mutate(rep = 1:n()) %>%
    gather("time", "Z", -rep) %>%
    mutate(time = as.numeric(time)) %>%
    arrange(rep, time) %>%
    mutate(method = method_name)
})} %>%
  bind_rows() %>%
  mutate(damage = damage_fn(Z)) %>%
  group_by(method, rep) %>%
  mutate(damage_discounted = damage * exp(-delta * time))

Z_ave = Z_all %>%
  group_by(method, time) %>%
  summarise(Z_ave = mean(Z), se = sd(Z), Z_upper = Z_ave+2*se, Z_lower=0) %>% #lower bound of damages is equal to zero
  mutate(damage = damage_fn(Z_ave), d_lower = damage_fn(Z_lower), d_upper = damage_fn(Z_upper)) #%>%
  mutate_each(funs(. * exp(-delta * time)), damage, d_lower, d_upper)

Z_tot = Z_all %>%
  group_by(rep, method) %>%
  summarize(total = sum(damage_discounted), damage_mean=mean(damage)  ) %>%
  group_by(method) %>%
  summarize(method_total = mean(total), method_sd  = sd(total), method_max = max(total), median(total))

Z_max = Z_all %>% 
    group_by(method) %>% 
    summarize(maxdam = max(damage), maxdeath = max(Z))

#using damages - 4 simulations
#sim 1 - SMU 110 
Z1_reps = filter(Z_all, method=="SMU_110", rep %in% 280:284)

plot110 <- ggplot(filter(Z_ave, method=="SMU_110"),
       aes(x = time, y = damage))+
       theme(legend.position="none")+
        labs(title="Maximum SMU= 110")+
  # geom_ribbon(mapping=aes(ymin = d_lower, ymax = d_upper)) + 
  geom_line()  +
  geom_line(data=Z1_reps, mapping=aes(color=as.factor(rep)))
plot110

#sim 2: SMU 8
Z2_reps = filter(Z_all, method=="SMU_8", rep %in% 90:94)
plot8 <- ggplot(filter(Z_ave, method=="SMU_8"),
       aes(x = time, y = damage))+
    theme(legend.position="none")+
    labs(title="Maximum SMU= 8")+
  # geom_ribbon(mapping=aes(ymin = d_lower, ymax = d_upper)) + 
  geom_line() +
  geom_line(data=Z2_reps, mapping=aes(color=as.factor(rep)))
plot8

#sim 3: SMU 3
Z3_reps = filter(Z_all, method=="SMU_3", rep %in% 90:94)
plot3 <- ggplot(filter(Z_ave, method=="SMU_3"),
       aes(x = time, y = damage))+
    theme(legend.position="none")+
            labs(title="Maximum SMU= 3")+
  # geom_ribbon(mapping=aes(ymin = d_lower, ymax = d_upper)) + 
  geom_line() +
  geom_line(data=Z3_reps, mapping=aes(color=as.factor(rep)))
plot3

#sim 4: SMU 4 
Z4_reps = filter(Z_all, method=="SMU_220", rep %in% 90:94)
plot220 <- ggplot(filter(Z_ave, method=="SMU_220"),
        aes(x = time, y = damage))+
        theme(legend.position="none")+
        labs(title="Maximum SMU= 220")+
  # geom_ribbon(mapping=aes(ymin = d_lower, ymax = d_upper)) + 
  geom_line() +
  geom_line(data=Z4_reps, mapping=aes(color=as.factor(rep)))
plot220  

#save 
ga <- grid.arrange(plot3, plot8, plot110, plot220, nrow=2, top = "Random simulations modelling damages over 100 years")
ggsave("Simulations.pdf", ga)



#plotting averages 
#sim 1 - SMU 110 
plot110 <- ggplot(filter(Z_ave, method=="SMU_110"),
       aes(x = time, y = damage))+
       theme(legend.position="none")+
        labs(title="Maximum SMU= 110")+
  # geom_ribbon(mapping=aes(ymin = d_lower, ymax = d_upper)) + 
  geom_line() # +
  #geom_line(data=Z1_reps, mapping=aes(color=as.factor(rep)))
plot110

#sim 2: SMU 8
plot8 <- ggplot(filter(Z_ave, method=="SMU_8"),
       aes(x = time, y = damage))+
    theme(legend.position="none")+
    labs(title="Maximum SMU= 8")+
  # geom_ribbon(mapping=aes(ymin = d_lower, ymax = d_upper)) + 
  geom_line() #+
  #geom_line(data=Z2_reps, mapping=aes(color=as.factor(rep)))
plot8

#sim 3: SMU 3
plot3 <- ggplot(filter(Z_ave, method=="SMU_3"),
       aes(x = time, y = damage))+
    theme(legend.position="none")+
            labs(title="Maximum SMU= 3")+
  # geom_ribbon(mapping=aes(ymin = d_lower, ymax = d_upper)) + 
  geom_line() #+
  #geom_line(data=Z3_reps, mapping=aes(color=as.factor(rep)))
plot3

#sim 4: SMU 4 
plot220 <- ggplot(filter(Z_ave, method=="SMU_220"),
        aes(x = time, y = damage))+
        theme(legend.position="none")+
        labs(title="Maximum SMU= 220")+
  # geom_ribbon(mapping=aes(ymin = d_lower, ymax = d_upper)) + 
  geom_line() #+
  #geom_line(data=Z4_reps, mapping=aes(color=as.factor(rep)))
plot220  




#plot all four in same panel, save as PDF 
gave <- grid.arrange(plot3, plot8, plot110, plot220, nrow=2, top = "Averages damages over 100 years by maximum pandemic size")
ggsave("Averages.pdf", gave)


# histogram of number of deaths     
h3<- ggplot(filter(Z_ave, method=="SMU_3"), aes(Z_ave)) +
 geom_histogram(col="black", 
                fill="darkblue", bins=30)+ 
 labs(title="Maximum SMU = 3", x="Deaths", y="Count")
h3 

h8<-ggplot(filter(Z_ave, method=="SMU_8"), aes(Z_ave)) +
 geom_histogram(col="black", 
                fill="darkblue", bins=30)+ 
 labs(title="Maximum SMU = 8", x="Deaths", y="Count")
h8
 
h110<-ggplot(filter(Z_ave, method=="SMU_110"), aes(Z_ave)) +
 geom_histogram(col="black", 
                fill="darkblue", bins=30)+ 
 labs(title="Maximum SMU = 110", x="Deaths", y="Count")
h110

h220<-ggplot(filter(Z_ave, method=="SMU_220"), aes(Z_ave)) +
 geom_histogram(col="black", 
                fill="darkblue", bins=30)+ 
 labs(title="Maximum SMU = 220", x="Deaths", y="Count")
h220

hs <- grid.arrange(h3,h8,h110,h220, nrow=2, top = "Frequency of years with a given death count, based on maximum pandemic size")
ggsave("DeathHisto.pdf", hs)

#all histograms (lines) in one plot 
chs <- ggplot(Z_ave, aes(Z_ave, colour = method)) +
  geom_freqpoly(bins = 40, size = 1, alpha=3/4)+
    labs(title="Distrubtion of annual death counts over 100 years, based on max pandemic size")+
    labs(x="Deaths")
chs
ggsave("ColorfulDeathHisto.pdf", chs)