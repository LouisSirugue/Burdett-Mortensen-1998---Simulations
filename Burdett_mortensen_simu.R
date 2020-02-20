
#####################
# Burdett Mortensen #
#####################

## Parametrization ##
#####################

rm(list = ls())
library(ggplot2)
library(ggpubr)

#Consider the following set of parameters:
# Minimum wage
mw <- .8
# Job destruction rate
s <- .287
# Job offer arrival rate
lambda <- .142
# Productivity of firms of type 1
y1 <- 2
# Productivity of firms of type 2
y2 <- 2.5
# Share of low-productivity firms
sigma <- .25
# Precision of the curves:
precision <- 1/1000

# The bounds of the wage distribution write:
lb1 <- mw 
ub1 <- lb1 + ((y1 - lb1) * (1 - ((s / (s + lambda))^2)))
lb2 <- ub1
ub2 <- lb2 + ((y2 - lb2) * (1 - ((s / (s + lambda))^2)))

# Compute F(w)
##############
# Over the domain of firm 1
w1 <- seq(lb1, ub1, precision)
Fw1 <- (lambda / (lambda + s))^-1 * (1 - sqrt((y1 - w1)/(y1 - lb1)))

# Over the domain of firm 2
w2 <- seq(lb2 + precision, ub2, precision)
Fw2 <- (lambda / (lambda + s))^-1 * (1 - sqrt((y2 - w2)/(y2 - lb2)))

# Over the whole domain
wage <- c(w1, w2)
Fw <- c(Fw1 * sigma, sigma + (Fw2 * (1-sigma)))

# Compute G(w)
##############
Gw <- (s * Fw) / (s + (lambda * (1 - Fw)))

# Compute f(w)
##############
# Over the domain of firm 1
fw1 <- (sigma *(s + lambda)) / (2 * lambda * sqrt(y1 - lb1) * sqrt(y1 - w1))

# Over the domain of firm 2
fw2 <- ((1-sigma) *(s + lambda)) / (2 * lambda * sqrt(y2 - lb2) * sqrt(y2 - w2))

# Over the whole support
fw <- c(fw1, fw2)

# Compute g(w)
##############
gw <- (s*(s+lambda)*fw) / ((s + (lambda * (1 - Fw)))^2)

# Plot the results
##################

data <- data.frame(wage, Fw, Gw, fw, gw)
cumulplot <- ggplot(data = data, aes(x = wage)) + 
  geom_point(aes(y = Fw), color = 'steelblue', 
             size = 1, alpha = .2) +
  geom_point(aes(y = Gw), color = 'black', 
             size = 1, alpha = .2) +
  geom_segment(aes(x=ub1,y=0,xend=ub1,yend=(sigma*max(Fw1))), 
               linetype = "dashed", size = 1, alpha = .6) +
  scale_y_continuous(name = "Wage offers (Blue) - Salaries (Black)", 
                     limits = c(0, 1), breaks = seq(0,1,.25)) +
  scale_x_continuous(name = "Wage", limits = c(lb1, ub2), 
                     breaks = seq(lb1, ub2, .2)) +
  annotate("text", x = 1.75, y = 0.75, label = "F(w)", size = 4) +
  annotate("text", x = 1.92, y = 0.55, label = "G(w)", size = 4) +
  ggtitle("Figure 1: Cumulative distributions")

densityplot <- ggplot(data = data, aes(x = wage)) + 
  geom_point(aes(y = fw), color = 'steelblue', 
             size = 1, alpha = .2) +
  geom_point(aes(y = gw), color = 'black', 
             size = 1, alpha = .2) +
  geom_segment(aes(x=ub1,y=0,xend=ub1,yend=(min(fw2))), 
               linetype = "dashed", size = 1, alpha = .6) +
  scale_y_continuous(name = "Wage offers (Blue) - Salaries (Black)", 
                     limits = c(0, 2.5), breaks = seq(0,2.5,.5)) +
  scale_x_continuous(name = "Wage", limits = c(lb1, ub2), 
                     breaks = seq(lb1, ub2, .2)) +
  annotate("text", x = 1.975, y = 1.25, label = "f(w)", size = 4) +
  annotate("text", x = 1.825, y = 1.9, label = "g(w)", size = 4) +
  ggtitle("Figure 2: Density distributions")

ggarrange(cumulplot, densityplot, ncol = 2, nrow = 1)

# Number of workers per firm earning w
lw <- (s*lambda) / ((s + (lambda * (1 - Fw)))^2)
data <- data.frame(data, lw)
plotlw <- ggplot(data = data, aes(x = wage)) + 
  geom_point(aes(y = lw), color = 'steelblue', 
             size = 1, alpha = .2) +
  geom_segment(aes(x=ub1,y=.15,xend=ub1,yend=(lw[match(round(ub1,2), wage)])), 
               linetype = "dashed", size = 1, alpha = .6) +
  scale_y_continuous(name = "Number of workers per firm", 
                     limits = c(.15, .55), breaks = seq(.2,.5,.1)) +
  scale_x_continuous(name = "Wage", limits = c(lb1, ub2), 
                     breaks = seq(lb1, ub2, .2)) +
  annotate("text", x = 1.82, y = .45, label = "l(w)", size = 4) +
  ggtitle("Figure 3: Labor force")

# Number of workers per firm being offered a better wage
exits <- lambda * (1-Fw) * lw
data <- data.frame(data, exits)
plotqw <- ggplot(data = data, aes(x = wage)) + 
  geom_point(aes(y = exits), color = 'steelblue', 
             size = 1, alpha = .2) +
  geom_segment(aes(x=ub1,y=0,xend=ub1,yend=(exits[match(round(ub1,2), wage)])), 
               linetype = "dashed", size = 1, alpha = .6) +
  scale_y_continuous(name = "Voluntary quits", 
                     limits = c(0, max(exits)), breaks = seq(0,max(exits),.01)) +
  scale_x_continuous(name = "Wage", limits = c(lb1, ub2), 
                     breaks = seq(lb1, ub2, .2)) +
  annotate("text", x = 1.95, y = .015, label = "q(w)", size = 4) +
  ggtitle("Figure 4: Voluntary quits")

ggarrange(plotlw, plotqw, ncol = 2, nrow = 1)

## Consequences of a minimum-wage increase
##########################################

# Consider wage inequality as the difference between the average wage 
# and the minimum wage. To evaluate the impact of a minimum wage 
# increase, on wage inequality, consider 4 economies where: 
# {sigma, y_1, y_2} = {.25, 2, 2.25}
# {sigma, y_1, y_2} = {.25, 2, 3}
# {sigma, y_1, y_2} = {.75, 2, 2.25}
# {sigma, y_1, y_2} = {.75, 2, 3}

param1 <- c(.25, 2, 2.25)
param2 <- c(.25, 2, 3)
param3 <- c(.75, 2, 2.25)
param4 <- c(.75, 2, 3)
param <- rbind(param1, param2, param3, param4)
s <- .287
lambda <- .142
precision <- 1/1000
inequalities <- c()
minimum_wages <- c(.8, 1)

for (mw in minimum_wages) {
  inequalitiesi <- c()
  
  for (i in seq(1, 4, 1)) {
    
    # Bounds of the distributions
    #############################
    lb1 <- mw
    ub1 <- lb1 + ((param[i, 2] - lb1) * (1 - ((s / (s + lambda))^2)))
    lb2 <- ub1
    ub2 <- lb2 + ((param[i, 3] - lb2) * (1 - ((s / (s + lambda))^2)))
    
    # Compute F(w)
    ##############
    # Over the support of firm 1
    w1 <- seq(lb1, ub1, precision)
    Fw1 <- (((lambda + s) / lambda) * (1 - sqrt((param[i, 2] - w1)/(param[i, 2] - lb1))))
    # Over the support of firm 2
    w2 <- seq(lb2 + precision, ub2, precision)
    Fw2 <- (((lambda + s) / lambda) * (1 - sqrt((param[i, 3] - w2)/(param[i, 3] - lb2))))
    # Over the whole support
    wage <- c(w1, w2)
    Fw <- c(Fw1 * param[i, 1], param[i, 1] + (Fw2 * (1-param[i, 1])))
    
    # Compute f(w)
    ##############
    # Over the support of firm 1
    fw1 <- ((param[i, 1] *(s + lambda)) / (2 * lambda * sqrt(param[i, 2] - lb1) * sqrt(param[i, 2] - w1)))
    # Over the support of firm 2
    fw2 <- (((1-param[i, 1]) *(s + lambda)) / (2 * lambda * sqrt(param[i, 3] - lb2) * sqrt(param[i, 3] - w2)))
    # Over the whole support
    fw <- c(fw1, fw2)
    
    # Compute g(w)
    ##############
    gw <- (s*(s+lambda)*fw) / ((s + (lambda * (1 - Fw)))^2)
    
    # Compute the level of inequalities
    ###################################
    mean_wage <-  sum(gw * wage) / sum(gw)
    inequalitiesi <- c(inequalitiesi, mean_wage - mw)
    
  }
  
  inequalities <- rbind(inequalities, inequalitiesi)
}

# Compute the percentage change of the level of inequalities in each economy.
#############################################################################
pct_decrease <- c()
for (i in 1:4){
  pct <- (inequalities[1,i] - inequalities[2,i]) / inequalities[1,i]
  pct_decrease <- c(pct_decrease, pct)
}
pct_decrease <- round(pct_decrease * 100)

# Frame the results
###################
table <- rbind(round(inequalities, 2), pct_decrease) 
rownames(table) <- c("Inequalities (mw = .8)", 
                     "Inequalities (mw = 1)", 
                     "Percentage decrease")
colnames(table) <- c("{.25, 2.25}", "{.25, 3}", 
                     "{.75, 2.25}", "{.75, 3}")
table

# And graphically:
##################

plotEconomy <- function(parameters) {
  
  # Compute G(w) and the average wage under mw = .8
  mw <- .8
  s <- .287
  lambda <- .142
  y1 <- 2
  precision <- 1/1000
  lb1 <- mw 
  ub1 <- lb1 + ((y1 - lb1) * (1 - ((s / (s + lambda))^2)))
  lb2 <- ub1
  ub2 <- lb2 + ((parameters[2] - lb2) * (1 - ((s / (s + lambda))^2)))
  w1 <- seq(lb1, ub1, precision)
  Fw1 <- (lambda / (lambda + s))^-1 * (1 - sqrt((y1 - w1)/(y1 - lb1)))
  w2 <- seq(lb2 + precision, ub2, precision)
  Fw2 <- (lambda / (lambda + s))^-1 * (1 - sqrt((parameters[2] - w2)/(parameters[2] - lb2)))
  wage <- c(w1, w2)
  Fw <- c(Fw1 * parameters[1], parameters[1] + (Fw2 * (1-parameters[1])))
  fw1 <- (parameters[1] *(s + lambda)) / (2 * lambda * sqrt(y1 - lb1) * sqrt(y1 - w1))
  fw2 <- ((1-parameters[1]) *(s + lambda)) / (2 * lambda * sqrt(parameters[2] - lb2) * sqrt(parameters[2] - w2))
  fw <- c(fw1, fw2)
  Gw <- (s * Fw) / (s + (lambda * (1 - Fw)))
  gw <- (s*(s+lambda)*fw) / ((s + (lambda * (1 - Fw)))^2)
  mean_wage <-  sum(gw * wage) / sum(gw)
  
  # Compute G(w) and the average wage under mw = 1
  mw <- 1
  lb1 <- mw
  lb1id <- which.min(abs(wage-lb1))
  ub1 <- lb1 + ((y1 - lb1) * (1 - ((s / (s + lambda))^2)))
  lb2 <- ub1
  ub1id <- which.min(abs(wage-ub1))
  ub2 <- lb2 + ((parameters[2] - lb2) * (1 - ((s / (s + lambda))^2)))
  ubmax <- ub2
  upperw <- seq(wage[length(wage)], ubmax, precision)
  wage <- c(wage, upperw)
  upperGw <- rep(NA, length(upperw))
  Gw <- c(Gw, upperGw)
  w1 <- wage[lb1id:ub1id]
  Fw1 <- (lambda / (lambda + s))^-1 * (1 - sqrt((y1 - w1)/(y1 - lb1)))
  w2 <- wage[seq(ub1id, length(wage), 1)]
  Fw2 <- (lambda / (lambda + s))^-1 * (1 - sqrt((parameters[2] - w2)/(parameters[2] - lb2)))
  wage2 <- c(w1, w2)
  Fw <- c(Fw1 * parameters[1], parameters[1] + (Fw2 * (1-parameters[1])))
  fw1 <- (parameters[1] *(s + lambda)) / (2 * lambda * sqrt(y1 - lb1) * sqrt(y1 - w1))
  fw2 <- ((1-parameters[1]) *(s + lambda)) / (2 * lambda * sqrt(parameters[2] - lb2) * sqrt(parameters[2] - w2))
  fw <- c(fw1, fw2)
  Gw2 <- (s * Fw) / (s + (lambda * (1 - Fw)))
  gw <- (s*(s+lambda)*fw) / ((s + (lambda * (1 - Fw)))^2)
  mean_wage2 <-  sum(gw * wage2) / sum(gw)
  seqbelowGw2 <- rep(NA, length(wage) - length(wage2))
  Gw2 <- c(seqbelowGw2, Gw2)
  
  # Plot the economy
  ##################
  data <- data.frame(wage, Gw, Gw2)
  ggplot(data = data, aes(x = wage)) + 
    geom_point(aes(y = Gw), na.rm=TRUE , color = 'steelblue', 
               size = 1, alpha = .2) +
    geom_segment(aes(x=.8,y=7/8,xend=.8,yend=1), 
                 color = "steelblue", size = 1, alpha = .6) +
    geom_segment(aes(x=mean_wage,y=7/8,xend=mean_wage,yend=1), 
                 color = "steelblue", size = 1, alpha = .6) +
    geom_segment(aes(x=.8,y=7.5/8,xend=.8+.02,yend=7.5/8-.02), 
                 color = "steelblue", size = 1, alpha = .6) +
    geom_segment(aes(x=.8,y=7.5/8,xend=.8+.02,yend=7.5/8+.02), 
                 color = "steelblue", size = 1, alpha = .6) +
    geom_segment(aes(x=mean_wage,y=7.5/8,xend=mean_wage -.02,
                     yend=7.5/8-.02), 
                 color = "steelblue", size = 1, alpha = .6) +
    geom_segment(aes(x=mean_wage,y=7.5/8,xend=mean_wage -.02,
                     yend=7.5/8+.02), 
                 color = "steelblue", size = 1, alpha = .6) +
    geom_segment(aes(x=.8,y=7.5/8,xend=mean_wage,yend=7.5/8), 
                 color = "steelblue", linetype = "dashed", 
                 size = 1, alpha = .6) +
    annotate("text", x = (mean_wage + .8)/2, y = 7.5/8 + .05, label = 
               paste("Inequality (mw=.8): ", round(mean_wage - .8, 2), 
                     sep = ""), size = 4, color = "steelblue") +
    annotate("text", x = .8 , y = 6.75/8, label = 
               "mw", size = 4, color = "steelblue") +
    annotate("text", x = mean_wage, y = 6.75/8, label = 
               "mean", size = 4, color = "steelblue") +
    geom_point(aes(y = Gw2), na.rm=TRUE , color = 'black', 
               size = 1, alpha = .2) +
    geom_segment(aes(x=1,y=5/8,xend=1,yend=6/8), 
                 color = "black", size = 1, alpha = .6) +
    geom_segment(aes(x=mean_wage2,y=5/8,xend=mean_wage2,yend=6/8), 
                 color = "black", size = 1, alpha = .6) +
    geom_segment(aes(x=1,y=5.5/8,xend=1+.02,yend=5.5/8-.02), 
                 color = "black", size = 1, alpha = .6) +
    geom_segment(aes(x=1,y=5.5/8,xend=1+.02,yend=5.5/8+.02), 
                 color = "black", size = 1, alpha = .6) +
    geom_segment(aes(x=mean_wage2,y=5.5/8,xend=mean_wage2 -.02,
                     yend=5.5/8-.02), 
                 color = "black", size = 1, alpha = .6) +
    geom_segment(aes(x=mean_wage2,y=5.5/8,xend=mean_wage2 -.02,
                     yend=5.5/8+.02), 
                 color = "black", size = 1, alpha = .6) +
    geom_segment(aes(x=1,y=5.5/8,xend=mean_wage2,yend=5.5/8), 
                 color = "black", linetype = "dashed", 
                 size = 1, alpha = .6) +
    annotate("text", x = (mean_wage2 + 1)/2, y = 5.5/8 + .05, label = 
               paste("Inequality (mw=1): ", round(mean_wage2 - 1, 2), 
                     sep = ""), size = 4, color = "black") +
    annotate("text", x = 1 , y = 4.75/8, label = 
               "mw", size = 4, color = "black") +
    annotate("text", x = mean_wage2, y = 4.75/8, label = 
               "mean", size = 4, color = "black") +
    scale_y_continuous(name = "Salaries repartition", 
                       limits = c(0, 1), breaks = seq(0,1,.25)) +
    scale_x_continuous(name = "Wage", limits = c(.8, ub2), 
                       breaks = seq(.8, ub2, .2))
}

parameters1 <- c(.25, 2.25)
parameters2 <- c(.75, 2.25)
parameters3 <- c(.25, 3)
parameters4 <- c(.75, 3)

ggarrange(plotEconomy(parameters1), plotEconomy(parameters2), 
          plotEconomy(parameters3), plotEconomy(parameters4), 
          ncol = 2, nrow = 2)
