#### Question 1 answers ####
library(ggpubr)
library(tidyverse)
library(ggrepel) #these packages let me perform the operations/functions that I need 

mico.data <- read.csv("C:/Users/katie/Downloads/MycotoxinData.csv", na.strings = "na") #this codes na data as "na"
str(mico.data)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")#this is the color blind palette

mico.data$DON <- as.factor(mico.data$DON) #this treats DON as a continuous variable
mico.data$Treatment <- as.factor(mico.data$Treatment)

str(mico.data$DON)
str(mico.data$Treatment)

#### Question 1 ####
mico.ques1 <- ggplot(mico.data, aes(x = Treatment, y = DON, color = Cultivar)) + #this defines the aesthetics and allows me to color by cultivar 
  geom_boxplot(position = position_dodge()) + # this provides boxplots that do not overlap 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6)) + #this allows data points that do no overlap 
  ylab("DON(ppm)")+ #this labels the y axis
  xlab("") + #this labels teh x axis
  scale_color_manual(values = cbbPalette, name = "", labels = c("Wheaton", "Ambassador")) + # this gives me colors from the colorblind palette 
  theme_classic() + #this is a classic theme for the plot 
  theme(strip.background = element_blank(), legend.position = "right") + #this puts the legend to the right hand side
  facet_wrap(~Cultivar) #this create separate panels for the cultivar
print(mico.ques1)


#### Question 2 answers ####

mico.data$Treatment <- factor(mico.data$Treatment, levels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70")) #this rearranges the y labels

mico.ques2 <- ggplot(mico.data, aes(x = Treatment, y = DON, color = Cultivar)) +
  geom_boxplot(position = position_dodge(.6)) + #this adjusts the position of the horizontal bars 
  geom_point(position = position_jitterdodge(.6)) + #this allows the data points to be not overlapping 
  ylab("DON(ppm)")+
  xlab("") +
  scale_color_manual(values = cbbPalette, name = "", labels = c("Wheaton", "Ambassador")) +
  theme_classic() +
  theme(strip.background = element_blank(), legend.position = "right") +
  facet_wrap(~Cultivar)
print(mico.ques2)


#### Question 3 answers ####

mico.ques3 <- ggplot(mico.data, aes(x = MassperSeed_mg, y = X15ADON, color = Cultivar)) + #this is when you put massperseed_mg on the x axis and X15ADOn is on the y axis 
  geom_boxplot(position = position_dodge(.85)) +
  geom_point(position = position_jitterdodge(.05)) +
  ylab("X15ADON")+
  xlab("Seed Mass (mg)") +
  scale_color_manual(values = cbbPalette, name = "", labels = c("Wheaton", "Ambassador")) +
  theme_classic() +
  theme(strip.background = element_blank(), legend.position = "right") +
  facet_wrap(~Cultivar)
print(mico.ques3)


#### Question 4 answers ####

figure1 <- ggarrange(mico.ques1, 
                     mico.ques2, 
                     mico.ques3, 
                     labels = "auto", #this labels the plots as A, B, C
                     nrow = 3, 
                     ncol =  1, 
                     common.legend = T) #this allows you to put several plots stacked on top of one another in one column 
print(figure1)

# the common.legend function allows you to utilize only one legend for different plots 

#### Question 5 answers ####

mico.ques.ttest1 <- mico.ques1 + 
  geom_pwc(aes(group = Treatment, method = "t_test", label = "p.adj.format")) #the geom_pwc is a pairwise comparison test that allows you to compare all treatments against each other 
print(mico.ques.ttest1)

mico.ques.ttest2 <- mico.ques2 +
  geom_pwc(aes(group = Treatment, method = "t_test", label = "p.adj.format"))
print(mico.ques.ttest2)


mico.ques.ttest3 <- mico.ques3 +
  geom_pwc(aes(group = MassperSeed_mg, method = "t_test", label = "p.adj.format"))
print(mico.ques.ttest3)


figure2 <- ggarrange(mico.ques.ttest1, 
                     mico.ques.ttest2, 
                     mico.ques.ttest3,
                     labels = "auto", 
                     nrow = 3, 
                     ncol = 1, 
                     common.legend = T) 
print(figure2)

















































