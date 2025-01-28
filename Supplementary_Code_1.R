#Supplementary_Code_1

#Code for "Pre- and post-infection priority effects have contrasting outcomes for parasite prevalence in host populations"
#Joshua I. Brian

library(tidyverse)
library(cowplot)
library(ggpubr)
library(matrixcalc)

#This code provides one run-through with one possible combination of trematode prevalence and bitterling prevalence
#For complete definition of all symbols, see Appendix_S1

#Combinations explored in the paper:
#Combo 1: Pt=0.16, Pb=0.16
#Combo 2: Pt=0.16, Pb=0.36
#Combo 3: Pt=0.16, Pb=0.57
#Combo 4: Pt=0.31, Pb=0.16
#Combo 5: Pt=0.31, Pb=0.36
#Combo 6: Pt=0.31, Pb=0.57

############################################################################################################################

#PROBABILITY TREE SIMULATION METHOD

#For parasite choice model (pre-infection priority effects):

S <- 0.7
Pt <- 0.16
Pb <- 0.16
w <- seq(from=0.2, to=0.8, by=0.01)

bothinfected <- Pt*(((1 - w)*Pb)/(Pt - w*(2*Pt - 1)))*S

tremonly <- Pt*(((1 - w)*Pb)/(Pt - w*(2*Pt - 1)))*(1-S) + Pt*(1 - ((1 - w)*Pb)/(Pt - w*(2*Pt - 1)))

bittonly <- (1 - Pt)*((w*Pb)/(Pt - w*(2*Pt - 1)))*S

uninfected <- (1 - Pt)*((w*Pb)/(Pt - w*(2*Pt - 1)))*(1-S) + (1 - Pt)*(1 - ((w*Pb)/(Pt - w*(2*Pt - 1))))

#Cu = (w*Pb)/(Pt - w*(2*Pt - 1))

#Ci = ((1 - w)*Pb)/(Pt - w*(2*Pt - 1))

#Put them all into a dataset
#(I just show one here, just run the code in lines 21-32 six times, once for each combo)

Infections <- c(bothinfected, tremonly, bittonly, uninfected)
Weighting <- rep(w, 4)
Status <- c(rep("Dual infection", 61), rep("Trematodes only", 61),
                rep("Bitterling only", 61), rep("Uninfected", 61))
Tremprevcombo <- rep("Pt=0.16", 244)
Bittprevcombo <- rep("Pb=0.16", 244)

Dataframecombo1 <- data.frame(Infections, Weighting, Status, Tremprevcombo, Bittprevcombo)
Dataframecombo2 <- data.frame(Infections, Weighting, Status, Tremprevcombo, Bittprevcombo)
Dataframecombo3 <- data.frame(Infections, Weighting, Status, Tremprevcombo, Bittprevcombo)
Dataframecombo4 <- data.frame(Infections, Weighting, Status, Tremprevcombo, Bittprevcombo)
Dataframecombo5 <- data.frame(Infections, Weighting, Status, Tremprevcombo, Bittprevcombo)
Dataframecombo6 <- data.frame(Infections, Weighting, Status, Tremprevcombo, Bittprevcombo)

Dataframe <- bind_rows(Dataframecombo1, Dataframecombo2, Dataframecombo3, Dataframecombo4, 
                       Dataframecombo5, Dataframecombo6)

Dataframe$Status <- as.factor(Dataframe$Status)
Dataframe$Tremprevcombo <- as.factor(Dataframe$Tremprevcombo)
Dataframe$Bittprevcombo <- as.factor(Dataframe$Bittprevcombo)

#Plot using facet_grid

prevchoice <- ggplot(data=Dataframe, aes(x=Weighting, y=Infections, group=Status)) + 
  geom_line(aes(linetype=Status), size=0.6) +
  facet_grid(Tremprevcombo ~ Bittprevcombo) +
  scale_linetype_manual(values=c("dashed", "solid", "twodash", "dotted"))+
  labs(x=expression(w[c]), y="Proportion of mussel population") + 
  theme_bw() + theme(legend.position = "top") 
prevchoice

ggsave("bitterlingchoicegraph.svg", width=150, height=100, units="mm")

#For parasite survival model (post-infection priority effects):

S <- 0.7
Pt <- 0.16
Pb <- 0.16
w <- seq(from=0.3, to=0.8, by=0.01)

bothinfected <- Pt*Pb*((S*(1 - w))/w)

tremonly <- Pt*Pb*(1 - ((S*(1 - w))/w)) + Pt*(1 - Pb)

bittonly <- (1 - Pt)*Pb*S
bittonly <- rep(bittonly, 51)

uninfected <- (1 - Pt)*Pb*(1 - S) + (1 - Pb)*(1 - Pt)
uninfected <- rep(uninfected, 51)
  
#Si = (S*(1 - w))/w

#Put them all into a dataset
#(I just show one here, just run the code in lines 76-89 six times, once for each combo)

Infections <- c(bothinfected, tremonly, bittonly, uninfected)
Weighting <- rep(w, 4)
Status <- c(rep("Dual infection", 51), rep("Trematodes only", 51),
            rep("Bitterling only", 51), rep("Uninfected", 51))
Tremprevcombo <- rep("Pt=0.16", 204)
Bittprevcombo <- rep("Pb=0.16", 204)

Dataframecombo1 <- data.frame(Infections, Weighting, Status, Tremprevcombo, Bittprevcombo)
Dataframecombo2 <- data.frame(Infections, Weighting, Status, Tremprevcombo, Bittprevcombo)
Dataframecombo3 <- data.frame(Infections, Weighting, Status, Tremprevcombo, Bittprevcombo)
Dataframecombo4 <- data.frame(Infections, Weighting, Status, Tremprevcombo, Bittprevcombo)
Dataframecombo5 <- data.frame(Infections, Weighting, Status, Tremprevcombo, Bittprevcombo)
Dataframecombo6 <- data.frame(Infections, Weighting, Status, Tremprevcombo, Bittprevcombo)

Dataframesurv <- bind_rows(Dataframecombo1, Dataframecombo2, Dataframecombo3, Dataframecombo4, 
                       Dataframecombo5, Dataframecombo6)

Dataframesurv$Status <- as.factor(Dataframesurv$Status)
Dataframesurv$Tremprevcombo <- as.factor(Dataframesurv$Tremprevcombo)
Dataframesurv$Bittprevcombo <- as.factor(Dataframesurv$Bittprevcombo)

#Plot using facet_grid

prevsurvival <- ggplot(data=Dataframesurv, aes(x=Weighting, y=Infections, group=Status)) + 
  geom_line(aes(linetype=Status), size=0.6) +
  facet_grid(Tremprevcombo ~ Bittprevcombo) +
  scale_linetype_manual(values=c("dashed", "solid", "twodash", "dotted"))+
  labs(x=expression(w[s]), y="Proportion of mussel population") + 
  theme_bw() + theme(legend.position = "top") 
prevsurvival

ggsave("bitterlingsurvivalgraph.svg", width=150, height=100, units="mm")

############################################################################################################################

#DISCRETE TIME MARKOV CHAIN SIMULATION METHOD

#EVEN ODDS FOR PT=0.31, PB=0.36 (translates to actual final prevalence of 0.252)

MCneutral <- matrix(c(0.9, 0, 0.1, 0,
                      0, 0.9, 0, 0.1,
                      0.3, 0, 0.7, 0,
                      0, 0.3, 0, 0.7), nrow=4, byrow=TRUE)
show(MCneutral)

initialstate <- c(0.69, 0.31, 0, 0)

#wc=0.5, ws=0.5
initialstate %*% matrix.power(MCneutral, 50)
# 0.5175 0.2325 0.1725 0.0775

#FOR PREINFECTION PRIORITY EFFECTS (adjusting transition probabilities to yield the tested wc values)

#wc=0.6
MCw0.6 <- matrix(c(0.88, 0, 0.12, 0,
                   0, 0.92, 0, 0.08,
                   0.3, 0, 0.7, 0,
                   0, 0.3, 0, 0.7), nrow=4, byrow=TRUE)
initialstate %*% matrix.power(MCw0.6, 50)
# 0.4928571 0.2447368 0.1971429 0.06526316

#wc=0.7
MCw0.7 <- matrix(c(0.86, 0, 0.14, 0,
                   0, 0.94, 0, 0.06,
                   0.3, 0, 0.7, 0,
                   0, 0.3, 0, 0.7), nrow=4, byrow=TRUE)
initialstate %*% matrix.power(MCw0.7, 50)
# 0.4704545 0.2583333 0.2195455 0.05166667

#wc=0.4
MCw0.4 <- matrix(c(0.92, 0, 0.08, 0,
                   0, 0.88, 0, 0.12,
                   0.3, 0, 0.7, 0,
                   0, 0.3, 0, 0.7), nrow=4, byrow=TRUE)
initialstate %*% matrix.power(MCw0.4, 50)
# 0.5447368 0.2214286 0.1452632 0.08857143

#wc=0.3
MCw0.3 <- matrix(c(0.94, 0, 0.06, 0,
                   0, 0.86, 0, 0.14,
                   0.3, 0, 0.7, 0,
                   0, 0.3, 0, 0.7), nrow=4, byrow=TRUE)
initialstate %*% matrix.power(MCw0.3, 50)
# 0.575 0.2113636 0.115 0.09863636

#FOR POSTINFECTION PRIORITY EFFECTS (adjusting transition probabilities to yield the tested ws values)

#ws=0.6
MCwS0.6 <- matrix(c(0.9, 0, 0.1, 0,
                    0, 0.9, 0, 0.1,
                    0.3, 0, 0.7, 0,
                    0, 0.47, 0, 0.53), nrow=4, byrow=TRUE)
initialstate %*% matrix.power(MCwS0.6, 50)
# 0.5175 0.255614 0.1725 0.05438596

#ws=0.7
MCwS0.7 <- matrix(c(0.9, 0, 0.1, 0,
                    0, 0.9, 0, 0.1,
                    0.3, 0, 0.7, 0,
                    0, 0.7, 0, 0.3), nrow=4, byrow=TRUE)
initialstate %*% matrix.power(MCwS0.7, 50)
#  0.5175 0.27125 0.1725 0.03875

#ws=0.42
MCwS0.42 <- matrix(c(0.9, 0, 0.1, 0,
                     0, 0.9, 0, 0.1,
                     0.3, 0, 0.7, 0,
                     0, 0.03, 0, 0.97), nrow=4, byrow=TRUE)
initialstate %*% matrix.power(MCwS0.42, 50)
#   0.5175 0.07176408 0.1725 0.2382359

#ws=0.45
MCwS0.45 <- matrix(c(0.9, 0, 0.1, 0,
                     0, 0.9, 0, 0.1,
                     0.3, 0, 0.7, 0,
                     0, 0.15, 0, 0.85), nrow=4, byrow=TRUE)
initialstate %*% matrix.power(MCwS0.45, 50)
#  0.5175 0.1860001 0.1725 0.1239999

#Make them into dataframes

w <- c(0.3, 0.4, 0.5, 0.6, 0.7)
w2 <- c(0.42, 0.45, 0.5, 0.6, 0.7)
preinfectionchain <- data.frame(Status=c(rep("Dual infection", 5), rep("Trematodes only", 5),
                                         rep("Bitterling only", 5), rep("Uninfected", 5)), 
                                Weighting=rep(w, 4),
                                Infections=c(0.09863636, 0.08857143, 0.0775, 0.06526316, 0.05166667,
                                             0.2113636, 0.2214286, 0.2325, 0.2447368, 0.2583333,
                                             0.115, 0.1452632, 0.1725, 0.1971429, 0.2195455,
                                             0.575, 0.5447368, 0.5175, 0.4928571, 0.4704545))

postinfectionchain <- data.frame(Status=c(rep("Dual infection", 5), rep("Trematodes only", 5),
                                          rep("Bitterling only", 5), rep("Uninfected", 5)), 
                                 Weighting=rep(w2, 4),
                                 Infections=c(0.2382359, 0.1239999, 0.0775, 0.05438596, 0.03875,
                                              0.07176408, 0.1860001, 0.2325, 0.255614, 0.27125,
                                              0.1725, 0.1725, 0.1725, 0.1725, 0.1725,
                                              0.5175, 0.5175, 0.5175, 0.5175, 0.5175))

#Plot them

markovchoice <- ggplot(data=preinfectionchain, aes(x=Weighting, y=Infections, group=Status)) + 
  geom_line(aes(linetype=Status), size=0.6) +
  scale_linetype_manual(values=c("dashed", "solid", "twodash", "dotted"))+
  labs(x=expression(w[c]), y="Proportion of mussel population") + 
  theme_bw() + theme(legend.position = "top") + ylim(0, 0.6)
markovchoice

markovsurvive <- ggplot(data=postinfectionchain, aes(x=Weighting, y=Infections, group=Status)) + 
  geom_line(aes(linetype=Status), size=0.6) +
  scale_linetype_manual(values=c("dashed", "solid", "twodash", "dotted"))+
  labs(x=expression(w[s]), y="Proportion of mussel population") + 
  theme_bw() + theme(legend.position = "top") + ylim(0, 0.6)
markovsurvive

#Plot both together

markov <- ggarrange(markovchoice, markovsurvive, ncol=2, labels=c("A", "B"))
markov
ggsave("markov.svg", height=100, width=200, units="mm")



