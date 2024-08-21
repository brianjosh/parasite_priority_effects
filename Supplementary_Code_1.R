#Supplementary_Code_1

#Code for "Pre- and post-infection priority effects have contrasting outcomes for parasite prevalence in host populations"
#Joshua I. Brian

library(tidyverse)

#This code provides one run-through with one possible combination of trematode prevalence and bitterling prevalence
#For complete definition of all symbols, see Supporting_Information_S1

#Combinations explored in the paper:
#Combo 1: Pt=0.16, Pb=0.16
#Combo 2: Pt=0.16, Pb=0.36
#Combo 3: Pt=0.16, Pb=0.57
#Combo 4: Pt=0.31, Pb=0.16
#Combo 5: Pt=0.31, Pb=0.36
#Combo 6: Pt=0.31, Pb=0.57

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



