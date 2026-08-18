
library(tidyverse)
library(pairwise)
library(catR)
library(TAM)
#_______________________________________________________________________________

## Hinweis für mich: Der Code war ursprünglich für die Rohdaten, ggf. muss er für die wide-Daten in GitHub angepasst werden.

rownames(Data) <- paste0(rownames(Data), "-", Data$SFB)                         # Adding Special Needs Status to Person ID
Data <- select(Data, starts_with("P"))                                          # Only Test 1: Phonological Awareness

#_______________________________________________________________________________
# 2. Prepare Input Data

Pair <- pair(Data)                                                              # Rasch model on Item Pool 1

IP1 <- data.frame(a=1, b=Pair$sigma, c=0, d=1)                                  # Item Difficulties Item pool 1
rownames(IP1)[rownames(IP1) == "P_5A"] <- "start"                               # Set Start Item Item pool 1
ggplot(IP1, aes(x=b)) +                                                         # Plot Item Pool 1
  geom_histogram(binwidth=0.2, color="black", fill="grey") + theme_bw() + 
  labs(title="σ of 36 original items",x="σ", y = "count") + 
  xlim(-2.5,2.5) + ylim(0, 30)

IP2 <- rbind(IP1, IP1, IP1, IP1, IP1, IP1)                                      # Item Difficulties Item pool 2
rownames(IP1)[rownames(IP2) == "P_5A"] <- "start"                               # Set Start Item pool 2
ggplot(IP2, aes(x=b)) +                                                         # Plot Item Pool 2
  geom_histogram(binwidth=0.2, color="black", fill="grey") + theme_bw() + 
  labs(title="σ of 216 original items",x="σ", y = "count") + 
  xlim(-2.5,2.5) + ylim(0, 30)

IP3 <- genDichoMatrix(items=200, model="1PL", bPrior=c("norm", 0, 1), seed=1)   # Item Difficulties Item pool 3
rownames(IP3)[rownames(IP3) == "78"] <- "start"                                 # Set Start Item pool 3
ggplot(IP3, aes(x=b)) +                                                         # Plot Item Pool 3
  geom_histogram(binwidth=0.2, color="black", fill="grey") + theme_bw() + 
  labs(title="σ of 200 generated items",x="σ", y = "count") + 
  xlim(-2.5,2.5) + ylim(0, 30)

Pers <- pers(Pair)                                                              # Person Parameter
Pers <- select(as.data.frame(Pers$pers[-9]), c(persID, WLE))                    # Person Parameters with Disability Status
Pers <- separate(Pers, col=persID, sep="-", into=c("ID","SUB"), extra="merge")  # Disability Status in new column
Pers_LD <- rnorm(1000, mean=mean((filter(Pers, SUB==1))$WLE, na.rm=T),          # Generate Person Parameter of Persons with Disability
                       sd = sd((filter(Pers, SUB==1))$WLE, na.rm=T))
Pers_ND <- rnorm(1000, mean=mean((filter(Pers, SUB==0))$WLE, na.rm=T),          # Generate Person Parameter of Persons without Disability
                 sd = sd((filter(Pers, SUB==0))$WLE, na.rm=T))

#_______________________________________________________________________________
# 3. Setting up CAT Algorithm

## 3.1 Overall Settings
Start_1 <- list(theta =0,fixItems=which(rownames(IP1)=="start"))                # First Item fixed for Item pool 1
Start_2 <- list(theta =0,fixItems=which(rownames(IP2)=="start"))                # First Item fixed for Item pool 1
Start_3 <- list(theta =0,fixItems=which(rownames(IP3)=="start"))                # First Item fixed for Item pool 1
Test <- list(method="BM", itemSelect="MFI")                                     # Estimation of Test Step
Final <- list(method="ML")                                                      # Final Estimation

## 3.2 Stopping Rules
Rule_1 <- list(rule = "precision", thr= 0.3)                                    # First Stopping Rule
Rule_2 <- list(rule = "precision", thr= 0.4)                                    # Second Stopping Rule
Rule_3 <- list(rule = "precision", thr= 0.5)                                    # Third Stopping Rule
Rule_4 <- list(rule = c("precision", "length"), thr= c(0.5, 36))                # Forth Stopping Rule

#_______________________________________________________________________________
# 4. Simulation of Study 1

set.seed(1)

## 4.1 Item Pool 1 

Sim_IP1_1 <- simulateRespondents(itemBank = IP1, thetas = Pers_ND,              # SE 0.3 - without disabilities
             start = Start_1, test = Test, stop = Rule_1, final = Final)
Sim_IP1_2 <- simulateRespondents(itemBank = IP1, thetas = Pers_LD,              # SE 0.3 - with disabilities
             start = Start_1, test = Test, stop = Rule_1, final = Final)
Sim_IP1_3 <- simulateRespondents(itemBank = IP1, thetas = Pers_ND,              # SE 0.4 - without disabilities
             start = Start_1, test = Test, stop = Rule_2, final = Final)
Sim_IP1_4 <- simulateRespondents(itemBank = IP1, thetas = Pers_LD,              # SE 0.4 - with disabilities
             start = Start_1, test = Test, stop = Rule_2, final = Final)
Sim_IP1_5 <- simulateRespondents(itemBank = IP1, thetas = Pers_ND,              # SE 0.5 - without disabilities
             start = Start_1, test = Test, stop = Rule_3, final = Final)
Sim_IP1_6 <- simulateRespondents(itemBank = IP1, thetas = Pers_LD,              # SE 0.5 - with disabilities
             start = Start_1, test = Test, stop = Rule_3, final = Final)
Sim_IP1_7 <- simulateRespondents(itemBank = IP1, thetas = Pers_ND,              # SE 0.5 & length - without disabilities
             start = Start_1, test = Test, stop = Rule_4, final = Final)
Sim_IP1_8 <- simulateRespondents(itemBank = IP1, thetas = Pers_LD,              # SE 0.5 & length - with disabilities
             start = Start_1, test = Test, stop = Rule_4, final = Final)

print(Sim_IP1_1)
print(Sim_IP1_2)
print(Sim_IP1_3)
print(Sim_IP1_4)
print(Sim_IP1_5)
print(Sim_IP1_6)
print(Sim_IP1_7)
print(Sim_IP1_8)

## 4.1 Item Pool 2 

Sim_IP2_1 <- simulateRespondents(itemBank = IP2, thetas = Pers_ND,              # SE 0.3 - without disabilities
             start = Start_2, test = Test, stop = Rule_1, final = Final)
Sim_IP2_2 <- simulateRespondents(itemBank = IP2, thetas = Pers_LD,              # SE 0.3 - with disabilities
             start = Start_2, test = Test, stop = Rule_1, final = Final)
Sim_IP2_3 <- simulateRespondents(itemBank = IP2, thetas = Pers_ND,              # SE 0.4 - without disabilities
             start = Start_2, test = Test, stop = Rule_2, final = Final)
Sim_IP2_4 <- simulateRespondents(itemBank = IP2, thetas = Pers_LD,              # SE 0.4 - with disabilities
             start = Start_2, test = Test, stop = Rule_2, final = Final)
Sim_IP2_5 <- simulateRespondents(itemBank = IP2, thetas = Pers_ND,              # SE 0.5 - without disabilities
             start = Start_2, test = Test, stop = Rule_3, final = Final)
Sim_IP2_6 <- simulateRespondents(itemBank = IP2, thetas = Pers_LD,              # SE 0.5 - with disabilities
             start = Start_2, test = Test, stop = Rule_3, final = Final)
Sim_IP2_7 <- simulateRespondents(itemBank = IP2, thetas = Pers_ND,              # SE 0.5 & length - without disabilities
             start = Start_2, test = Test, stop = Rule_4, final = Final)
Sim_IP2_8 <- simulateRespondents(itemBank = IP2, thetas = Pers_LD,              # SE 0.5 & length - with disabilities
             start = Start_2, test = Test, stop = Rule_4, final = Final)

print(Sim_IP2_1)
print(Sim_IP2_2)
print(Sim_IP2_3)
print(Sim_IP2_4)
print(Sim_IP2_5)
print(Sim_IP2_6)
print(Sim_IP2_7)
print(Sim_IP2_8)

## 4.1 Item Pool 3 

Sim_IP3_1 <- simulateRespondents(itemBank = IP3, thetas = Pers_ND,              # SE 0.3 - without disabilities
             start = Start_3, test = Test, stop = Rule_1, final = Final)
Sim_IP3_2 <- simulateRespondents(itemBank = IP3, thetas = Pers_LD,              # SE 0.3 - with disabilities
             start = Start_3, test = Test, stop = Rule_1, final = Final)
Sim_IP3_3 <- simulateRespondents(itemBank = IP3, thetas = Pers_ND,              # SE 0.4 - without disabilities
             start = Start_3, test = Test, stop = Rule_2, final = Final)
Sim_IP3_4 <- simulateRespondents(itemBank = IP3, thetas = Pers_LD,              # SE 0.4 - with disabilities
             start = Start_3, test = Test, stop = Rule_2, final = Final)
Sim_IP3_5 <- simulateRespondents(itemBank = IP3, thetas = Pers_ND,              # SE 0.5 - without disabilities
             start = Start_3, test = Test, stop = Rule_3, final = Final)
Sim_IP3_6 <- simulateRespondents(itemBank = IP3, thetas = Pers_LD,              # SE 0.5 - with disabilities
             start = Start_3, test = Test, stop = Rule_3, final = Final)
Sim_IP3_7 <- simulateRespondents(itemBank = IP3, thetas = Pers_ND,              # SE 0.5 & length 36 - without disabilities
             start = Start_3, test = Test, stop = Rule_4, final = Final)
Sim_IP3_8 <- simulateRespondents(itemBank = IP3, thetas = Pers_LD,              # SE 0.5 & length 36 - with disabilities
             start = Start_3, test = Test, stop = Rule_4, final = Final)

print(Sim_IP3_1)
print(Sim_IP3_2)
print(Sim_IP3_3)
print(Sim_IP3_4)
print(Sim_IP3_5)
print(Sim_IP3_6)
print(Sim_IP3_7)
print(Sim_IP3_8)

#_______________________________________________________________________________
# 5. Analysis Simulations

plot(Sim_IP1_7, "trueEst")                                                      # item pool 1
plot(Sim_IP1_8, "trueEst")
plot(Sim_IP2_7, "trueEst")                                                      # item pool 2
plot(Sim_IP2_8, "trueEst")
plot(Sim_IP3_7, "trueEst")                                                      # item pool 3
plot(Sim_IP3_8, "trueEst")

#_______________________________________________________________________________
# 6. Simulations Study 2
set.seed(1)

Group_1 <- data.frame(theta = rnorm(1000, mean = 0, sd = 1))
Group_2 <- data.frame(theta = rnorm(1000, mean = -1, sd = 1))
Group_3 <- data.frame(theta = rnorm(1000, mean = -2, sd = 1))
  
ggplot(Group_1, aes(x=theta)) +                                                 # Plot Group 1
  geom_histogram(binwidth=0.2, color="black", fill="grey") + theme_bw() + 
  labs(title="θ of Group 1",x="θ", y = "count") + 
  xlim(-6,6) + ylim(0, 95)

ggplot(Group_2, aes(x=theta)) +                                                 # Plot Group 1
  geom_histogram(binwidth=0.2, color="black", fill="grey") + theme_bw() + 
  labs(title="θ of Group 2",x="θ", y = "count") + 
  xlim(-6,6) + ylim(0, 95)

ggplot(Group_3, aes(x=theta)) +                                                 # Plot Group 1
  geom_histogram(binwidth=0.2, color="black", fill="grey") + theme_bw() + 
  labs(title="θ of Group 3",x="θ", y = "count") + 
  xlim(-6,6) + ylim(0, 95)

Sim_IP2_9 <- simulateRespondents(itemBank = IP2, thetas = Group_1$theta,        # SE 0.5 - group 1
             start = Start_2, test = Test, stop = Rule_3, final = Final)
Sim_IP2_10 <- simulateRespondents(itemBank = IP2, thetas = Group_1$theta,       # SE 0.5 & length - group 1
             start = Start_2, test = Test, stop = Rule_4, final = Final) 
Sim_IP2_11 <- simulateRespondents(itemBank = IP2, thetas = Group_2$theta,       # SE 0.5 - group 2
             start = Start_2, test = Test, stop = Rule_3, final = Final)
Sim_IP2_12 <- simulateRespondents(itemBank = IP2, thetas = Group_2$theta,       # SE 0.5 & length - group 2
             start = Start_2, test = Test, stop = Rule_4, final = Final)
Sim_IP2_13 <- simulateRespondents(itemBank = IP2, thetas = Group_3$theta,       # SE 0.5 - group 3
             start = Start_2, test = Test, stop = Rule_3, final = Final)
Sim_IP2_14 <- simulateRespondents(itemBank = IP2, thetas = Group_3$theta,       # SE 0.5 & length - group 3
             start = Start_2, test = Test, stop = Rule_4, final = Final)

print(Sim_IP2_9)
print(Sim_IP2_10)
print(Sim_IP2_11)
print(Sim_IP2_12)
print(Sim_IP2_13)
print(Sim_IP2_14)

plot(Sim_IP2_9, "trueEst")                                                      # item pool 1
plot(Sim_IP2_10, "trueEst")
plot(Sim_IP2_11, "trueEst")                                                     # item pool 2
plot(Sim_IP2_12, "trueEst")
plot(Sim_IP2_13, "trueEst")                                                     # item pool 3
plot(Sim_IP2_14, "trueEst")

#_______________________________________________________________________________
#_______________________________________________________________________________
