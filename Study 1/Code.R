# 0. Required Packages ####
library(tidyverse) # Data Wrangling and Plotting
library(pairwise) # Rasch Analysis
library(TAM) # IRT Analysis
library(catR) # unidimensional CAT Simulations
library(mirtCAT) # multidimensional CAT Simulations

# 1. Reading Data ####
SEL <- read.csv2("")
ZASM <- read.csv("")

# 2. Sample ####
ZASM_sample <- read.csv2("")
ZASM_sample_test <- ZASM_sample %>% 
  select(c(6, 17:67)) %>% 
  mutate(rowsum = rowSums(.[2:81], na.rm=T)) %>%
  select(c(SPF, rowsum))
ZASM_sample_test[is.na(ZASM_sample_test)] <- "Kein SPF"
ZASM_sample_test[ZASM_sample_test == "SPF ESE"] <- "SPF"
ZASM_sample_test[ZASM_sample_test == "SPF Hoeren"] <- "SPF"
ZASM_sample_test[ZASM_sample_test == "SPF Lernen"] <- "SPF"
ZASM_sample_test[ZASM_sample_test == "SPF Sprache"] <- "SPF"
table(ZASM_sample_test$SPF)
t.test(ZASM_sample_test$rowsum~ZASM_sample_test$SPF)


# 2. Data Wrangling ####
SEL <- SEL %>% select(c(5:223)) %>% select_if(~sum(!is.na(.)) > 0) # Remove all columns with only missing answers
SEL <- SEL[, colSums(SEL != 0, na.rm = TRUE) > 0] # Remove all columns with only wrong answers
ZASM <- ZASM %>% select(c(2:81)) %>% select_if(~sum(!is.na(.)) > 0) # Remove all columns with only missing answers
ZASM <- ZASM[, colSums(ZASM != 0, na.rm = TRUE) > 0] # Remove all columns with only wrong answers

# 3. Rasch Analyses ####

## 3.1 Parameter Estimation ####
SEL_ip <- pair(SEL)
SEL_pp <- pers(SEL_ip)
ZASM_ip <- pair(ZASM, m=2)
ZASM_pp <- pers(ZASM_ip)

## 3.2 Model Fit ####
SEL_1PL <- tam(SEL, irtmodel = "1PL") # Reading Data in 1PL Model
SEL_2PL <- tam.mml.2pl(SEL, irtmodel = "2PL") # Reading Data in 2PL Model
SEL_3PL <- tam.mml.3pl(SEL) # Reading Data in 3PL Model

ZASM_1PL <- tam(ZASM, irtmodel = "1PL") # Maths Data in 1PL Model
ZASM_2PL <- tam.mml.2pl(ZASM, irtmodel = "2PL") # Maths Data in 2PL Model
ZASM_3PL <- tam.mml.3pl(ZASM) # Maths Data in 3PL Model

IRT.compareModels(ZASM_1PL, ZASM_2PL, ZASM_3PL)
IRT.compareModels(SEL_1PL, SEL_2PL, SEL_3PL)

### 3.2.2 Mehrdimensionales Rasch-Modell
Q <- read.csv2("C:/Users/Nikola/OneDrive - Microsoft Cloud - Bereich Universität Regensburg/Daten/Sven Mathedaten JERO/Items_Jero.csv")
Q <- Q %>% 
  arrange(Item.Number) %>% 
  unite(Item, Zeitpunkt, Item.Number, RRR, sep="_") %>% 
  select(-Item) %>%
  head(.,51)

Rasch_3D <- tam(ZASM, Q=Q, control=list(snodes=1500, QMC=T)) 
#_______________________________________________________________________________
Q <- data.frame("Item" = colnames(SEL_tam))

Q2 <- Q %>% mutate("D1" = case_when(startsWith(Item, "SEL2") ~ 1, TRUE ~ 0),
                   "D2" = case_when(startsWith(Item, "SEL4") ~ 1, TRUE ~ 0),
                   "D3" = case_when(startsWith(Item, "SEL6") ~ 1, TRUE ~ 0))

Rasch_3D <- tam(SEL_tam, Q=select(Q2,-c("Item")),control=list(snodes=1000, QMC=T)) 

## 3.3 Model Fit
andersentest.pers(SEL_pp) # Chi=104.99, df=347, p=1
andersentest.pers(ZASM_pp) # Chi=196, df=261, p=1

IRT.compareModels(ZASM_1PL, Rasch_3D)
IRT.compareModels(SEL_1PL, Rasch_3D)

## 3.4 Item Fit ####
msq.itemfitWLE(SEL_1PL) 
msq.itemfitWLE(ZASM_1PL) 
itemfit_SEL <- pairwise.item.fit(SEL_pp)
itemfit_ZASM <- pairwise.item.fit(ZASM_pp)
write.csv(itemfit_SEL, "C:/Users/Nikola/Downloads/itemfit_SEL.csv")
write.csv(itemfit_ZASM, "C:/Users/Nikola/Downloads/itemfit_ZASM.csv")

## 3.5 Item-Person-Map ####
plot(SEL_pp, itemNames = F, ra=6)
plot(ZASM_pp, itemNames = F, ra=6)

# 4. CAT Simulations ####
## 4.1 Data Wrangling ####
ZASM_sigma <- data.frame(a=1, b=ZASM_ip$sigma, c=0, d=1) # Set Item Parameters for Rasch Model
SEL_sigma  <- data.frame(a=1, b=SEL_ip$sigma,  c=0, d=1)
rownames(ZASM_sigma)[rownames(ZASM_sigma) == "MZP1_I33_result"] <- "start" # Set Start Item (Item difficulty closest to 0)
rownames(SEL_sigma) [rownames(SEL_sigma) == "SEL2I50.1"]        <- "start"

## 4.2 Setting up CAT
set.seed(1)

Theta_ZASM <- rnorm(1000, # Normal distributed Thetas
                       mean=mean(ZASM_pp$pers$WLE, na.rm=T),
                       sd = sd(ZASM_pp$pers$WLE, na.rm=T))
Theta_SEL <- rnorm(1000,  # Normal distributed Thetas
                    mean=mean(SEL_pp$pers$WLE, na.rm=T),
                    sd = sd(SEL_pp$pers$WLE, na.rm=T))
start_ZASM <- list(theta = 0, fixItems = which(rownames(ZASM_sigma) == "start")) # Start item is fixed with difficulty closest to 0
start_SEL <- list(theta = 0, fixItems = which(rownames(SEL_sigma) == "start")) # Start item is fixed with difficulty closest to 0
test <- list(method="BM", itemSelect="MFI") # Next Item is chosen by Fisher-Information, Estimation is performed by Bayes modas method
final <- list(method="ML") # Final Estimation is performed with Maximum Likelihood
stop_1 <- list(rule = "precision", thr= 0.3)
stop_2 <- list(rule = "precision", thr= 0.4)
stop_3 <- list(rule = "precision", thr= 0.5)
stop_4_SEL  <- list(rule = c("precision", "length"), thr= c(0.5, 37))
stop_4_ZASM <- list(rule = c("precision", "length"), thr= c(0.5, 24))

## 4.1 First Simulation Round (SE=0.3) ####
CAT_SEL_1 <- simulateRespondents( # Reading CAT
  itemBank = SEL_sigma,
  thetas = Theta_SEL, start = start_SEL, test = test, stop = stop_1, final = final)

CAT_ZASM_1 <- simulateRespondents( # Maths CAT
  itemBank = ZASM_sigma,
  thetas = Theta_ZASM, start = start_ZASM, test = test, stop = stop_1, final = final)

## 4.2 Second Simulation Round (SE=0.4) ####
CAT_SEL_2 <- simulateRespondents( # Reading CAT
  itemBank = SEL_sigma,
  thetas = Theta_SEL, start = start_SEL, test = test, stop = stop_2, final = final)

CAT_ZASM_2 <- simulateRespondents( # Maths CAT
  itemBank = ZASM_sigma,
  thetas = Theta_ZASM, start = start_ZASM, test = test, stop = stop_2, final = final)

## 4.3 Third Simulation Round (SE=0.5) ####
CAT_SEL_3 <- simulateRespondents( # Reading CAT
  itemBank = SEL_sigma,
  thetas = Theta_SEL, start = start_SEL, test = test, stop = stop_3, final = final)

CAT_ZASM_3 <- simulateRespondents( # Maths CAT
  itemBank = ZASM_sigma,
  thetas = Theta_ZASM, start = start_ZASM, test = test, stop = stop_3, final = final)

## 4.4 Analysis of Simulation Rounds
print(CAT_SEL_1)
mean(CAT_SEL_1$estimatedThetas)
sd(CAT_SEL_1$estimatedThetas)
print(CAT_SEL_2)
mean(CAT_SEL_2$estimatedThetas)
sd(CAT_SEL_2$estimatedThetas)
print(CAT_SEL_3)
mean(CAT_SEL_3$estimatedThetas)
sd(CAT_SEL_3$estimatedThetas)

print(CAT_ZASM_1)
mean(CAT_ZASM_1$estimatedThetas)
sd(CAT_ZASM_1$estimatedThetas)
print(CAT_ZASM_2)
mean(CAT_ZASM_2$estimatedThetas)
sd(CAT_ZASM_2$estimatedThetas)
print(CAT_ZASM_3)
mean(CAT_ZASM_3$estimatedThetas)
sd(CAT_ZASM_3$estimatedThetas)

#_______________________________________________________________________________

## 4.2 Setting up CAT
set.seed(1)
final <- list(method="BM") # Final Estimation is performed with Bayes

## 4.1 First Simulation Round (SE=0.3) ####
CAT_SEL_1 <- simulateRespondents( # Reading CAT
  itemBank = SEL_sigma,
  thetas = Theta_SEL, start = start_SEL, test = test, stop = stop_1, final = final)

CAT_ZASM_1 <- simulateRespondents( # Maths CAT
  itemBank = ZASM_sigma,
  thetas = Theta_ZASM, start = start_ZASM, test = test, stop = stop_1, final = final)

## 4.2 Second Simulation Round (SE=0.4) ####
CAT_SEL_2 <- simulateRespondents( # Reading CAT
  itemBank = SEL_sigma,
  thetas = Theta_SEL, start = start_SEL, test = test, stop = stop_2, final = final)

CAT_ZASM_2 <- simulateRespondents( # Maths CAT
  itemBank = ZASM_sigma,
  thetas = Theta_ZASM, start = start_ZASM, test = test, stop = stop_2, final = final)

## 4.3 Third Simulation Round (SE=0.5) ####
CAT_SEL_3 <- simulateRespondents( # Reading CAT
  itemBank = SEL_sigma,
  thetas = Theta_SEL, start = start_SEL, test = test, stop = stop_3, final = final)

CAT_ZASM_3 <- simulateRespondents( # Maths CAT
  itemBank = ZASM_sigma,
  thetas = Theta_ZASM, start = start_ZASM, test = test, stop = stop_3, final = final)

## 4.4 Analysis of Simulation Rounds
print(CAT_SEL_1)
mean(CAT_SEL_1$estimatedThetas)
sd(CAT_SEL_1$estimatedThetas)
print(CAT_SEL_2)
mean(CAT_SEL_2$estimatedThetas)
sd(CAT_SEL_2$estimatedThetas)
print(CAT_SEL_3)
plot(CAT_SEL_3)
mean(CAT_SEL_3$estimatedThetas)
sd(CAT_SEL_3$estimatedThetas)

print(CAT_ZASM_1)
mean(CAT_ZASM_1$estimatedThetas)
sd(CAT_ZASM_1$estimatedThetas)
print(CAT_ZASM_2)
mean(CAT_ZASM_2$estimatedThetas)
sd(CAT_ZASM_2$estimatedThetas)
print(CAT_ZASM_3)
mean(CAT_ZASM_3$estimatedThetas)
sd(CAT_ZASM_3$estimatedThetas)

## 4.4 Forth Simulation Round (SE=0.5, fixed length) ####
CAT_SEL_4 <- simulateRespondents( # Reading CAT
  itemBank = SEL_sigma,
  thetas = Theta_SEL, start = start_SEL, test = test, stop = stop_4_SEL, final = final)

CAT_ZASM_4 <- simulateRespondents( # Maths CAT
  itemBank = ZASM_sigma,
  thetas = Theta_ZASM, start = start_ZASM, test = test, stop = stop_4_ZASM, final = final)

print(CAT_SEL_4)
mean(CAT_SEL_4$estimatedThetas)
sd(CAT_SEL_4$estimatedThetas)
print(CAT_ZASM_4)
mean(CAT_ZASM_4$estimatedThetas)
sd(CAT_ZASM_4$estimatedThetas)
plot(CAT_SEL_4)
plot(CAT_ZASM_4)

# 5.1 Multidimensional CAT ####

## 5.2 Setting up CAT ####
set.seed(1)

model <- mirt.model(as.matrix(select(Q2, -Item)))
MIRT <- mirt(ZASM, model)
x <- as.matrix(select(Rasch_8D$person, c(6, 8, 10, 12, 14, 16, 18, 20)))
responsepattern <- generate_pattern(mo = MIRT, Theta = x)

CAT1 <- mirtCAT(mo=MIRT,
        method='MAP',
        criteria='Drule',
        start_item = 33,
        local_pattern = responsepattern,
        design = list(min_SEM=0.3),
        true_thetas=T, progress=T)

CAT2 <- mirtCAT(mo=MIRT,
                method='MAP',
                criteria='Drule',
                start_item = 33,
                local_pattern = responsepattern,
                design = list(min_SEM=0.4),
                true_thetas=T, progress=T)

CAT3 <- mirtCAT(mo=MIRT,
                method='MAP',
                criteria='Drule',
                start_item = 33,
                local_pattern = responsepattern,
                design = list(min_SEM=0.5),
                true_thetas=T, progress=T)

print(CAT1)
summary(CAT1)

CAT1 <- mirtCAT(mo=MIRT,
                method='MAP',
                criteria='Drule',
                start_item = 'Drule',
                local_pattern = responsepattern,
                design = list(min_SEM=0.5, max_items= ,thetas.start=x),
                true_thetas=T, progress=T)



                      
