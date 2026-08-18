#_______________________________________________________________________________
# This Syntax belongs to: 
# Nikola Ebenbeck (2023)
# Computerized Adaptive Testing in Inclusive Education - Dissertation
#_______________________________________________________________________________

x <- c("tidyverse", "lubridate", "apaTables", "pairwise", "catR")
lapply(x, require, character.only = TRUE) # load packages

#_______________________________________________________________________________

#---------------------------- 0. CREATE FUNCTIONS ------------------------------

sum_sd <- function(x) { # Printing summary and sd of a vector
  print(summary(x)) 
  print(sd(x, na.rm=T)) }

rasch_lr <- function(x) { # Calculate Rasch model and Andersen LR Test
  a <- pers(pair(x))
  print(andersentest.pers(a, split="median"))
  print(andersentest.pers(a, split="random"))
  return(a) }

#_______________________________________________________________________________

#----------------------------- 0. DATA WRANGLING -------------------------------

# set working directory
setwd("")

#---------------------------- 0.1 Subtest Results ------------------------------
# read data
x <- lapply(list.files("Original", pattern="*.csv", full.names=T), read.csv2)
names(x) <- rep(c("B","P", "S", "W"), each=4)

# set and recode data in long format
long <- x %>% map(~select(
  .x,Kind_ID,Geschlecht,SPF,Geburtsdatum,Testdatum, LRS,item,time,result)) %>%
  # standardize data format
  lapply(transform, Geschlecht = as.character(Geschlecht)) %>%
  lapply(transform, SPF = as.character(SPF)) %>%
  lapply(transform, LRS = as.character(LRS)) %>%
  # combine data in long format
  bind_rows(.id="column_label") %>%
  # recode data
  mutate(Geschlecht = replace(Geschlecht, Geschlecht=="Maedchen", 0)) %>%
  mutate(Geschlecht = replace(Geschlecht, Geschlecht=="Junge", 1)) %>%
  mutate(SPF = replace(SPF, SPF=="Kein SPF"|is.na(SPF), 0)) %>%
  mutate(SPF = replace(SPF, SPF=="SPF Lernen", 1)) %>%
  mutate(SPF = replace(SPF, SPF=="SPF Sprache", 3)) %>%
  mutate(LRS = replace(LRS, LRS=="TRUE"|LRS=="LRS", 1)) %>%
  mutate(LRS = replace(LRS, LRS!="1"|is.na(LRS), 0)) %>%
  # transform data format
  transform(Geschlecht = as.factor(Geschlecht)) %>%
  transform(SPF = as.factor(SPF)) %>%
  transform(LRS = as.factor(LRS))

# set data in wide format
wide <- long %>%
  select(-c(time)) %>% unite("item", c(column_label, item), sep="_") %>%
  pivot_wider(names_from=item,values_from=result)%>% select(-c(S_I36:S_I75))%>%
  # number of right answers
  mutate(Sum_1=rowSums(select(., starts_with("P_")), na.rm=T), 
         Sum_2=rowSums(select(., starts_with("W_")), na.rm=T), 
         Sum_3=rowSums(select(., starts_with("B_")), na.rm=T), 
         Sum_4=rowSums(select(., starts_with("S_")), na.rm=T)) %>%
  # number of wrong answers
  mutate(Err_1 = rowSums(select(., starts_with("P_")) == 0, na.rm = T),
         Err_2 = rowSums(select(., starts_with("W_")) == 0, na.rm = T),
         Err_3 = rowSums(select(., starts_with("B_")) == 0, na.rm = T),
         Err_4 = rowSums(select(., starts_with("S_")) == 0, na.rm = T)) %>%
  # number of attempts
  mutate(All_1 = Sum_1 + Err_1, All_2 = Sum_2 + Err_2,
         All_3 = Sum_3 + Err_3, All_4 = Sum_4 + Err_4)

# filter students and add students with GB
GB <- filter(wide, SPF==2)
wide <- wide %>% filter(All_1!=0 & All_2!=0 & All_3!=0 & All_4!=0) %>% rbind(GB)

#-------------------------- 0.2 Student Information ----------------------------
# read data
x <- lapply(list.files("Schüler", pattern="*.csv",full.names=T), read.csv2)
names(x) <- c("A", "B", "C", "D")

# combine sample data frames
y <- select(x$D, c(Kind_ID, Schulbesuchsjahr, Geburtsdatum, GB, Klassenstufe))
x <- bind_rows(x$A,x$B,x$C) %>% 
  select(c(Kind_ID,Schulbesuchsjahr,Klassenstufe)) %>%
  transform(Klassenstufe = as.character(Klassenstufe)) %>% bind_rows(y)

wide <- wide %>% 
  # combine information in dataframe
  left_join(x, Geburtsdatum, by="Kind_ID") %>% mutate_all(na_if,"") %>%
  # format dates
  mutate(Testdatum=parse_date_time(Testdatum, orders=c("dmy", "ymd"))) %>%
  mutate(Geburtsdatum.x=parse_date_time(Geburtsdatum.x,c("dmy","ymd")))%>%
  mutate(Geburtsdatum.y=parse_date_time(Geburtsdatum.y,c("dmy","ymd")))%>%
  mutate(Geburtsdatum = coalesce(Geburtsdatum.x, Geburtsdatum.y)) %>%
  # calculate age
  mutate(Alter = trunc((Geburtsdatum%--%Testdatum)/years(1))) %>%
  select(-c(Geburtsdatum.x, Geburtsdatum.y, Geburtsdatum, Testdatum)) %>%
  select(Kind_ID,Schulbesuchsjahr,Klassenstufe,GB,Alter,everything()) %>%
  # remove students with missing or wrong student information
  filter_at(vars(Schulbesuchsjahr, Klassenstufe), any_vars(!is.na(.))) %>%
  mutate(Alter = replace(Alter, Alter==2, 8)) %>% unique()

write.csv2(wide, "data_wide.csv")

#_______________________________________________________________________________

#------------------------------- 1. DESCRIPTION --------------------------------

wide <- read.csv2("data_wide.csv")

# number of items per task
n_items_1  <- sum(substring(colnames(wide), 1, 2)=="P_")
n_items_2  <- sum(substring(colnames(wide), 1, 2)=="W_")
n_items_3  <- sum(substring(colnames(wide), 1, 2)=="B_")
n_items_4  <- sum(substring(colnames(wide), 1, 2)=="S_")

# inclusive sample
x <- filter(wide, SPF!=2)
lapply(list(x$Schulbesuchsjahr, x$Alter), sum_sd)
lapply(list(x$Klassenstufe, x$SPF, x$LRS), table)

# ID sample 
x <- filter(wide, SPF==2)
lapply(list(x$Schulbesuchsjahr, x$Alter), sum_sd)
lapply(list(x$Klassenstufe, x$SPF, x$GB), table)
lapply(list(x$Sum_1, x$Sum_2, x$Sum_3, x$Sum_4), table) # n test runs

# total sample
((table(wide$SPF))/400)*100
((summary(wide$Geschlecht))/400)*100

#_______________________________________________________________________________

#------------------------------ 2. STUDY ONE -----------------------------------

## ----------------------- 2.1 Performance Analysis ----------------------------

lapply(list(wide$All_1, wide$All_2, wide$All_3, wide$All_4,
            wide$Sum_1/wide$All_1, wide$Sum_2/wide$All_2,
            wide$Sum_3/wide$All_3, wide$Sum_4/wide$All_4), sum_sd)

# plot summative score, error score and attempt
select(wide, Sum_1:All_4) %>% 
  pivot_longer(everything(), names_to="Score", values_to="n") %>%
  separate(Score, into=c("Score", "Task"), sep="_") %>% 
  ggplot(aes(x=Task, y=n, fill=Score)) + geom_boxplot() + theme_bw()

##------------------------ 2.2 Graphical Model Test ----------------------------

set.seed(5) 

# Rasch model and LR Test per subtest
Rasch1 <- rasch_lr(select(wide,starts_with("P_")))
Rasch2 <- rasch_lr(select(wide,starts_with("W_")))
Rasch3 <- rasch_lr(select(wide,starts_with("B_")))
Rasch4 <- rasch_lr(select(wide,starts_with("S_")))

# GRM and again RM and LR Test for subtest 4
x <- select(wide,starts_with("S_"))
x <- select(x, -c("S_I05","S_I30","S_I35","S_I19","S_I16","S_I20","S_I34"))
plot(grm(daten=x,m=2,split="median"), itemNames=T)
Rasch4 <- rasch_lr(x)

##-------------------------- 2.3 Item Difficulties -----------------------------

lapply(list(
  Rasch1$pair$sigma, Rasch2$pair$sigma, Rasch3$pair$sigma, Rasch4$pair$sigma,
  pairwise.item.fit(Rasch1)$OUTFIT.MSQ, pairwise.item.fit(Rasch1)$INFIT.MSQ,
  pairwise.item.fit(Rasch2)$OUTFIT.MSQ, pairwise.item.fit(Rasch2)$INFIT.MSQ,
  pairwise.item.fit(Rasch3)$OUTFIT.MSQ, pairwise.item.fit(Rasch3)$INFIT.MSQ,
  pairwise.item.fit(Rasch4)$OUTFIT.MSQ, pairwise.item.fit(Rasch4)$INFIT.MSQ), 
  sum_sd)

lapply(list(Rasch1, Rasch2, Rasch3, Rasch4), plot, itemNames=F, ra=6)

##------------------------- 2.4 SubCAT Settings --------------------------------

set.seed(1)

# generate person parameters
PP <- data.frame(Task_1 = (Rasch1$pers)$WLE, Task_2 = (Rasch2$pers)$WLE,
                 Task_3 = (Rasch3$pers)$WLE, Task_4 = (Rasch4$pers)$WLE)
PP %>% pivot_longer(1:4, names_to="Task", values_to="PP") %>%
  ggplot(aes(x=Task, y=PP)) + geom_boxplot() + ylim(-4, 8) + theme_bw()

PPgen <- data.frame(Task_1=rnorm(1000, mean=mean(PP$Task_1), sd=sd(PP$Task_1)),
                    Task_2=rnorm(1000, mean=mean(PP$Task_2), sd=sd(PP$Task_2)),
                    Task_3=rnorm(1000, mean=mean(PP$Task_3), sd=sd(PP$Task_3)),
                    Task_4=rnorm(1000, mean=mean(PP$Task_4, na.rm=T), 
                                 sd=sd(PP$Task_4, na.rm=T)))
PPgen %>% pivot_longer(1:4, names_to="Task", values_to="PP") %>%
  ggplot(aes(x=Task, y=PP)) + geom_boxplot() +  ylim(-4, 8) + theme_bw()

PPfixed <- rnorm(1000)

# set item parameters
IP1 <- data.frame(a=1, b=Rasch1$pair$sigma, c=0, d=1) 
IP2 <- data.frame(a=1, b=Rasch2$pair$sigma, c=0, d=1)
IP3 <- data.frame(a=1, b=Rasch3$pair$sigma, c=0, d=1)
IP4 <- data.frame(a=1, b=Rasch4$pair$sigma, c=0, d=1)
IP5 <- data.frame(a=1, b=runif(100, min=-3, max=3), c=0, d=1) 

# set start items
rownames(IP1)[rownames(IP1) == "P_4I"] <- "start"
rownames(IP2)[rownames(IP2) == "W_I10"] <- "start"    
rownames(IP3)[rownames(IP3) == "B_1G"] <- "start"    
rownames(IP4)[rownames(IP4) == "S_I01"] <- "start"
rownames(IP5)[rownames(IP5) == "16"] <- "start"

# set start rule
Start_1 <- list(theta =0,fixItems=which(rownames(IP1)=="start"))
Start_2 <- list(theta =0,fixItems=which(rownames(IP2)=="start"))
Start_3 <- list(theta =0,fixItems=which(rownames(IP3)=="start"))
Start_4 <- list(theta =0,fixItems=which(rownames(IP4)=="start"))
Start_5 <- list(theta =0,fixItems=which(rownames(IP5)=="start"))

# set estimators
Test <- list(method="BM", itemSelect="MFI")
Final <- list(method="ML")

# set precision stopping rules 
Rule_1 <- list(rule = "precision", thr= 0.3)                                  
Rule_2 <- list(rule = "precision", thr= 0.4)                                  
Rule_3 <- list(rule = "precision", thr= 0.5)

##----------------- 2.5 Simulations with one Stopping Rule ---------------------

# simulation round 1: precision rule 0.3
a <- simulateRespondents(itemBank=IP1, thetas=PPgen$Task_1, start=Start_1, 
                         test=Test, stop=Rule_1, final=Final)
b <- simulateRespondents(itemBank=IP2, thetas=PPgen$Task_2, start=Start_2, 
                         test=Test, stop=Rule_1, final=Final)
c <- simulateRespondents(itemBank=IP3, thetas=PPgen$Task_3, start=Start_3, 
                         test=Test, stop=Rule_1, final=Final)
d <- simulateRespondents(itemBank=IP4, thetas=PPgen$Task_4, start=Start_4, 
                         test=Test, stop=Rule_1, final=Final)
print(a)
print(b)
print(c)
print(d)

# simulation round 2: precision rule 0.4
a <- simulateRespondents(itemBank=IP1, thetas=PPgen$Task_1, start=Start_1, 
                         test=Test, stop=Rule_2, final=Final)
b <- simulateRespondents(itemBank=IP2, thetas=PPgen$Task_2, start=Start_2, 
                         test=Test, stop=Rule_2, final=Final)
c <- simulateRespondents(itemBank=IP3, thetas=PPgen$Task_3, start=Start_3, 
                         test=Test, stop=Rule_2, final=Final)
d <- simulateRespondents(itemBank=IP4, thetas=PPgen$Task_4, start=Start_4, 
                         test=Test, stop=Rule_2, final=Final)
print(a)
print(b)
print(c)
print(d)

# simulation round 3: precision rule 0.5
a <- simulateRespondents(itemBank=IP1, thetas=PPgen$Task_1, start=Start_1, 
                         test=Test, stop=Rule_3, final=Final)
b <- simulateRespondents(itemBank=IP2, thetas=PPgen$Task_2, start=Start_2, 
                         test=Test, stop=Rule_3, final=Final)
c <- simulateRespondents(itemBank=IP3, thetas=PPgen$Task_3, start=Start_3, 
                         test=Test, stop=Rule_3, final=Final)
d <- simulateRespondents(itemBank=IP4, thetas=PPgen$Task_4, start=Start_4, 
                         test=Test, stop=Rule_3, final=Final)
print(a)
print(b)
print(c)
print(d)

plot(a, "trueEst")
plot(a, "condThr")
plot(a, "cumNumberItems")
plot(b, "trueEst")
plot(b, "condThr")
plot(b, "cumNumberItems")
plot(c, "trueEst")
plot(c, "condThr")
plot(c, "cumNumberItems")
plot(d, "trueEst")
plot(d, "condThr")
plot(d, "cumNumberItems")

##-------------- 2.6 Single Simulations with one Stopping Rule -----------------

# simulating single theta = -1
a <- randomCAT(-1, IP1, start=Start_1, test=Test, stop=Rule_3, final=Final)
b <- randomCAT(-1, IP2, start=Start_2, test=Test, stop=Rule_3, final=Final)
c <- randomCAT(-1, IP3, start=Start_3, test=Test, stop=Rule_3, final=Final)
d <- randomCAT(-1, IP4, start=Start_4, test=Test, stop=Rule_3, final=Final)

plot(a)
plot(b)
plot(c)
plot(d)

x <- data.frame(item=a$itemNames,sigma=as.data.frame(a$itemPar)$b, 
                pattern=as.factor(a$pattern))
x <- data.frame(item=b$itemNames,sigma=as.data.frame(b$itemPar)$b, 
                pattern=as.factor(b$pattern))
x <- data.frame(item=c$itemNames,sigma=as.data.frame(c$itemPar)$b, 
                pattern=as.factor(c$pattern))
x <- data.frame(item=d$itemNames,sigma=as.data.frame(d$itemPar)$b, 
                pattern=as.factor(d$pattern))

ggplot(x, aes(x=as.numeric(rownames(x)), y=sigma)) + 
  geom_line() +geom_point(aes(shape=pattern), size=3) +  
  scale_shape_manual(values=c(1, 19))+theme_bw()+geom_hline(yintercept = -1)+
  ylim(-3,3) + scale_x_continuous(expand = c(0.01, 0))

# simulating single theta = -3
a <- randomCAT(-3, IP1, start=Start_1, test=Test, stop=Rule_3, final=Final)
b <- randomCAT(-3, IP2, start=Start_2, test=Test, stop=Rule_3, final=Final)
c <- randomCAT(-3, IP3, start=Start_3, test=Test, stop=Rule_3, final=Final)
d <- randomCAT(-3, IP4, start=Start_4, test=Test, stop=Rule_3, final=Final)

plot(a)
plot(b)
plot(c)
plot(d)

x <- data.frame(item=a$itemNames,sigma=as.data.frame(a$itemPar)$b, 
                pattern=as.factor(a$pattern))
x <- data.frame(item=b$itemNames,sigma=as.data.frame(b$itemPar)$b, 
                pattern=as.factor(b$pattern))
x <- data.frame(item=c$itemNames,sigma=as.data.frame(c$itemPar)$b, 
                pattern=as.factor(c$pattern))
x <- data.frame(item=d$itemNames,sigma=as.data.frame(d$itemPar)$b, 
                pattern=as.factor(d$pattern))

ggplot(x, aes(x=as.numeric(rownames(x)), y=sigma)) + 
  geom_line() +geom_point(aes(shape=pattern), size=3) +  
  scale_shape_manual(values=c(1, 19))+theme_bw()+geom_hline(yintercept = -3)+
  ylim(-3,3) + scale_x_continuous(expand = c(0.01, 0))

# simulating single theta = 3
a <- randomCAT(3, IP1, start=Start_1, test=Test, stop=Rule_3, final=Final)
b <- randomCAT(3, IP2, start=Start_2, test=Test, stop=Rule_3, final=Final)
c <- randomCAT(3, IP3, start=Start_3, test=Test, stop=Rule_3, final=Final)
d <- randomCAT(3, IP4, start=Start_4, test=Test, stop=Rule_3, final=Final)

plot(a)
plot(b)
plot(c)
plot(d)

x <- data.frame(item=a$itemNames,sigma=as.data.frame(a$itemPar)$b, 
                pattern=as.factor(a$pattern))
x <- data.frame(item=b$itemNames,sigma=as.data.frame(b$itemPar)$b, 
                pattern=as.factor(b$pattern))
x <- data.frame(item=c$itemNames,sigma=as.data.frame(c$itemPar)$b, 
                pattern=as.factor(c$pattern))
x <- data.frame(item=d$itemNames,sigma=as.data.frame(d$itemPar)$b, 
                pattern=as.factor(d$pattern))

ggplot(x, aes(x=as.numeric(rownames(x)), y=sigma)) + 
  geom_line() +geom_point(aes(shape=pattern), size=3) +  
  scale_shape_manual(values=c(1, 19))+theme_bw()+geom_hline(yintercept = 3)+
  ylim(-3,3) + scale_x_continuous(expand = c(0.01, 0))

##---------------- 2.7 Simulations with Generated Item Pool --------------------

# generated item pool sigma
IP5 %>% ggplot(aes(x = 1:100, y=b)) + geom_point(shape=1, size=2) + theme_bw()

# simulations
a <- simulateRespondents(itemBank=IP5, thetas=PPgen$Task_1, start=Start_5, 
                         test=Test, stop=Rule_3, final=Final)
b <- simulateRespondents(itemBank=IP5, thetas=PPgen$Task_2, start=Start_5, 
                         test=Test, stop=Rule_3, final=Final)
c <- simulateRespondents(itemBank=IP5, thetas=PPgen$Task_3, start=Start_5, 
                         test=Test, stop=Rule_3, final=Final)
d <- simulateRespondents(itemBank=IP5, thetas=PPgen$Task_4, start=Start_5, 
                         test=Test, stop=Rule_3, final=Final)
print(a)
print(b)
print(c)
print(d)

plot(a, "trueEst")
plot(b, "trueEst")
plot(c, "trueEst")
plot(d, "trueEst")

##------------- 2.8 Single Simulations with Generated Item Pool ----------------

# simulating single theta = -3
a <- randomCAT(-3, IP5, start=Start_5, test=Test, stop=Rule_3, final=Final)
plot(a)

x <- data.frame(item=a$itemNames,sigma=as.data.frame(a$itemPar)$b, 
                pattern=as.factor(a$pattern))

ggplot(x, aes(x=as.numeric(rownames(x)), y=sigma)) + 
  geom_line() +geom_point(aes(shape=pattern), size=3) +  
  scale_shape_manual(values=c(1, 19))+theme_bw()+geom_hline(yintercept = -3)+
  ylim(-3,3) + scale_x_continuous(expand = c(0.01, 0))

# simulating single theta = 3
a <- randomCAT(3, IP5, start=Start_5, test=Test, stop=Rule_3, final=Final)
plot(a)

x <- data.frame(item=a$itemNames,sigma=as.data.frame(a$itemPar)$b, 
                pattern=as.factor(a$pattern))

ggplot(x, aes(x=as.numeric(rownames(x)), y=sigma)) + 
  geom_line() +geom_point(aes(shape=pattern), size=3) +  
  scale_shape_manual(values=c(1, 19))+theme_bw()+
  ylim(-3,3) + scale_x_continuous(expand = c(0.01, 0))

#_______________________________________________________________________________

#------------------------------ 3. STUDY TWO -----------------------------------

##--------------------------- 3.1 CAT Settings ---------------------------------

# person parameters per subtest (not generated)
PP <- data.frame(Task_1=(Rasch1$pers)$WLE,Task_2=(Rasch2$pers)$WLE,
                 Task_3=(Rasch3$pers)$WLE,Task_4=(Rasch4$pers)$WLE)%>%drop_na()

# create function for connected CATs
simulate_CAT <- function(trueTheta.vector, startTheta.vector, itemBank) {
  theta.list <- list()
  testrun.list <- list()
  for (i in 1:length(trueTheta.vector)) {
    f <- randomCAT(itemBank=itemBank, trueTheta=trueTheta.vector[[i]], 
                   start=list(theta = startTheta.vector[[i]]), 
                   test=Test, stop=Rule_3, final=Final)
    theta.df <- data.frame("run" = i,
                           "true_theta" = f$trueTheta, 
                           "final_theta" = f$thFinal, 
                           "start_theta" = f$startTheta,
                           "final_SE" = f$seFinal)
    testrun.df <- data.frame("run" = i,
                             "test_items" = f$testItems, 
                             "item_parameters" = as.data.frame(f$itemPar)$b, 
                             "pattern" = f$pattern, 
                             "theta_provided" = f$thetaProv, 
                             "SE_provided" = f$seProv)
    theta.list[[i]] <- theta.df
    testrun.list[[i]] <- testrun.df
  }
  theta.results.df <- bind_rows(theta.list)
  testrun.results.df <- bind_rows(testrun.list)
  results <- list(theta.results.df, testrun.results.df)
  names(results) = c("results_theta", "results_testrun")
  return(results)
}

##--------------------------- 3.2 CAT Simulation -------------------------------

# simulation of single subCATs
a <- simulateRespondents(itemBank=IP1, thetas=PP$Task_1, start=Start_1, 
                         test=Test, stop=Rule_3, final=Final)
b <- simulateRespondents(itemBank=IP2, thetas=PP$Task_2, start=Start_2, 
                         test=Test, stop=Rule_3, final=Final)
c <- simulateRespondents(itemBank=IP3, thetas=PP$Task_3, start=Start_3, 
                         test=Test, stop=Rule_3, final=Final)
d <- simulateRespondents(itemBank=IP4, thetas=PP$Task_4, start=Start_4, 
                         test=Test, stop=Rule_3, final=Final)
print(a)
print(b)
print(c)
print(d)

# simulation of connected subCATs with true theta input
e <- simulate_CAT(PP$Task_2, a$final.values.df$estimated.theta, IP2)
e_theta <- e$results_theta
e_testrun <- e$results_testrun

f <- simulate_CAT(PP$Task_3, e_theta$final_theta, IP3)
f_theta <- f$results_theta
f_testrun <- f$results_testrun

g <- simulate_CAT(PP$Task_4, f_theta$final_theta, IP4)
g_theta <- g$results_theta
g_testrun <- g$results_testrun

# simulation of single subCATs with fixed theta input
a <- simulateRespondents(itemBank=IP1, thetas=PPfixed, start=Start_1, 
                         test=Test, stop=Rule_3, final=Final)
b <- simulateRespondents(itemBank=IP2, thetas=PPfixed, start=Start_2, 
                         test=Test, stop=Rule_3, final=Final)
c <- simulateRespondents(itemBank=IP3, thetas=PPfixed, start=Start_3, 
                         test=Test, stop=Rule_3, final=Final)
d <- simulateRespondents(itemBank=IP4, thetas=PPfixed, start=Start_4, 
                         test=Test, stop=Rule_3, final=Final)
print(a)
print(b)
print(c)
print(d)

# simulation of connected subCATs with fixed theta input
a <- simulateRespondents(itemBank=IP1, thetas=PPfixed, start=Start_1, 
                         test=Test, stop=Rule_3, final=Final)
print(a)

e <- simulate_CAT(PPfixed, a$final.values.df$estimated.theta, IP2)
e_theta <- e$results_theta
e_testrun <- e$results_testrun

f <- simulate_CAT(PPfixed, e_theta$final_theta, IP3)
f_theta <- f$results_theta
f_testrun <- f$results_testrun

g <- simulate_CAT(PPfixed, f_theta$final_theta, IP4)
g_theta <- g$results_theta
g_testrun <- g$results_testrun

# simulation of connected subCATs with generated Item Pool and true Thetas
a <- simulateRespondents(itemBank=IP5, thetas=PP$Task_1, start=Start_5, 
                         test=Test, stop=Rule_3, final=Final)
print(a)

e <- simulate_CAT(PP$Task_2, a$final.values.df$estimated.theta, IP5)
e_theta <- e$results_theta
e_testrun <- e$results_testrun

f <- simulate_CAT(PP$Task_3, e_theta$final_theta, IP5)
f_theta <- f$results_theta
f_testrun <- f$results_testrun

g <- simulate_CAT(PP$Task_4, f_theta$final_theta, IP5)
g_theta <- g$results_theta
g_testrun <- g$results_testrun

# simulation of single subCATs with generated Item Pool and true Thetas
b <- simulateRespondents(itemBank=IP5, thetas=PP$Task_2, start=Start_5, 
                         test=Test, stop=Rule_3, final=Final)
c <- simulateRespondents(itemBank=IP5, thetas=PP$Task_3, start=Start_5, 
                         test=Test, stop=Rule_3, final=Final)
d <- simulateRespondents(itemBank=IP5, thetas=PP$Task_4, start=Start_5, 
                         test=Test, stop=Rule_3, final=Final)
print(b)
print(c)
print(d)

##------------------------- 3.3 Analysis of SubCATs ----------------------------

# test length
x <- list(e=e_testrun, f=f_testrun, g=g_testrun) %>% map(~add_count(.x, run)) 
summary(x$e$n)
summary(x$f$n)
summary(x$g$n)

# amount of satisfied stop
x <- x %>% map(~select(.x, c(run, n))) %>% map(~unique(.x))
nrow(filter(x$e, n<52))/391
nrow(filter(x$f, n<30))/391
nrow(filter(x$g, n<28))/391
nrow(filter(x$e, n<52))/1000
nrow(filter(x$f, n<30))/1000
nrow(filter(x$g, n<28))/1000

# accuracy
cor.test(e_theta$true_theta, e_theta$final_theta)
cor.test(f_theta$true_theta, f_theta$final_theta)
cor.test(g_theta$true_theta, g_theta$final_theta)

##----------------------- 3.4 Single Student Analysis --------------------------

# theta=-1.82|-0.27|-0.03|-2.50 with fixed start item 
a <- randomCAT(-1.82, IP1, start=Start_1, test=Test, stop=Rule_3, final=Final)
b <- randomCAT(-0.27, IP2, start=Start_2, test=Test, stop=Rule_3, final=Final)
c <- randomCAT(-0.03, IP3, start=Start_3, test=Test, stop=Rule_3, final=Final)
d <- randomCAT(-2.50, IP4, start=Start_4, test=Test, stop=Rule_3, final=Final)

x <- rbind(data.frame(item=a$itemNames,sigma=as.data.frame(a$itemPar)$b, 
                      pattern=as.factor(a$pattern), theta=a$thetaProv),
           data.frame(item=b$itemNames,sigma=as.data.frame(b$itemPar)$b, 
                      pattern=as.factor(b$pattern), theta=b$thetaProv),
           data.frame(item=c$itemNames,sigma=as.data.frame(c$itemPar)$b, 
                      pattern=as.factor(c$pattern), theta=c$thetaProv), 
           data.frame(item=d$itemNames,sigma=as.data.frame(d$itemPar)$b, 
                      pattern=as.factor(d$pattern), theta=d$thetaProv))

ggplot(x, aes(x=as.numeric(rownames(x)), y=sigma)) + 
  geom_line() +geom_point(aes(shape=pattern), size=3) + theme_bw() +
  scale_shape_manual(values=c(1, 19)) + ylim(-3,3) + 
  scale_x_continuous(expand = c(0.01, 0)) + geom_vline(xintercept = 17.5) +
  geom_vline(xintercept = 30.5) + geom_vline(xintercept = 43.5) 

ggplot(x, aes(x=as.numeric(rownames(x)), y=theta)) + + ylim(-3,3)
geom_line(linetype=2) + theme_bw() + scale_x_continuous(expand = c(0.01, 0))

# theta=-1.82|-0.27|-0.03|-2.50 with theta input
a <- randomCAT(-1.82, IP1, start=Start_1, test=Test, stop=Rule_3, final=Final)
b <- randomCAT(-0.27, IP2, start=list(theta=a$thFinal), test=Test, stop=Rule_3, 
               final=Final)
c <- randomCAT(-0.03, IP3, start=list(theta=b$thFinal), test=Test, stop=Rule_3, 
               final=Final)
d <- randomCAT(-2.50, IP4, start=list(theta=c$thFinal), test=Test, stop=Rule_3, 
               final=Final)

x <- rbind(data.frame(item=a$itemNames,sigma=as.data.frame(a$itemPar)$b, 
                      pattern=as.factor(a$pattern), theta=a$thetaProv),
           data.frame(item=b$itemNames,sigma=as.data.frame(b$itemPar)$b, 
                      pattern=as.factor(b$pattern), theta=b$thetaProv),
           data.frame(item=c$itemNames,sigma=as.data.frame(c$itemPar)$b, 
                      pattern=as.factor(c$pattern), theta=c$thetaProv), 
           data.frame(item=d$itemNames,sigma=as.data.frame(d$itemPar)$b, 
                      pattern=as.factor(d$pattern), theta=d$thetaProv))

ggplot(x, aes(x=as.numeric(rownames(x)), y=sigma)) + 
  geom_line() +geom_point(aes(shape=pattern), size=3) + theme_bw() +
  scale_shape_manual(values=c(1, 19)) + ylim(-3,3) + 
  scale_x_continuous(expand = c(0.01, 0)) + geom_vline(xintercept = 13.5) +
  geom_vline(xintercept = 26.5) + geom_vline(xintercept = 39.5) 

ggplot(x, aes(x=as.numeric(rownames(x)), y=theta)) + ylim(-3,3) +
  geom_line(linetype=2) + theme_bw() + scale_x_continuous(expand = c(0.01, 0))
  
# theta=-1 with theta input
a <- randomCAT(-1, IP1, start=Start_1, test=Test, stop=Rule_3, final=Final)
b <- randomCAT(-1, IP2, start=list(theta=a$thFinal), test=Test, stop=Rule_3, 
               final=Final)
c <- randomCAT(-1, IP3, start=list(theta=b$thFinal), test=Test, stop=Rule_3, 
               final=Final)
d <- randomCAT(-1, IP4, start=list(theta=c$thFinal), test=Test, stop=Rule_3, 
               final=Final)

x <- rbind(data.frame(item=a$itemNames,sigma=as.data.frame(a$itemPar)$b, 
                      pattern=as.factor(a$pattern), theta=a$thetaProv),
           data.frame(item=b$itemNames,sigma=as.data.frame(b$itemPar)$b, 
                      pattern=as.factor(b$pattern), theta=b$thetaProv),
           data.frame(item=c$itemNames,sigma=as.data.frame(c$itemPar)$b, 
                      pattern=as.factor(c$pattern), theta=c$thetaProv), 
           data.frame(item=d$itemNames,sigma=as.data.frame(d$itemPar)$b, 
                      pattern=as.factor(d$pattern), theta=d$thetaProv))

ggplot(x, aes(x=as.numeric(rownames(x)), y=sigma)) + 
  geom_line() +geom_point(aes(shape=pattern), size=3) + theme_bw() +
  scale_shape_manual(values=c(1, 19)) + ylim(-3,3) + 
  scale_x_continuous(expand = c(0.01, 0)) + geom_vline(xintercept = 13.5) +
  geom_vline(xintercept = 26.5) + geom_vline(xintercept = 39.5) 

ggplot(x, aes(x=as.numeric(rownames(x)), y=theta)) + ylim(-3,3) +
geom_line(linetype=2) + theme_bw() + scale_x_continuous(expand = c(0.01, 0))  

#_______________________________________________________________________________

#------------------------------ 4. STUDY THREE ---------------------------------

#----------------------------3.1 Descriptive Analysis---------------------------

# prepare input data with special needs information
data <- wide %>% 
  select(-c("S_I05","S_I30","S_I35","S_I19","S_I16","S_I20","S_I34")) %>%
  mutate(KS = Klassenstufe) %>%
  mutate(KS = as.numeric(ifelse(substr(KS, 1, 2) == "MS", 5, KS)))
rownames(data) <- paste0(rownames(data), "-", data$SPF)

# correlation of covariates
apa.cor.table(select(data, Schulbesuchsjahr, KS, Alter))
table(data$KS)

# scores comparison between grades per subtest
Sub1 <- select(data, KS, SPF, GB, Sum_1) %>%
  pivot_longer(c(Sum_1), names_to="Score", values_to = "num")
Sub2 <- select(data, KS, SPF, GB, Sum_2) %>%
  pivot_longer(c(Sum_2), names_to="Score", values_to = "num")
Sub3 <- select(data, KS, SPF, GB, Sum_3) %>%
  pivot_longer(c(Sum_3), names_to="Score", values_to = "num")
Sub4 <- select(data, KS, SPF, GB, Sum_4) %>%
  pivot_longer(c(Sum_4), names_to="Score", values_to = "num")

summary(aov(data$Sum_1~as.factor(data$KS)))
summary(aov(data$Sum_2~as.factor(data$KS)))
summary(aov(data$Sum_3~as.factor(data$KS)))
summary(aov(data$Sum_4~as.factor(data$KS)))

t.test((filter(Sub1, KS==2|KS==3))$num ~ (filter(Sub1, KS==2|KS==3))$KS)
t.test((filter(Sub2, KS==2|KS==3))$num ~ (filter(Sub2, KS==2|KS==3))$KS)
t.test((filter(Sub3, KS==2|KS==3))$num ~ (filter(Sub3, KS==2|KS==3))$KS)
t.test((filter(Sub4, KS==2|KS==3))$num ~ (filter(Sub4, KS==2|KS==3))$KS)

t.test((filter(Sub1, KS==4|KS==3))$num ~ (filter(Sub1, KS==4|KS==3))$KS)
t.test((filter(Sub2, KS==4|KS==3))$num ~ (filter(Sub2, KS==4|KS==3))$KS)
t.test((filter(Sub3, KS==4|KS==3))$num ~ (filter(Sub3, KS==4|KS==3))$KS)
t.test((filter(Sub4, KS==4|KS==3))$num ~ (filter(Sub4, KS==4|KS==3))$KS)

t.test((filter(Sub1, KS==2|KS==5))$num ~ (filter(Sub1, KS==2|KS==5))$KS)
t.test((filter(Sub2, KS==2|KS==5))$num ~ (filter(Sub2, KS==2|KS==5))$KS)
t.test((filter(Sub3, KS==2|KS==5))$num ~ (filter(Sub3, KS==2|KS==5))$KS)
t.test((filter(Sub4, KS==2|KS==5))$num ~ (filter(Sub4, KS==2|KS==5))$KS)

t.test((filter(Sub1, KS==3|KS==5))$num ~ (filter(Sub1, KS==3|KS==5))$KS)
t.test((filter(Sub2, KS==3|KS==5))$num ~ (filter(Sub2, KS==3|KS==5))$KS)
t.test((filter(Sub3, KS==3|KS==5))$num ~ (filter(Sub3, KS==3|KS==5))$KS)
t.test((filter(Sub4, KS==3|KS==5))$num ~ (filter(Sub4, KS==3|KS==5))$KS)

t.test((filter(Sub1, KS==4|KS==5))$num ~ (filter(Sub1, KS==4|KS==5))$KS)
t.test((filter(Sub2, KS==4|KS==5))$num ~ (filter(Sub2, KS==4|KS==5))$KS)
t.test((filter(Sub3, KS==4|KS==5))$num ~ (filter(Sub3, KS==4|KS==5))$KS)
t.test((filter(Sub4, KS==4|KS==5))$num ~ (filter(Sub4, KS==4|KS==5))$KS)

# scores comparison between disability status per subtest
summary(aov(data$Sum_1~as.factor(data$SPF)))
summary(aov(data$Sum_2~as.factor(data$SPF)))
summary(aov(data$Sum_3~as.factor(data$SPF)))
summary(aov(data$Sum_4~as.factor(data$SPF)))

t.test((filter(Sub1, SPF==1|SPF==3))$num ~ (filter(Sub1, SPF==1|SPF==3))$SPF)
t.test((filter(Sub2, SPF==1|SPF==3))$num ~ (filter(Sub2, SPF==1|SPF==3))$SPF)
t.test((filter(Sub3, SPF==1|SPF==3))$num ~ (filter(Sub3, SPF==1|SPF==3))$SPF)
t.test((filter(Sub4, SPF==1|SPF==3))$num ~ (filter(Sub4, SPF==1|SPF==3))$SPF)

t.test((filter(Sub1, SPF==1|SPF==2))$num ~ (filter(Sub1, SPF==1|SPF==2))$SPF)
t.test((filter(Sub2, SPF==1|SPF==2))$num ~ (filter(Sub2, SPF==1|SPF==2))$SPF)
t.test((filter(Sub3, SPF==1|SPF==2))$num ~ (filter(Sub3, SPF==1|SPF==2))$SPF)
t.test((filter(Sub4, SPF==1|SPF==2))$num ~ (filter(Sub4, SPF==1|SPF==2))$SPF)

t.test((filter(Sub1, SPF==3|SPF==2))$num ~ (filter(Sub1, SPF==3|SPF==2))$SPF)
t.test((filter(Sub2, SPF==3|SPF==2))$num ~ (filter(Sub2, SPF==3|SPF==2))$SPF)
t.test((filter(Sub3, SPF==3|SPF==2))$num ~ (filter(Sub3, SPF==3|SPF==2))$SPF)
t.test((filter(Sub4, SPF==3|SPF==2))$num ~ (filter(Sub4, SPF==3|SPF==2))$SPF)

Sub1$SPF[Sub1$SPF != "0"] <- "1"
Sub2$SPF[Sub2$SPF != "0"] <- "1"
Sub3$SPF[Sub3$SPF != "0"] <- "1"
Sub4$SPF[Sub4$SPF != "0"] <- "1"

t.test(Sub1$num ~ Sub1$SPF)
t.test(Sub2$num ~ Sub2$SPF)
t.test(Sub3$num ~ Sub3$SPF)
t.test(Sub4$num ~ Sub4$SPF)

All1 <- select(data, KS, SPF, GB, All_1) %>%
  pivot_longer(c(All_1), names_to="Score", values_to = "num")
All2 <- select(data, KS, SPF, GB, All_2) %>%
  pivot_longer(c(All_2), names_to="Score", values_to = "num")
All3 <- select(data, KS, SPF, GB, All_3) %>%
  pivot_longer(c(All_3), names_to="Score", values_to = "num")
All4 <- select(data, KS, SPF, GB, All_4) %>%
  pivot_longer(c(All_4), names_to="Score", values_to = "num")

summary(aov(data$All_1~as.factor(data$SPF)))
summary(aov(data$All_2~as.factor(data$SPF)))
summary(aov(data$All_4~as.factor(data$SPF)))

TukeyHSD(aov(data$All_1~as.factor(data$SPF)))
TukeyHSD(aov(data$All_2~as.factor(data$SPF)))
TukeyHSD(aov(data$All_4~as.factor(data$SPF)))

All1$SPF[All1$SPF != "0"] <- "1"
All2$SPF[All2$SPF != "0"] <- "1"
All3$SPF[All3$SPF != "0"] <- "1"
All4$SPF[All4$SPF != "0"] <- "1"

t.test(All1$num ~ All1$SPF)
t.test(All2$num ~ All2$SPF)
t.test(All3$num ~ All3$SPF)
t.test(All4$num ~ All4$SPF)

#-------------------------------3.2 CAT Settings--------------------------------

set.seed(1)

# item difficulties and person parameter calculation
Rasch1 <- pers(pair(select(data,starts_with("P_"))))
Rasch2 <- pers(pair(select(data,starts_with("W_"))))
Rasch3 <- pers(pair(select(data,starts_with("B_"))))
Rasch4 <- pers(pair(select(data,starts_with("S_"))))

# set item parameters
IP1 <- data.frame(a=1, b=Rasch1$pair$sigma, c=0, d=1) 
IP2 <- data.frame(a=1, b=Rasch2$pair$sigma, c=0, d=1)
IP3 <- data.frame(a=1, b=Rasch3$pair$sigma, c=0, d=1)
IP4 <- data.frame(a=1, b=Rasch4$pair$sigma, c=0, d=1)
IP5 <- data.frame(a=1, b=runif(100, min=-3, max=3), c=0, d=1) 

# set medium start items (0)
rownames(IP1)[rownames(IP1) == "P_4I"] <- "start1"
rownames(IP2)[rownames(IP2) == "W_I10"] <- "start1"    
rownames(IP3)[rownames(IP3) == "B_1G"] <- "start1"    
rownames(IP4)[rownames(IP4) == "S_I01"] <- "start1"
rownames(IP5)[rownames(IP5) == "16"] <- "start1"

# set easy start items (-1)
rownames(IP1)[rownames(IP1) == "P_4C"] <- "start2"
rownames(IP2)[rownames(IP2) == "W_I31"] <- "start2"    
rownames(IP3)[rownames(IP3) == "B_3C"] <- "start2"    
rownames(IP4)[rownames(IP4) == "S_I017"] <- "start2"
rownames(IP5)[rownames(IP5) == "64"] <- "start2"

# set start rules
Start_1m <- list(theta =0,fixItems=which(rownames(IP1)=="start1"))
Start_2m <- list(theta =0,fixItems=which(rownames(IP2)=="start1"))
Start_3m <- list(theta =0,fixItems=which(rownames(IP3)=="start1"))
Start_4m <- list(theta =0,fixItems=which(rownames(IP4)=="start1"))
Start_5m <- list(theta =0,fixItems=which(rownames(IP5)=="start1"))

Start_1e <- list(theta =0,fixItems=which(rownames(IP1)=="start2"))
Start_2e <- list(theta =0,fixItems=which(rownames(IP2)=="start2"))
Start_3e <- list(theta =0,fixItems=which(rownames(IP3)=="start2"))
Start_4e <- list(theta =0,fixItems=which(rownames(IP4)=="start2"))
Start_5e <- list(theta =0,fixItems=which(rownames(IP5)=="start2"))

# set estimators and stop rule
Test <- list(method="BM", itemSelect="MFI")
Final <- list(method="ML")
Stop <- list(rule = "precision", thr= 0.5)

# person parameter of students with and without SEN
PP1 <- select(as.data.frame(Rasch1$pers[-9]), c(persID, WLE)) %>%
  separate(col=persID, sep="-", into=c("ID","SUB"), extra="merge")
PP2 <- select(as.data.frame(Rasch2$pers[-9]), c(persID, WLE)) %>%
  separate(col=persID, sep="-", into=c("ID","SUB"), extra="merge")
PP3 <- select(as.data.frame(Rasch3$pers[-9]), c(persID, WLE)) %>%
  separate(col=persID, sep="-", into=c("ID","SUB"), extra="merge")
PP4 <- select(as.data.frame(Rasch4$pers[-9]), c(persID, WLE)) %>%
  separate(col=persID, sep="-", into=c("ID","SUB"), extra="merge")

PP1$SUB[PP1$SUB != "0"] <- "1"
PP2$SUB[PP2$SUB != "0"] <- "1"
PP3$SUB[PP3$SUB != "0"] <- "1"
PP4$SUB[PP4$SUB != "0"] <- "1"

PPgenSEN <- data.frame(ST1=rnorm(1000, mean=mean((filter(PP1, SUB==1))$WLE),
                                 sd = sd((filter(PP1, SUB==1))$WLE)),
                       ST2=rnorm(1000, mean=mean((filter(PP2, SUB==1))$WLE),
                                 sd = sd((filter(PP2, SUB==1))$WLE)),
                       ST3=rnorm(1000, mean=mean((filter(PP3, SUB==1))$WLE),
                                 sd = sd((filter(PP3, SUB==1))$WLE)),
                       ST4=rnorm(1000, mean=mean((filter(PP4, SUB==1))$WLE, 
                                                 na.rm=T),
                                 sd = sd((filter(PP4, SUB==1))$WLE, na.rm=T)))
                       
PPgen <- data.frame(ST1=rnorm(1000, mean=mean((filter(PP1, SUB==0))$WLE),
                                 sd = sd((filter(PP1, SUB==0))$WLE)),
                       ST2=rnorm(1000, mean=mean((filter(PP2, SUB==0))$WLE),
                                 sd = sd((filter(PP2, SUB==0))$WLE)),
                       ST3=rnorm(1000, mean=mean((filter(PP3, SUB==0))$WLE),
                                 sd = sd((filter(PP3, SUB==0))$WLE)),
                       ST4=rnorm(1000, mean=mean((filter(PP4, SUB==0))$WLE),
                                 sd = sd((filter(PP4, SUB==0))$WLE)))                                              

PPgen %>% pivot_longer(1:4, names_to="Subtest", values_to="theta") %>%
  ggplot(aes(x=Subtest, y=theta)) + geom_boxplot() +  ylim(-4, 8) + theme_bw()
PPgenSEN %>% pivot_longer(1:4, names_to="Subtest", values_to="theta") %>%
  ggplot(aes(x=Subtest, y=theta)) + geom_boxplot() +  ylim(-4, 8) + theme_bw()

#----------------------------3.3 Simulation of CAT------------------------------

# simulation 1
a1 <- simulateRespondents(itemBank=IP1, thetas=PPgenSEN$ST1, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(35)), final=Final)
b1 <- simulateRespondents(itemBank=IP2, thetas=PPgenSEN$ST2, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(52)), final=Final)
c1 <- simulateRespondents(itemBank=IP3, thetas=PPgenSEN$ST3, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(30)), final=Final)
d1 <- simulateRespondents(itemBank=IP4, thetas=PPgenSEN$ST4, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(28)), final=Final)
print(a1)
print(b1)
print(c1)
print(d1)

# simulation 2
a <- simulateRespondents(itemBank=IP1, thetas=PPgen$ST1, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(35)), final=Final)
b <- simulateRespondents(itemBank=IP2, thetas=PPgen$ST2, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(52)), final=Final)
c <- simulateRespondents(itemBank=IP3, thetas=PPgen$ST3, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(30)), final=Final)
d <- simulateRespondents(itemBank=IP4, thetas=PPgen$ST4, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(28)), final=Final)
print(a)
print(b)
print(c)
print(d)

# simulation 3
a <- simulateRespondents(itemBank=IP1, thetas=PPgenSEN$ST1, 
                         start=Start_1m, test=Test, stop=Stop, final=Final)
b <- simulateRespondents(itemBank=IP2, thetas=PPgenSEN$ST2, 
                         start=Start_2m, test=Test, stop=Stop, final=Final)
c <- simulateRespondents(itemBank=IP3, thetas=PPgenSEN$ST3, 
                         start=Start_3m, test=Test, stop=Stop, final=Final)
d <- simulateRespondents(itemBank=IP4, thetas=PPgenSEN$ST4, 
                         start=Start_4m, test=Test, stop=Stop, final=Final)

print(a)
print(b)
print(c)
print(d)

# simulation 4
a <- simulateRespondents(itemBank=IP1, thetas=PPgen$ST1, 
                         start=Start_1m, test=Test, stop=Stop, final=Final)
b <- simulateRespondents(itemBank=IP2, thetas=PPgen$ST2, 
                         start=Start_2m, test=Test, stop=Stop, final=Final)
c <- simulateRespondents(itemBank=IP3, thetas=PPgen$ST3, 
                         start=Start_3m, test=Test, stop=Stop, final=Final)
d <- simulateRespondents(itemBank=IP4, thetas=PPgen$ST4, 
                         start=Start_4m, test=Test, stop=Stop, final=Final)

print(a)
print(b)
print(c)
print(d)

# simulation 5
a5 <- simulateRespondents(itemBank=IP1, thetas=PPgenSEN$ST1, 
                         start=Start_1e, test=Test, stop=Stop, final=Final)
b5 <- simulateRespondents(itemBank=IP2, thetas=PPgenSEN$ST2, 
                         start=Start_2e, test=Test, stop=Stop, final=Final)
c5 <- simulateRespondents(itemBank=IP3, thetas=PPgenSEN$ST3, 
                         start=Start_3e, test=Test, stop=Stop, final=Final)
d5 <- simulateRespondents(itemBank=IP4, thetas=PPgenSEN$ST4, 
                         start=Start_4e, test=Test, stop=Stop, final=Final)

print(a5)
print(b5)
print(c5)
print(d5)

# simulation 6
a <- simulateRespondents(itemBank=IP1, thetas=PPgen$ST1, 
                         start=Start_1e, test=Test, stop=Stop, final=Final)
b <- simulateRespondents(itemBank=IP2, thetas=PPgen$ST2, 
                         start=Start_2e, test=Test, stop=Stop, final=Final)
c <- simulateRespondents(itemBank=IP3, thetas=PPgen$ST3, 
                         start=Start_3e, test=Test, stop=Stop, final=Final)
d <- simulateRespondents(itemBank=IP4, thetas=PPgen$ST4, 
                         start=Start_4e, test=Test, stop=Stop, final=Final)

print(a)
print(b)
print(c)
print(d)


# simulation 7
a7 <- simulateRespondents(itemBank=IP5, thetas=PPgenSEN$ST1, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(100)), final=Final)
b7 <- simulateRespondents(itemBank=IP5, thetas=PPgenSEN$ST2, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(100)), final=Final)
c7 <- simulateRespondents(itemBank=IP5, thetas=PPgenSEN$ST3, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(100)), final=Final)
d7 <- simulateRespondents(itemBank=IP5, thetas=PPgenSEN$ST4, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(100)), final=Final)
print(a7)
print(b7)
print(c7)
print(d7)

# simulation 8
a <- simulateRespondents(itemBank=IP5, thetas=PPgen$ST1, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(100)), final=Final)
b <- simulateRespondents(itemBank=IP5, thetas=PPgen$ST2, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(100)), final=Final)
c <- simulateRespondents(itemBank=IP5, thetas=PPgen$ST3, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(100)), final=Final)
d <- simulateRespondents(itemBank=IP5, thetas=PPgen$ST4, 
                         start=list(fixItems=NULL),
                         test=list(itemSelect="random"), 
                         stop=list(rule="length", thr=c(100)), final=Final)
print(a)
print(b)
print(c)
print(d)

# simulation 9
a <- simulateRespondents(itemBank=IP5, thetas=PPgenSEN$ST1, 
                         start=Start_1m, test=Test, stop=Stop, final=Final)
b <- simulateRespondents(itemBank=IP5, thetas=PPgenSEN$ST2, 
                         start=Start_2m, test=Test, stop=Stop, final=Final)
c <- simulateRespondents(itemBank=IP5, thetas=PPgenSEN$ST3, 
                         start=Start_3m, test=Test, stop=Stop, final=Final)
d <- simulateRespondents(itemBank=IP5, thetas=PPgenSEN$ST4, 
                         start=Start_4m, test=Test, stop=Stop, final=Final)

print(a)
print(b)
print(c)
print(d)

# simulation 10
a <- simulateRespondents(itemBank=IP5, thetas=PPgen$ST1, 
                         start=Start_1m, test=Test, stop=Stop, final=Final)
b <- simulateRespondents(itemBank=IP5, thetas=PPgen$ST2, 
                         start=Start_2m, test=Test, stop=Stop, final=Final)
c <- simulateRespondents(itemBank=IP5, thetas=PPgen$ST3, 
                         start=Start_3m, test=Test, stop=Stop, final=Final)
d <- simulateRespondents(itemBank=IP5, thetas=PPgen$ST4, 
                         start=Start_4m, test=Test, stop=Stop, final=Final)

print(a)
print(b)
print(c)
print(d)

# simulation 11
a11 <- simulateRespondents(itemBank=IP5, thetas=PPgenSEN$ST1, 
                         start=Start_1e, test=Test, stop=Stop, final=Final)
b11 <- simulateRespondents(itemBank=IP5, thetas=PPgenSEN$ST2, 
                         start=Start_2e, test=Test, stop=Stop, final=Final)
c11 <- simulateRespondents(itemBank=IP5, thetas=PPgenSEN$ST3, 
                         start=Start_3e, test=Test, stop=Stop, final=Final)
d11 <- simulateRespondents(itemBank=IP5, thetas=PPgenSEN$ST4, 
                         start=Start_4e, test=Test, stop=Stop, final=Final)

print(a11)
print(b11)
print(c11)
print(d11)

# simulation 12
a <- simulateRespondents(itemBank=IP5, thetas=PPgen$ST1, 
                         start=Start_1e, test=Test, stop=Stop, final=Final)
b <- simulateRespondents(itemBank=IP5, thetas=PPgen$ST2, 
                         start=Start_2e, test=Test, stop=Stop, final=Final)
c <- simulateRespondents(itemBank=IP5, thetas=PPgen$ST3, 
                         start=Start_3e, test=Test, stop=Stop, final=Final)
d <- simulateRespondents(itemBank=IP5, thetas=PPgen$ST4, 
                         start=Start_4e, test=Test, stop=Stop, final=Final)

print(a)
print(b)
print(c)
print(d)

#------------------------3.4 Correct and Wrong Answers--------------------------

set.seed(1)

# answers of simulation 1
pattern1a <- select(a1$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern1a$Sum, pattern1a$Err, pattern1a$PercSum, pattern1a$PercErr), 
       sum_sd)

pattern1b <- select(b1$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern1b$Sum, pattern1b$Err, pattern1b$PercSum, pattern1b$PercErr), 
       sum_sd)

pattern1c <- select(c1$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern1c$Sum, pattern1c$Err, pattern1c$PercSum, pattern1c$PercErr), 
       sum_sd)

pattern1d <- select(d1$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern1d$Sum, pattern1d$Err, pattern1d$PercSum, pattern1d$PercErr), 
       sum_sd)

# answers of simulation 5
pattern5a <- select(a5$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern5a$Sum, pattern5a$Err, pattern5a$PercSum, pattern5a$PercErr), 
       sum_sd)

pattern5b <- select(b5$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern5b$Sum, pattern5b$Err, pattern5b$PercSum, pattern5b$PercErr), 
       sum_sd)

pattern5c <- select(c5$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern5c$Sum, pattern5c$Err, pattern5c$PercSum, pattern5c$PercErr), 
       sum_sd)

pattern5d <- select(d5$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern5d$Sum, pattern5d$Err, pattern5d$PercSum, pattern5d$PercErr), 
       sum_sd)

# answers of simulation 7
pattern7a <- select(a7$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern7a$Sum, pattern7a$Err, pattern7a$PercSum, pattern7a$PercErr), 
       sum_sd)

pattern7b <- select(b7$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern7b$Sum, pattern7b$Err, pattern7b$PercSum, pattern7b$PercErr), 
       sum_sd)

pattern7c <- select(c7$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern7c$Sum, pattern7c$Err, pattern7c$PercSum, pattern7c$PercErr), 
       sum_sd)

pattern7d <- select(d7$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern7d$Sum, pattern7d$Err, pattern7d$PercSum, pattern7d$PercErr), 
       sum_sd)

# answers of simulation 11
pattern11a <- select(a11$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern11a$Sum, pattern11a$Err, pattern11a$PercSum,
            pattern11a$PercErr), sum_sd)

pattern11b <- select(b11$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern11b$Sum, pattern11b$Err, pattern11b$PercSum,
            pattern11b$PercErr), sum_sd)

pattern11c <- select(c11$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern11c$Sum, pattern11c$Err, pattern11c$PercSum,
            pattern11c$PercErr), sum_sd)

pattern11d <- select(d11$responses.df, starts_with("response")) %>%
  mutate_all(~na_if(., -99)) %>%
  mutate(Sum=rowSums(select(., starts_with("response")), na.rm=T),
         Err=rowSums(select(.,starts_with("response"))==0, na.rm=T)) %>%
  mutate(All=Sum+Err) %>% mutate(PercSum=(Sum/All)*100, PercErr=(Err/All)*100)
lapply(list(pattern11d$Sum, pattern11d$Err, pattern11d$PercSum,
            pattern11d$PercErr), sum_sd)

#_______________________________________________________________________________
#_______________________________________________________________________________
