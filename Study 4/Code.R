# 1. Packages ------------------------------------------------------------------
library(tidyverse) # Data Wrangling & Visualization
library(pairwise)  # Rasch Modeling
library(eRm)       # Rasch Modeling
library(ggpubr)    # Advanced ggplots (ggarrange)
library(patchwork) # Combining multiple plots
library(apaTables) # Tables in APA format

# 2. Load Data -----------------------------------------------------------------

# Daten in wide-format with person's answers per row
Data <- read.csv2("")

# 3. Data Wrangling ------------------------------------------------------------

Data1 <- Data %>% 
  # Create columns with SEN information and Person-ID
  mutate(FB = case_when(SPF != "0" ~ 1, SPF=="0" ~ 0), FB_ID = 1:400) %>%
  # Select only flash reading and SEN columns
  select(starts_with("B_") | starts_with("FB"))
Data2 <- Data1 %>%
  # Unite SEN and ID column
  unite("ID", FB:FB_ID) %>%
  # Put ID as row names
  remove_rownames %>% column_to_rownames(var="ID")

# 4. Rasch Analysis ------------------------------------------------------------

# 4.1 Rasch modelling ####
set.seed(29042024)
pair <- pair(Data2) # Rasch Model calculation
pers <- pers(pair) # Person Parameter estimation
andersentest.pers(pers, split="median") # Andersen LR Test

# 4.2 Wald Test (Graphical Model Check for Dif) ####
plot(grm(daten=Data2, m=2,split=Data1$FB), itemNames=F)
Data3 <- Data2 %>% select(-B_2G)
pair2 <- pair(Data3) # Rasch Model calculation
pers2 <- pers(pair2) # Person Parameter estimation
rm(pair, pers, Data2) # Remove unnecessary objects

# 4.3. Graphical Model Plot ####
png("GRM.png", width=1200, height=1200, res=300)
par(mar=c(5, 4, 3, 2))
plot(grm(daten = Data3, m = 2, split = Data1$FB), itemNames = FALSE,
     main = "", xaxt = "n", yaxt = "n", xlab="", ylab="")
title(xlab = "Students without SEN", ylab = "Students with SEN", mgp=c(2,1,0))
# Achsenbeschriftungen hinzufügen
axis(1, -3:3, col = "darkgray", font = 1, cex.axis = 0.8, las = 1, tck = -0.02)
axis(2, -3:3, col = "darkgray", font = 1, cex.axis = 0.8, las = 2, tck = -0.02)
# Gitterlinien hinzufügen
grid()
# Rand hinzufügen
box()
dev.off()

# 4.4 Item-Person-Map ####
## Formatting sigma values in crescending order 
a <- data.frame("sigma"=pers2$pair$sigma) %>% 
  arrange(sigma) %>% mutate("order"=1:29, "Items"="1")
## Create item map in ggplot
itemmap <- ggplot(a, aes(x=order, y=sigma, fill=Items)) +
  # Define Plot
  geom_point(color="blue", size=2) + 
  # Text on axes
  labs(x = "Items of Item Pool", y="") +
  # Text on Legend
  scale_fill_discrete(name="Item difficulty", labels=c("")) +
  # Theme and Axis size margins
  theme_bw() + 
  # Graphical parameters
  theme(
    # Position of legend
    legend.position="top",
    # Axis text and ticks
    axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    # Plot margins (for combining the plots)
    plot.margin = unit(c(0,1,0,-1.25), "lines")) +
  scale_y_continuous(limits=c(-3, 5)) +
  scale_x_continuous(expand=c(0.02, 0))
itemmap

## Formatting theta values with SEN variable
b <- data.frame("ID" = pers2$pers$persID,"theta"=pers2$pers$WLE) %>% 
  separate(ID, into=c("SEN", "ID"), sep="_")

## Create item map in ggplot
persmap <- ggplot(b, aes(x=theta, fill=SEN)) + 
  # Define plot
  geom_histogram(colour = "black", position="stack", binwidth=0.4) + 
  # Definition of Legend
  scale_fill_manual(name="Students", labels=c("without SEN", "with SEN"), 
                    values=c("grey", "grey30"))+
  # Text on axes
  labs(x = "Logits", y="Number of Respondents") +
  # Theme and Axis size margins
  theme_bw() + 
  # Graphical parameters
  theme(
    # Position and graphics of legend
    legend.position="top",
    # Plot margins (for combining the plots)
    plot.margin = unit(c(0,0,0,0), "lines")) +
  scale_x_continuous(limits=c(-3, 5)) +
  # Flipping the plot
  coord_flip() + scale_y_reverse(expand=c(0.005,0))
persmap

## Arrange Itemmap and Personmap
itempersplots <- ggarrange(persmap, itemmap, nrow=1, widths=c(1.2, 1))
itempersplots
ggsave("person_item_map.png", width = 16, height = 8, units = "cm")

rm(b, c, errormap, GRM, itemmap, itempersmap, itempersplots, persmap, x, y)

# 4.3 Sigma and Theta ####
summary(a$sigma)
sd(a$sigma)
summary(b$theta)
sd(b$theta)

# 5. Descriptive Analysis of DGICs ---------------------------------------------
setwd("H:/Texte/Publikationen/2024 Blitzlesetest (MDPI)/Abbildungen")

# Updating Item Data
Items1 <- a %>% 
  mutate(itemID = row.names(.)) %>% # Define Item IDs as column
  select(c(itemID, sigma, order)) %>% # Select necessary information
  full_join(Items, join_by(itemID==Item)) %>% # Join all infos in one df
  na.omit() %>% # Remove item which did not fit the Rasch model
  mutate(length_var=case_when(length<5 ~ "<5", length==5 ~ "5", length==6 ~ "6", 
                              length==7 ~ "7", length>7 ~ ">7")) %>%
  mutate(length_var=factor(length_var, levels=c("<5", "5", "6", "7", ">7")))

# Visualization of Item Difficulty and UVs
## Sigma and Duration ####
plot_sigma_dur <- ggplot(Items1, aes(x=order, y=sigma, fill=as.factor(Dauer))) +
  # Define Plot
  geom_point(size=2.5, shape=21, color="black") +
  geom_text(aes(x=1.5, y=2, label="A"), size=5) +
  scale_fill_manual(values=c("#c6dbef", "#6baed6", "#2271b5", "#08306b")) +
  # Change Plot Texts
  labs(x = element_blank(), y="Logits", # Text on Axes
       fill = "Duration (ms)") + # Text on Legend
  # Theme
  theme_bw() + 
  # Graphical parameters
  theme(legend.position="right", legend.key.height= unit(0.4, 'cm'),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  # Setting Axis limits and zooming
  scale_y_continuous(limits=c(-1, 2.5)) +
  scale_x_continuous(expand=c(0.02, 0))
plot_sigma_dur

## Sigma and Length ####
plot_sigma_len <- ggplot(Items1, aes(x=order, y=sigma, fill=length_var)) +
  # Define Plot
  geom_point(size=2.5, shape=21, color="black") +
  geom_text(aes(x=1.5, y=2, label="B"), size=5) +
  scale_fill_manual(values=c("#ffeda0","#feb24c","#fd8d3c","#e3211c","#800f26")) +
  # Change Plot Texts
  labs(x = element_blank(), y="Logits", # Text on Axes
       fill = "N letters") + # Text on Legend
  # Theme
  theme_bw() + 
  # Graphical parameters
  theme(legend.position="right", legend.key.height= unit(0.4, 'cm'),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  # Setting Axis limits and zooming
  scale_y_continuous(limits=c(-1, 2.5)) +
  scale_x_continuous(expand=c(0.02, 0))
plot_sigma_len

## Sigma and Syllables ####
plot_sigma_syl <- ggplot(Items1, aes(x=order, y=sigma, fill=as.factor(syl))) +
  # Define Plot
  geom_point(size=2.5, shape=21, color="black") +
  geom_text(aes(x=1.5, y=2, label="C"), size=5) +
  scale_fill_manual(values=c("#f0f0f0", "#969696", "#252525")) +
  # Change Plot Texts
  labs(x = "Items of Item Pool", y="Logits", # Text on Axes
       fill = "N syllables") + # Text on Legend
  # Theme
  theme_bw() + 
  # Graphical parameters
  theme(legend.position="right", legend.key.height= unit(0.4, 'cm'),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  # Setting Axis limits and zooming
  scale_y_continuous(limits=c(-1, 2.5)) +
  scale_x_continuous(expand=c(0.02, 0))
plot_sigma_syl

## Combine images of AV and UVs
plot_sigma_UVs <- plot_sigma_dur / plot_sigma_len / plot_sigma_syl
plot_sigma_UVs
ggsave("AV_UV.png", width = 19, height = 9, units = "cm")

# Correlation of Variables
var_cor <- Items1 %>% select(sigma, Dauer, syl, length) %>% 
  apa.cor.table(filename="cor_table.doc")
var_cor

rm(a, b, itemmap, itempersplots, persmap, plot_grm, plot_rasch, Items)

# 5. Regression Analysis of DGICs ----------------------------------------------

# Linear Regression Analysis
var_reg1_1 <- lm(sigma ~ length, data=Items1) # letter length
summary(var_reg1_1)
var_reg1_2 <- lm(sigma ~ Dauer, data=Items1) # duration
summary(var_reg1_2)
var_reg1_3 <- lm(sigma ~ syl, data=Items1) # syllable length
summary(var_reg1_3)

apa.reg.table(var_reg1_1, var_reg1_2, var_reg1_3, filename="reg1.doc")

# Muliple Regression Analysis
var_reg2 <- lm(sigma ~ length+Dauer, data=Items1)
summary(var_reg2)
var_reg3 <- lm(sigma ~ length+Dauer+syl, data=Items1)
summary(var_reg3)

apa.reg.table(var_reg1_1, var_reg2, var_reg3, filename="reg2.doc")

# 6. LLTM ----------------------------------------------------------------------

# Define DGICs
Items2 <- Items1 %>%
  mutate(length_more_as_med = case_when(
    length>median(Items2$length) ~ 1, length<=median(Items2$length) ~ 0)) %>%
  mutate(dur_short = case_when(Dauer>1000 ~ 0, Dauer<=1000 ~ 1))

# Redefine order of item results df
Data4 <- Data3 %>% select(Items2$itemID)

# Define design matrices
matrix1_1 <- Items2 %>% select(length_more_as_med) %>% as.matrix()
matrix1_2 <- Items2 %>% select(dur_short) %>% as.matrix()
matrix2 <- Items2 %>% select(length_more_as_med,dur_short)%>%as.matrix()

# LLTM modeling
rasch <- RM(Data4)
var_lltm1_1 <- LLTM(Data4, W=matrix1_1, mpoints=1, groupvec=1)
var_lltm1_2 <- LLTM(Data4, W=matrix1_2, mpoints=1, groupvec=1)
var_lltm2 <- LLTM(Data4, W=matrix2, mpoints=1, groupvec=1)

# Model comparison
anova(rasch, var_lltm1_1, var_lltm1_2, var_lltm2)
IC(person.parameter(var_lltm1_1))
IC(person.parameter(var_lltm1_2))
IC(person.parameter(var_lltm2))
IC(person.parameter(rasch))

# 7. Performance Analysis ------------------------------------------------------

# Prepare data

matrix_new <- matrix %>%
  as.data.frame() %>%
  mutate(DGIC = case_when(
    length_more_as_med == 0 & dur_short == 0 ~ 1,
    length_more_as_med == 0 & dur_short == 1 ~ 2,
    length_more_as_med == 1 & dur_short == 0 ~ 3,
    length_more_as_med == 1 & dur_short == 1 ~ 4))
## items grouped by DGIC
items_1 <- matrix_new %>% filter(DGIC==1) %>% pull(itemID)
items_2 <- matrix_new %>% filter(DGIC==2) %>% pull(itemID)
items_3 <- matrix_new %>% filter(DGIC==3) %>% pull(itemID)
items_4 <- matrix_new %>% filter(DGIC==4) %>% pull(itemID)
## Create variables
Data5 <- Data %>%
  select(Kind_ID, SPF, LRS, starts_with("B_"), -B_2G) %>%
  mutate(SEN = factor(case_when(LRS==1 ~ 4, LRS==0 ~ SPF), 
                         levels=c(0, 3, 4, 1, 2)),
         SEN_name = factor(case_when(
           SEN==0 ~ "No SEN", SEN==1 ~ "Learning", SEN==2 ~ "Intelligence",
           SEN==3 ~ "Speech", SEN==4 ~ "Dyslexia"),
           levels=c("No SEN", "Speech", "Dyslexia", "Learning", "Intelligence")),
         Score_1 = rowSums(select(., items_1)),
         Score_2 = rowSums(select(., items_2)),
         Score_3 = rowSums(select(., items_3)),
         Score_4 = rowSums(select(., items_4)),
         Score = rowSums(select(., starts_with("B_")))) %>%
  mutate(Score_1_p = Score_1/max(Score_1),
         Score_2_p = Score_2/max(Score_2),
         Score_3_p = Score_3/max(Score_3),
         Score_4_p = Score_4/max(Score_4),
         Score_p = Score/max(Score)) %>%
  select(Kind_ID, SEN, SEN_name, Score, ends_with("p")) %>%
  pivot_longer(cols = ends_with("p"), 
               names_to = "ScoreType", values_to = "ScoreValue") %>%
  mutate(ScoreType_names = case_when(
    ScoreType=="Score_1_p" ~ "Short words and long duration",
    ScoreType=="Score_2_p" ~ "Short words and short duration",
    ScoreType=="Score_3_p" ~ "Long words and long duration",
    ScoreType=="Score_4_p" ~ "Long words and short duration"))

table(filter(Data5, ScoreType=="Score_p")$SEN_name)

# Plot of Score for SEN types
plot_sen <- ggplot(filter(Data5, ScoreType=="Score_p"), 
                   aes(x=SEN_name, y=ScoreValue)) +
  geom_boxplot(fill="Grey") + 
  theme_bw() + labs(x = "SEN Status", y="Score (%)")
plot_sen

# Plot of Sub-Scores for SEN types
plot_sen_2 <- ggplot(filter(Data5, ScoreType!="Score_p"), 
                     aes(x=SEN_name, y=ScoreValue, fill=ScoreType_names)) +
  geom_boxplot() + 
  scale_fill_brewer(palette="Set2") +
  # Change Plot Texts
  labs(x = element_blank(), y="Score (%)", # Text on Axes
       fill = "DGIC Item Group") + # Text on Legend 
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        legend.position = "right")
plot_sen_2
