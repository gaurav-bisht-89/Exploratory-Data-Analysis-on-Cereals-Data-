setwd("E:/Data Science/R/class 7")
getwd()
library(tidyverse)
library(psych)
list.files()
#-----------Importing the cereals_data file-----------------
cereals_data<-read.csv("E:/Data Science/R/class 7/SM_24052020_Project_Cereals/cereals_data.csv")
View(cereals_data)
dim(cereals_data)
str(cereals_data)
cereals_data1<-cereals_data
View(cereals_data1)

#=============Data Clening & wrangling===============

#---------Replacing short column names with complete name--------
colnames(cereals_data1) <-c("Name", "Manufacturer", "Type", "Calories", "Protein", "Fat", 
                            "Sodium", "Fiber", "Carbohydrates", "Sugar", "Potassium", 
                            "Vitamins", "Shelf", "Weight", "Cups", "Rating")
variable.names(cereals_data1)
variable.names(cereals_data)
#-----------creating anothe variable Manufacturer_Name------
cereals_data1$Manufacturer_Name <- cereals_data1$Manufacturer
dim(cereals_data1)
cereals_data1$Manufacturer_Name <- gsub(pattern = "P", replacement = "Post",x = cereals_data1$Manufacturer_Name)
cereals_data1$Manufacturer_Name <- gsub(pattern = "A", replacement = "American Home..",x = cereals_data1$Manufacturer_Name)
cereals_data1$Manufacturer_Name <- gsub(pattern = "G", replacement = "General Mills",x = cereals_data1$Manufacturer_Name)
cereals_data1$Manufacturer_Name <- gsub(pattern = "K", replacement = "Kelloggs",x = cereals_data1$Manufacturer_Name)
cereals_data1$Manufacturer_Name <- gsub(pattern = "N", replacement = "Nabisco",x = cereals_data1$Manufacturer_Name)
cereals_data1$Manufacturer_Name <- gsub(pattern = "Q", replacement = "Quaker Oats",x = cereals_data1$Manufacturer_Name)
cereals_data1$Manufacturer_Name <- gsub(pattern = "R", replacement = "Ralston Purina",x = cereals_data1$Manufacturer_Name)
variable.names(cereals_data1)
cereals_data1$Manufacturer_Name
dim(cereals_data1)
#--------Replace H and C in Type with Hot and Cold-------
cereals_data1$Type <- gsub("H", "Hot", x = cereals_data1$Type)
cereals_data1$Type <- gsub("C", "Cold", x = cereals_data1$Type)

#--------Rounding off Rating to two decimal points-------
cereals_data1$Rating<-round(cereals_data1$Rating,2)
#----Removing 1st variable "Names" and alloting to rows------
library(dplyr)
rownames(cereals_data1)<-cereals_data1[ ,1] #---OR---rownames(cereals_data1)= cereals_data1$Name;rownames(cereals_data1)
View(cereals_data1)
cereals_data2 <-cereals_data1[ ,-1]
rownames(cereals_data2)
View(cereals_data2)

# ------Change cereal type,shelf to factor----
str(cereals_data2)
cereals_data2$Type <- factor(cereals_data2$Type)
cereals_data2$Shelf <- factor(cereals_data2$Shelf)
sapply(cereals_data2, FUN = class)
#=========Evaluating each variable through concepts of sample statistics=====
summary(cereals_data2)
describe(cereals_data2$Calories)
describe(cereals_data2$Protein)
describe(cereals_data2$Fat)
describe(cereals_data2$Sodium)
describe(cereals_data2$Fiber)
describe(cereals_data2$Carbohydrates)
describe(cereals_data2$Sugar)
describe(cereals_data2$Potassium)
describe(cereals_data2$Vitamins)

#===========Data Visualization-I ============

library(ggplot2)
library(gplots)
library(superheat)
library(corrplot)
library(readr)
library(plotrix)
library(ggcorrplot)
library(ggpubr)

#-----All nutrients in single plot---
c1<- subset(cereals_data2, select = c(Calories:Vitamins))
as.data.frame(c1)%>%
  gather() %>%                             
  ggplot(aes(value)) +                      
  facet_wrap(~ key, scales = "free") + 
  geom_histogram(fill = "darkgreen") + theme(strip.background = element_rect(fill="lightgreen"))+
  theme(panel.background = element_rect(fill = 'grey', colour = 'black'))

#-----Histogram-Distribution of weight and cups-----
dev.off()
p<-cereals_data2$Weight

hist(p,
       breaks=7,
       col="green",
       xlab = "Weight",
       ylab = "Count",
       main = "Histogram of Weights", plot=TRUE) 

q<-cereals_data2$Cups
hist(q,
     breaks=7,
     col="yellow",
     xlab = "Cups",
     ylab = "Count",
     main = "Histogram of Cups", plot=TRUE) 

#--------3d--Pie Chart- Type wise product percentage------
dev.off()
ctype_cat<-c("Cold","Hot")
ctype_count<-c(74,3)
ctype_count_pct<-round(ctype_count/sum(ctype_count)*100)
ctype_count_pct
lbls<-paste(ctype_cat,"= ",ctype_count, ";", ctype_count_pct,"%",sep = " ")

a<-table(cereals_data2$Type)
grp<-c("Cold","Hot")
cnt<-round(a)
grpd<-paste(grp,"=",cnt)
lbl<-paste(grpd,";",ctype_count_pct,"%")
pie3D(a,
      labels = lbl,
      labelcex=0.9,
      main = "Type wise product percentage",
      col = rainbow(length(a)))

#------Co-relation plot------
variable.names(cereals_data2)
pairs.panels(cereals_data2[,-c(1,2,12,13,14,16,11,17,18)],
             method = "pearson", #coorelation method
             hist.col = "blue",
             main="Correlation of calories and Nutrients",
             density = TRUE, # show density plots
             ellipses = TRUE, # show correlation ellipses
             lm=TRUE, #linear regression fits 
             cex.cor = 2,
             cex.labels=1.5
)
#==============Data Manipulation===========

#-----------Creating additional variable Calories_Cat,Rating_Cat---------
table(cereals_data2$Calories)
cereals_data2 <- within(cereals_data2, {
  Calories_cat <- NA
  Calories_cat[cereals_data2$Calories <= 100] <-"Low_Calories"
  Calories_cat[cereals_data2$Calories > 100 & cereals_data2$Calories < 130] <-"Medium_Calories"
  Calories_cat[cereals_data2$Calories >= 130] <-"High_Calories"
})
table(cereals_data2$Calories_cat)
cereals_data2$Calories_cat<-factor(cereals_data2$Calories_cat)
str(cereals_data2$Calories_cat)

cereals_data2<- within(cereals_data2, {
  Rating_Cat <- NA
  Rating_Cat[cereals_data2$Rating <= 40] <- "Poor"
  Rating_Cat[cereals_data2$Rating > 40 & cereals_data2$Rating < 64] <- "Satisfactory"
  Rating_Cat[cereals_data2$Rating >= 64] <- "Good"
})
table(cereals_data2$Rating_Cat)

#==============Missing Value Treatment using kNN Imputation==============

#----Calculate NAs------
sum(is.na(cereals_data2))
summary(cereals_data2)

#-------Histograms to check the distribution for imputation-----
dev.off()
par(mfrow=c(1,3))
x<-cereals_data2$Carbohydrates
h<-hist(x,
       breaks=10,
       col="skyblue",
       xlab = "Carbohydrates",
       ylab = "Count",
       main = "Histogram of Carbohydrates")
xfit<-seq(min(x,na.rm = T),max(x,na.rm = T),length(40))
yfit<-dnorm(xfit, mean = mean(x,na.rm = T), sd=sd(x,na.rm = T))
yfit<-yfit * diff(h$mids[1:2]*length(x)) #h$mids are mid points of the intervals
lines(xfit, yfit, col="blue",lwd=3)

y<-cereals_data2$Sugar
h<-hist(y,
        breaks=10,
        col="lightgreen",
        xlab = "Sugar",
        ylab = "Count",
        main = "Histogram of Sugar")
xfit<-seq(min(y,na.rm = T),max(y,na.rm = T),length(40));xfit
yfit<-dnorm(xfit, mean = mean(y,na.rm = T), sd=sd(y,na.rm = T))
yfit<-yfit * diff(h$mids[1:2]*length(y))
lines(xfit, yfit, col="blue",lwd=3)

z<-cereals_data2$Potassium
h<-hist(z,
        breaks=10,
        col="khaki",
        xlab = "Potassium",
        ylab = "Count",
        main = "Histogram of Potassium")
xfitt<-seq(min(z,na.rm = T),max(z,na.rm = T),length(40))
yfitt<-dnorm(xfitt, mean = mean(z,na.rm = T), sd=sd(z,na.rm = T))
yfitt<-yfitt * diff(h$mids[1:2]*length(z))
lines(xfitt, yfitt, col="blue",lwd=3)

#----kNN Imputation-----
library(VIM)
cereals_data2<-kNN(cereals_data2, k=5)
sum(is.na(cereals_data2))
variable.names(cereals_data2)
#--------Removing additional rows form 18 to 34 after kNN-------
cereals_data2<-select(cereals_data2,-19:-36)
dim(cereals_data2)
sum(is.na(cereals_data2))
variable.names(cereals_data2)
#============Data Visualisation-II ===========

#---Box plot of all nutrients----
dev.off()
cereals_data3<-cereals_data2
names(cereals_data3)
cereals_data3<- subset(cereals_data3, select = c(Calories:Potassium))
str(cereals_data3)
cereals_data3<- scale(cereals_data3)
head(cereals_data3,5)

boxplot(cereals_data3,
        main = "Boxplot for all nutrients",
        xlab = "Nutrients",
        ylab = "Level for outlier detection",
        col = c("orange","red","orange","red","orange","red","orange","red"),
        border = "brown",
        horizontal = F,
        notch = TRUE)

#----Pie Chart for Manufacturer wise distribution-------
dev.off()
cereals_data4<- cereals_data2%>%
  count(Manufacturer_Name) %>%
  arrange(Manufacturer_Name) %>%
  mutate(prop = round(n * 100 / sum(n), 0),
         lab.ypos = (cumsum(prop) - (0.8*prop)), 
         labl = paste0(prop, "", "%"))
pie(cereals_data4$n, 
    radius = 0.5,
    labels = cereals_data4$labl, 
    main = "Pie Chart for distribution of manufacturers",
    col = rainbow(length(cereals_data4$n)))
legend("topright",as.vector(cereals_data4$Manufacturer_Name), cex = 0.55,
       fill = rainbow(length(cereals_data4$n)))

#-----------Manufacturer wise average content of nutrient----------

head(cereals_data2)
str(cereals_data2)
hist_data <- cereals_data2
hist_data <- cereals_data2[, c(3:11,16)]
str(hist_data)
rownames(hist_data) <- c()
head(hist_data)
hist_data1 <- hist_data %>%
  group_by(Manufacturer_Name) %>% 
  summarise(Calories = round(mean(Calories),2), 
            Protein = round(mean(Protein),2),
            Fat = round(mean(Fat),2),
            Fiber = round(mean(Fiber),2),
            Sugar = round(mean(Sugar),2),
            Potassium = round(mean(Potassium),2),
  )

head(hist_data1,20)
str(hist_data1)
n1 <- ggplot(hist_data1, aes(x = Manufacturer_Name, y = Calories)) +
  geom_bar(stat = "identity", fill = "brown1") +
  geom_text(aes(label = Calories), position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 75, size = 10, face = "bold")) +
  labs(title = "Avg. content of Calories (per serving)",
       x = "",
       y = "")
n2 <- ggplot(hist_data1, aes(x = Manufacturer_Name, y = Protein)) +
  geom_bar(stat = "identity", fill = "cyan1") +
  geom_text(aes(label = Protein), position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 75, size = 10, face = "bold")) +
  labs(title = "Avg. content of Protien (grams)",
       x = "",
       y = "")
n3 <- ggplot(hist_data1, aes(x = Manufacturer_Name, y = Fiber)) +
  geom_bar(stat = "identity", fill = "darkorchid1") +
  geom_text(aes(label = Fiber), position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 75, size = 10, face = "bold")) +
  labs(title = "Avg. content of Fiber (grams)",
       x = "",
       y = "")
n4 <- ggplot(hist_data1, aes(x = Manufacturer_Name, y = Sugar)) +
  geom_bar(stat = "identity", fill = "indianred") +
  geom_text(aes(label = Sugar), position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 75, size = 10, face = "bold")) +
  labs(title = "Avg. content of Sugar (grams)",
       x = "",
       y = "")
n5 <- ggplot(hist_data1, aes(x = Manufacturer_Name, y = Fat)) +
  geom_bar(stat = "identity", fill = "olivedrab1") +
  geom_text(aes(label = Fat), position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 75, size = 10, face = "bold")) +
  labs(title = "Avg. content of Fat (grams)",
       x = "",
       y = "")
n6 <- ggplot(hist_data1, aes(x = Manufacturer_Name, y = Potassium )) +
  geom_bar(stat = "identity", fill = "khaki4") +
  geom_text(aes(label = Fat), position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 75, size = 10, face = "bold")) +
  labs(title = "Avg. content of Potassium  (milligrams)",
       x = "",
       y = "")

figure <- ggarrange(n1,n2,n3,n4,n5,n6, ncol = 3, nrow = 2)
figure

#-----------Histogram of Rating Vs Calories level across manufacturers---

ggplot(cereals_data2, aes(x = Rating)) +
  geom_histogram(color = "white",
                 fill = "indianred",
                 binwidth = 10) +
  facet_grid(Calories_cat ~ Manufacturer_Name) +
  labs(title = "Rating Vs Calories level across Manufacturers",
       x = "Rating")

#------Histogram of Distribution of Cups per serving across manufacturers-I----
ggplot(cereals_data2, aes(x = Cups)) +
  geom_histogram(color = "white",
                 fill = "darkorange3",
                 binwidth = 0.25) +
  scale_x_continuous(breaks = seq(0,1.5,0.5)) +
  scale_y_continuous(breaks = seq(0,25,1)) +
  facet_grid(~ Manufacturer_Name) +
  labs(title = "Distribution of number of Cups per serving across Manufacturers",
       x = "No. of Cups per Serving", y = "Count")

#------Histogram of Distribution of Cups per serving across manufacturers-II----
cereals_data2 %>% 
  ggplot(aes(x = Cups, fill = Manufacturer_Name)) +
  geom_histogram() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(name = "Number of cups in one serving", expand = c(0,0),breaks = seq(0,1.5,0.1)) +
  scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 35),breaks = seq(0,30,2)) +
  labs(fill = "Manufacturer", title = "Distribution of number of Cups per Serving across different manufacturers") +
  theme_minimal()

#------Histogram of Distribution of Weight per serving across manufacturers----
ggplot(cereals_data2, aes(x = Weight)) +
  geom_histogram(color = "white",
                 fill = "darkorchid3",
                 binwidth = 0.25) +
  scale_x_continuous(breaks = seq(0,2,0.5)) +
  scale_y_continuous(breaks = seq(0,25,1)) +
  facet_grid(~ Manufacturer_Name) +
  labs(title = "Distribution of Weight per serving across different Manufacturers",
       x = "Weight per Serving (in ounces)", y = "Count")

#---------Calorie level across Manufacturers-----
ggplot(cereals_data2, 
       aes(x = Manufacturer_Name, 
           fill = Calories_cat)) + 
  geom_bar(position = "stack")  +
  labs(y = "Count", 
       fill = "Calories",
       x = "",
       title = "Calorie levels across Manufacturers") + 
  theme(axis.text.x= element_text(angle = 45, hjust = 1, size = 12),
        text = element_text(size = 14)) 

#---------Product Type across Manufacturers-----
ggplot(cereals_data2, 
       aes(x = Manufacturer_Name, 
           fill = Type)) + 
  geom_bar(position = "stack")  +
  labs(y = "Count", 
       fill = "Type",
       x = "Manufacturer",
       title = "Product type across manufacturer") + 
  theme(axis.text.x= element_text(angle = 45, hjust = 1,size = 12),
        text = element_text(size = 14)) 

#----------Rating category across Shelves----------
ggplot(cereals_data2, 
       aes(x = factor(Shelf, labels = c("Shelf-1","Shelf-2","Shelf-3")),
           fill = Rating_Cat)) + 
  geom_bar(position = "dodge")  +
  labs(y = "Count", 
       fill = "Rating",
       x = "",
       title = "Distribution of Rating levels of products across Shelf numbers"
  ) + 
  scale_y_continuous(breaks = seq(0, 25, 2)) +
  theme(axis.text.x = element_text( size = 12, face = "bold"),
        text = element_text(size = 14))

#-----Grouped kernel density plots of calorie distribution across manufacturers----
table(cereals_data2$Manufacturer)

ggplot(cereals_data2,
       aes(x = Calories,
           fill = Manufacturer_Name)) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(breaks = seq(50,180,10)) +
  labs(title = "Kernel density plot for manufacturer wise Calories distribution",
       x = "Calories",
       y = "Density")

#----Heatmap------
dev.off()
names(cereals_data2)
cereals_datahm<-cereals_data2[ ,3:11]
rownames(cereals_datahm)<-cereals_data[,1]
variable.names(cereals_datahm)
str(cereals_datahm)
head(cereals_datahm)
cereals_datahm2<-cereals_datahm[1:38, ]
superheat(cereals_datahm2,scale = TRUE, row.dendrogram = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,)

cereals_datahm3<-cereals_datahm[39:77, ]
superheat(cereals_datahm3, scale = TRUE, row.dendrogram = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,)

#===============cluster Analysis=============

#--------Select required Variables from main data-------
cereals_cluster1<- select(cereals_data2,Calories,Protein,Fat,Sugar,Rating)
View(cereals_cluster1)
cereals_cluster2<-cereals_cluster1
dim(cereals_cluster2)
summary(cereals_cluster2)
sum(is.na(cereals_cluster2))
cereals_cluster2<-na.omit(cereals_cluster2)
#-------Scale the data-----
cereals_cluster2.scaled<-scale(cereals_cluster2)
View(cereals_cluster2.scaled)
head(cereals_cluster2.scaled,3)
str(cereals_cluster2.scaled)
#---------Calculate the euclidean distance----
library(cluster)
library(factoextra)
dev.off()
dist.ecul_cereals_cluster2.scaled<-dist(cereals_cluster2.scaled,method = "euclidean")
(dist.ecul_cereals_cluster2.scaled)
head(dist.ecul_cereals_cluster2.scaled)
str(dist.ecul_cereals_cluster2.scaled)
class(dist.ecul_cereals_cluster2.scaled)
round(as.matrix(dist.ecul_cereals_cluster2.scaled)[1:3,1:3],1) #Show only three products
#-----Dissimilarity matrix---- 
#Darker color: Higher Distance ; Light Color:Lesser Distance
fviz_dist(dist.ecul_cereals_cluster2.scaled)
#----Optimum number of clusters----
dev.off()
fviz_nbclust(cereals_cluster2, kmeans ,method = 'wss')+
  geom_vline(xintercept = 4 , linetype = 5 , col="red" )
fviz_nbclust(cereals_cluster2.scaled, kmeans ,method = 'wss')+
  geom_vline(xintercept = 4, linetype = 5 , col="red" )

#-----Apply Kmeans----
set.seed(123)
km.cereal<-kmeans(cereals_cluster2.scaled,4,nstart = 20)
km.cereal
km.cereal$cluster
km.cereal$totss
km.cereal$centers
km.cereal$size
km.cereal$betweenss
aggregate(cereals_cluster1, by = list(km.cereal$cluster), mean)
km.cereal$cluster
cereals_cluster<-cbind(cereals_cluster1,cluster = km.cereal$cluster)
head(cereals_cluster)
View(cereals_cluster)
#write.csv(km.cereal,"km.cereal.csv")
#----Kmeans cluster plot---
fviz_cluster(km.cereal, data = cereals_cluster2.scaled, palette = c("#2E9FDF","#D95F02","#E7B800","#66A61E"),
             ellipse.type = "euclid",
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme())
#-----Hierarchical Clustering: (Agglomeration) Linkage Methods----
cereals_cluster3<-hclust(d =dist.ecul_cereals_cluster2.scaled, method = "ward.D2")
#-----Cluster Dendrogram--Black and White---
fviz_dend(cereals_cluster3, cex = 0.5)
#-----Cluster Dendrogram--coloured---
fviz_dend(cereals_cluster3, k=4 ,cex=0.5 , k_colors = c("#2E9FDF","#D95F02","#E7B800","#66A61E"),
          color_labels_by_k = TRUE,
          rect = TRUE)
#------Cut tree-----another way to represent cluster plot---
cereals_cluster4<- cutree(cereals_cluster3, k=4)
head(cereals_cluster4, n=4)
table(cereals_cluster4)
rownames(cereals_cluster2.scaled)[cereals_cluster4==1]

fviz_cluster(list(data = cereals_cluster2.scaled, cluster = cereals_cluster4),
             palette = c("#2E9FDF","#D95F02","#E7B800","#66A61E"),
             ellipse = TRUE,
             ellipse.type = "convex",
             repel = TRUE,
             show.clust.cent = FALSE,
             ggtheme = theme_classic())
#-----Principal component analysis-PCA Plot----
PCA_data<-cereals_cluster1

PCA_cereals <- prcomp(PCA_data[], scale. = T)
summary(PCA_cereals)

fviz_pca_var(PCA_cereals, 
             col.var = "contrib", 
             repel = T, 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Principal Component Analysis: Variable Contribution",
             legend.title = "Contribution"
)
#===============Word cloud==============

#install.packages("devtools")
#devtools::install_github("gaospecial/wordcloud2")
#install.packages("stringr")
#install.packages("stringi")
#install.packages("lettercloud")
#library("Hmisc")
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(stringr)
library(stringi)
library(NLP)
library(tm)
library(qdap)
library(qdapDictionaries)
# install.packages("rlang")
x<-readLines("cereals_wordcloud_final.txt")
nchar(x)
x<-tolower(x)
#casefold(s,upper=T)
#S<-toupper(s)
#S
x<-paste(x,collapse = " ")
x
nchar(x)
#library(tm)-text mining
#NLP-Natural language processing
stopwords()
sort(stopwords())
Top200Words
sort(Top200Words)
x<-removeWords(x,stopwords())
nchar(x)
x<-gsub(pattern = "\\W", replace = " ",x)
x
x<-gsub(pattern = "\\d", replace = " ",x)
#initiate library(qdap) ; replace_number(x) in case we donot want to delete numbers.
x
#\\b is boundary and b{1} is single character
x<-gsub(pattern = "\\b[a-z]\\b{1}", replace = " ",x)
x
t=Top200Words
x<-removeWords(x,t)
nchar(x)
x<-stripWhitespace(x)
x
nchar(x)
freq_terms(x,top = 100)
xx<-freq_terms(x,last = 30, at.least = 3, extend = TRUE)
xxx<-freq_terms(x,top = 50 , at.least =1, extend = TRUE)
wordcloud2(data = xx, figPath = "spoon.png", size =0.25,minSize =0,shuffle = FALSE,ellipticity=0.7, color = "random-dark", backgroundColor="white")
wordcloud2(data = xxx, figPath = "bbboowl.png", size =0.55,minSize =0.55,shuffle = FALSE,ellipticity=1, color = "random-dark", backgroundColor="white")

