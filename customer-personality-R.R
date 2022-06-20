# 1. Data cleansing and exploration------------------------
library(dplyr)
library(Hmisc)
library(ggplot2)
library(ggfortify)

data = read.csv("customer-personality.csv")

# get age
data$Age = 2022-data$Year_Birth
data = dplyr::select(data, -Year_Birth)

# dt_customer
data$Dt_Customer = as.numeric(as.Date(data$Dt_Customer, format = '%d-%m-%Y'))
dt_customer_max = max(data$Dt_Customer)
data$Dt_customer_scaled = (dt_customer_max - data$Dt_Customer)/dt_customer_max
data = dplyr::select(data, -Dt_Customer)

# drop NA
data = na.omit(data)

# see distribution of all continuous variables
df1 = dplyr::select(data, -c('Education', 'Marital_Status', 'Kidhome', 'Teenhome'))
par(mfrow = c(1, 1))
hist.data.frame(df1)
# outliers in income, age, MntMeatProducts?

# deeper look at outliers:
# plot boxplot for age
par(mfrow = c(3, 1))
boxplot(data$Age,
        xlab = "age",
        ylab = "",
        horizontal = TRUE,
        notch = TRUE
)
# plot boxplot for income
boxplot(data$Income,
        xlab = "Income",
        ylab = "",
        horizontal = TRUE,
        notch = TRUE
)
# plot boxplot for MntMeatProducts
boxplot(data$MntMeatProducts,
        xlab = "MntMeatProducts",
        ylab = "",
        horizontal = TRUE,
        notch = TRUE
)
# remove rows with Year-Birth == 1893, 1899, 1900
data = data[data$Age < 100,]
# remove income >180000
data = data[data$Income < 180000,]
# remove MntMeatProducts >180000
data = data[data$MntMeatProducts < 1000,]

# tidy Marital Status col: group "YOLO", "Absurd" to Others; "Alone" to Single
data[data$Marital_Status == "YOLO","Marital_Status"] = "Single"
data[data$Marital_Status == "Absurd","Marital_Status"] = "Single"
data[data$Marital_Status == "Alone","Marital_Status"] = "Single"
data[data$Marital_Status == "Divorced","Marital_Status"] = "Single"
data[data$Marital_Status == "Widow","Marital_Status"] = "Single"
data[data$Marital_Status == "Married","Marital_Status"] = "Relationship"
data[data$Marital_Status == "Together","Marital_Status"] = "Relationship"

# create children dummy to represent kidhome/teenhome
data['Child']=data$Kidhome+data$Teenhome
data[data$Child == 0,"Child"] = "WithoutKids"
data[data$Child == 1,"Child"] = "WithKids"
data[data$Child == 2,"Child"] = "WithKids"
data[data$Child == 3,"Child"] = "WithKids"
data = dplyr::select(data, -c("Teenhome", "Kidhome"))

# total spent
data['total_spent']=data$MntMeatProducts+data$MntFishProducts+data$MntWines+
        data$MntFruits+data$MntSweetProducts+data$MntGoldProds

# save data for rfm
data_rfm = data
# remove ID column
data = dplyr::select(data, -ID)
# save data frame for running factor analysis
factor_data = dplyr::select(data, -total_spent)

# save data for cluster features
data_nondummy = data
data_nondummy[data_nondummy$Marital_Status == "Single","Marital_Status"] = 1
data_nondummy[data_nondummy$Marital_Status == "Relationship","Marital_Status"] = 0
data_nondummy$Marital_Status<- strtoi(data_nondummy$Marital_Status)

data_nondummy[data_nondummy$Education == "Basic","Education"] = 0
data_nondummy[data_nondummy$Education == "2n Cycle","Education"] = 1
data_nondummy[data_nondummy$Education == "Graduation","Education"] = 2
data_nondummy[data_nondummy$Education == "Master","Education"] = 3
data_nondummy[data_nondummy$Education == "PhD","Education"] = 4
data_nondummy$Education<- strtoi(data_nondummy$Education)

data_nondummy[data_nondummy$Child == "WithKids","Child"] = 1
data_nondummy[data_nondummy$Child == "WithoutKids","Child"] = 0
data_nondummy$Child<- strtoi(data_nondummy$Child)


# convert columns to dummies
library(fastDummies)
data <- dummy_cols(data, select_columns = c('Marital_Status', 'Education','Child'))
data = dplyr::select(data, -c("Marital_Status", "Education",'Child'))

# plot correlation
library(corrplot)
par(mfrow = c(1, 1))
corrplot(cor(data), tl.col = 'black', tl.cex = 0.7, order = 'FPC')

# 2. RFM ----------------------------
library(gridExtra)
# build rfm data, extract recency, frequency and monetary
data_rfm = data_rfm %>% group_by(ID) %>% 
        summarise(Recency = Recency, Frequency = 
                          (NumWebPurchases+NumCatalogPurchases+ 
                                   NumStorePurchases+NumDealsPurchases), 
                  Monetary = total_spent)
# plot the rfm for customers
r = ggplot(data_rfm) +geom_density(aes(x= Recency))
f = ggplot(data_rfm) +geom_density(aes(x = Frequency))
m = ggplot(data_rfm) +geom_density(aes(x = Monetary))
grid.arrange(r, f, m, nrow = 3)

#calculate rfm score
summary(data_rfm)
# assign socre based on quartiles
data_rfm$R_score = 0
data_rfm$R_score[data_rfm$Recency >= 74] = 1
data_rfm$R_score[data_rfm$Recency >= 49 & data_rfm$Recency <74] = 2
data_rfm$R_score[data_rfm$Recency >= 24 & data_rfm$Recency <49] = 3
data_rfm$R_score[data_rfm$Recency < 24] = 4
data_rfm$F_score = 0
data_rfm$F_score[data_rfm$Frequency >=21] = 4
data_rfm$F_score[data_rfm$Frequency <21 & data_rfm$Frequency >= 15] = 3
data_rfm$F_score[data_rfm$Frequency <15 & data_rfm$Frequency >= 8] = 2
data_rfm$F_score[data_rfm$Frequency <8] = 1
data_rfm$M_score = 0
data_rfm$M_score[data_rfm$Monetary >= 1044.2] = 4
data_rfm$M_score[data_rfm$Monetary < 1044.2 & data_rfm$Monetary >= 396] = 3
data_rfm$M_score[data_rfm$Monetary >= 69 & data_rfm$Monetary < 396] = 2
data_rfm$M_score[data_rfm$Monetary <69] = 1
# get rfm score for customers
data_rfm = data_rfm %>% mutate(RFM_score = 100 *R_score +10 * F_score + M_score)
# segmentation
data_rfm$Segment = "0"
data_rfm$Segment[which(data_rfm$RFM_score %in% c(444,434,443, 344, 442, 244, 424, 441  ))] ="Loyalists"
data_rfm$Segment[which(data_rfm$RFM_score %in% c(332,333,342, 343, 334, 412,413,414,431,432,441,421,422,423, 424, 433 ))] = "Potential Loyalists"
data_rfm$Segment[which(data_rfm$RFM_score %in% c(233,234, 241,311, 312, 313,314,321,322,323,324, 331,  341))] = "Promising"
data_rfm$Segment[which(data_rfm$RFM_score %in% c(124, 133, 134, 142, 143, 144, 214,224,234, 242, 243, 232 ))] = "Hesitant"
data_rfm$Segment[which(data_rfm$RFM_score %in% c(122, 123,131 ,132, 141, 212, 213, 221, 222, 223, 231 ))] = "Need attention"
data_rfm$Segment[which(data_rfm$RFM_score %in% c(111, 112, 113, 114, 121, 131, 211, 311, 411 ))] ="Detractors"

data_rfm$Segment_num = 0
data_rfm[data_rfm$Segment == "Detractors","Segment_num"] = 1
data_rfm[data_rfm$Segment == "Need attention","Segment_num"] = 2
data_rfm[data_rfm$Segment == "Hesitant","Segment_num"] = 3
data_rfm[data_rfm$Segment == "Promising","Segment_num"] = 4
data_rfm[data_rfm$Segment == "Potential Loyalists","Segment_num"] = 5
data_rfm[data_rfm$Segment == "Loyalists","Segment_num"] = 6
data_rfm$Segment_num<- strtoi(data_rfm$Segment_num)


# visualisation
table(data_rfm$Segment)
ggplot(data_rfm) + geom_bar(aes(x = Segment, fill = Segment))+theme(axis.text.x=element_text(angle=90,hjust=1)) +
        labs(title = "Barplot for Segments of customers")
x = table(data_rfm$Segment)
piepercent<- round(100*x/sum(x), 1)
lbls = paste(names(x), " ", piepercent,"%")
pie(x, labels = lbls,
    main="Pie chart for Customer Segments")



# 3. Factor Analysis ----------------------------
factor_data[factor_data$Marital_Status == "Single","Marital_Status"] = 1
factor_data[factor_data$Marital_Status == "Relationship","Marital_Status"] = 0
factor_data$Marital_Status<- strtoi(factor_data$Marital_Status)

factor_data[factor_data$Education == "Basic","Education"] = 0
factor_data[factor_data$Education == "2n Cycle","Education"] = 1
factor_data[factor_data$Education == "Graduation","Education"] = 2
factor_data[factor_data$Education == "Master","Education"] = 3
factor_data[factor_data$Education == "PhD","Education"] = 4
factor_data$Education<- strtoi(factor_data$Education)

factor_data[factor_data$Child == 'WithKids',"Child"] = 1
factor_data[factor_data$Child == 'WithoutKids',"Child"] = 0
factor_data$Child<- strtoi(factor_data$Child)

factor_data <- factor_data[c(-19)]
# scale factor data
factor_data = scale(factor_data)

# determine the no. of factors
fa.cor = cor(factor_data)
fa.eigen = eigen(fa.cor)
fa.eigen$values
# use the scree plot 
plot(fa.eigen$values, type = "b", ylab = "Eigenvalues", xlab = "Factor")
### factor analysis
#install.packages('psych')
library(psych)
### factor rotation
fa.res = factanal(x = factor_data, factors = 4, rotation = "promax")
print(fa.res, cut = 0.2)

# 4. PCA and k-means clustering ----------------------------
# scale the data
data_pca = data
data_pca = select(data_pca, -total_spent)
for (i in 1:15){
        data_pca[i] = scale(data_pca[i],center = TRUE, scale=TRUE)
}
# PCA
library(cluster)
library(factoextra)
library(tidyverse)
pr.out = prcomp(data_pca)
biplot(pr.out, scale = 0,cex=0.5)
pr.var = pr.out$sdev ^2
pve = pr.var/sum(pr.var)
defpar = par()
par(mfrow=c(1,1))
plot(pve, xlab = " Principal Component", ylab = "Proportion of
Variance Explained", ylim = c(0,1), type = "b")
# save the PCA data for kmeans clustering
data_transform = as.data.frame(-pr.out$x[,1:6])


# kmeans
set.seed(12939)
seg.k<-kmeans(data_transform, centers = 3)
aggregate(data_transform, list(seg.k$cluster), mean)
fviz_nbclust(data_transform, kmeans, method = "wss")
fviz_cluster(seg.k, data = data_transform)
par(defpar)
table(seg.k$cluster)

# plot all three scree graphs
par(mfrow=c(1,3))
plot(fa.eigen$values, type = "b", ylab = "Eigenvalues", xlab = "Factor")
plot(pve, xlab = " Principal Component", ylab = "Proportion of
Variance Explained", ylim = c(0,1), type = "b")
wss = function(k) {
    kmeans(data_transform, k, nstart = 10 )$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values = 1:10
# extract wss for 2-10 clusters
wss_values = map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b",
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# visualize PCA based on clusters
autoplot(pr.out, data = data_pca,colour = (seg.k$cluster)+1,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, label.color='blue')

# visualize clusters
par(mfrow=c(3,4))
boxplot(data$Income~seg.k$cluster, ylab = "Income",
        xlab = "Cluster",
        horizontal = FALSE,
        notch = TRUE)
boxplot(data$Age~seg.k$cluster, ylab = "Age",
        xlab = "Cluster",
        horizontal = FALSE,
        notch = TRUE)
boxplot(data$total_spent~seg.k$cluster, ylab = "Total Spent",
        xlab = "Cluster",
        horizontal = FALSE,
        notch = TRUE)
boxplot(data_nondummy$Marital_Status~seg.k$cluster, ylab = "Marital",
        xlab = "Cluster",
        horizontal = FALSE,
        notch = TRUE)
boxplot(data_nondummy$Education~seg.k$cluster, ylab = "Education",
        xlab = "Cluster",
        horizontal = FALSE,
        notch = TRUE)
boxplot(data_nondummy$Child~seg.k$cluster, ylab = "Child",
        xlab = "Cluster",
        horizontal = FALSE,
        notch = TRUE)
boxplot(data$Dt_customer_scaled~seg.k$cluster, ylab = "Dt_customer_scaled",
        xlab = "Cluster",
        horizontal = FALSE,
        notch = TRUE)
boxplot(data$NumWebPurchases~seg.k$cluster, ylab = "NumWebPurchases",
        xlab = "Cluster",
        horizontal = FALSE,
        notch = TRUE)
boxplot(data$NumCatalogPurchases~seg.k$cluster, ylab = "NumCatalogPurchases",
        xlab = "Cluster",
        horizontal = FALSE,
        notch = TRUE)
boxplot(data$NumStorePurchases~seg.k$cluster, ylab = "NumStorePurchases",
        xlab = "Cluster",
        horizontal = FALSE,
        notch = TRUE)
boxplot(data$NumWebVisitsMonth~seg.k$cluster, ylab = "NumWebVisitsMonth",
        xlab = "Cluster",
        horizontal = FALSE,
        notch = TRUE)
boxplot(data$NumDealsPurchases~seg.k$cluster, ylab = "NumDealsPurchases",
        xlab = "Cluster",
        horizontal = FALSE,
        notch = TRUE)
# analysing RFM for different clusters
data_rfm$cluster = seg.k$cluster
rfm_1 = filter(data_rfm, cluster == 1)
rfm_2 = filter(data_rfm, cluster == 2)
rfm_3 = filter(data_rfm, cluster == 3)
a = table(rfm_1$Segment)
b = table(rfm_2$Segment)
c = table(rfm_3$Segment)# visualisation
par(mfrow=c(1,3))
piepercent<- round(100*a/sum(a), 1)
lbls = paste(names(a), " ", piepercent,"%")
pie(a, labels = lbls)
title("Cluster 1", line = -8)
piepercent<- round(100*b/sum(b), 1)
lbls = paste(names(b), " ", piepercent,"%")
pie(b, labels = lbls)
title("Cluster 2", line = -8)
piepercent<- round(100*c/sum(c), 1)
lbls = paste(names(c), " ", piepercent,"%")
pie(c, labels = lbls)
title("Cluster 3", line = -8)

