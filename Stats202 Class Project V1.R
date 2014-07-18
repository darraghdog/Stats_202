# Load the packages relevants
library('RColorBrewer')
library('lattice')
library('gridExtra')
library('fBasics')
library('ggplot2')
library('reshape2')
library('caret')
library('e1071')
library('rpart')


# Set working directory and load the data
setwd("C:/Users/dahanley/Desktop")
train <-read.csv("training.csv",header=TRUE)
test <-read.csv("test.csv",header=TRUE)
# Check the numbers of rows and the names of the columns
nrow(train)
names(train)
# number of unique columns
length(unique(train$url_id))
length(unique(train$query_id))
nrow(train)/length(unique(train$url_id))
nrow(train)/length(unique(train$query_id))
# Change certain columns to factors - query_id, url_id and is_homepage
lapply(train,class)
train[,c("query_id")] <- as.factor(train[,c("query_id")])
train[,c("url_id")] <- as.factor(train[,c("url_id")])
train[,c("is_homepage")] <- as.factor(train[,c("is_homepage")])
# train[,c("relevance")] <- as.factor(train[,c("relevance")])
test[,c("query_id")] <- as.factor(test[,c("query_id")])
test[,c("url_id")] <- as.factor(test[,c("url_id")])
test[,c("is_homepage")] <- as.factor(test[,c("is_homepage")])
# test[,c("relevance")] <- as.factor(test[,c("relevance")])

# Check the duplicated records and the number of n/a's
sum(duplicated(train))
sum(duplicated(test))
sapply(train, function(x)all(is.na(x)))
sapply(test, function(x)all(is.na(x)))
# Create summery statistics - exclude the factors
basicstats <- basicStats(train[c(-1,-2,-4,-13)])[c(2:8,13:14),]
write.csv(basicstats, file = "basic_stats.csv")

# Histogram of the normall distributed variables
unbiased_signals <- melt(train[,c("sig1", "sig2", "sig7", "sig8")])
p1 <- ggplot(unbiased_signals,aes(x = value)) +      facet_wrap(~variable,scales = "free_x",nrow=1) + geom_histogram() + ggtitle("Normally Distributed Attributes") + theme_bw()
# Histogram of the evenly distributed variables
biased_signals <- melt(train[,c("sig3", "sig4", "sig5", "sig6")])
p2 <- ggplot(biased_signals ,aes(x = value)) +      facet_wrap(~variable,scales = "free_x", nrow=1) + geom_histogram() + ggtitle("Attributes showing right skew") + theme_bw()
train_temp <- train[,c(3:13)]
train1 <- train_temp
train1[,c("sig3", "sig4", "sig5", "sig6")] [train1[,c("sig3", "sig4", "sig5", "sig6")]  ==0 ] <- .001
train1[,c("sig3", "sig4", "sig5", "sig6")] <- log(train1[,c("sig3", "sig4", "sig5", "sig6")])
names(train1) <- c("query_length", "is_homepage", "sig1" , "sig2" , "log(sig3)", "log(sig4)", "log(sig5)", "log(sig6)", "sig7", "sig8", "relevance")
logged_signals <- melt(train1[,c("log(sig3)", "log(sig4)", "log(sig5)", "log(sig6)")])
p3 <- ggplot(logged_signals ,aes(x = value)) +      facet_wrap(~variable,scales = "free_x", nrow=1) + geom_histogram() + ggtitle("Log of attributes showing right skew ") + theme_bw()
png(file = "log_histogram1.png", bg = "transparent", width = 720, height = 960)
grid.arrange(p1,p2,p3)
dev.off()

# get the correlation coefficient between variables.
rgb.palette <- colorRampPalette(brewer.pal(9,"BrBG"))
train_temp <- train[,c(3:13)]
cor_matrix <- cor(train_temp[sapply(train_temp, is.numeric)])
cor_matrix  
# get the correlation coefficient after logging the variables which contain strong right bias.
train_temp <- train[,c(3:13)]
train1 <- train_temp
train1[,c("sig3", "sig4", "sig5", "sig6")] [train1[,c("sig3", "sig4", "sig5", "sig6")]  ==0 ] <- .001
train1[,c("sig3", "sig4", "sig5", "sig6")] <- log(train1[,c("sig3", "sig4", "sig5", "sig6")])
names(train1) <- c("query_length", "is_homepage", "sig1" , "sig2" , "log(sig3)", "log(sig4)", "log(sig5)", "log(sig6)", "sig7", "sig8", "relevance")
cor_matrix_log <- cor(train1[sapply(train1, is.numeric)])
cor_matrix_log
# Plot the two correlation coefficients.
plot_no_log <- levelplot(round(cor_matrix,3), main="Numeric Attribute Correlation Plot", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(-1,1,0.02),  scales=list(x=list(rot=45, cex=1),y=list(cex=1)))
plot_log <- levelplot(round(cor_matrix_log,3), main="Numeric Attribute Correlation Plot with Logging", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(-1,1,0.02),  scales=list(x=list(rot=45, cex=1), y=list(cex=1)))
tiff(file = "correlation_matrix.tif", bg = "transparent", width = 960, height = 480, compression = "lzw")
grid.arrange(plot_no_log,plot_log, ncol=2)
dev.off()

## mosaic plot for is_homepage and query_length
train_temp <- train
train_temp$relevance1[train$relevance==1] <- "Yes"
train_temp$relevance1[train$relevance==0] <- "No"
train_temp$is_homepage1[train$is_homepage==1] <- "Yes"
train_temp$is_homepage1[train$is_homepage==0] <- "No"

mosaicplot(is_homepage1 ~ relevance1, data = train_temp, color = TRUE, las=1, main="Relevant URLs proportional to \n Queries from Homepages ", xlab="Query from Homepage", ylab="Relevant URL")
mosaic_lth <- mosaicplot(query_length ~ relevance1, data = train_temp, color = TRUE, las=1, main="Relevant URLs proportional to \n Query length", xlab="Query length", ylab="Relevant URL")
tiff(file = "moasic.tif", bg = "transparent", width = 960, height = 480, compression = "lzw")
par( mfrow = c( 1, 2 ) )
mosaicplot(is_homepage ~ relevance, data = train_temp, las=1, main="Relevant URLs proportional to \n Queries from Homepages ", xlab="Query from Homepage", ylab="Relevant URL", cex.axis = 1)
# abline(h=.437086, col = "red", lty = "dotted")
# text(.4,.46, "Total Proportion of Relevant URLs", col = "red")
mosaicplot(query_length ~ relevance, data = train_temp, las=1, main="Relevant URLs proportional to \n Query length", xlab="Query length", ylab="Relevant URL", cex.axis = 1)
# abline(h=.437086, col = "red", lty = "dotted")
# text(.4,.46, "Total Proportion of Relevant URLs", col = "red")
dev.off()

## Output the aggregated max, min, mean, median per query to a correlation matrix
rgb.palette <- colorRampPalette(brewer.pal(9,"BrBG"))

querymax <- aggregate(. ~ query_id, max, data=train)
trainmax <- merge(train[,c(1,13)], querymax[,c(1:12)], by.x="query_id", by.y="query_id", all=TRUE)
cor_matrix_max <- cor(trainmax[sapply(trainmax[4:13], is.numeric)], is.numeric(trainmax$relevance))

querymin <- aggregate(. ~ query_id, min, data=train)
trainmin <- merge(train[,c(1,13)], querymin[,c(1:12)], by.x="query_id", by.y="query_id", all=TRUE)
cor_matrix_min <- cor(trainmin[sapply(trainmin, is.numeric)], trainmin$relevance)

querymedian <- aggregate(. ~ query_id, median, data=train)
trainmedian <- merge(train[,c(1,13)], querymedian[,c(1:12)], by.x="query_id", by.y="query_id", all=TRUE)
cor_matrix_median <- cor(trainmedian[sapply(trainmedian, is.numeric)], trainmedian$relevance)

querymean <- aggregate(. ~ query_id, mean, data=train)
trainmean <- merge(train[,c(1,13)], querymean[,c(1:12)], by.x="query_id", by.y="query_id", all=TRUE)
cor_matrix_mean <- cor(trainmean[sapply(trainmean, is.numeric)], trainmean$relevance)

cor_matrix_original <- cor(train[sapply(train, is.numeric)], train$relevance)

cor_matrix_qu <- cbind(cor_matrix_original[c(1:12),], cor_matrix_max[c(1,3:13),], cor_matrix_min[c(1,3:13),], cor_matrix_median[c(1,3:13),], cor_matrix_mean[c(1,3:13),])
colnames(cor_matrix_qu) <- c("original value", "query max","query min", "query median", "query mean")

tiff(file = "aggregate_by_query.tif", bg = "transparent", width = 960, height = 480, compression = "lzw")
levelplot(round(cor_matrix_qu,3), main="Relevance Correlation Plot for Different Attribute Transformations", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(-1,1,0.02),  scales=list(x=list(rot=45, cex=1),y=list(cex=1)))
dev.off()
write.csv(as.data.frame(cor_matrix_qu), file="query_aggregated_correlation.csv", scientific=FALSE)



## probabibility that homepage and relevancy is just to chance
table(train$relevance, train$is_homepage)
pbinom(10471,size=(11057+10471),p=.4370862)
pbinom(24516,size=(24516+34002),p=.4370862)

binom.test(10471,21528,p=.4370862, alternative="two.sided") 
## https://class.coursera.org/statistics-001/lecture

## Create the table to be used in models with,
lapply(train,class)
train2 <- train 
## 1) Change is_homepage to a factor
train2[,c("is_homepage")] <- as.factor(train2[,c("is_homepage")])
## 2) Log the sig 3 though sig6
train2[,c("sig3", "sig4", "sig5", "sig6")] [train2[,c("sig3", "sig4", "sig5", "sig6")]  ==0 ] <- .001
train2[,c("sig3", "sig4", "sig5", "sig6")] <- log(train2[,c("sig3", "sig4", "sig5", "sig6")])
colnames(train2)[c(7:10)] <- c( "log_sig3", "log_sig4", "log_sig5", "log_sig6")
## 3) Aggregate sig8 over query_id and get the minimum value; and remove the original sig8 value.
querymin <- aggregate(sig8 ~ query_id, min, data=train)
colnames(querymin)[2] <- c("min_sig8")
train2 <- merge(train2, querymin, by.x="query_id", by.y="query_id", all=TRUE)
train2[,c("sig8")] <- NULL
train2 <- train2[,c(1:11,13,12)]
## 4) get summary statitics
basicstats2 <- basicStats(train2[c(-1,-2,-4)])[c(2:8,13:14),]
write.csv(basicstats2, file = "basic_stats_transformed.csv")


# Divide the training data are divided into five folds and cross validation is performed.
library(caret)
set.seed(32343)
folds <- createFolds(y=train2$relevance[], k=5, list=TRUE, returnTrain=TRUE)
head(train2[folds[[1]],],n=10)
head(train2[folds[[2]],],n=10)
head(train2[-folds[[1]],],n=10)

# Build up the Decision tree
accuracy = as.data.frame(matrix(ncol=3, nrow=40))
names(accuracy) <- c("Mis", "Branches", "Split_Criterion")
Parms_list <- list("information","gini")
fold_result <- vector()
count=1
# run the recursive partition for different levels of branch depths; for different measurements to select splits
for(i in 1:12){
	for(j in 1:2){
		for (k in 1:5) {
			class_train <- as.factor(train2[folds[[k]],ncol(train2)])
			class_test <- as.factor(train2[-folds[[k]],ncol(train2)])
			fit<-rpart(class_train ~.,train2[folds[[k]],3:12], control=rpart.control(minsplit=0,minbucket=0,cp=-1,maxcompete=0, maxsurrogate=0, usesurrogate=0, xval=0,maxdepth=i), parms = list(split = Parms_list[[j]]),  method='class')
			fold_result[k] <- sum(class_test!=predict(fit,train2[-folds[[k]],3:12],type="class"))/ length(class_test)
			}
		accuracy1 <- mean(fold_result)
		accuracy[count,1] <- accuracy1
		accuracy[count,2] <- i
		accuracy[count,3] <- Parms_list[[j]]
		count=count+1
	}
	}
accuracy$Split_Criterion <- factor(accuracy$Split_Criterion, levels=c("information","gini"), labels=c("Entropy","Gini"))

# Print out graphe of Decision tree results
tiff(file = "rpart_misclassification_error.tif", bg = "transparent", width = 720, height = 240, compression = "lzw")
ggplot(accuracy, aes(x=Branches, y=Mis, colour=Split_Criterion)) + geom_line(aes(group=Split_Criterion)) + geom_point(size=3, aes(shape=Split_Criterion)) + scale_x_continuous(breaks=1:12) + xlab("Number of Branches") +
  ylab("Misclassification Error") + ggtitle("Decision tree misclassification error") + labs(shape="Split Criterion", colour="Split Criterion")
dev.off()

# Build up the SVM
