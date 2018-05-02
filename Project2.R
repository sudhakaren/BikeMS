rm(list = ls())
TeamPart <- read.csv("/home/ubuntu/dataset/TeamParticipantAnalysis.csv", header = TRUE, sep = "," )
attach(TeamPart)
str(TeamPart)
TeamPart$CaptainAcceptEmail <- as.logical(CaptainAcceptEmail);
TeamPart$PriorYearParticipation <- as.logical(PriorYearParticipation)
TeamPart$GenderMale <- as.numeric(GenderMale)
TeamPart$GenderFemale <- as.numeric(GenderFemale)
TeamPart$RegOnline <- as.numeric(RegOnline)
TeamPart$RegOffline <- as.numeric(RegOffline)
TeamPart$PriorParticipant <- as.numeric(PriorParticipant)
TeamPart$NotPriorParticipant <- as.numeric(NotPriorParticipant)
TeamPart$ConnectionMS <- as.numeric(ConnectionMS)
TeamPart$NoConnectionMS <- as.numeric(NoConnectionMS)
TeamPart$RegActive <- as.numeric(RegActive)
TeamPart$RegInActive <- as.numeric(RegInActive)
TeamPart$TotalFromParticipants <- as.numeric(TotalFromParticipants)
TeamPart$TotalNotFromParticipants <- as.numeric(TotalNotFromParticipants)

str(TeamPart)

summary(TeamPart)

fit_Participants <- lm(TotalFromParticipants ~ CaptainAcceptEmail+PriorYearParticipation + GenderMale + GenderFemale + GenderNotSpecified + RegOnline + RegOffline + PriorParticipant + NotPriorParticipant + ConnectionMS + NoConnectionMS +  RegActive + RegInActive)
summary(fit_Participants)
fit_NonParticipants <- lm(TotalNotFromParticipants ~ CaptainAcceptEmail+PriorYearParticipation + GenderMale + GenderFemale + GenderNotSpecified + RegOnline + RegOffline + PriorParticipant + NotPriorParticipant + ConnectionMS + NoConnectionMS +  RegActive + RegInActive)
summary(fit_NonParticipants)
#install.packages("ggm" , dependencies = TRUE)
#######################
#TeamPart <- subset(TeamPart, select = -CaptainAcceptEmail)
#TeamPart <- subset(TeamPart, select = -PriorYearParticipation)
summary(TeamPart)
res <- cor(TeamPart)
round(res,2)
cor(TeamPart, use="complete.obs")
rcorr(x, type=c("pearson", "spearman"))
#install.packages("Hmisc" , dependencies = TRUE)
library("Hmisc")
res2 <- rcorr(as.matrix(TeamPart))
res2
res2$r
res2$P


# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#res2<-rcorr(as.matrix(mtcars[,1:7]))
res2 <- rcorr(as.matrix(TeamPart[,1:13]))
flattenCorrMatrix(res2$r, res2$P)

symnum(res, abbr.colnames = FALSE)



#install.packages("corrplot" , dependencies = TRUE)
library(corrplot)
{plot.new(); dev.off()}
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", p.mat = res2$P, sig.level = 0.01, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", p.mat = res2$P, sig.level = 0.01, insig = "blank")

#install.packages("PerformanceAnalytics" , dependencies = TRUE)
library("PerformanceAnalytics")
my_data <- TeamPart[, c(1,2,3,4,5,6,7,8,9,10,11,12,13)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

# Get some colors
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

###########################
#par("mar")
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
par("mar")
par(mfrow=c(4,3))
plot(TotalFromParticipants,CaptainAcceptEmail); abline(0,1,lty=2); title("$ Vs Accept email")
plot(TotalFromParticipants, PriorParticipant); abline(0,1,lty=2); title("$ Vs Prior year participation")
plot(TotalFromParticipants,GenderMale); abline(0,1,lty=2); title("$ Vs Males")
plot(TotalFromParticipants,GenderFemale); abline(0,1,lty=2); title("$ Vs Females")
plot(TotalFromParticipants, GenderNotSpecified); abline(0,1,lty=2); title("$ Vs Gender not specified")
plot(TotalFromParticipants, RegOnline); abline(0,1,lty=2); title("$ Vs RegOnline")
plot(TotalFromParticipants,RegOffline); abline(0,1,lty=2); title("$ Vs RegOffline")
plot(TotalFromParticipants,ConnectionMS); abline(0,1,lty=2); title("$ Vs Connection to MS")
plot(TotalFromParticipants,NoConnectionMS); abline(0,1,lty=2); title("$ Vs No Conn to MS")
plot(TotalFromParticipants,RegActive); abline(0,1,lty=2); title("$ Vs Active Registration")
plot(TotalFromParticipants, RegInActive); abline(0,1,lty=2); title("$ Vs InActive Registration")

#########################################
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
par("mar")
par(mfrow=c(4,3))
plot(TotalNotFromParticipants,CaptainAcceptEmail); abline(0,1,lty=2); title("$ Vs Accept email")
plot(TotalNotFromParticipants, PriorParticipant); abline(0,1,lty=2); title("$ Vs Prior year participation")
plot(TotalNotFromParticipants,GenderMale); abline(0,1,lty=2); title("$ Vs Males")
plot(TotalNotFromParticipants,GenderFemale); abline(0,1,lty=2); title("$ Vs Females")
plot(TotalNotFromParticipants, GenderNotSpecified); abline(0,1,lty=2); title("$ Vs Gender not specified")
plot(TotalNotFromParticipants, RegOnline); abline(0,1,lty=2); title("$ Vs RegOnline")
plot(TotalNotFromParticipants,RegOffline); abline(0,1,lty=2); title("$ Vs RegOffline")
plot(TotalNotFromParticipants,ConnectionMS); abline(0,1,lty=2); title("$ Vs Connection to MS")
plot(TotalNotFromParticipants,NoConnectionMS); abline(0,1,lty=2); title("$ Vs No Conn to MS")
plot(TotalNotFromParticipants,RegActive); abline(0,1,lty=2); title("$ Vs Active Registration")
plot(TotalNotFromParticipants, RegInActive); abline(0,1,lty=2); title("$ Vs InActive Registration")
#########################################



#######################
library(ggm)
cor(GenderMale, TotalFromParticipants)
plot(GenderMale, TotalFromParticipants)
plot(GenderFemale, TotalFromParticipants)
