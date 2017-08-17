rm(list = ls())

library(MASS)
library(stargazer)
library(scales)
library(locfit)
library(caret)
library(rpart)

# Open data
load("NIDS.hhdata.small.rda")
sample = hhdata.small$members <= 15 & hhdata.small$rooms <= 15
hhdata.small = hhdata.small[sample,]
for (i in 2:3) {hhdata.small[which(hhdata.small[,i]<0),i]=NA}
hhdata.small$income=as.numeric(hhdata.small$income)
hhdata.small$members=as.integer(hhdata.small$members)
hhdata.small$rooms=as.integer(hhdata.small$rooms)
lm.fit = lm(income~.,data=hhdata.small)
sample.omit = lm.fit$na.action
hhdata.small = hhdata.small[-sample.omit,]
attach(hhdata.small)
maxvars = 10

# Data partition
set.seed(7)
inTrain = createDataPartition(y=income,p=0.75,list=F)
training= hhdata.small[inTrain,]
testing= hhdata.small[-inTrain,]
formula = income~.

# Define multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# 2d line plots
f2=kde2d(hhdata.small$members, hhdata.small$rooms, h=4,n = 150, lims = c(range(hhdata.small$members), range(hhdata.small$rooms)))
f2$z = max(f2$z)-f2$z
image(f2, col=heat.colors(30),xlab="Members",ylab="Rooms")

# 1d line plots
regVar <- c("members", "rooms")
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = hhdata.small[, regVar], 
            y = hhdata.small$income, 
            plot = "scatter", 
            type = c("p", "smooth"),
            span = .5,
            layout = c(2, 1))

# Linear model
lm.fit1 = lm(income~rooms+members,data=hhdata.small)
summary(lm.fit1)
sqrt(mean((income-predict(lm.fit1,hhdata.small))^2))

table1 = stargazer(lm.fit1, title="Regression Results",header=FALSE, type='html', align=TRUE, 
          covariate.labels=c("Number of rooms","Number of household members"),
          dep.var.labels=c("Log per capita household income"), omit.stat=c("LL","ser","f"), no.space=TRUE)

my.colors = colorRampPalette(c("#a50026", "red","orange","yellow","white","#4575b4", "#313695","black"))
grid.graph=data.frame(members=rep(seq(1,max(hhdata.small$members),0.1),each=((max(hhdata.small$rooms)-1)*10+1)),rooms=rep(seq(1,max(hhdata.small$rooms),0.1),((max(hhdata.small$members)-1)*10+1)))
h <-ggplot(grid.graph,aes(members, rooms))

ols1.hat.grid <- predict(lm.fit1, grid.graph)

v = rescale(c(2,5.5,6,6.5,7,8,9,12))
l = c(2, 12)
ols.plot1 = h + geom_point(aes(color = ols1.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l)  + ggtitle("OLS")

#Nearest neighbour regression
grid.graph1=data.frame(members=rep(seq(1,max(hhdata.small$members),0.025),each=((max(hhdata.small$rooms)-1)*40+1)),rooms=rep(seq(1,max(hhdata.small$rooms),0.025),((max(hhdata.small$members)-1)*40+1)))
h1 <-ggplot(grid.graph1,aes(members, rooms))
grid.graph1 = grid.graph1 + matrix(rnorm(dim.data.frame(grid.graph1)[1]*2,mean = 0, sd = 0.00000001), ncol=2)

knn1.fit=knnreg(hhdata.small[,2:3],hhdata.small[,1],test=NULL , k = 100)
knn1.hat.grid = predict(knn1.fit,grid.graph1)
knn2.fit=knnreg(hhdata.small[,2:3],hhdata.small[,1],test=NULL , k = 20)
knn2.hat.grid = predict(knn2.fit,grid.graph1)
knn3.fit=knnreg(hhdata.small[,2:3],hhdata.small[,1],test=NULL , k = 5)
knn3.hat.grid = predict(knn3.fit,grid.graph1)
knn4.fit=knnreg(hhdata.small[,2:3],hhdata.small[,1],test=NULL , k = 1)
knn4.hat.grid = predict(knn4.fit,grid.graph1)

v = rescale(c(2,5.5,6,6.5,7,8,9,12))
l = c(2, 12)
knn.fig4 = h1 + geom_jitter(aes(color = knn1.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l)  + ggtitle("100 Nearest Neighbours")
knn.fig3 = h1 + geom_jitter(aes(color = knn2.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("20 Nearest Neighbours")
knn.fig2 = h1 + geom_jitter(aes(color = knn3.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("5 Nearest Neighbours")
knn.fig1 = h1 + geom_jitter(aes(color = knn4.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("1 Nearest Neighbour")

multiplot(knn.fig1, knn.fig3, knn.fig2, knn.fig4, cols=2)


loclin1.fit = locfit(income~members+rooms,alpha=c(0,1.1),kern="epan",deg=1)
loclin1.hat.grid = predict(loclin1.fit,grid.graph)
loclin2.fit = locfit(income~members+rooms,alpha=c(0,3),kern="epan",deg=1)
loclin2.hat.grid = predict(loclin2.fit,grid.graph)
loclin3.fit = locfit(income~members+rooms,alpha=c(0,5),kern="epan",deg=1)
loclin3.hat.grid = predict(loclin3.fit,grid.graph)
loclin4.fit = locfit(income~members+rooms,alpha=c(0,15),kern="epan",deg=1)
loclin4.hat.grid = predict(loclin4.fit,grid.graph)

v = rescale(c(2,5.5,6,6.5,7,8,9,12))
l = c(2, 12)
loclin.fig1 = h + geom_point(aes(color = loclin1.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l)  + ggtitle("Bandwidth = 1.1")
loclin.fig2 = h + geom_point(aes(color = loclin2.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("Bandwidth = 3")
loclin.fig3 = h + geom_point(aes(color = loclin3.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("Bandwidth = 5")
loclin.fig4 = h + geom_point(aes(color = loclin4.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("Bandwidth = 15")

multiplot(loclin.fig1, loclin.fig3, loclin.fig2, loclin.fig4, cols=2)


#  Effect of the bandwidth on sampling variability
set.seed(1234567890)
#sample1 = sample(1:length(hhdata$income),length(hhdata$income)/2,replace=T)
#sample2 = - sample1
#sample3 = sample(1:length(hhdata$income),length(hhdata$income)/2,replace=T)
#sample4 = - sample2
sample1 = sample(c(1:length(hhdata.small$income)),length(hhdata.small$income)/4)
sample2 = sample(c(1:length(hhdata.small$income))[-sample1],length(hhdata.small$income)/4)
sample3 = sample(c(1:length(hhdata.small$income))[-c(sample1,sample2)],length(hhdata.small$income)/4)
sample4 = c(1:length(hhdata.small$income))[-c(sample1,sample2,sample3)]

knn1.fit1 = locfit(income~members+rooms,alpha=c(0,1.1),kern="epan",deg=1,data=hhdata.small[sample1,])
knn1.hat.grid1 = predict(knn1.fit1,grid.graph)
knn1.fit2 = locfit(income~members+rooms,alpha=c(0,1.1),kern="epan",deg=1,data=hhdata.small[sample2,])
knn1.hat.grid2 = predict(knn1.fit2,grid.graph)
knn1.fit3 = locfit(income~members+rooms,alpha=c(0,1.1),kern="epan",deg=1,data=hhdata.small[sample3,])
knn1.hat.grid3 = predict(knn1.fit3,grid.graph)
knn1.fit4 = locfit(income~members+rooms,alpha=c(0,1.1),kern="epan",deg=1,data=hhdata.small[sample4,])
knn1.hat.grid4 = predict(knn1.fit4,grid.graph)

v = rescale(c(2,5.5,6,6.5,7,8,9,12))
l = c(2, 12)
fig11 = h + geom_point(aes(color = knn1.hat.grid1)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
fig12 = h + geom_point(aes(color = knn1.hat.grid2)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
fig13 = h + geom_point(aes(color = knn1.hat.grid3)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
fig14 = h + geom_point(aes(color = knn1.hat.grid4)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

knn2.fit1 = locfit(income~members+rooms,alpha=c(0,3),kern="epan",deg=1,data=hhdata.small[sample1,])
knn2.hat.grid1 = predict(knn2.fit1,grid.graph)
knn2.fit2 = locfit(income~members+rooms,alpha=c(0,3),kern="epan",deg=1,data=hhdata.small[sample2,])
knn2.hat.grid2 = predict(knn2.fit2,grid.graph)
knn2.fit3 = locfit(income~members+rooms,alpha=c(0,3),kern="epan",deg=1,data=hhdata.small[sample3,])
knn2.hat.grid3 = predict(knn2.fit3,grid.graph)
knn2.fit4 = locfit(income~members+rooms,alpha=c(0,3),kern="epan",deg=1,data=hhdata.small[sample4,])
knn2.hat.grid4 = predict(knn2.fit4,grid.graph)

fig21 = h + geom_point(aes(color = knn2.hat.grid1)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
fig22 = h + geom_point(aes(color = knn2.hat.grid2)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
fig23 = h + geom_point(aes(color = knn2.hat.grid3)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
fig24 = h + geom_point(aes(color = knn2.hat.grid4)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

knn3.fit1 = locfit(income~members+rooms,alpha=c(0,5),kern="epan",deg=1,data=hhdata.small[sample1,])
knn3.hat.grid1 = predict(knn3.fit1,grid.graph)
knn3.fit2 = locfit(income~members+rooms,alpha=c(0,5),kern="epan",deg=1,data=hhdata.small[sample2,])
knn3.hat.grid2 = predict(knn3.fit2,grid.graph)
knn3.fit3 = locfit(income~members+rooms,alpha=c(0,5),kern="epan",deg=1,data=hhdata.small[sample3,])
knn3.hat.grid3 = predict(knn3.fit3,grid.graph)
knn3.fit4 = locfit(income~members+rooms,alpha=c(0,5),kern="epan",deg=1,data=hhdata.small[sample4,])
knn3.hat.grid4 = predict(knn3.fit4,grid.graph)

fig31 = h + geom_point(aes(color = knn3.hat.grid1)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
fig32 = h + geom_point(aes(color = knn3.hat.grid2)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
fig33 = h + geom_point(aes(color = knn3.hat.grid3)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
fig34 = h + geom_point(aes(color = knn3.hat.grid4)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

knn4.fit1 = locfit(income~members+rooms,alpha=c(0,15),kern="epan",deg=1,data=hhdata.small[sample1,])
knn4.hat.grid1 = predict(knn4.fit1,grid.graph)
knn4.fit2 = locfit(income~members+rooms,alpha=c(0,15),kern="epan",deg=1,data=hhdata.small[sample2,])
knn4.hat.grid2 = predict(knn4.fit2,grid.graph)
knn4.fit3 = locfit(income~members+rooms,alpha=c(0,15),kern="epan",deg=1,data=hhdata.small[sample3,])
knn4.hat.grid3 = predict(knn4.fit3,grid.graph)
knn4.fit4 = locfit(income~members+rooms,alpha=c(0,15),kern="epan",deg=1,data=hhdata.small[sample4,])
knn4.hat.grid4 = predict(knn4.fit4,grid.graph)

fig41 = h + geom_point(aes(color = knn4.hat.grid1)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
fig42 = h + geom_point(aes(color = knn4.hat.grid2)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
fig43 = h + geom_point(aes(color = knn4.hat.grid3)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
fig44 = h + geom_point(aes(color = knn4.hat.grid4)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l,guide=F)  +   theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
multiplot(fig11, fig21,fig31,fig41,fig12,fig22,fig32,fig42, fig13,fig23,fig33,fig43, fig14,fig24,fig34,fig44, cols=4)


set.seed(825)
sample1 = sample(c(1:length(hhdata.small$income)),length(hhdata.small$income)/5)
sample2 = sample(c(1:length(hhdata.small$income))[-sample1],length(hhdata.small$income)/5)
sample3 = sample(c(1:length(hhdata.small$income))[-c(sample1,sample2)],length(hhdata.small$income)/5)
sample4 = sample(c(1:length(hhdata.small$income))[-c(sample1,sample2,sample3)],length(hhdata.small$income)/5)
sample5 = c(1:length(hhdata.small$income))[-c(sample1,sample2,sample3,sample4)]

cv.eval.test = function(bw) {
  RMSE.test=rep(0,5)
  knn.fit1 = locfit(income~members+rooms,alpha=c(0,bw),kern="epan",deg=1,data=hhdata.small[-sample1,])
  knn.fit2 = locfit(income~members+rooms,alpha=c(0,bw),kern="epan",deg=1,data=hhdata.small[-sample2,])
  knn.fit3 = locfit(income~members+rooms,alpha=c(0,bw),kern="epan",deg=1,data=hhdata.small[-sample3,])
  knn.fit4 = locfit(income~members+rooms,alpha=c(0,bw),kern="epan",deg=1,data=hhdata.small[-sample4,])
  knn.fit5 = locfit(income~members+rooms,alpha=c(0,bw),kern="epan",deg=1,data=hhdata.small[-sample5,])
  RMSE.test[1] = sqrt(mean((income[sample1]-predict(knn.fit1,hhdata.small[sample1,]))^2))
  RMSE.test[2] = sqrt(mean((income[sample2]-predict(knn.fit2,hhdata.small[sample2,]))^2))
  RMSE.test[3] = sqrt(mean((income[sample3]-predict(knn.fit3,hhdata.small[sample3,]))^2))
  RMSE.test[4] = sqrt(mean((income[sample4]-predict(knn.fit4,hhdata.small[sample4,]))^2))
  RMSE.test[5] = sqrt(mean((income[sample5]-predict(knn.fit5,hhdata.small[sample5,]))^2))
  mean(RMSE.test)
}

cv.eval.train = function(bw) {
  RMSE.train=rep(0,5)
  knn.fit1 = locfit(income~members+rooms,alpha=c(0,bw),kern="epan",deg=1,data=hhdata.small[sample1,])
  knn.fit2 = locfit(income~members+rooms,alpha=c(0,bw),kern="epan",deg=1,data=hhdata.small[sample2,])
  knn.fit3 = locfit(income~members+rooms,alpha=c(0,bw),kern="epan",deg=1,data=hhdata.small[sample3,])
  knn.fit4 = locfit(income~members+rooms,alpha=c(0,bw),kern="epan",deg=1,data=hhdata.small[sample4,])
  knn.fit5 = locfit(income~members+rooms,alpha=c(0,bw),kern="epan",deg=1,data=hhdata.small[sample5,])
  RMSE.train[1] = sqrt(mean((income[sample1]-predict(knn.fit1,hhdata.small[sample1,]))^2))
  RMSE.train[2] = sqrt(mean((income[sample2]-predict(knn.fit2,hhdata.small[sample2,]))^2))
  RMSE.train[3] = sqrt(mean((income[sample3]-predict(knn.fit3,hhdata.small[sample3,]))^2))
  RMSE.train[4] = sqrt(mean((income[sample4]-predict(knn.fit4,hhdata.small[sample4,]))^2))
  RMSE.train[5] = sqrt(mean((income[sample5]-predict(knn.fit5,hhdata.small[sample5,]))^2))
  mean(RMSE.train)
}

bw = c(seq(from = 1.1, to = 3, by =0.1),4:6)
cv.error.test= rep(0 ,23)
cv.error.train= rep(0 ,23)
for (i in 1:23) {
  cv.error.test[i]=cv.eval.test(bw[i])
  cv.error.train[i]=cv.eval.train(bw[i])
}

plot(c(1,6),c(0.94,1),type="n",xlab="Bandwidth",ylab="RMSE")
lines(bw,cv.error.test,col="red",lwd=2.5)
lines(bw,cv.error.train,col="blue",lwd=2.5)
legend(4,0.96,c("Test sample","Train sample"),lty=c(1,1,1,1),lwd=c(2.5,2.5),col=c("red","blue"))

knn.fit.best = locfit(income~members+rooms,alpha=c(0,2.2),kern="epan",deg=1,data=hhdata.small)
knn.best.grid = predict(knn.fit.best,grid.graph)

v = rescale(c(2,5.5,6,6.5,7,8,9,12))
l = c(2, 12)
best.knn = h + geom_point(aes(color = knn.best.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("Local linear")

#Polynomial
lm.fit1 = lm(income~rooms+members,data=hhdata.small)
#summary(lm.fit1)
ols1.hat = lm.fit1$fitted.values
lm.fit2 = lm(income~rooms*members+I(rooms^2)*I(members^2),data=hhdata.small)
#summary(lm.fit2)
ols2.hat = lm.fit2$fitted.values
lm.fit3 = lm(income~rooms*members+I(rooms^2)*I(members^2)+poly(rooms,maxvars)*poly(members,maxvars),data=hhdata.small)
ols3.hat = lm.fit3$fitted.values

stargazer(lm.fit1, lm.fit2, title="Regression Results",header=FALSE, type='html', align=TRUE, 
          covariate.labels=c("Rooms","Members","Rooms squared","Members squared","Rooms x Members","Rooms squared x Members squared"),
          dep.var.labels=c("Log per capita household income"), omit.stat=c("LL","ser","f"), no.space=TRUE)

my.colors = colorRampPalette(c("#a50026", "red","orange","yellow","white","#4575b4", "#313695","black"))
grid.graph=data.frame(members=rep(seq(1,max(hhdata.small$members),0.1),each=((max(hhdata.small$rooms)-1)*10+1)),rooms=rep(seq(1,max(hhdata.small$rooms),0.1),((max(hhdata.small$members)-1)*10+1)))
h <-ggplot(grid.graph,aes(members, rooms))

lm.fit3=lm(income~poly(rooms,5)*poly(members,5),data=hhdata.small)
ols3.hat = lm.fit3$fitted.values
lm.fit4=lm(income~poly(rooms,10)*poly(members,10),data=hhdata.small)
ols4.hat = lm.fit3$fitted.values

ols1.hat.grid <- predict(lm.fit1, grid.graph)
ols2.hat.grid <- predict(lm.fit2, grid.graph)
ols3.hat.grid <- predict(lm.fit3, grid.graph)
ols4.hat.grid <- predict(lm.fit4, grid.graph)

v = rescale(c(2,5.5,6,6.5,7,8,9,12))
l = c(2, 12)
poly.fig1 = h + geom_point(aes(color = ols1.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l)  + ggtitle("Linear")
poly.fig2 = h + geom_point(aes(color = ols2.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("Quadratic")
poly.fig3 = h + geom_point(aes(color = ols3.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("Order 5 polynomial")
poly.fig4 = h + geom_point(aes(color = ols4.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("Order 10 polynomial")

multiplot(poly.fig1, poly.fig3, poly.fig2, poly.fig4, cols=2)

## 3.1 In-sample fit

R.squared=matrix(,nrow=maxvars,ncol=maxvars)
Adj.R.squared=matrix(,nrow=maxvars,ncol=maxvars)
AIC=matrix(,nrow=maxvars,ncol=maxvars)
BIC=matrix(,nrow=maxvars,ncol=maxvars)

for (rooms.i in (1:maxvars)) {
  for (members.i in (1:maxvars)) {
    R.squared[rooms.i,members.i]=summary(lm(income~poly(rooms,rooms.i)*poly(members,members.i),data=hhdata.small))$r.squared
    Adj.R.squared[rooms.i,members.i]=summary(lm(income~poly(rooms,rooms.i)*poly(members,members.i),data=hhdata.small))$adj.r.squared
    AIC[rooms.i,members.i]=AIC(lm(income~poly(rooms,rooms.i)*poly(members,members.i),data=hhdata.small),k=2)
    BIC[rooms.i,members.i]=AIC(lm(income~poly(rooms,rooms.i)*poly(members,members.i),data=hhdata.small),k=log(dim(hhdata.small)[1]))
  }
}
dimnames(R.squared) = list(rooms=1:maxvars, members=1:maxvars)
dimnames(Adj.R.squared) = list(rooms=1:maxvars, members=1:maxvars)
dimnames(AIC) = list(rooms=1:maxvars, members=1:maxvars)
dimnames(BIC) = list(rooms=1:maxvars, members=1:maxvars)

## 3.2 Out-of-sample model fit
set.seed(825)
inTraining <- createDataPartition(hhdata.small$income, p = .75, list = FALSE)
training <- hhdata.small[inTraining,1:3]
testing <- hhdata.small[-inTraining,1:3]
fitControl <- trainControl(method = "repeatedcv", number = 5,  repeats = 5)

RMSE=matrix(,nrow=maxvars,ncol=maxvars)
for (rooms.i in (1:maxvars)) {
  for (members.i in (1:maxvars)) {
    j <- bquote(income~poly(rooms,.(rooms.i))*poly(members,.(members.i)))
    LinearRegressor <- train(as.formula(j), data=hhdata.small, method = "lm", trControl = fitControl)
    RMSE[rooms.i,members.i] <- LinearRegressor$results$RMSE
  }
}

dimnames(R.squared) = list(rooms=1:maxvars, members=1:maxvars)
dimnames(RMSE) = list(rooms=1:maxvars, members=1:maxvars)
#kable(RMSE,row.names=T,col.names=1:maxvars,digits=4,caption="RMSE")
#paste("Minimum RMSE is achieved at", which(RMSE==min(RMSE),arr.ind=TRUE)[1], "rooms and", which(RMSE==min(RMSE),arr.ind=TRUE)[2], "members.")

poly.fit.best=lm(income~poly(rooms,4)*poly(members,3),data=hhdata.small)
poly.best.grid <- predict(poly.fit.best, grid.graph)
best.poly = h + geom_point(aes(color = poly.best.grid)) + scale_colour_gradientn(colours = my.colors(20), 
                                                                                 values = rescale(c(2,5.5,6,6.5,7,8,9,12)),limits=c(2, 12))  + ggtitle("OLS (Polynomial)") 
### Linear regressions with Fourier transforms
series.estimation = function(rooms.n,members.n,train) {
  members.series.varlist = paste("cos((members)*", 1:members.n,"*pi/15)",sep="")
  rooms.series.varlist = paste("cos((rooms)*", 1:rooms.n,"*pi/15)",sep="")
  fmla=as.formula(paste("income ~ (",paste(members.series.varlist, collapse= "+"),")*(",paste(rooms.series.varlist, collapse= "+"),")"))
  lm(fmla,subset=train)
}

series.estimation.fmla = function(rooms.n,members.n) {
  members.series.varlist = paste("cos((members)*", 1:members.n,"*3.141593/15)",sep="")
  rooms.series.varlist = paste("cos((rooms)*", 1:rooms.n,"*3.141593/15)",sep="")
  fmla=as.formula(paste("income ~ (",paste(members.series.varlist, collapse= "+"),")*(",paste(rooms.series.varlist, collapse= "+"),")"))
}

series.fit1=series.estimation(1,1,NULL)
series.fit1.grid <- predict(series.fit1, grid.graph)
series.fit2=series.estimation(2,2,NULL)
series.fit2.grid <- predict(series.fit2, grid.graph)
series.fit4=series.estimation(4,4,NULL)
series.fit4.grid <- predict(series.fit2, grid.graph)
series.fit6=series.estimation(6,6,NULL)
series.fit6.grid <- predict(series.fit6, grid.graph)

v = rescale(c(2,5.5,6,6.5,7,8,9,12))
l = c(2, 12)
series.fig1 = h + geom_point(aes(color = series.fit1.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l)  + ggtitle("Fourier series of order 1")
series.fig2 = h + geom_point(aes(color = series.fit2.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("Fourier series of order 2")
series.fig3 = h + geom_point(aes(color = series.fit4.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("Fourier series of order 4")
series.fig4 = h + geom_point(aes(color = series.fit6.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("Fourier series of order 6")

multiplot(series.fig1, series.fig3, series.fig2, series.fig4, cols=2)

set.seed(825)
fitControl <- trainControl(method = "repeatedcv", number = 5,  repeats = 5)

RMSE=matrix(,nrow=maxvars,ncol=maxvars)
for (rooms.i in (1:maxvars)) {
  for (members.i in (1:maxvars)) {
    LinearRegressor <- train(series.estimation.fmla(rooms.i,members.i), data=hhdata.small, method = "lm", trControl = fitControl)
    RMSE[rooms.i,members.i] <- LinearRegressor$results$RMSE
  }
}

dimnames(RMSE) = list(rooms=1:maxvars, members=1:maxvars)
#kable(RMSE,row.names=T,col.names=1:maxvars,digits=4,caption="RMSE")
paste("Minimum RMSE is achieved at", which(RMSE==min(RMSE),arr.ind=TRUE)[1], "rooms and", which(RMSE==min(RMSE),arr.ind=TRUE)[2], "members.")

fourier.fit.best=series.estimation(2,7,NULL)
series.best.grid <- predict(fourier.fit.best, grid.graph)
best.series = h + geom_point(aes(color = series.best.grid)) + scale_colour_gradientn(colours = my.colors(20), 
                                                                                     values = rescale(c(2,5.5,6,6.5,7,8,9,12)),limits=c(2, 12))  + ggtitle("OLS (Fourier)")

### Regression trees


tree.fit1 =rpart(income~.,data=hhdata.small,control=rpart.control(minsplit=30, cp=0.01))
tree.fit2 =rpart(income~.,data=hhdata.small,control=rpart.control(minsplit=30, cp=0.005))
tree.fit3 =rpart(income~.,data=hhdata.small,control=rpart.control(minsplit=30, cp=0.0005))
tree.fit4 =rpart(income~.,data=hhdata.small,control=rpart.control(minsplit=30, cp=0.00001))

tree1.hat.grid=predict(tree.fit1, grid.graph)
tree2.hat.grid=predict(tree.fit2,grid.graph)
tree3.hat.grid=predict(tree.fit3,grid.graph)
tree4.hat.grid=predict(tree.fit4,grid.graph)

v = rescale(c(2,5.5,6,6.5,7,8,9,12))
l = c(2, 12)
tree.fig1 = h + geom_point(aes(color = tree1.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l)  + ggtitle("Cp = 0.01")
tree.fig2 = h + geom_point(aes(color = tree2.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("Cp = 0.005")
tree.fig3 = h + geom_point(aes(color = tree3.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("Cp = 0.0005")
tree.fig4 = h + geom_point(aes(color = tree4.hat.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l) + ggtitle("Cp = 0.000001")

multiplot(tree.fig1, tree.fig3, tree.fig2, tree.fig4, cols=2)


cv.eval = function(cp) {
  RMSE=rep(0,5)
  tree1.fit =rpart(income~.,data=hhdata.small[-sample1,],control=rpart.control(minsplit=30, cp=cp))
  tree2.fit =rpart(income~.,data=hhdata.small[-sample2,],control=rpart.control(minsplit=30, cp=cp))
  tree3.fit =rpart(income~.,data=hhdata.small[-sample3,],control=rpart.control(minsplit=30, cp=cp))
  tree4.fit =rpart(income~.,data=hhdata.small[-sample4,],control=rpart.control(minsplit=30, cp=cp))
  tree5.fit =rpart(income~.,data=hhdata.small[-sample5,],control=rpart.control(minsplit=30, cp=cp))
  RMSE[1] = sqrt(mean((income[sample1]-predict(tree1.fit,hhdata.small[sample1,]))^2))
  RMSE[2] = sqrt(mean((income[sample2]-predict(tree2.fit,hhdata.small[sample2,]))^2))
  RMSE[3] = sqrt(mean((income[sample3]-predict(tree3.fit,hhdata.small[sample3,]))^2))
  RMSE[4] = sqrt(mean((income[sample4]-predict(tree4.fit,hhdata.small[sample4,]))^2))
  RMSE[5] = sqrt(mean((income[sample5]-predict(tree5.fit,hhdata.small[sample5,]))^2))
  mean(RMSE)
}

cp = seq(from=2,to=5, by = 0.25)
cv.error.13= rep(0 ,13)
for (i in 1:13) {
  cv.error.13[i]=cv.eval(10^(-cp[i]))
}
#cp[which.min(cv.error.13)]

cp = seq(from = 3, to = 4, by =0.05)
cv.error.21= rep(0 ,21)
for (i in 1:21) {
  cv.error.21[i]=cv.eval(10^(-cp[i]))
}
#cp[which.min(cv.error.21)]

tree.fit.best =rpart(income~.,data=hhdata.small,control=rpart.control(minsplit=30, cp=10^(-3.7)))
tree.best.grid=predict(tree.fit.best, grid.graph)

best.tree = h + geom_point(aes(color = tree.best.grid)) + scale_colour_gradientn(colours = my.colors(20), values = v,limits=l)  +   
  ggtitle("Tree")

multiplot(best.poly, best.knn, best.series, best.tree, cols=2)

best.polynomial=predict(poly.fit.best,hhdata.small)
best.fourier=predict(fourier.fit.best,hhdata.small)
best.loclinear=predict(knn.fit.best,hhdata.small)
best.tree=predict(tree.fit.best,hhdata.small)

d=data.frame(best.polynomial,best.fourier,best.loclinear,best.tree)
cor(d)
