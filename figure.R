setwd("~/Documents/tex_projects/r_guide_slide")
library(ggplot2)
library(reshape2)
library(lattice)
library(Cairo)
library(maptools)
library(sp)
library(rgdal)
library(ggplot2movies)
library(plyr)
library(maps)
library(raster)
library(rasterVis)
library(ggsn)
library(sf)
library(RColorBrewer)
library(viridis)
library(broom)
library(ggmap)
library(ggspatial)

###  -------------------------------------------------------------------
###  chp3:程序控制
###  -------------------------------------------------------------------
fun1<-function(x){
  df<-data.frame()
  for(i in 1:nrow(x)){
    row<-x[i,]
    df<-rbind(df,mean(row))
  }
}
fun2 <-function(x){
  apply(x,1,mean)
}
x <- cbind(x1=3, x2=1:100,x3=c(5:55,1:50))
system.time(fun1(x))
system.time(fun2(x))


### -------------------------------------------------------------------
### chp3:数据操作-表达式
### -------------------------------------------------------------------
png("expression-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar=c(2,2,0,0)+0.1)
plot(1:10, 1:10)
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(4, 8.4, "expression(hat(beta) == (X^t * X)^{-1} * X^t * y)", cex = .8)
text(4, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
text(4, 6.4, "expression(bar(x) == sum(frac(x[i], n), i==1, n))", cex = .8)
text(8, 5, expression(paste(frac(1, sigma*sqrt(2*pi)), " ", plain(e)^{frac(-(x-mu)^2, 2*sigma^2)})), cex = 1.2)
dev.off()


### -------------------------------------------------------------------
### chp4:绘图参数
### -------------------------------------------------------------------
png("par-example.png",width=1600, height=1200,res=300,pointsize=6)
opar<-par()
par(mfrow=c(2,2),mar=c(3,3,3,3))
plot(x=1,y=1,col="red",xlab="x",ylab="y")
text(1,1,"adj=c(0.5,0.5)")
text(1,1,"adj=c(0,1)",adj=c(0,1))
plot(1:10,1:10,mgp=c(5,3,1),xlab="x",ylab="y",main="mgp=c(5,3,1)")
plot(1:10,1:10,tcl=1,xlab="x",ylab="y")
text(2,7,"srt=30",srt=30)
plot(x=c(0,1,0),y=c(0,1,2),type="l",lwd=10,ljoin=2,main="ljoin=2",xlab="x",ylab="y")
par(opar)
dev.off()


### -------------------------------------------------------------------
### chp4:高级绘图函数
### -------------------------------------------------------------------
png("高级绘图函数.png",res=300)
x = runif(100); y = 0.2*x + 0.1*rnorm(100)
opar <- par()
par(mar=c(2,2,0.1,0.1))
plot(x,y)
par(opar)
dev.off()


### -------------------------------------------------------------------
### chp4:低级绘图函数
### -------------------------------------------------------------------
png("低级绘图函数.png",res=300)
opar <- par()
par(mar=c(2,2,0.1,0.1))
plot(x,y)
fit=lm(y~x)
abline(fit)
par(opar)
dev.off()


### -------------------------------------------------------------------
### chp4:plot函数参数type的九种效果示例
### -------------------------------------------------------------------
png("plot-example.png",width=1600, height=1200,res=300,pointsize=6)
showCols1 <- function(bg = "gray", cex = 0.75, srt = 30) {
    m <- ceiling(sqrt(n <- length(cl <- colors())))
    length(cl) <- m*m; cm <- matrix(cl, m)
    ##
    require("graphics")
    op <- par(mar=rep(0,4), ann=FALSE, bg = bg); on.exit(par(op))
    plot(1:m,1:m, type="n", axes=FALSE)
    text(col(cm), rev(row(cm)), cm,  col = cl, cex=cex, srt=srt)
}
showCols1()
dev.off()


### -------------------------------------------------------------------
### chp4:颜色bar
### -------------------------------------------------------------------
png("colors-bar.png")
opar <- par()
par(mar = c(0, 6, 0, 0) + 0.1, height=30, yaxs = "i")
barplot(rep(1,20), col = c("tomato","lightcyan4","navajowhite1",
"gray5","seagreen3","ghostwhite","grey60","cornsilk4","linen","darkblue",
"gray26","steelblue1","grey","plum4","violet","gray83","red",
"thistle1","palegreen4","oldlace"),names.arg = c("tomato","lightcyan4","navajowhite1",
"gray5","seagreen3","ghostwhite","grey60","cornsilk4","linen","darkblue",
"gray26","steelblue1","grey","plum4","violet","gray83","red",
"thistle1","palegreen4","oldlace"), horiz = TRUE, las = 1,
xaxt = "n")
par(opar)
dev.off()


### -------------------------------------------------------------------
### chp4:颜色bar渐变色
### -------------------------------------------------------------------
png("rgb-bar.png",width=1600,height=600,res=300,pointsize=3)
par(mar = c(0.2, 2, 1, 0) + 0.1, yaxs = "i")
x = rgb(1, seq(0, 1, length = 20), 0)
barplot(rep(1, 20), col = x)
dev.off()


### -------------------------------------------------------------------
### chp4:特定主题调色板1
### -------------------------------------------------------------------
require(datasets)
#require(grDevices); require(graphics)
png("terrain_colors.png")
par(mar = c(2,2,2,0) + 0.1)
x <- 10*(1:nrow(volcano)); x.at <- seq(100, 800, by=100)
y <- 10*(1:ncol(volcano)); y.at <- seq(100, 600, by=100)
# Using Terrain Colors 
image(x, y, volcano, col=terrain.colors(100),axes=FALSE,xlab="",ylab="")
contour(x, y, volcano, levels=seq(90, 200, by=5), add=TRUE, col="brown")
axis(1, at=x.at)
axis(2, at=y.at)
box()
title(main="col=terrain.colors(100)", font.main=4)
dev.off()


### -------------------------------------------------------------------
### chp4:特定主题调色板2
### -------------------------------------------------------------------
png("heat_colors.png")
par(mar = c(2,2,2,0) + 0.1)
image(x, y, volcano, col=heat.colors(100), axes=FALSE)
contour(x, y, volcano, levels=seq(90, 200, by=5), add=TRUE, col="brown")
axis(1, at=x.at)
axis(2, at=y.at)
box()
title(main="col=heat.colors(100)", font.main=4)
dev.off()


### -------------------------------------------------------------------
### chp4:特定主题调色板3
### -------------------------------------------------------------------
png("gray_colors.png")
par(mar = c(2,2,2,0) + 0.1)
image(x, y, volcano, col=gray(100:200/200), axes=FALSE)
contour(x, y, volcano, levels=seq(90, 200, by=5), add=TRUE, col="black")
axis(1, at=x.at)
axis(2, at=y.at)
box()
title(main="col=gray(100:200/200)", font.main=4)
dev.off()


### -------------------------------------------------------------------
### chp4:RColorBrewer包演示
### -------------------------------------------------------------------
png("rcolorbrewer.png")
layout(matrix(1:3, 3), heights = c(2, 1, 1))
par(mar = c(0, 4, 0, 0))
# 连续型:18种
display.brewer.all(type = "seq")
# 极端化:9种
display.brewer.all(type = "div")
# 离散型:8种
display.brewer.all(type = "qual")
dev.off()


### -------------------------------------------------------------------
### chp4:pch参数不同取值的点类型
### -------------------------------------------------------------------
library(cairoDevice)
Cairo_png("pch-example.png",width=15,height=15)
par(mar = c(0,0,0,0) + 0.1)
pchShow <-
  function(extras = c("*",".", "o","O","0","+","-","|","%","#"),
           cex = 3, ## good for both .Device=="postscript" and "x11"
           col = "red3", bg = "gold", coltext = "brown", cextext = 1.2,
           main = NULL
           )
  {
    nex <- length(extras)
    np  <- 26 + nex
    ipch <- 0:(np-1)
    k <- floor(sqrt(np))
    dd <- c(-1,1)/2
    rx <- dd + range(ix <- ipch %/% k)
    ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
    pch <- as.list(ipch) # list with integers & strings
    if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
    plot(rx, ry, type = "n", axes  =  FALSE, xlab = "", ylab = "", main = main)
    abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
    for(i in 1:np) {
      pc <- pch[[i]]
      ## 'col' symbols with a 'bg'-colored interior (where available) :
      points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex)
      if(cextext > 0)
          text(ix[i] - 0.3, iy[i], pc, col = coltext, cex = cextext)
    }
  }

pchShow()
dev.off()


### -------------------------------------------------------------------
### chp4:点的艺术
### -------------------------------------------------------------------
library(cairoDevice)
i <- 1
for (n in c(63, 60, 76, 74)) {
    Cairo_png(paste0("points-art0", as.character(i), ".png"), width = 15,
              height = 15)
    par(mar = c(0, 0, 0, 0))
    set.seed(711)
    plot.new()
    size = c(replicate(n, 1/rbeta(2, 1.5, 4)))
    center = t(replicate(n, runif(2)))
    center = center[rep(1:n, each = 2), ]
    color = apply(replicate(2 * n, sample(c(0:9, LETTERS[1:6]),8, replace = TRUE)),
                  2, function(x) sprintf("#%s",paste(x, collapse = "")))
    points(center, cex = size, pch = rep(20:21, n), col = color)
    dev.off()
    i <- i+1
}


##library(cairoDevice)
##Cairo_png("line-example.png",width=15,height=15)

### -------------------------------------------------------------------
### chp4:直线、曲线、线段和箭头示例
### -------------------------------------------------------------------
png("line-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(2.5,2.5,0,0) + 0.1)
# 不作图,只画出框架,且指定坐标轴范围
plot(1:10, type = "n", xlim = c(0, 10), ylim = c(0,10),cex.axis=2)
# 10个正态随机数绝对值的波动线
lines(1:10, abs(rnorm(10)))
# 不同的直线
abline(a = 0, b = 1, col = "gray")
abline(v = 2, lty = 2)
abline(h = 2, lty = 2)
#添加文本
text(8, 3, "abline(a = 0, b = 1)",cex=2)
# 添加箭头
arrows(8, 3.5, 6, 5.7, angle = 40,cex=2)
# 参数用了向量:不同灰度的线段
segments(rep(3, 4), 6:9, rep(5, 4), 6:9, col = gray(seq(0.2,
0.8, length = 4)))
text(4, 9.8, "segments",cex=2)
dev.off()


### -------------------------------------------------------------------
### chp4:样条曲线示例
### -------------------------------------------------------------------
png("open-xspline.png",width=1000,height=1000,res=300,pointsize=6)
#png("open-xspline.png")
op <- par(mfrow = c(3,3), mar = rep(0,4), oma = c(0,0,2,0))
xsplineTest <- function(s, open = TRUE, x = c(1,1,3,3)/4, y = c(1,3,3,1)/4, cex=2,...) {
    plot(c(0,1), c(0,1), type = "n", axes = FALSE, xlab = "", ylab = "")
    points(x, y, pch = 19)
    xspline(x, y, s, open, ...)
    text(x+0.05*c(-1,-1,1,1), y+0.05*c(-1,1,1,-1), s, cex=2)
}

xsplineTest(c(0, -1, -1, 0))
xsplineTest(c(0, -1,  0, 0))
xsplineTest(c(0, -1,  1, 0))
xsplineTest(c(0,  0, -1, 0))
xsplineTest(c(0,  0,  0, 0))
xsplineTest(c(0,  0,  1, 0))
xsplineTest(c(0,  1, -1, 0))
xsplineTest(c(0,  1,  0, 0))
xsplineTest(c(0,  1,  1, 0))
title(main=list("Open X-splines",cex=2), outer = TRUE)
par(op)
dev.off()


### -------------------------------------------------------------------
### chp4:多边形和矩形示例
### -------------------------------------------------------------------
png("polygon-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(2.5,2.5,0,0) + 0.1)
# 产生40个正态随机数
x = rnorm(40)
# 画线图
plot(x, xlab = "", ylab="", type = "l")
# 多边形的连线路径
polygon(c(1, 1:40, 40), c(0, x, 0), col = "red")
# 获取当前图形区域坐标范围,以便下用
xy = par("usr")
# 用白色矩形挡住了0以下的部分
rect(xy[1], xy[3], xy[2], 0, col = "white")
# 重画一遍x的线条
lines(x)
# 添加水平线
abline(h = 0, col = "lightgray")
dev.off()


### -------------------------------------------------------------------
### chp4:线的艺术1
### -------------------------------------------------------------------
CairoPNG("line-art1.png",width=1000*3,height=1000*3,res=72*3,bg="black")
theta = 1:100
x = sin(theta)
y = cos(theta)
op = par(bg = 'black', mar = rep(0.5, 4))
plot.new()
plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
lines(x, y, col = hsv(0.65, 1, 1))
lines(0.8 * x, 0.8 * y, col = hsv(0.8, 1, 1))
lines(0.6 * x, 0.6 * y, col = hsv(0.9, 1, 1))
lines(0.4 * x, 0.4 * y, col = hsv(0.95, 1, 1))
dev.off()

### 羽毛
## CairoPNG("line-art1.png",width=1000*3,height=1000*3,res=72*3)
## x1 = c(seq(0, pi, length = 50), seq(pi, 2*pi, length = 50))
## y1 = cos(x1) / sin(x1)
## x2 = seq(1.02 * 2 * pi + pi/2, 4*pi + pi/2, length = 50)
## y2 = tan(x2)
## op = par(bg="black", mar=rep(.5,4))
## plot(c(x1, x2), c(y1, y2), type = "n", ylim = c(-11, 11))
## for (i in seq(-10, 10, length = 100))
## {
##   lines(x1, y1 + i, col = hsv(runif(1,.65,.7), 1, 1, runif(1,.7)),
##         lwd = 4 * runif(1, 0.3))
##   lines(x2, y2 + i, col = hsv(runif(1,.65,.7), 1, 1, runif(1,.7)),
##         lwd = 4 * runif(1, 0.3))
## }
## dev.off()


### -------------------------------------------------------------------
### chp4:线的艺术2
### -------------------------------------------------------------------
CairoPNG("line-art2.png",width=1000*3,height=1000*3,res=72*3,bg="black")
x = seq(-50, 50, by = 1)
y = -(x^2)
par(bg = 'black', mar = rep(0.5, 4))
plot(y, x, type = 'n')
lines(y, x, lwd = 2*runif(1), col = hsv(0.08, 1, 1, alpha = runif(1, 0.5, 0.9)))
for (i in seq(10, 2500, 10)){
lines(y-i, x, lwd = 2*runif(1), col = hsv(0.08, 1, 1, alpha = runif(1, 0.5, 0.9)))
}
for (i in seq(500, 600, 10)){
lines(y - i, x, lwd = 2*runif(1), col = hsv(0, 1, 1, alpha = runif(1, 0.5, 0.9)))
}


### -------------------------------------------------------------------
### chp4:线的艺术3
### -------------------------------------------------------------------
CairoPNG("line-art3.png",width=1000*3,height=1000*3,res=72*3,bg="black")
theta = seq(0, pi, length = 300)
x = cos(theta)
y = sin(theta)
op = par(bg = "black", mar = rep(0.5, 4))
plot(x, y, type = 'n')
segments(rep(0, 299), rep(0, 299), x[1:299] * runif(299, 0.7),
         y[1:299] * runif(299, 0.7),
         col = hsv(runif(299, 0.45, 0.55), 1, 1, runif(299, 0.5)),
         lwd = 5*runif(299))
dev.off()


### -------------------------------------------------------------------
### chp4:grid函数示例
### -------------------------------------------------------------------
png("grid-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(2.5,2.5,0,0) + 0.1)
with(iris,
     {
     plot(Sepal.Length, Sepal.Width, col = as.integer(Species),
          panel.first = grid(8, lty = 1, lwd = 2))
     }
     )
dev.off()


### -------------------------------------------------------------------
### chp4:文本函数示例
### -------------------------------------------------------------------
png("text-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(4, 4, 4, 3))
plot(0:10, type = "n", xlab = "", ylab = "", xlim = c(0,12))
grid(col = "gray")
title(main = "Demonstration of text in R Graphics",
xlab = "X-axis title", ylab = "Y-axis title")
mtext("Here is \"side = 4\"", side = 4, line = 1)
x = c(6, 4, 6, 8)
y = c(8, 5, 2, 5)
s = c(0, 90, 180, 270)
for (i in 1:4) text(x[i], y[i], sprintf("srt = %d",s[i]), srt = s[i])
segments(c(6, 0, 6, 12), c(10, 5, 0, 5), c(0, 6,12, 6), c(5, 0, 5, 10), lty = c(2, 1, 1, 2))
dev.off()


### -------------------------------------------------------------------
### chp4:图例函数示例
### -------------------------------------------------------------------
png("legend-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(4, 4, 4, 3))
plot(0:10, type = "n", xlab = "", ylab = "", xlim = c(0,12))
grid(col = "gray")
title(main = "Demonstration of text in R Graphics",
xlab = "X-axis title", ylab = "Y-axis title")
mtext("Here is \"side = 4\"", side = 4, line = 1)
x = c(6, 4, 6, 8)
y = c(8, 5, 2, 5)
s = c(0, 90, 180, 270)
for (i in 1:4) text(x[i], y[i], sprintf("srt = %d",s[i]), srt = s[i])
segments(c(6, 0, 6, 12), c(10, 5, 0, 5), c(0, 6,12, 6), c(5, 0, 5, 10), lty = c(2, 1, 1, 2))
legend(-0.2, 9.8, c("Upper", "Lower"), lty = 2:1, cex = 0.8, bty = "n",text.col="red",col="red")
dev.off()


### -------------------------------------------------------------------
### chp4:坐标轴函数示例
### -------------------------------------------------------------------
png("axis-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(2, 4, 0, 4)+0.1)
x <- 1:2; y <- runif(2, 0, 100)
plot(x, y, type="n", xlim=c(0.5, 2.5), ylim=c(-10, 110), axes=FALSE, ann=FALSE)
axis(2, at=seq(0, 100, 20))
mtext("Temperature (Centigrade)", side=2, line=3)
axis(1, at=1:2, labels=c("Treatment 1", "Treatment 2"))
axis(4, at=seq(0, 100, 20), labels=seq(0, 100, 20)*9/5 + 32)
mtext("Temperature (Fahrenheit)", side=4, line=3)
box()
segments(x, 0, x, 100, lwd=20, col="dark gray")
segments(x, 0, x, 100, lwd=16, col="white")
segments(x, 0, x, y, lwd=16, col="red")
dev.off()


### -------------------------------------------------------------------
### chp4:直方图与密度曲线的结合 
### -------------------------------------------------------------------
png("hist-example.png",width=1000,height=1000,res=300,pointsize=6)
data(geyser, package = "MASS")
par(mar = c(1.8, 3, 0.5, 0.1), mgp = c(2, 0.5, 0))
data(geyser, package = "MASS")
hst = hist(geyser$waiting, probability = TRUE, main = "", xlab = "waiting")
d = density(geyser$waiting)
polygon(c(min(d$x), d$x, max(d$x)), c(0, d$y, 0),
col = "lightgray", border = NA)
lines(d)
ht = NULL
brk = seq(40, 110, 5)
for (i in brk) ht = c(ht, d$y[which.min(abs(d$x -i))])
segments(brk, 0, brk, ht, lty = 3)
dev.off()


### -------------------------------------------------------------------
### chp4:箱线图示例 
### -------------------------------------------------------------------
png("boxplot-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(4, 4, 4, 0.1)+0.1)
boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset = supp == "VC", col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg",
        ylab = "tooth length",
        xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")
boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        subset = supp == "OJ", col = "orange")
legend(2, 9, c("Ascorbic acid", "Orange juice"), fill = c("yellow", "orange"))
dev.off()


### -------------------------------------------------------------------
### chp4:堆砌和并列的条形图示例
### -------------------------------------------------------------------
png("barplot-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(4, 4, 4, 0)+0.1)
library(RColorBrewer)
par(mfrow = c(2, 1), mar = c(3, 2.5, 0.5, 0.1))
death = t(VADeaths)[, 5:1]
barplot(death, col = brewer.pal(4, "Set1"))
barplot(death, col = brewer.pal(4, "Set1"), beside = TRUE, legend = TRUE)
dev.off()


### -------------------------------------------------------------------
### chp4:等高线图示例
### -------------------------------------------------------------------
png("contour-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(4, 4, 0, 0)+0.1)
data(ChinaLifeEdu, package="MSG")
x = ChinaLifeEdu
plot(0, 0, type = "n", xlim = range(x[, 1]), ylim = range(x[,2]), xlab = "预期寿命", ylab ="高学历人数")
u = par("usr")
rect(u[1], u[3], u[2], u[4], col = "antiquewhite", border = "red")
library(KernSmooth)
est = bkde2D(x, apply(x, 2, dpik))
contour(est$x1, est$x2, est$fhat, nlevels = 15, col = "darkgreen", add = TRUE, vfont = c("sans serif", "plain"))
points(x)
dev.off()


### -------------------------------------------------------------------
### chp4:三维透视图示例
### -------------------------------------------------------------------
png("persp-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(0, 0, 0, 0))
data(ChinaLifeEdu, package="MSG")
x = ChinaLifeEdu
library(KernSmooth)
est = bkde2D(x, apply(x, 2, dpik))
persp(est[["x1"]], est[["x2"]], est[["fhat"]], shade = 0.75, col = "lightblue", phi = 20, theta = 15, box = TRUE)
dev.off()


### -------------------------------------------------------------------
### chp4:散点矩阵图示例
### -------------------------------------------------------------------
png("pairs-example.png",width=1000,height=1000,res=300,pointsize=6)
panel.hist = function(x, ...) {
   usr = par("usr")
   on.exit(par(usr))
   par(usr = c(usr[1:2], 0, 1.5))
   h = hist(x, plot = FALSE)
   nB = length(breaks <- h$breaks)
   y = h$counts/max(h$counts)
   rect(breaks[-nB], 0, breaks[-1], y, col = "beige")
}
idx = as.integer(iris[["Species"]])
pairs(iris[1:4], upper.panel = function(x, y, ...) points(x, y, pch = c(17, 16, 6)[idx], col = idx), pch = 20, oma = c(2, 2, 2, 2), lower.panel = panel.smooth, diag.panel = panel.hist)
dev.off()


### -------------------------------------------------------------------
### chp4:平滑散点图示例
### -------------------------------------------------------------------
png("smoothscatter-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(2, 2, 0.1, 0.1))
data(BinormCircle, package="MSG")
smoothScatter(BinormCircle)
dev.off()


### -------------------------------------------------------------------
### chp4:热图示例
### -------------------------------------------------------------------
png("heatmap-example.png",width=1000,height=1000,res=300,pointsize=6)
heatmap(as.matrix(mtcars), col = brewer.pal(9, "RdYlBu"), scale = "column", margins = c(4, 8))
dev.off()


### -------------------------------------------------------------------
### chp4:小提琴图示例
### -------------------------------------------------------------------
png("vioplot-example.png",width=1000,height=1000,res=300,pointsize=6)
library(vioplot)
par(mar = c(2, 2, 0.1, 0.1))
x <- rnorm(100)
y <- rnorm(100)
plot(x, y, xlim=c(-5,5), ylim=c(-5,5))
vioplot(x, col="tomato", horizontal=TRUE, at=-4, add=TRUE,lty=2, rectCol="gray")
vioplot(y, col="cyan", horizontal=FALSE, at=-4, add=TRUE,lty=2)
dev.off()


### -------------------------------------------------------------------
### chp4:地图示例
### -------------------------------------------------------------------
png("map-example.png",width=1000,height=1000,res=300,pointsize=6)
library(maps)
par(mar = c(0.1, 0.1, 0.1, 0.1))
map("state", interior = FALSE)
map("state", boundary = FALSE, lty = 2, add = TRUE)
dev.off()


### -------------------------------------------------------------------
### chp4:脸谱图示例
### -------------------------------------------------------------------
png("teachingdemos-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(0.1, 0.1, 0.1, 0.1))
library(TeachingDemos)
faces2(mtcars[, c("hp", "disp", "mpg", "qsec", "wt")], which = c(14, 9, 11, 6, 5), adj = c(0.5, 0))
dev.off()


### -------------------------------------------------------------------
### chp4:平行坐标图示例
### -------------------------------------------------------------------
png("parcoord-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(2, 0.3, 0.1, 0.3))
library(MASS)
ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
parcoord(log(ir)[, c(3, 4, 2, 1)], col = 1 + (0:149)%/%50)
dev.off()


### -------------------------------------------------------------------
### chp5:不同的GvHD病患者在细胞检测中的FSC-H结果数据
### -------------------------------------------------------------------
trellis.par.set(theme = col.whitebg())
library(lattice)
png("lattice-example1.png",width=1920,height=1080,res=300,pointsize=12)
library(flowViz)
lw <- list(left.padding = list(x = 0, units = "inches"))
lw$right.padding <- list(x = -0.1, units = "inches")
lh$top.padding <- list(x = -0.1, units = "inches")
lh$bottom.padding <- list(x = -0.2, units = "inches")
lattice.options(layout.widths = lw, layout.heights = lh)
data(GvHD, package = "flowCore")
densityplot(Visit ~ `FSC-H` | Patient, data = GvHD, ylim=c(0.9,9))
dev.off()


### -------------------------------------------------------------------
### chp5:泰坦尼克号生存率的交叉分类数据
### -------------------------------------------------------------------
png("lattice-example2.png",width=1920,height=1080,res=300,pointsize=12)
trellis.par.set(theme = col.whitebg())
lw <- list(left.padding = list(x = 0, units = "inches"))
lw$right.padding <- list(x = -0.1, units = "inches")
lh$top.padding <- list(x = -0.1, units = "inches")
lh$bottom.padding <- list(x = 0, units = "inches")
lattice.options(layout.widths = lw, layout.heights = lh)
bc.titanic <-
barchart(Class ~ Freq | Sex + Age, as.data.frame(Titanic),
groups = Survived, stack = TRUE, layout = c(4, 1),
auto.key = list(title = "Survived", columns = 2),
scales = list(x = "free"))

update(bc.titanic,
panel = function(...) {
panel.grid(h = 0, v = -1)
panel.barchart(...)
})
dev.off()


### -------------------------------------------------------------------
### chp5:在panel中绘制单变量数据
### -------------------------------------------------------------------
png("lattice-parameter1.png")
densityplot(~mpg, data=mtcars)
dev.off()


### -------------------------------------------------------------------
### chp5:在不同panel中绘制单变量分类数据
### -------------------------------------------------------------------
png("lattice-parameter2.png")
densityplot(~mpg|cyl, data=mtcars,layout=c(1,3))
dev.off()


### -------------------------------------------------------------------
### chp5:在同一panel中绘制单变量分类数据
### -------------------------------------------------------------------
png("lattice-parameter3.png")
densityplot(~mpg, groups=cyl, data=mtcars)
dev.off()


### -------------------------------------------------------------------
### chp5:在不同panel中绘制多变量分类数据
### -------------------------------------------------------------------
png("lattice-parameter4.png")
EE <- equal.count(ethanol$E, number=9, overlap=1/4)
## Constructing panel functions on the fly; prepanel
xyplot(NOx ~ C | EE, data = ethanol)
dev.off()


### -------------------------------------------------------------------
### chp5:在不同panel中绘制多变量分类数据
### -------------------------------------------------------------------
## png("lattice-parameter4.png")
## EE <- equal.count(ethanol$E, number=9, overlap=1/4)
## ## Constructing panel functions on the fly; prepanel
## xyplot(NOx ~ C | EE, data = ethanol,
##        prepanel = function(x, y) prepanel.loess(x, y, span = 1),
##        xlab = "Compression Ratio", ylab = "NOx (micrograms/J)",
##        panel = function(x, y) {
##            panel.grid(h = -1, v = 2)
##            panel.xyplot(x, y)
##            panel.loess(x, y, span=1)
##        },
##        aspect = "xy")
## dev.off()


### -------------------------------------------------------------------
### chp5:lattice中的标准高级绘图函数
### -------------------------------------------------------------------
png("lattice-all.png")
x <- 1:5
y <- 1:5
g <- factor(1:5)
types <- c("barchart", "bwplot", "densityplot", "dotplot",
           "histogram", "qqmath", "stripplot", "qq",
           "xyplot", "levelplot", "contourplot",
           "cloud", "wireframe", "splom", "parallel")
angle <- seq(0, 2*pi, length=21)[-21]
xx <- cos(angle)
yy <- sin(angle)
gg <- factor(rep(1:2, each=10))

aaa <- seq(0, pi, length=10)
xxx <- rep(aaa, 10)
yyy <- rep(aaa, each=10)
zzz <- sin(xxx) + sin(yyy)


doplot <- function(name, ...) {
  do.call(name, 
          list(..., scales=list(draw=FALSE), xlab=NULL, ylab=NULL,
               strip=function(which.panel, ...) { 
                       grid.rect(gp=gpar(fill="grey90")); grid.text(name) 
                     }))
}
plot <- vector("list", 15)
plot[[1]] <- doplot("barchart", y ~ g | 1)
plot[[2]] <- doplot("bwplot", yy ~ gg | 1, 
                    par.settings=list(box.umbrella=list(lwd=0.5)))
plot[[3]] <- doplot("densityplot", ~ yy | 1)
plot[[4]] <- doplot("dotplot", y ~ g | 1)
plot[[5]] <- doplot("histogram", ~ yy | 1)
plot[[6]] <- doplot("qqmath", ~ yy | 1)
plot[[7]] <- doplot("stripplot", yy ~ gg | 1)
plot[[8]] <- doplot("qq", gg ~ yy | 1)
plot[[9]] <- doplot("xyplot", xx ~ yy | 1)
plot[[10]] <- doplot("levelplot", zzz ~ xxx + yyy | 1, colorkey=FALSE)
plot[[11]] <- doplot("contourplot", zzz ~ xxx + yyy | 1, labels=FALSE, cuts=8)
plot[[12]] <- doplot("cloud", zzz ~ xxx + yyy | 1, zlab=NULL, zoom=0.9, 
                     par.settings=list(box.3d=list(lwd=0.01)))
plot[[13]] <- doplot("wireframe", zzz ~ xxx + yyy | 1, zlab=NULL, zoom=0.9,
                     drape=TRUE, par.settings=list(box.3d=list(lwd=0.01)),
                     colorkey=FALSE)
plot[[14]] <- doplot("splom", ~ data.frame(x=xx[1:10], y=yy[1:10]) | 1, 
                     pscales=0)
plot[[15]] <- doplot("parallel", ~ data.frame(x=xx[1:10], y=yy[1:10]) | 1)

grid.newpage()
pushViewport(viewport(layout=grid.layout(4, 4)))
for (i in 1:15) {
  pushViewport(viewport(layout.pos.col=((i - 1) %% 4) + 1,
                        layout.pos.row=((i - 1) %/% 4) + 1))
  print(plot[[i]], newpage=FALSE, 
        panel.width=list(1.025, "inches"),
        panel.height=list(1.025, "inches"))
  popViewport()
}
popViewport()
dev.off()


### -------------------------------------------------------------------
### chp5:在高级绘图函数xyplot中自定义panel和strip
### -------------------------------------------------------------------
library(Cairo)
png("panel-example1.png")
types.plain <- c("p", "l", "o", "r", "g", "s", "S", "h", "a", "smooth")
types.horiz <- c("s", "S", "h", "a", "smooth")
horiz <- rep(c(FALSE, TRUE), c(length(types.plain), length(types.horiz)))

types <- c(types.plain, types.horiz)

x <- sample(seq(-10, 10, length.out = 15), 30, TRUE)
y <- x + 0.25 * (x + 1)^2 + rnorm(length(x), sd = 5)

xyplot(y ~ x | gl(1, length(types)),
       xlab = "type", 
       ylab = list(c("horizontal=TRUE", "horizontal=FALSE"), y = c(1/6, 4/6)),
       as.table = TRUE, layout = c(5, 3),
       between = list(y = c(0, 1)),
       strip = function(...) {
           panel.fill(trellis.par.get("strip.background")$col[1])
           type <- types[panel.number()]
           grid::grid.text(label = sprintf('"%s"', type), 
                           x = 0.5, y = 0.5)
           grid::grid.rect()
       },
       scales = list(alternating = c(0, 2), tck = c(0, 0.7), draw = FALSE),
       par.settings = 
       list(layout.widths = list(strip.left = c(1, 0, 0, 0, 0))),
       panel = function(...) {
           type <- types[panel.number()]
           horizontal <- horiz[panel.number()]
           panel.xyplot(..., 
                        type = type,
                        horizontal = horizontal)
       })[rep(1, length(types))]
dev.off()


### -------------------------------------------------------------------
### chp5:通过外部自定义panel函数来绘制图形
### -------------------------------------------------------------------
CairoPNG("panel-example2.png",width=800*3,height=800*3,res=72*3)
panel.hypotrochoid <- function(r, d, cycles = 10, density = 30)
{
    if (missing(r)) r <- runif(1, 0.25, 0.75)
    if (missing(d)) d <- runif(1, 0.25 * r, r)
    t <- 2*pi*seq(0,cycles,by = 1/density)
    x <- (1-r)*cos(t)+d*cos((1-r)*t/r)
    y <- (1-r)*sin(t)-d*sin((1-r)*t/r)
    panel.lines(x, y)
}
prepanel.hypocycloid <- function(x, y) {
    list(xlim = c(-1, 1), ylim = c(-1, 1))
}
p <- xyplot(x=1~1, aspect = 1, cycles = 15, scales = list(draw = FALSE), xlab = "", ylab = "", prepanel = prepanel.hypocycloid, panel = panel.hypotrochoid)
p[rep(1, 9)]
dev.off()

### -------------------------------------------------------------------
### chp5:trellis 对象中所有的图形参数
### -------------------------------------------------------------------
CairoPNG("show_settings.png",width=1200*3,height=800*3,res=72*5)
show.settings()
dev.off()

### -------------------------------------------------------------------
### chp5:通过直接修改trellis对象的图形参数实现修改图形
### -------------------------------------------------------------------
CairoPNG("trellis_par_set1.png",width=800*3,height=800*3,res=72*3)
 # 绘制dotplot传递给trellis对象vad.plot
vad.plot <- 
    dotplot(reorder(Var2, Freq) ~ Freq | Var1,
            data = as.data.frame.table(VADeaths), 
            origin = 0, type = c("p", "h"),
            main = "Death Rates in Virginia - 1940", 
            xlab = "Number of deaths per 100")
vad.plot
dev.off()

CairoPNG("trellis_par_set2.png",width=800*3,height=800*3,res=72*3)
vad.plot <- 
    dotplot(reorder(Var2, Freq) ~ Freq | Var1,
            data = as.data.frame.table(VADeaths), 
            origin = 0, type = c("p", "h"),
            main = "Death Rates in Virginia - 1940", 
            xlab = "Number of deaths per 100")
dot.line.settings <- trellis.par.get("dot.line")
str(dot.line.settings)
dot.line.settings$col <- "transparent"
trellis.par.set("dot.line", dot.line.settings)
plot.line.settings <- trellis.par.get("plot.line")
str(plot.line.settings)
plot.line.settings$lwd <- 3
trellis.par.set("plot.line", plot.line.settings)
vad.plot
dev.off()


### -------------------------------------------------------------------
### chp5:通过update函数和par.settings对象修改图形
### -------------------------------------------------------------------
CairoPNG("update-example1.png",width=800*3,height=800*3,res=72*3)
p <-
cloud(depth ~ long + lat, quakes, zlim = c(690, 30),
pch = ".", cex = 4, zoom = 1,
xlab = NULL, ylab = NULL, zlab = NULL,
par.settings = list(axis.line = list(col = "transparent")),
scales = list(draw = FALSE))
p
dev.off()

CairoPNG("update-example2.png",width=800*3,height=800*3,res=72*3)
p <-
cloud(depth ~ long + lat, quakes, zlim = c(690, 30),
pch = ".", cex = 4, zoom = 1,
xlab = NULL, ylab = NULL, zlab = NULL,
par.settings = list(axis.line = list(col = "transparent")),
scales = list(draw = FALSE))
p
npanel <- 2
rotz <- seq(-30, 30, length = npanel)
roty <- c(3, 0)
update(p[rep(1, 2 * npanel)],
layout = c(2, npanel),
panel = function(..., screen) {
crow <- current.row()
ccol <- current.column()
panel.cloud(..., screen = list(z = rotz[crow], x = -60, y = roty[ccol]))},
par.settings=list(axis.line=list(col="red")))
dev.off()


### -------------------------------------------------------------------
### chp5:ggplot程序包-映射1
### -------------------------------------------------------------------                                        
CairoPDF("ggplot_mapping1.pdf",10,5)
ggplot(mtcars,aes(mpg,wt,colour=cyl)) +
    geom_point() +
    theme(axis.title.x =element_text(size=14), axis.title.y=element_text(size=14))
dev.off()

### -------------------------------------------------------------------
### chp5:ggplot程序包-映射2
### -------------------------------------------------------------------   
CairoPDF("ggplot_mapping2.pdf",10,5)
library(nlme)
ggplot(Oxboys, aes(age,height, group= Subject)) + geom_line() +
    geom_smooth(aes(group=1), method = "lm", size = 2, se=F)
dev.off()

### -------------------------------------------------------------------
### chp5:ggplot程序包-标度
### -------------------------------------------------------------------   
CairoPDF("ggplot_scales.pdf",10,5)
p <- qplot(sleep_total, sleep_cycle, data = msleep, colour = vore)
p + scale_colour_hue("What does\nit eat?",
breaks = c("herbi", "carni", "omni", NA),
labels = c("plants", "meat", "both", "don’t know"))
dev.off()

### -------------------------------------------------------------------
### chp5:ggplot程序包-分面 网格型分面变量的示例
### -------------------------------------------------------------------   
CairoPDF("ggplot_facet2.pdf",10,5)
mpg2 <- subset(mpg, cyl != 5 & drv %in% c("4", "f"))
ggplot(mpg2, aes(cty,hwy)) + geom_point() + facet_grid(drv ~ cyl)
dev.off()


### -------------------------------------------------------------------
### chp5:ggplot程序包-分面 封装型分面变量的示例
### -------------------------------------------------------------------
CairoPDF("ggplot_facet3.pdf",10,5)
movies$decade <- round_any(movies$year, 10, floor)
ggplot(subset(movies, decade > 1890),aes(rating))+
    geom_histogram(aes(y=..density..),binwidth=0.5) + 
    facet_wrap(~ decade, ncol = 6)
dev.off()


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
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

## p1 <- ggplot(mpg,aes(cty, hwy)) + geom_point() +
##     facet_wrap(~ cyl) 
## p2 <- ggplot(mpg,aes(cty, hwy)) + geom_point() +
##     facet_wrap(~ cyl, scales = "free")
## multiplot(p1,p2,cols=2)

### -------------------------------------------------------------------
### chp5:控制标度的示例
### -------------------------------------------------------------------
CairoPDF("ggplot_facet4_1.pdf",5,5)
ggplot(mpg,aes(cty, hwy)) + geom_point() +
    facet_wrap(~ cyl, scales="fixed")
dev.off()
CairoPDF("ggplot_facet4_2.pdf",5,5)
ggplot(mpg,aes(cty, hwy)) + geom_point() +
    facet_wrap(~ cyl, scales = "free")
dev.off()


### -------------------------------------------------------------------
### chp5:ggplot2程序包-位置调整 控制标度的示例
### -------------------------------------------------------------------
CairoPDF("ggplot_position1_1.pdf",5,5)
ggplot(diamonds, aes(clarity, fill = cut)) +
    geom_bar(position = "stack")
dev.off()
CairoPDF("ggplot_position1_2.pdf",5,5)
ggplot(diamonds, aes(clarity, fill = cut)) +
    geom_bar(position = "fill")
dev.off()
CairoPDF("ggplot_position1_3.pdf",5,5)
ggplot(diamonds, aes(clarity, fill = cut)) +
    geom_bar(position = "dodge")
dev.off()


### -------------------------------------------------------------------
### chp5:ggplot2程序包-坐标系 直线与矩形在不同坐标系的变换示例
### -------------------------------------------------------------------
rect <- data.frame(x = 50, y = 50)
line <- data.frame(x = c(1, 200), y = c(100, 1))
CairoPDF("ggplot_coord1_1.pdf",5,5)
ggplot(mapping = aes(x, y)) + 
  geom_tile(data = rect, aes(width = 50, height = 50)) + 
  geom_line(data = line)
dev.off()
CairoPDF("ggplot_coord1_2.pdf",5,5)
ggplot(mapping = aes(x, y)) + 
  geom_tile(data = rect, aes(width = 50, height = 50)) + 
    geom_line(data = line) + coord_polar("x")
dev.off()
#CairoPNG("ggplot_coord1_3.png",width=500,height=500,res=72)
CairoPDF("ggplot_coord1_3.pdf",5,5)
ggplot(mapping = aes(x, y)) + 
  geom_tile(data = rect, aes(width = 50, height = 50)) + 
    geom_line(data = line) + coord_polar("y")
dev.off()
#CairoPNG("ggplot_coord1_4.png",width=500,height=500,res=72)
CairoPDF("ggplot_coord1_4.pdf",5,5)
ggplot(mapping = aes(x, y)) + 
  geom_tile(data = rect, aes(width = 50, height = 50)) + 
    geom_line(data = line) + coord_flip()
dev.off()
#CairoPNG("ggplot_coord1_5.png",width=500,height=500,res=72)
CairoPDF("ggplot_coord1_5.pdf",5,5)
ggplot(mapping = aes(x, y)) + 
  geom_tile(data = rect, aes(width = 50, height = 50)) + 
    geom_line(data = line) + coord_trans(y = "log10")
dev.off()
#CairoPNG("ggplot_coord1_6.png",width=500,height=500,res=72)
CairoPDF("ggplot_coord1_6.pdf",5,5)
ggplot(mapping = aes(x, y)) + 
  geom_tile(data = rect, aes(width = 50, height = 50)) + 
    geom_line(data = line) + coord_equal()
dev.off()


### -------------------------------------------------------------------
### chp5:ggplot2程序包-绘图过程
### -------------------------------------------------------------------
CairoPDF("ggplot_example1.pdf",10, 5)
ggplot(mpg, aes(x=cty, y=hwy))
dev.off()
CairoPDF("ggplot_example2.pdf",10, 5)
ggplot(mpg, aes(x=cty, y=hwy))+
    facet_wrap(~ year,ncol=2)
dev.off()
CairoPDF("ggplot_example3.pdf",10, 5)
ggplot(mpg, aes(x=cty, y=hwy))+
    facet_wrap(~ year,ncol=2)+
    geom_point(aes(colour=class,size=displ),alpha=0.6,position = "jitter")+  
    stat_smooth()
dev.off()
CairoPDF("ggplot_example4.pdf",10, 5)
ggplot(mpg, aes(x=cty, y=hwy))+
    facet_wrap(~ year,ncol=2)+
    geom_point(aes(colour=class,size=displ),alpha=0.6,position = "jitter")+  
    stat_smooth()+  
    scale_size(range = c(5, 10))
dev.off()
CairoPDF("ggplot_example5.pdf",10, 5)
CairoFonts(regular = "WenQuanYi Micro Hei", bold = "WenQuanYi Micro Hei")
ggplot(mpg, aes(x=cty, y=hwy))+   
  geom_point(aes(colour=class,size=displ),alpha=0.6,position = "jitter")+  
  stat_smooth()+  
  scale_size(range = c(5, 10))+  
  facet_wrap(~ year,ncol=2)+  
  ggtitle("汽车油耗与型号")+  
  labs(y='每加仑高速公路行驶距离',  
       x='每加仑城市公路行驶距离')+  
  guides(size=guide_legend(title='排量'),  
         colour = guide_legend(title='车型',  
                               override.aes=list(size=5)))
dev.off()


### -------------------------------------------------------------------
### chp6:R的空间数据类
### -------------------------------------------------------------------
bb <- matrix(c(114.25, 22.45, 114.85, 23.16), ncol = 2, dimnames = list(NULL, c("min", "max")))
Spatial(bb, proj4string = CRS("+proj=longlat"))

bb <- matrix(c(350, 85, 370, 95), ncol = 2, dimnames = list(NULL,c("min", "max")))
Spatial(bb, proj4string = CRS("+proj=longlat +datum=WGS84"))


### -------------------------------------------------------------------
### chp6:空间数据点类
### -------------------------------------------------------------------
CRAN_df <- read.table("data/CRAN051001a.txt", header = TRUE)
CRAN_mat <- cbind(CRAN_df$long, CRAN_df$lat)
llCRS <- CRS("+proj=longlat +ellps=WGS84")
CRAN_sp <- SpatialPoints(CRAN_mat, proj4string = llCRS)

# 将matrix的序号作为行名
row.names(CRAN_mat) <- 1:nrow(CRAN_mat)
CRAN_spdf1 <- SpatialPointsDataFrame(CRAN_mat, CRAN_df, proj4string = llCRS, match.ID = TRUE)
coords <- CRAN_mat[3,]


### -------------------------------------------------------------------
### chp6:空间数据点类 海龟迁徙轨迹示例
### -------------------------------------------------------------------
CairoPDF("spatial_points_example.pdf",10, 5)
library(sp)
turtle_df <- read.csv("data/seamap105_mod.csv")
summary(turtle_df)
timestamp <- as.POSIXlt(strptime(as.character(turtle_df$obs_date), "%m/%d/%Y %H:%M:%S"), "GMT")
turtle_df1 <- data.frame(turtle_df, timestamp=timestamp)
turtle_df1$lon <- ifelse(turtle_df1$lon < 0, turtle_df1$lon+360, turtle_df1$lon)
turtle_sp <- turtle_df1[order(turtle_df1$timestamp),]
coordinates(turtle_sp) <- c("lon", "lat")
proj4string(turtle_sp) <- CRS("+proj=longlat +ellps=WGS84")
library(maptools)
gshhs.c.b <- system.file("share/gshhs_c.b", package="maptools")
pac <- Rgshhs(gshhs.c.b, level=1, xlim=c(130,250), ylim=c(15,60), verbose=FALSE)
par(mar=c(2,2,0.3,0.1))
plot(pac$SP, axes=TRUE, col="khaki2", xaxs="i", yaxs="i")
plot(turtle_sp, add=TRUE)
m_rle <- rle(months(turtle_sp$timestamp))
clen <- cumsum(m_rle$lengths[-length(m_rle$lengths)])-1
crds <- coordinates(turtle_sp)
text(crds[clen,], labels=m_rle$values[-1], pos=3, offset=1.5, srt=45)
dev.off()


### -------------------------------------------------------------------
### chp6:空间线类：绘制中国地图示例
### -------------------------------------------------------------------
CairoPDF("spatial_lines_example1.pdf",10, 6)
library(maps)
china<- map("world", "china", plot=FALSE)
tw <- map("world","taiwan",plot=FALSE)
china$x <- c(china$x,NA,tw$x)
china$y <- c(china$y,NA,tw$y)
china$range <- c(range(china$range[1:2],tw$range[1:2]),range(china$range[3:4],tw$range[3:4]))
china$names <- c(china$names,tw$names)
p4s <- CRS("+proj=longlat +ellps=WGS84")
library(maptools)
SLchina <- map2SpatialLines(china, proj4string=p4s)
#attr <- data.frame(num=sapply(slot(SLchina,"lines"), function(x) slot(x,"ID")))
                                        #res <- SpatialLinesDataFrame(SLchina,attr)
china<- map("world", "china", fill=TRUE,plot=FALSE)
SPchina <- map2SpatialPolygons(china, IDs=sapply(china$names,"[",1L),proj4string=p4s)
par(mar=c(0.1,0.1,0.1,0.1))
plot(SLchina)
dev.off()


### -------------------------------------------------------------------
### chp6:空间线类：绘制等高线示例
### -------------------------------------------------------------------
CairoPDF("spatial_lines_example2.pdf",10, 6)
volcano_sl <- ContourLines2SLDF(contourLines(volcano))
t(slot(volcano_sl, "data"))
par(mar=c(0.1,0.1,0.1,0.1))
plot(volcano_sl)
dev.off()

## SPchina <- map2SpatialPolygons(china,IDs=sapply(slot(SPchina,"polygons"), function(x) slot(x,"ID")), proj4string=p4s)
## SLchina <- map2SpatialLines(china, proj4string=p4s)


### -------------------------------------------------------------------
### chp6:空间面类：绘制加州地图示例
### -------------------------------------------------------------------
CairoPDF("spatial_polygons_example1.pdf")
library(maps)
state.map <- map("state", plot=FALSE, fill=TRUE)
IDs <- sapply(strsplit(state.map$names, ":"), function(x) x[1])
library(maptools)
state.sp <- map2SpatialPolygons(state.map, IDs=IDs,
  proj4string=CRS("+proj=longlat +ellps=WGS84"))
sat <- read.table("data/state.sat.data_mod.txt", row.names=5, header=TRUE)
str(sat)
id <- match(row.names(sat), row.names(state.sp))
row.names(sat)[is.na(id)]
sat1 <- sat[!is.na(id),]
state.spdf <- SpatialPolygonsDataFrame(state.sp, sat1)
str(slot(state.spdf, "data"))
str(state.spdf, max.level=2)
#CairoPDF("spatial_polygons_example1.pdf",4,5)
california <- state.spdf[state.spdf$oname== "calif",]
par(mar=c(0,0,0,0))
plot(california,col="khaki2")
dev.off()


### -------------------------------------------------------------------
### chp6:空间面类：岛和洞示例
### -------------------------------------------------------------------
CairoPDF("spatial_polygons_example2.pdf",5,3.5)
load("data/high.RData")
manitoulin_sp <- high$SP
sapply(manitoulin_sp@polygons[[1]]@Polygons, function(x) slot(x, "hole"))
sapply(manitoulin_sp@polygons[[1]]@Polygons, function(x) slot(x, "ringDir"))
par(mar=c(0,0,0,0))
plot(manitoulin_sp, pbg="lightsteelblue2", col="khaki2", usePolypath=FALSE)
text(t(sapply(manitoulin_sp@polygons[[1]]@Polygons, function(x) slot(x, "labpt")))[-c(1,2),],
     label=high$polydata$level[-c(1,2)], col="black", font=2, cex=1)
dev.off()

## CairoPDF("spatial_lines_example3.pdf",10, 6)
## library(maps)
## china<- map("world", "china", fill=TRUE, plot=FALSE)
## tw <- map("world","taiwan",fill=TRUE, plot=FALSE)
## china$x <- c(china$x,NA,tw$x)
## china$y <- c(china$y,NA,tw$y)
## china$range <- c(range(china$range[1:2],tw$range[1:2]),range(china$range[3:4],tw$range[3:4]))
## china$names <- c(china$names,tw$names)
## p4s <- CRS("+proj=longlat +ellps=WGS84")
## library(maptools)
## SPchina <- map2SpatialPolygons(china, IDs=sapply(china$names, function(x) x[1]),proj4string=p4s)
## par(mar=c(0.1,0.1,0.1,0.1))
## plot(SPchina, col="khaki2")
## dev.off()


### -------------------------------------------------------------------
### chp6:栅格数据类 空的栅格
### -------------------------------------------------------------------
CairoPDF("spatial_grid_example1.pdf",10, 5)
load("data/high.RData")
manitoulin_sp <- high$SP
bb <- bbox(manitoulin_sp)
cs <- c(0.01, 0.01)
cc <- bb[,1]+(cs/2)
cd <- ceiling(diff(t(bb))/cs)
manitoulin_grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
p4s <- CRS(proj4string(manitoulin_sp))
manitoulin_SG <- SpatialGrid(manitoulin_grd, proj4string=p4s)
par(mar=c(2,2,0.5,0.5))
plot(manitoulin_SG,axes=TRUE)
dev.off()


### -------------------------------------------------------------------
### chp6:栅格数据类 stack函数示例
### -------------------------------------------------------------------
CairoPDF("spatial_raster_example1.pdf",18,5)
data <- system.file("external/test.grd", package="raster")
r1 <- raster(data)
r2 <- r1 * r1
r3 <- sqrt(r1)
s <- stack(r1,r2,r3)
op <- par()
par(oma=c(0,2,0.1,0.1),cex.main=2)
plot(s,nc=3,nr=1)
par(op)
dev.off()


### -------------------------------------------------------------------
### chp6:栅格数据类
### -------------------------------------------------------------------
CairoPDF("spatial_raster_example2.pdf",12,10)
r <- raster("data/70042108.tif")
out <- raster(r)
bs <- blockSize(out)
out <- writeStart(out, filename=tempfile(), overwrite=TRUE)
for (i in 1:bs$n) {
    v <- getValues(r, row=bs$row[i], nrows=bs$nrows[i])
    v[v <= 0] <- NA
    writeValues(out, v, bs$row[i])
}
out <- writeStop(out)
par(mar=c(2,2,0.1,2))
plot(out, col = terrain.colors(100))
dev.off()


### -------------------------------------------------------------------
### chp6:栅格数据类 sp包和raster包对象转换
### -------------------------------------------------------------------
# RasterLayer对象转换为SpatialGridDataFrame对象
r1 <- as(out, "SpatialGridDataFrame")
str(r1, max.level=2)
# SpatialGridDataFrame转换为RasterLayer对象
r2 <- as(r1, "RasterLayer")
str(r2, max.level=2)
              

## cs2cs +proj=latlong +datum=WGS84 +to +proj=utm +zone=17 -r <<EOF
## 43d38'33.24"N 79d23'13.7"W
## EOF

### -------------------------------------------------------------------
### chp6:坐标参考系统 CRS转换
### -------------------------------------------------------------------
y <- as.numeric(char2dms("43d38'33.24\"N"))
x <- as.numeric(char2dms("79d23'13.7\"W"))
xy<- SpatialPoints(cbind(x,y),proj4string=CRS("+proj=longlat +datum=WGS84"))
spTransform(xy,CRS("+proj=utm +zone=17 +datum=WGS84"))

SP <- SpatialPoints(cbind(126.59,-14.30), proj4string=CRS("+proj=longlat +datum=WGS84")) 
coordinatesUTM <- spTransform(SP, CRS("+proj=utm +south +zone=52 +datum=WGS84"))

## data(meuse)
## coordinates(meuse) <- c("x", "y")
## proj4string(meuse) <- CRS(paste("+init=epsg:28992", "+towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812"))


### -------------------------------------------------------------------
### chp6:坐标参考系统 中国地图的坐标转换
### -------------------------------------------------------------------
CairoPDF("spTransform_example.pdf",10,5)
library(maps)
library(maptools)
china<- map("world", "china", fill=TRUE,plot=FALSE)
tw <- map("world","taiwan",fill=TRUE,plot=FALSE)
china$x <- c(china$x,NA,tw$x)
china$y <- c(china$y,NA,tw$y)
china$range <- c(range(china$range[1:2],tw$range[1:2]),range(china$range[3:4],tw$range[3:4]))
china$names <- c(china$names,tw$names)
p4s <- CRS("+proj=longlat +datum=WGS84")
SPchina <- map2SpatialPolygons(china,IDs=sapply(china$names, function(x) x[1]),proj4string=p4s)
SPchina2 <-spTransform(SPchina,CRS("+proj=utm +zone=49 +datum=WGS84"))

op <- par()
par(mfrow=c(1,2),mar=c(2,2,1,0.1))
plot(SPchina,axes=TRUE,col="khaki2")
par(mar=c(2,4,1,0.1))
plot(SPchina2,axes=TRUE,col="khaki2")
par(op)
dev.off()


### -------------------------------------------------------------------
### chp6:矢量格式文件交换
### -------------------------------------------------------------------
ogrInfo(./data,scot)
scot_LL <- readOGR(dsn="data/scot.shp", layer="scot", integer64="allow.loss")
proj4string(scot_LL)
proj4string(scot_LL) <- CRS("+proj=longlat +ellps=WGS84")

drv <- "ESRI Shapefile"
writeOGR(scot_LL, dsn=".", layer="data/scot_LL", driver=drv, overwrite_layer=TRUE)
list.files("./data",pattern = "^scot_LL")

auck_el1 <- readGDAL("data/70042108.tif")
str(auck_el1,max.level=2)


### -------------------------------------------------------------------
### chp6:栅格格式文件交换 读取外部栅格文件示例
### -------------------------------------------------------------------
CairoPDF("readgdal_example1.pdf",10,5)
fn <- system.file("pictures/erdas_spnad83.tif", package = "rgdal")[1]
x <- readGDAL(fn)
str(x,max.level=2)
#y <- readGDAL(fn, offset=c(50, 100), region.dim=c(400, 400), output.dim=c(100,100))
y <- readGDAL(fn, offset=c(50, 100), region.dim=c(400, 400))
str(y,max.level=2)

op <- par()
par(mar = c(2,2,2,2))
layout(matrix(c(1,2),1,2),widths = c(5,5))
#layout.show(2)
image(x, col=grey(1:99/100),axes=TRUE)
image(y, col=grey(1:99/100),axes=TRUE)
par(op)
dev.off()

## layout(matrix(1:3, 1, 3), widths = c(6,4,1))
## plot(meuse.grid, what = "image", zlim = c(0,1))
## plot(meuse.grid["dist"], what = "image", zlim = c(0,1))
## plot(meuse.grid["dist"], what = "scale", zlim = c(0,1))

## layout(matrix(c(1,2,3),1,3),widths = c(5,5,1))
## plot(x, what="image",col=grey(1:99/100),axes=TRUE)
## plot(y, what="image",col=grey(1:99/100),axes=TRUE)
## plot(y,what="scale")


### -------------------------------------------------------------------
### chp6:栅格格式文件交换 readGDAL和writeGDAL函数示例
### -------------------------------------------------------------------
# 读取原始tiff格式文件到sp对象
auck_el1 <- readGDAL("data/70042108.tif")
is.na(auck_el1$band1) <- auck_el1$band1 <= 0 | auck_el1$band1 > 1e+4
# 自定义数据分类
brks <- c(0,10,20,50,100,150,200,300,400,500,600,700)
# 自定义渐变颜色方案
pal <- terrain.colors(11)
length(pal) == length(brks)-1
# 将数据按照等级进行划分
auck_el1$band1 <- findInterval(auck_el1$band1, vec=brks, all.inside=TRUE)-1
# 将sp对象导出到外部栅格文件，其中栅格要素按照自定义等级配色
writeGDAL(auck_el1, "figures/demIndex.tif", drivername="GTiff", type="Byte", colorTable=list(pal), mvFlag=length(brks)-1)
GDALinfo("data/demIndex.tif")


### -------------------------------------------------------------------
### chp6:栅格格式文件交换 GDAL.open函数示例
### -------------------------------------------------------------------
fn <- system.file("pictures/erdas_spnad83.tif", package = "rgdal")[1]
x <- GDAL.open(fn)
xx <- getDriver(x)
#xx #do not show pointer
getDriverLongName(xx)
#x #do not show pointer
dim(x)
y <- asSGDF_GROD(x,output.dim=c(400, 400))
GDAL.close(x)


### -------------------------------------------------------------------
### chp6:lattice空间绘图 基础点、线、面和栅格示例
### -------------------------------------------------------------------
CairoPDF("sp_plot1.pdf",10,5)
op <- par()
par(mfrow=c(1,4),mar=c(0.1,6,2,0.1))
data(meuse)
coordinates(meuse) <- c("x", "y")
par(mar=c(0.1,0.1,2,3))
plot(meuse)
title("points",cex.main=3)
cc <- coordinates(meuse)
m.sl <- SpatialLines(list(Lines(list(Line(cc)), "line1")))
par(mar=c(0.1,0.1,2,3))
plot(m.sl)
title("lines",cex.main=3)
data(meuse.riv)
meuse.lst <- list(Polygons(list(Polygon(meuse.riv)), "meuse.riv"))
meuse.pol <- SpatialPolygons(meuse.lst)
par(mar=c(0.1,0.1,2,3))
plot(meuse.pol, col = "grey")
title("polygons",cex.main=3)
data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
meuse.grid <- as(meuse.grid, "SpatialPixels")
par(mar=c(0.1,0.1,2,0.1))
image(meuse.grid, col = "grey")
title("grid",cex.main=3)
par(op)
dev.off()


### -------------------------------------------------------------------
### chp6:lattice空间绘图 add参数示例
### -------------------------------------------------------------------
CairoPDF("sp_plot2.pdf",5,5)
par(mar=c(0.1,0.1,0.1,0.1))
image(meuse.grid, col = "khaki2")
plot(meuse.pol, col = "lightsteelblue2", add = TRUE)
plot(meuse, add = TRUE, col = "brown", cex = .5)
dev.off()


### -------------------------------------------------------------------
### chp6:lattice空间绘图 绘制坐标轴和布局控制示例1
### -------------------------------------------------------------------
CairoPDF("sp_axis1.pdf",10,5)
CairoFonts(regular = "WenQuanYi Micro Hei", bold = "WenQuanYi Micro Hei")
layout(matrix(c(1,2),1,2))
par(mar=c(2,2,3,0.1))
plot(meuse.pol, axes = TRUE)
title("add=TRUE",cex.main=2)
plot(meuse.pol, axes = FALSE)
axis(1, at = c(178000 + 0:2 * 2000), cex.axis = .7)
axis(2, at = c(326000 + 0:3 * 4000), cex.axis = .7)
title("自定义坐标轴",cex.main=2)
box()
dev.off()


### -------------------------------------------------------------------
### chp6:lattice空间绘图 绘制坐标轴和布局控制示例2
### -------------------------------------------------------------------
CairoPDF("sp_axis2.pdf",10,5)
CairoFonts(regular = "WenQuanYi Micro Hei", bold = "WenQuanYi Micro Hei")
oldpar = par(no.readonly = TRUE)
layout(matrix(c(1,2),1,2))
plot(meuse, axes = TRUE, cex = 0.6)
plot(meuse.pol, add = TRUE)
title("示例位置",cex.main=2)
par(mar=c(0,0,0,0)+.1)
plot(meuse, axes = FALSE, cex = 0.6)
plot(meuse.pol, add = TRUE)
box()
par(oldpar)
dev.off()


### -------------------------------------------------------------------
### chp6:lattice空间绘图 绘制比例尺
### -------------------------------------------------------------------
CairoPDF("sp_mapelement1.pdf",5,5)
par(mar=c(0,0,0,0)+.1)
plot(meuse,axes=FALSE)
plot(meuse.pol, add=TRUE)
box()
SpatialPolygonsRescale(layout.scale.bar(), offset = c(180200,329600),
    scale = 1000, fill=c("transparent","black"), plot.grid = FALSE)
text(x = c(180200,181200), y = rep(329750, 2), c("0", "1 km"))
SpatialPolygonsRescale(layout.north.arrow(), offset = c(178750,332500),
                       scale = 400, plot.grid = FALSE)
dev.off()


### -------------------------------------------------------------------
### chp6:lattice空间绘图 绘制十进制度坐标刻度
### -------------------------------------------------------------------
CairoPDF("sp_mapelement2.pdf",10,5)
par(mar=c(2,2,1,0)+.1)
nc <- readOGR(dsn=system.file("shapes",package="maptools"),layer="sids")
proj4string(nc) <- CRS("+proj=longlat +datum=NAD27")
rrt <- nc$SID74/nc$BIR74
brks <- quantile(rrt, seq(0,1,1/5))
library(RColorBrewer)
cols <- brewer.pal(5, "Reds")
plot(nc, col=cols[findInterval(rrt, brks, all.inside=TRUE)], axes = FALSE)
box()
degAxis(1)
degAxis(2, at=34:37)
dev.off()


### -------------------------------------------------------------------
### chp6:lattice空间绘图 gridlines示例
### -------------------------------------------------------------------
CairoPDF("sp_mapelement3.pdf",10,5)
library(maptools)
library(maps)
wrld <- map("world", interior=FALSE, xlim=c(-179,179), 
   ylim=c(-89,89), plot=FALSE)
wrld_p <- pruneMap(wrld, xlim=c(-179,179))
llCRS <- CRS("+proj=longlat +ellps=WGS84")
wrld_sp <- map2SpatialLines(wrld_p, proj4string=llCRS)
prj_new <- CRS("+proj=moll +ellps=WGS84")
library(rgdal)
wrld_proj <- spTransform(wrld_sp, prj_new)
wrld_grd <- gridlines(wrld_sp, easts=c(-179,seq(-150,150,50), 179.5),              norths=seq(-75,75,15), ndiscr=100)
wrld_grd_proj <- spTransform(wrld_grd, prj_new)
at_sp <- gridat(wrld_sp, easts=0, norths=seq(-75,75,15), offset=0.3)
at_proj <- spTransform(at_sp, prj_new)
par(mar=c(0,0,0,0)+.1)
plot(wrld_proj, col="grey60")
plot(wrld_grd_proj, add=TRUE, lty=3, col="grey70")
text(coordinates(at_proj), pos=at_proj$pos, offset=at_proj$offset,                 labels=parse(text=as.character(at_proj$labels)), cex=1)
dev.off()

### -------------------------------------------------------------------
### chp6:lattice空间绘图 legend示例
### -------------------------------------------------------------------
CairoPDF("sp_mapelement4.pdf",5,5)
par(mar=c(0,0,0,0)+.1)
cols <- brewer.pal(4, "Accent")
image(zn.idw, col = cols, breaks=log(c(100,200,400,800,1800)))
plot(meuse.pol, add = TRUE)
plot(meuse, pch = 1, cex = sqrt(meuse$zinc)/20, add = TRUE)
legVals <- c(100, 200, 500, 1000, 2000)
legend("left", legend=legVals, pch = 1, pt.cex = sqrt(legVals)/20, bty = "n",
  title="measured, ppm", cex=1.2, y.inter=1)
legend("topleft", fill = cols, legend=c("100-200","200-400","400-800","800-1800"),
       bty = "n", title = "interpolated, ppm", cex=1.2, y.inter=1)
dev.off()

### -------------------------------------------------------------------
### chp6:lattice空间绘图 绘图交互
### -------------------------------------------------------------------
CairoPDF("sp_mapelement5.pdf",5,5)
par(mar=c(0,0,0,0)+.1)
plot(meuse,axes=FALSE)
plot(meuse.pol, add=TRUE)
box()
SpatialPolygonsRescale(layout.scale.bar(), offset = locator(1),
    scale = 1000, fill=c("transparent","black"), plot.grid = FALSE)
text(locator(1), "0")
text(locator(1), "1 km")
SpatialPolygonsRescale(layout.north.arrow(), offset = locator(1),
                       scale = 400, plot.grid = FALSE)
dev.off()

### -------------------------------------------------------------------
### chp6:lattice空间绘图 spplot函数示例
### -------------------------------------------------------------------
CairoPDF("spplot1.pdf",10,10.5)
CairoFonts(regular = "WenQuanYi Micro Hei", bold = "WenQuanYi Micro Hei")
data(meuse)
coordinates(meuse) <- ~x+y
## lattice.options(
##   layout.heights=list(bottom.padding=list(x=0, top.padding=list(x=-0.5)),
##   layout.widths=list(left.padding=list(x=0), right.padding=list(x=0))
## )
cuts=c(0,20,50,200,500,2000)
grys <- brewer.pal(7, "Reds")
meuse$lead.st = as.vector(scale(meuse$lead))
meuse$zinc.st = as.vector(scale(meuse$zinc))
meuse$copper.st = as.vector(scale(meuse$copper))
meuse$cadmium.st = as.vector(scale(meuse$cadmium))
l2 = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(178750,332500), 
	scale = 400)
l3 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(180500,329800), 
	scale = 500, fill=c("transparent","black"))
l4 = list("sp.text", c(180500,329950), "0")
l5 = list("sp.text", c(181000,329950), "500 m")
cuts=c(-1.2,0,1,2,3,5)
spplot(meuse, c("cadmium.st", "copper.st", "lead.st", "zinc.st"),
       sp.layout=list(l2,l3,l4,l5), layout=c(2,2), aspect=1,
       key.space="right", main=list("标准差",cex=2),
       par.strip.text=list(cex=2),
       cuts = cuts,col.regions=grys)
dev.off()


### -------------------------------------------------------------------
### chp6:lattice空间绘图 添加布局项
### -------------------------------------------------------------------
CairoPDF("spplot2.pdf",9,11)
data(meuse)
coordinates(meuse) <- ~x+y
data(meuse.grid)
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- T
zn <- krige(zinc~1,meuse,meuse.grid)
zn$direct <- zn$var1.pred
zn$log <- exp(krige(log(zinc)~1,meuse,meuse.grid)$var1.pred)
data(meuse.riv)
meuse.lst <- list(Polygons(list(Polygon(meuse.riv)), "meuse.riv"))
meuse.pol <- SpatialPolygons(meuse.lst)
river <- list("sp.polygons", meuse.pol)
north <- list("SpatialPolygonsRescale", layout.north.arrow(), offset = 
  c(178750,332500),
    scale = 400)
scale <- list("SpatialPolygonsRescale", layout.scale.bar(), offset = 
  c(180200, 329800), scale = 1000, fill=c("transparent","black"))
txt1 <- list("sp.text", c(180200, 329950), "0")
txt2 <- list("sp.text", c(181200, 329950), "1 km")
pts <- list("sp.points", meuse, pch = 3, col = "black")
meuse.layout <- list(river, north, scale, txt1, txt2, pts)
grys <- brewer.pal(7, "Reds")
spplot(zn["log"], sp.layout = meuse.layout, cuts=5, aspect=4/3, col.regions=grys)
dev.off()

### -------------------------------------------------------------------
### chp6:lattice空间绘图 协克里金方差矩阵的统计图形展示
### -------------------------------------------------------------------
CairoPDF("spplot3.pdf",9,11)
g <- gstat(NULL, "logCd", log(cadmium)~1, meuse)
g <- gstat(g, "logCu", log(copper)~1, meuse)
g <- gstat(g, "logPb", log(lead)~1, meuse)
g <- gstat(g, "logZn", log(zinc)~1, meuse)
g
vm <- variogram(g)
vm.fit <- fit.lmc(vm, g, vgm(1, "Sph", 800, 1))
cok.maps <- predict(vm.fit, meuse.grid)
names(cok.maps)
pal = function(n = 9) brewer.pal(n, "BrBG")
print(spplot.vcov(cok.maps, cuts=6, col.regions=pal(7)))
dev.off()


### -------------------------------------------------------------------
### chp6:ggplot2空间绘图 绘制矢量空间数据示例
### -------------------------------------------------------------------
CairoPDF("spggplot1.pdf",6,7.5)
data(meuse)
coordinates(meuse) <- ~x+y
m <- as(meuse,"data.frame")
ggplot(m, aes(x, y)) + geom_point() + coord_equal()
dev.off()

### -------------------------------------------------------------------
### chp6:ggplot2空间绘图 伦敦运动参与示例
### -------------------------------------------------------------------
CairoPDF("spggplot2.pdf",6,4)
sport <- readOGR(dsn = "./data/", "london_sport")
sport.f <- fortify(sport, region = "ons_label")
sport.f <- merge(sport.f, sport@data, by.x = "id", by.y = "ons_label")
Map <- ggplot(sport.f, aes(long, lat, group = group, fill = Partic_Per)) +
    geom_polygon() + geom_path(colour="white",lwd=0.3) +
    coord_equal() + labs(x = "Easting (m)", y = "Northing (m)", fill = "% Sport Partic.") + 
    ggtitle("London Sports Participation")
Map + scale_fill_gradient(low = "green", high = "red") +
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
#ggsave("spggplot2.pdf")
dev.off()

### -------------------------------------------------------------------
### chp6:ggplot2空间绘图 伦敦历年人口变化示例
### -------------------------------------------------------------------
CairoPDF("spggplot3.pdf",10,7)
library(reshape2)
london.data <- read.csv("data/census-historic-population-borough.csv")
london.data <- subset(london.data, select=-c(Pop_1801))
london.data.melt <- melt(london.data, id = c("Area.Code", "Area.Name"))
plot.data <- merge(sport.f, london.data.melt, by.x = "id", by.y = "Area.Code")
ggplot(data = plot.data, aes(x = long, y = lat, fill = value, group = group)) + 
    geom_polygon() + geom_path(colour = "white", lwd = 0.3) + coord_equal() + 
    facet_wrap(~variable)
dev.off()

### -------------------------------------------------------------------
### chp6:ggplot2空间绘图 geom_sf示例
### -------------------------------------------------------------------
CairoPDF("spggplot4.pdf",10,3.5)
devtools::install_github("tidyverse/ggplot2")
require(ggplot2)
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
                                        #ggplot(nc) + geom_sf(aes(fill = AREA))
nc_3857 <- sf::st_transform(nc, "+init=epsg:3857")
nc_3857$mid <- sf::st_centroid(nc_3857$geometry)
ggplot(nc_3857) +
    geom_sf(aes(fill = AREA), colour = "white", lwd = 0.5) +
    geom_sf(aes(geometry = mid, size = BIR74), show.legend = "point") +
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
dev.off()

### -------------------------------------------------------------------
### chp6:ggplot2空间绘图 绘制栅格空间数据示例
### -------------------------------------------------------------------
CairoPDF("spraster1.pdf",10,3.5)
library(rasterVis)
library(raster)
carmel_bay <- brick("data/carmel_bay_bathy.tif")
bay_spdf <- as(carmel_bay, "SpatialPixelsDataFrame")
bay_df <- as.data.frame(bay_spdf)
bay_df <- melt(bay_df, id=c("x","y"))
ggplot() +
    geom_raster(data=bay_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
    scale_fill_viridis() +
    coord_equal() +
    facet_wrap(~variable) +
    theme(panel.spacing = unit(0.2, "in"))
dev.off()

### -------------------------------------------------------------------
### chp6:ggplot2空间绘图 rasterVis示例
### -------------------------------------------------------------------
CairoPDF("spraster2.pdf",8,5)
r <- raster(system.file("external/test.grd", package="raster"))
s <- stack(r, r*2)
names(s) <- c('meuse', 'meuse x 2')
theme_set(theme_bw())
gplot(s) + geom_tile(aes(fill = value)) +
    scale_fill_gradientn(colours = viridis(256, option = "C")) + 
    coord_equal() + facet_wrap(~ variable)
dev.off()

### -------------------------------------------------------------------
### chp6:ggplot2空间绘图 绘制地图要素示例1
### -------------------------------------------------------------------
devtools::install_github('oswaldosantos/ggsn')
CairoPDF("ggsn1.pdf",8,10)
library(ggsn);library(sf)
dsn <- system.file('extdata', package = 'ggsn')
map <- st_read(dsn, 'sp', quiet = TRUE)
ggplot(map, aes(fill = nots)) +
    geom_sf() +
    scale_fill_brewer(name = 'Animal abuse\nnotifications', palette = 8) +
    north(map) +
    scalebar(map, dist = 5, dd2km = TRUE, model = 'WGS84')
dev.off()


### -------------------------------------------------------------------
### chp6:ggplot2空间绘图 绘制地图要素示例2
### -------------------------------------------------------------------
CairoPDF("ggsn2.pdf",8,10)
# GCS转换到PCS
map2 <- st_transform(map, 31983)
ggplot(map2) +
    geom_sf(aes(fill = nots)) +
    north(map2, symbol = 16, scale = 0.15) +
    scale_fill_brewer(name = 'Animal abuse\nnotifications', palette = 8) +
    scalebar(map2, dist = 5, dd2km=FALSE) +
    coord_sf(datum = st_crs(31983)) +
    xlab('Meters') +
    ylab('Meters')
dev.off()


### -------------------------------------------------------------------
### chp6:ggplot2空间绘图 ggspatial示例
### -------------------------------------------------------------------
CairoPDF("ggspatial1.pdf",8,10)
library(ggspatial)
data(longlake_waterdf)
ggspatial(longlake_waterdf, fill = "lightblue")
dev.off()

CairoPDF("ggspatial2.pdf",8,10)
#
ggosm() + 
    geom_spatial(longlake_waterdf, fill = "lightblue", alpha=1)
dev.off()


### -------------------------------------------------------------------
### chp6:ggplot2空间绘图 ggmap示例
### -------------------------------------------------------------------
CairoPDF("ggmap.pdf",8,10)
library(ggmap)
library(sp)
library(rgdal)
library(broom)
sp <- get_googlemap("圣保罗")
bb <- c(st_bbox(map) * matrix(rep(c(1.001, 0.999), e = 2), ncol = 2))
nms <- names(attr(sp, "bb"))
attr(sp, "bb")[1, ] <- bb[c(2, 1, 4, 3)]

map_sp <- readOGR(dsn, "sp")
map_sp@data$id <- 0:(nrow(map_sp@data) - 1)
map_sp <- merge(tidy(map_sp), map_sp, by = 'id')

ggmap(sp) +
    geom_polygon(data = map_sp, aes(long, lat, group = group, fill = nots),
                 alpha = .7) +
    coord_equal() +
    geom_path(data = map_sp, aes(long, lat, group = group)) +
    blank() +
    scalebar(map_sp, dist = 5, dd2km = T, model = 'WGS84') +
    north(map) +
    scale_fill_brewer(name = 'Animal abuse\nnotifications', palette = 8) +
    theme(legend.position = c(0.9, 0.35))
dev.off()

library(ggmap)
 
# load the data
tartu_housing <- read.csv("data/tartu_housing_xy_wgs84_a.csv", sep = ";")
 
# Download the base map
tartu_map_g_str <- get_map(location = "tartu", zoom = 13)
# Draw the heat map
ggmap(tartu_map_g_str, extent = "device") + geom_density2d(data = tartu_housing, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = tartu_housing, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)


