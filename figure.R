setwd("~/Documents/tex_projects/r_guide_slide")


png("expression-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar=c(2,2,0,0)+0.1)
plot(1:10, 1:10)
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(4, 8.4, "expression(hat(beta) == (X^t * X)^{-1} * X^t * y)", cex = .8)
text(4, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
text(4, 6.4, "expression(bar(x) == sum(frac(x[i], n), i==1, n))", cex = .8)
text(8, 5, expression(paste(frac(1, sigma*sqrt(2*pi)), " ", plain(e)^{frac(-(x-mu)^2, 2*sigma^2)})), cex = 1.2)
dev.off()


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


png("高级绘图函数.png",res=300)
x = runif(100); y = 0.2*x + 0.1*rnorm(100)
opar <- par()
par(mar=c(2,2,0.1,0.1))
plot(x,y)
par(opar)
dev.off()


png("低级绘图函数.png",res=300)
opar <- par()
par(mar=c(2,2,0.1,0.1))
plot(x,y)
fit=lm(y~x)
abline(fit)
par(opar)
dev.off()


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


png("rgb-bar.png",width=1600,height=600,res=300,pointsize=3)
par(mar = c(0.2, 2, 1, 0) + 0.1, yaxs = "i")
x = rgb(1, seq(0, 1, length = 20), 0)
barplot(rep(1, 20), col = x)
dev.off()


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


png("heat_colors.png")
par(mar = c(2,2,2,0) + 0.1)
image(x, y, volcano, col=heat.colors(100), axes=FALSE)
contour(x, y, volcano, levels=seq(90, 200, by=5), add=TRUE, col="brown")
axis(1, at=x.at)
axis(2, at=y.at)
box()
title(main="col=heat.colors(100)", font.main=4)
dev.off()


png("gray_colors.png")
par(mar = c(2,2,2,0) + 0.1)
image(x, y, volcano, col=gray(100:200/200), axes=FALSE)
contour(x, y, volcano, levels=seq(90, 200, by=5), add=TRUE, col="black")
axis(1, at=x.at)
axis(2, at=y.at)
box()
title(main="col=gray(100:200/200)", font.main=4)
dev.off()


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


##-------- Showing all the extra & some char graphics symbols ---------
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


#library(cairoDevice)
#Cairo_png("line-example.png",width=15,height=15)
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


png("grid-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(2.5,2.5,0,0) + 0.1)
with(iris,
     {
     plot(Sepal.Length, Sepal.Width, col = as.integer(Species),
          panel.first = grid(8, lty = 1, lwd = 2))
     }
     )
dev.off()


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


png("barplot-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(4, 4, 4, 0)+0.1)
library(RColorBrewer)
par(mfrow = c(2, 1), mar = c(3, 2.5, 0.5, 0.1))
death = t(VADeaths)[, 5:1]
barplot(death, col = brewer.pal(4, "Set1"))
barplot(death, col = brewer.pal(4, "Set1"), beside = TRUE, legend = TRUE)
dev.off()


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


png("persp-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(0, 0, 0, 0))
data(ChinaLifeEdu, package="MSG")
x = ChinaLifeEdu
library(KernSmooth)
est = bkde2D(x, apply(x, 2, dpik))
persp(est[["x1"]], est[["x2"]], est[["fhat"]], shade = 0.75, col = "lightblue", phi = 20, theta = 15, box = TRUE)
dev.off()


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


png("smoothscatter-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(2, 2, 0.1, 0.1))
data(BinormCircle, package="MSG")
smoothScatter(BinormCircle)
dev.off()


png("heatmap-example.png",width=1000,height=1000,res=300,pointsize=6)
heatmap(as.matrix(mtcars), col = brewer.pal(9, "RdYlBu"), scale = "column", margins = c(4, 8))
dev.off()


png("vioplot-example.png",width=1000,height=1000,res=300,pointsize=6)
library(vioplot)
par(mar = c(2, 2, 0.1, 0.1))
x <- rnorm(100)
y <- rnorm(100)
plot(x, y, xlim=c(-5,5), ylim=c(-5,5))
vioplot(x, col="tomato", horizontal=TRUE, at=-4, add=TRUE,lty=2, rectCol="gray")
vioplot(y, col="cyan", horizontal=FALSE, at=-4, add=TRUE,lty=2)
dev.off()


png("map-example.png",width=1000,height=1000,res=300,pointsize=6)
library(maps)
par(mar = c(0.1, 0.1, 0.1, 0.1))
map("state", interior = FALSE)
map("state", boundary = FALSE, lty = 2, add = TRUE)
dev.off()


png("teachingdemos-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(0.1, 0.1, 0.1, 0.1))
library(TeachingDemos)
faces2(mtcars[, c("hp", "disp", "mpg", "qsec", "wt")], which = c(14, 9, 11, 6, 5), adj = c(0.5, 0))
dev.off()


png("parcoord-example.png",width=1000,height=1000,res=300,pointsize=6)
par(mar = c(2, 0.3, 0.1, 0.3))
library(MASS)
ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
parcoord(log(ir)[, c(3, 4, 2, 1)], col = 1 + (0:149)%/%50)
dev.off()

