setwd("~/Documents/tex_projects/r_guide_slide")
library(ggplot2)
library(Cairo)
library(ggplot2movies)
library(plyr)

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


png("lattice-parameter1.png")
densityplot(~mpg, data=mtcars)
dev.off()

png("lattice-parameter2.png")
densityplot(~mpg|cyl, data=mtcars,layout=c(1,3))
dev.off()

png("lattice-parameter3.png")
densityplot(~mpg, groups=cyl, data=mtcars)
dev.off()

png("lattice-parameter4.png")
EE <- equal.count(ethanol$E, number=9, overlap=1/4)

## Constructing panel functions on the fly; prepanel
xyplot(NOx ~ C | EE, data = ethanol)
dev.off()



png("lattice-parameter4.png")
EE <- equal.count(ethanol$E, number=9, overlap=1/4)

## Constructing panel functions on the fly; prepanel
xyplot(NOx ~ C | EE, data = ethanol,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       xlab = "Compression Ratio", ylab = "NOx (micrograms/J)",
       panel = function(x, y) {
           panel.grid(h = -1, v = 2)
           panel.xyplot(x, y)
           panel.loess(x, y, span=1)
       },
       aspect = "xy")
dev.off()

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
p <- xyplot(c(-1, 1) ~ c(-1, 1), aspect = 1, cycles = 15, scales = list(draw = FALSE), xlab = "", ylab = "", panel = panel.hypotrochoid)
p[rep(1, 9)]
dev.off()

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

CairoPNG("show_settings.png",width=1200*3,height=800*3,res=72*5)
show.settings()
dev.off()

CairoPNG("line-art1.png",width=1000*3,height=1000*3,res=72*3)
x1 = c(seq(0, pi, length = 50), seq(pi, 2*pi, length = 50))
y1 = cos(x1) / sin(x1)
x2 = seq(1.02 * 2 * pi + pi/2, 4*pi + pi/2, length = 50)
y2 = tan(x2)
op = par(bg="black", mar=rep(.5,4))
plot(c(x1, x2), c(y1, y2), type = "n", ylim = c(-11, 11))
for (i in seq(-10, 10, length = 100))
{
  lines(x1, y1 + i, col = hsv(runif(1,.65,.7), 1, 1, runif(1,.7)),
        lwd = 4 * runif(1, 0.3))
  lines(x2, y2 + i, col = hsv(runif(1,.65,.7), 1, 1, runif(1,.7)),
        lwd = 4 * runif(1, 0.3))
}
dev.off()

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
for (i in seq(2000, 2300, 10)){
lines(y - i, x, lwd = 2*runif(1), col = hsv(0, 1, 1, alpha = runif(1, 0.5, 0.9)))
}
for (i in seq(100, 150, 10)){
lines(y - i, x, lwd = 2*runif(1), col = hsv(0, 1, 1, alpha = runif(1, 0.5, 0.9)))
}
dev.off()

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

                                        
CairoPDF("ggplot_mapping1.pdf",10,5)
ggplot(mtcars,aes(mpg,wt,colour=cyl)) +
    geom_point() +
    theme(axis.title.x =element_text(size=14), axis.title.y=element_text(size=14))
dev.off()

CairoPDF("ggplot_mapping2.pdf",10,5)
library(nlme)
ggplot(Oxboys, aes(age,height, group= Subject)) + geom_line() +
    geom_smooth(aes(group=1), method = "lm", size = 2, se=F)
dev.off()

CairoPDF("ggplot_scales.pdf",10,5)
p <- qplot(sleep_total, sleep_cycle, data = msleep, colour = vore)
p + scale_colour_hue("What does\nit eat?",
breaks = c("herbi", "carni", "omni", NA),
labels = c("plants", "meat", "both", "don’t know"))
dev.off()

CairoPDF("ggplot_facet2.pdf",10,5)
mpg2 <- subset(mpg, cyl != 5 & drv %in% c("4", "f"))
ggplot(mpg2, aes(cty,hwy)) + geom_point() + facet_grid(drv ~ cyl)
dev.off()

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
CairoPDF("ggplot_facet4_1.pdf",5,5)
ggplot(mpg,aes(cty, hwy)) + geom_point() +
    facet_wrap(~ cyl, scales="fixed")
dev.off()
CairoPDF("ggplot_facet4_2.pdf",5,5)
ggplot(mpg,aes(cty, hwy)) + geom_point() +
    facet_wrap(~ cyl, scales = "free")
dev.off()


#CairoPNG("ggplot_position1_1.png",width=500,height=500,res=72)
#CairoPNG("ggplot_position1_1.png",width=500,height=500,res=72)
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

rect <- data.frame(x = 50, y = 50)
line <- data.frame(x = c(1, 200), y = c(100, 1))
#CairoPNG("ggplot_coord1_1.png",width=500,height=500,res=72)
CairoPDF("ggplot_coord1_1.pdf",5,5)
ggplot(mapping = aes(x, y)) + 
  geom_tile(data = rect, aes(width = 50, height = 50)) + 
  geom_line(data = line)
dev.off()
#CairoPNG("ggplot_coord1_2.png",width=500,height=500,res=72)
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

ggplot(mpg, aes(x=cty, y=hwy))+
    #geom_point(aes(colour=class,size=displ),alpha=0.6,position = "jitter")+  
    #stat_smooth()+  
    #scale_size(range = c(5, 10))+
    facet_wrap(~ year,ncol=2)
