setwd("~/Documents/tex_projects/r_guide_slide")
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


x = runif(100); y = 0.2*x + 0.1*rnorm(100)
png("高级绘图函数.png",res=300)
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
par(mfrow = c(3, 3), mar = c(2, 2.5, 3, 2))
for (i in c("p", "l", "b", "c", "o", "h", "s", "S","n")) {
plot(c(1:5, 5:1), type = i, main = paste("Plot type: \"",
i, "\"", sep = ""), xlab = "")
}
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


library(cairoDevice)
Cairo_png("line-example.png",width=15,height=15)
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

library(xspline)
png("open-xspline.png")
op <- par(mfrow = c(3,3), mar = rep(0,4), oma = c(0,0,2,0))
xsplineTest <- function(s, open = TRUE, x = c(1,1,3,3)/4, y = c(1,3,3,1)/4, ...) {
    plot(c(0,1), c(0,1), type = "n", axes = FALSE, xlab = "", ylab = "")
    points(x, y, pch = 19)
    xspline(x, y, s, open, ...)
    text(x+0.05*c(-1,-1,1,1), y+0.05*c(-1,1,1,-1), s)
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
title("Open X-splines", outer = TRUE)
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
          panel.first = grid(3, lty = 1, lwd = 2))
     }
     )
dev.off()
