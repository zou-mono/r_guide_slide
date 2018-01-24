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
par(mar = c(0, 6, 0, 0) + 0.1, height=30, yaxs = "i")
barplot(rep(1,20), col = c("tomato","lightcyan4","navajowhite1",
"gray5","seagreen3","ghostwhite","grey60","cornsilk4","linen","darkblue",
"gray26","steelblue1","grey","plum4","violet","gray83","red",
"thistle1","palegreen4","oldlace"),names.arg = c("tomato","lightcyan4","navajowhite1",
"gray5","seagreen3","ghostwhite","grey60","cornsilk4","linen","darkblue",
"gray26","steelblue1","grey","plum4","violet","gray83","red",
"thistle1","palegreen4","oldlace"), horiz = TRUE, las = 1,
xaxt = "n")
dev.off()
