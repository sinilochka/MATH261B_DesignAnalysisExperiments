## This is the smart way to plot things, rather than specifying each set of lines and points by hand.
				
eggplot = ggplot(data = eggs.ind, aes(x = Trial, y = Score, color = Tester, size = Tester, linetype = Tester)) + geom_point(size = 2) + geom_line() +
				geom_line(data = eggs.mean, aes(y = Score, color = "Mean", linetype = "Mean", size = "Mean")) +
				ggtitle("Figure #: Tester responses versus run order") + scale_x_continuous("Trial number", breaks=1:16) + scale_y_continuous("Score",breaks=3:15) +
				scale_colour_manual(name = "Tester", values = c("CDSK" = cols[1], "CLK" = cols[2], "SJ" = cols[3], "SK" = cols[4], "Mean" = "black"), breaks = c("CDSK", "CLK", "SJ", "SK", "Mean")) +
				scale_linetype_manual(name = "Tester", values = c("CDSK" = 2, "CLK" = 2, "SJ" = 2, "SK" = 2, "Mean" = 1), breaks = c("CDSK", "CLK", "SJ", "SK", "Mean")) +
				scale_size_manual(name = "Tester", values = c("CDSK" = 0.5, "CLK" = 0.5, "SJ" = 0.5, "SK" = 0.5, "Mean" = 1.5), breaks = c("CDSK", "CLK", "SJ", "SK", "Mean")) +
				theme(legend.position = "top", axis.title.x = element_text(vjust = -1), axis.title.y = element_text(vjust = 1.5), plot.title = element_text(face = "bold"), plot.margin=unit(c(1,1,1.5,1.5),"lines"))

## Plots for normality assumptions:
egg.hist = qplot(eggs.ind$Score, geom="histogram", binwidth = 1)
egg.hist.l = qplot(log(Grand.Mean), geom="histogram", binwidth = 0.1)

##Can't really say on the basis of sixteen samples that the distribution of scores is at all normal--we're seeing a skew toward the higher scores. Logged scores aren't a whole lot better. Do we need the normality assumption here or are we okay without it? Talk to Professor Holt about this, and/or reread the book.
##Nope, scratch that. Check the qqplots:

##QQ plot for the mean response
qplot(data = eggs.mean, sample = Score) + geom_abline(intercept = 10.5, slope = 3, linetype = 3) + labs(x = "Theoretical quantiles", y = "Response means", title = "Figure #: QQ plot for response means") + theme(axis.title.x = element_text(vjust = -1), axis.title.y = element_text(vjust = 1.5), plot.title = element_text(face = "bold", vjust = 1.5), plot.margin=unit(c(1,1,1.5,1.5),"lines"))

##QQ plots by tester, totally unnecessary but lets me use facet_wrap.
qplot(sample = Score, color = Tester, data = eggs.ind.mean) + facet_wrap(~ Tester, nrow = 2, ncol = 3) + geom_abline(intercept = 10.5, slope = 3, linetype = 3) + labs(x = "Theoretical quantiles", y = "Response scores", title = "Figure #: QQ plots for all responses") + theme(axis.title.x = element_text(vjust = -1), axis.title.y = element_text(vjust = 1.5), plot.title = element_text(face = "bold", vjust = 1.5), plot.margin=unit(c(1,1,1.5,1.5),"lines"))

qplot(sample = Score, data = eggs.ind.mean, color = Tester)

## Lesson learned: Histograms are dumb when you have a small number of observations, so use a qq plot instead.

## Plots for residual analysis:
## MEANS MODEL
## QQ plot of residuals from the final means model, A + B + D + ABD.
qplot(sample = Residuals, data = egg.res) + geom_abline(intercept = 0, slope = 1, linetype = 3) + labs(x = "Theoretical quantiles", y = "Standardized residuals", title = "Figure #: QQ plot for standardized residuals") + theme(axis.title.x = element_text(vjust = -1), axis.title.y = element_text(vjust = 1.5), plot.title = element_text(face = "bold", vjust = 1.5), plot.margin=unit(c(1,1,1.5,1.5),"lines"))

##Plot of residuals versus fitted values for the final means model.
qplot(fitted(eggs.aov.r2),rstandard(eggs.aov.r2), geom="point") + geom_hline(yintercept = 1, linetype = 2) + geom_hline(yintercept = -1, linetype = 2) + labs(y = "Standardized residuals", title = "Figure #: Residuals versus fitted values") + theme(axis.title.x = element_text(vjust = -1), axis.title.y = element_text(vjust = 1.5), plot.title = element_text(face = "bold", vjust = 1.5), plot.margin=unit(c(1,1,1.5,1.5),"lines")) + scale_x_continuous("Fitted values")

##Plot of residuals versus run order for the final means model.
rr = qplot(data = egg.res, x = Run, y = Residuals) + geom_hline(yintercept = 1, linetype = 2) + geom_hline(yintercept = -1, linetype = 2) + labs(y = "Standardized residuals", title = "Figure #: Residuals versus run order") + theme(axis.title.x = element_text(size = 9, vjust = -0.25), axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.y = element_text(vjust = 1.5, size = 9), plot.title = element_text(vjust = 1.5, size = 10, face = "bold"), plot.margin=unit(c(1,1,1.5,1.5),"lines")) + scale_x_continuous("Trial number", breaks=1:16)

##Plots of residuals versus factor levels for the final means model.
r1 = qplot(data = egg.res, x = A, y = Residuals) + labs(y = "Standardized residuals", title = "Figure #: Residuals versus factor A\n (Fat used)") + theme(axis.title.x = element_text(size = 9), axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.y = element_text(vjust = 1.5, size = 9), plot.title = element_text(vjust = 1.5, size = 10, face = "bold"), plot.margin=unit(c(1,1,1.5,1.5),"lines")) + scale_x_continuous("Fat used", breaks = c(-1, 1), labels=c("Butter", "Oil"))
r2 = qplot(data = egg.res, x = B, y = Residuals) + labs(y = "Standardized residuals", title = "Figure #: Residuals versus factor B\n (Fraction of egg)") + theme(axis.title.x = element_text(size = 9), axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.y = element_text(vjust = 1.5, size = 9), plot.title = element_text(vjust = 1.5, size = 10, face = "bold"), plot.margin=unit(c(1,1,1.5,1.5),"lines")) + scale_x_continuous("Fraction of egg", breaks = c(-1, 1), labels=c("White", "Whole"))
r3 = qplot(data = egg.res, x = D, y = Residuals) + labs(y = "Standardized residuals", title = "Figure #: Residuals versus factor D\n (Dairy mix-in)") + theme(axis.title.x = element_text(size = 9), axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.y = element_text(vjust = 1.5, size = 9), plot.title = element_text(vjust = 1.5, size = 10, face = "bold"), plot.margin=unit(c(1,1,1.5,1.5),"lines")) + scale_x_continuous("Dairy mix-in", breaks = c(-1, 1), labels=c("Non-fat milk", "Sour cream"))

grid.arrange(rr, r1, r2, r3, ncol = 2)

##Also see if you can figure out how to do a cube plot...
##Later: Plots for the standard deviation model.


##All the data set-up is down here.
##Data for initial exploratory plots:
setwd("/Users/coronaviridae/Documents/School/Stanford & SJSU 2014-2016/2015 Spring/MATH 261B/Final Project")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

cols = gg_color_hue(4)

require(gdata) ## For loading in xlsx files.
require(FrF2) ## Just for the DanielPlot, since I don't think I can make ggplot do it.
require(ggplot2) ## Our workhorse for making pretty plots.
require(rsm) ## I forgot why this is in here.
require(grid) ## For units().
require(gridExtra) ## For grid.arrange().

eggs = read.xls("eggsperiment data.xlsx")

##This could undoubtedly be reshaped with melt(). If you have time, try that out for getting the SDs into a plottable format.
eggs.ind.mean = data.frame(as.numeric(rep(1:16, 5)), rep(c("CLK","CDSK","SJ","SK", "Mean"), each=16), as.numeric(c(eggs$CLK, eggs$CDSK, eggs$SJ, eggs$SK, eggs$Grand.Mean)))
colnames(eggs.ind.mean) = c("Trial","Tester","Score")
eggs.ind <- subset(eggs.ind.mean, !Tester=="Mean")
eggs.mean <- subset(eggs.ind.mean, Tester=="Mean", select=c(Trial, Score))

##This is probably a dumb way to subset but whatever, reusing code right now rather than rewriting it to be less idiotic.

##Data for residual analysis and the like:
##These are of course identical but whatever.
eggs.lm.final <- lm(Grand.Mean ~ A + B + D + A:B:D)
eggs.aov.r2 <- aov(Grand.Mean ~ A + B + D + A:B:D)

##Need to squash a residual df together for using with ggplot.
egg.res=data.frame(eggs$Run.Order,eggs$A,eggs$B,eggs$D,rstandard(eggs.aov.r2))
colnames(egg.res) = c("Run","A","B","D","Residuals")

cdsk <- subset(eggs.ind.mean, Tester=="CDSK", select=c(Trial, Score))
clk <- subset(eggs.ind.mean, Tester=="CLK", select=c(Trial, Score))
sj <- subset(eggs.ind.mean, Tester=="SJ", select=c(Trial, Score))
sk <- subset(eggs.ind.mean, Tester=="SK", select=c(Trial, Score))

eggplot = ggplot() + geom_point(data = eggs.ind, aes(x = Trial, y = Score, colour = Tester), size = 2) + xlab("Trial Number") + ylab("Score") + ggtitle("Figure 1: Tester Responses to Eggs") + scale_x_continuous(breaks=1:16) + scale_y_continuous(breaks=3:15) + geom_line(data = eggs.ind, aes(x = Trial, y = Score, colour = Tester), linetype = 2) + theme(legend.position = "top")
eggplot + geom_point(data = eggs.mean, aes(x = Trial, y = Score), size = 3) + geom_line(data = eggs.mean, aes(x = Trial, y = Score), color = "black", size = 1.5)

eggplot.mean = qplot(eggs.ind.mean$Trial, eggs.ind.mean$Score, colour = eggs.ind.mean$Tester, geom = "point", main = "Individual Tester Responses to Eggs", xlab = "Trial Number", ylab = "Rating")
eggplot.mean + theme(legend.position = "top") + labs(color = "Response") + scale_x_continuous(breaks=1:16) + scale_y_continuous(breaks=3:15) + geom_line() ## Do some work on this later to desaturate or fade the individual lines and turn the line for the mean black or another dark color so it pops. Also figure out how to pull it out on top of the other lines.

## This is just terrible and way too much work, since specifying scales means we don't have to DO this stuff.

eggplot = ggplot(data = eggs.ind.mean, aes(x = Trial)) + 
				geom_point(data = cdsk, aes(y = Score, color = "CDSK")) + geom_line(data = cdsk, aes(y = Score, color = "CDSK", linetype = "CDSK", size = "CDSK")) +
				geom_point(data = clk, aes(y = Score, color = "CLK")) + geom_line(data = clk, aes(y = Score, color = "CLK", linetype = "CLK", size = "CLK")) +
				geom_point(data = sj, aes(y = Score, color = "SJ")) + geom_line(data = sj, aes(y = Score, color = "SJ", linetype = "SJ", size = "SJ")) +
				geom_point(data = sk, aes(y = Score, color = "SK")) + geom_line(data = sk, aes(y = Score, color = "SK", linetype = "SK", size = "SK")) +
				geom_line(data = eggs.mean, aes(y = Score, color = "Mean", linetype = "Mean", size = "Mean")) +
				ggtitle("Figure 1: Tester Responses to Eggs") + scale_x_continuous("Trial Number", breaks=1:16) + scale_y_continuous("Score",breaks=3:15) +
				scale_colour_manual(name = "Tester", values = c("CDSK" = cols[1], "CLK" = cols[2], "SJ" = cols[3], "SK" = cols[4], "Mean" = "black"), breaks = c("CDSK", "CLK", "SJ", "SK", "Mean")) +
				scale_linetype_manual(name = "Tester", values = c("CDSK" = 2, "CLK" = 2, "SJ" = 2, "SK" = 2, "Mean" = 1), breaks = c("CDSK", "CLK", "SJ", "SK", "Mean")) +
				scale_size_manual(name = "Tester", values = c("CDSK" = 0.5, "CLK" = 0.5, "SJ" = 0.5, "SK" = 0.5, "Mean" = 1.5), breaks = c("CDSK", "CLK", "SJ", "SK","Mean")) +
				theme(legend.position = "top", axis.title.x = element_text(vjust = -1), axis.title.y = element_text(vjust = 1.5), plot.title = element_text(face = "bold"), plot.margin=unit(c(1,1,1.5,1.5),"lines"))
