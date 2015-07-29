require(gdata)
require(FrF2)
require(ggplot2)
require(rsm)

eggs = read.xls("eggsperiment data.xlsx")
attach(eggs)
eggs.sort = eggs[order(eggs$Actual.Order),]
eggsnew = data.frame(A, B, C, D, Grand.Mean, SD)
detach(eggs)
attach(eggsnew)

eggs.individuals = data.frame(eggs$Actual.Order, eggs$CLK, eggs$CDSK, eggs$SJ, eggs$SK)
colnames(eggs.individuals) = c("Actual Order", "CLK", "CDSK", "SJ", "SK")

eggs.ind = data.frame(as.numeric(rep(1:16, 4)), rep(c("CLK","CDSK","SJ","SK"), each=16), as.numeric(c(eggs$CLK, eggs$CDSK, eggs$SJ, eggs$SK)))
colnames(eggs.ind) = c("Trial","Tester","Score")
eggs.ind = eggs.ind[order(eggs.ind$Trial),]

eggs.ind.mean = data.frame(as.numeric(rep(1:16, 5)), rep(c("CLK","CDSK","SJ","SK", "Mean"), each=16), as.numeric(c(eggs$CLK, eggs$CDSK, eggs$SJ, eggs$SK, eggs$Grand.Mean)))
colnames(eggs.ind.mean) = c("Trial","Tester","Score")
eggs.ind.mean = eggs.ind.mean[order(eggs.ind.mean$Trial),]

eggs.mean <- subset(eggs.ind.mean, Tester=="Mean", select=c(Trial, Score))

##All the plotting stuff for initial variable exploration has been moved over to the accessory script so it quits cluttering everything up as I try to figure out what the hell to do with it.
##Here's a qq plot to show that our scores are normally distributed though:

qqnorm(eggs.mean$Score)

eggs.aov = aov(Grand.Mean ~ A*B*C*D)
summary(eggs.aov)

## Summary of eggs.aov
            Df Sum Sq Mean Sq
A            1  12.69   12.69
B            1  61.04   61.04
C            1   0.32    0.32
D            1   5.94    5.94
A:B          1   2.44    2.44
A:C          1   2.44    2.44
B:C          1   1.13    1.13
A:D          1   0.00    0.00
B:D          1   1.72    1.72
C:D          1   0.00    0.00
A:B:C        1   0.66    0.66
A:B:D        1   4.25    4.25
A:C:D        1   3.75    3.75
B:C:D        1   0.32    0.32
A:B:C:D      1   0.04    0.04

percont <- function(x){
	sumsq = summary(x)[[1]]$'Sum Sq'
	perc = data.frame(names(x$effects)[-1], round(sumsq,2), round(sumsq/sum(sumsq)*100,2))
	colnames(perc) = c("Effect", "Sum Sq", "% Contribution")
	return(perc)
}

percont(eggs.aov)

    Effect Sum Sq % Contribution
1        A  12.69          13.12
2        B  61.04          63.09
3        C   0.32           0.33
4        D   5.94           6.14
5      A:B   2.44           2.52
6      A:C   2.44           2.52
7      B:C   1.13           1.17
8      A:D   0.00           0.00
9      B:D   1.72           1.78
10     C:D   0.00           0.00
11   A:B:C   0.66           0.68
12   A:B:D   4.25           4.40
13   A:C:D   3.75           3.88
14   B:C:D   0.32           0.33
15 A:B:C:D   0.04           0.04

DanielPlot(eggs.aov, alpha = 0.1, pch = 0, code = TRUE, half = TRUE)
abline(0,1,lty=2)

## Okay, looks like A, B, possibly D, and possibly the ABD and ACD interactions are active. Lenth's method may not work here because it's not really good when fewer than 20% of factors are active, so we might try a correction to reduce the cutoff a bit. I'd like to investigate Dong's ASE but it's hard to find information on it.
## Another alternative is to experiment with stepwise fitting of a regression model.

eggs.aov.r = aov(Grand.Mean ~ A + B + C + D + A:B:D + A:C:D)
summary(eggs.aov.r)

            Df Sum Sq Mean Sq F value  Pr(>F)    
A            1  13.14   13.14  12.267  0.0067 ** 
B            1  62.02   62.02  57.895 3.3e-05 ***
C            1   0.39    0.39   0.365  0.5608    
D            1   5.64    5.64   5.266  0.0474 *  
A:B:D        1   4.52    4.52   4.216  0.0703 .  
A:C:D        1   3.52    3.52   3.282  0.1035    
Residuals    9   9.64    1.07                    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##A, B, D, and their three-way interaction are significant, and when we remember to include C for hierarchy, the ACD interaction is not. 

coef(eggs.aov.r)[-1]*2
      A       B       C       D   A:B:D   A:C:D 
-1.8125  3.9375 -0.3125  1.1875  1.0625  0.9375 

##So tentatively, moving from butter to oil makes for worse eggs, moving from egg whites to whole eggs makes for better eggs, and moving from milk to sour cream makes for better eggs. ACD and C can be neglected, but ABD might be interesting and worth retaining. It has a positive coefficient, so all three at the high level make for better eggs, and any combination of two at the low level and one at the high level make for better eggs. Although, the coefficient is relatively small compared to the coefficient for B, which is really dominating the analysis.

eggs.aov.r2 = aov(Grand.Mean ~ A + B + D + A:B:D)
summary(eggs.aov.r2)

            Df Sum Sq Mean Sq F value   Pr(>F)    
A            1  12.69   12.69  10.886  0.00709 ** 
B            1  61.04   61.04  52.353 1.67e-05 ***
D            1   5.94    5.94   5.096  0.04529 *  
A:B:D        1   4.25    4.25   3.649  0.08252 .  
Residuals   11  12.82    1.17                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
coef(eggs.aov.r2)[-1]*2
      A       B       D   A:B:D 
-1.8125  3.9375  1.1875  1.0625 

eggs.lm.final <- lm(Grand.Mean ~ A + B + D + A:B:D)
summary(eggs.lm.final)
Call:
lm.default(formula = Grand.Mean ~ A + B + D + A:B:D)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.42187 -0.45312 -0.07812  0.60938  1.57812 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  10.3594     0.2699  38.377 4.55e-13 ***
A            -0.8906     0.2699  -3.299  0.00709 ** 
B             1.9531     0.2699   7.236 1.67e-05 ***
D             0.6094     0.2699   2.257  0.04529 *  
A:B:D         0.5156     0.2699   1.910  0.08252 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.08 on 11 degrees of freedom
Multiple R-squared:  0.8674,	Adjusted R-squared:  0.8192 
F-statistic:    18 on 4 and 11 DF,  p-value: 8.599e-05


##Some residual analysis plots--have been moved over to the other file where I'm doing all the plotting

##Oh wow, look how perfect these residuals are though. There's really nothing here that looks bad at all, so clearly we're not in trouble with the ratings being sort of skewed off a perfect normal distribution.

eggs.dispersion <- aov(log(SD) ~ A*B*C*D)
summary(eggs.dispersion)

            Df Sum Sq Mean Sq
A            1 0.1548  0.1548
B            1 0.0219  0.0219
C            1 0.0024  0.0024
D            1 0.6102  0.6102
A:B          1 0.0452  0.0452
A:C          1 0.1302  0.1302
B:C          1 0.0801  0.0801
A:D          1 0.3298  0.3298
B:D          1 0.0970  0.0970
C:D          1 0.0236  0.0236
A:B:C        1 0.0796  0.0796
A:B:D        1 0.0372  0.0372
A:C:D        1 0.0047  0.0047
B:C:D        1 0.0302  0.0302
A:B:C:D      1 0.0008  0.0008

DanielPlot(eggs.dispersion, alpha = 0.1, pch = 0, code = TRUE, half = TRUE)

## Looks like AD and D are the only things involved in variability.
percont(eggs.dispersion)

    Effect Sum Sq % Contribution
1        A   0.15           9.40
2        B   0.02           1.33
3        C   0.00           0.15
4        D   0.61          37.03
5      A:B   0.05           2.74
6      A:C   0.13           7.90
7      B:C   0.08           4.86
8      A:D   0.33          20.01
9      B:D   0.10           5.89
10     C:D   0.02           1.43
11   A:B:C   0.08           4.83
12   A:B:D   0.04           2.26
13   A:C:D   0.00           0.28
14   B:C:D   0.03           1.83
15 A:B:C:D   0.00           0.05

##Let's fit a nice hierarchical model.

eggs.disp.r = aov(log(SD) ~ A + D + A:D)
summary(eggs.disp.r)

            Df Sum Sq Mean Sq F value  Pr(>F)   
A            1 0.1548  0.1548   3.360 0.09170 . 
D            1 0.6102  0.6102  13.243 0.00339 **
A:D          1 0.3298  0.3298   7.157 0.02021 * 
Residuals   12 0.5529  0.0461                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coef(eggs.disp.r)

(Intercept)           A           D         A:D 
 0.79080140 -0.09837052 -0.19528814 -0.14356203
  
##So we want D high both for less variability and better eggs, great. But the interaction effect suggests both A and D need to be high or low to reduce variability. But do we necessarily want to reduce noise, or have a really high mean rating for the eggs? I think we're probably better off focusing on the means model, since we don't really know what else might be involved in the noise we're seeing.
