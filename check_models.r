require(gdata)
require(FrF2)

eggs = read.xls("eggsperiment data.xlsx")
eggsnew = data.frame(A, B, C, D, Grand.Mean, SD)

eggs.lm1 = lm(Grand.Mean ~ 1, data = eggsnew)

##alpha-to-enter: 0.1

add1(eggs.lm1, Grand.Mean ~ A+B+C+D, test = "F")
##Adding B.

eggs.lm2 = update(eggs.lm1, . ~ . + B)
add1(eggs.lm2, Grand.Mean ~ A + B + C + D, test = "F")
##Adding A.

eggs.lm3 = update(eggs.lm2, . ~ . + A)
add1(eggs.lm3, Grand.Mean ~ A + B + C + D, test = "F")
##Adding D.

eggs.lm4 = update(eggs.lm3, . ~ . + D)
add1(eggs.lm4, Grand.Mean ~ A + B + C + D, test = "F")
##C doesn't meet the threshold, so we have all main effects in the model. Time to start looking at interactions.

drop1(eggs.lm4, test = "F")

add1(eggs.lm4, Grand.Mean ~ A*B*D, test = "F")
##None of the two-ways meet the threshold, what about the three-way?

add1(eggs.lm4, Grand.Mean ~ A + B + D + A:B:D, test = "F")
##Aha! The three-way interaction DOES meet the threshold. So it's a keeper.

eggs.lm.final <- update(eggs.lm4, . ~ . + A:B:D)
summary(eggs.lm.final)

Call:
lm.default(formula = Grand.Mean ~ B + A + D + B:A:D, data = eggsnew)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.42187 -0.45312 -0.07813  0.60938  1.57813 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  10.3594     0.2699  38.377 4.55e-13 ***
B             1.9531     0.2699   7.236 1.67e-05 ***
A            -0.8906     0.2699  -3.299  0.00709 ** 
D             0.6094     0.2699   2.257  0.04529 *  
B:A:D         0.5156     0.2699   1.910  0.08252 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.08 on 11 degrees of freedom
Multiple R-squared:  0.8674,	Adjusted R-squared:  0.8192 
F-statistic:    18 on 4 and 11 DF,  p-value: 8.599e-05

drop1(eggs.lm.final, test = "F")


##What if we do this as a backward selection? I think we might run into problems, but let's try.

eggs.lmb = lm(Grand.Mean ~ A*B*C*D, data = eggsnew)
##Alpha-to-leave: 0.1

drop1(eggs.lmb, test = "F")
Single term deletions

Model:
Grand.Mean ~ A * B * C * D
        Df Sum of Sq      RSS  AIC F value Pr(>F)
<none>               0.000000 -Inf               
A:B:C:D  1  0.035156 0.035156  -68               
Warning message:
attempting model selection on an essentially perfect fit is nonsense 

##Oops. Okay, let's try dropping the highest-order term and doing model selection on that.

eggs.lmb <- update(eggs.lmb, . ~ . - A:B:C:D)

drop1(eggs.lmb, test = "F")
## Out goes BCD, as the highest F value above 0.1.

eggs.lmb1 <- update(eggs.lmb, . ~ . - B:C:D)
drop1(eggs.lmb1, test = "F")
## Out goes ABC, as the remaining interaction above 0.1.

eggs.lmb2 <- update(eggs.lmb1, . ~ . - A:B:C)
drop1(eggs.lmb2, test = "F")
##Out goes BC.

eggs.lmb3 <- update(eggs.lmb2, . ~ . - B:C)
drop1(eggs.lmb3, test = "F")
##Gross. It gets stuck with most of the interactions and all of the main effects in the model. Backward selection is not a good idea here. Forward and stepwise seem to suggest the same model we'd get by just looking at the half-normal plot and percent contributions though, with judicious application of Lenth's method.
