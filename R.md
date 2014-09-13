#Motor Trends : Automatic or Manual transmission for better mileage ?

#Executive Summary

#We performed a series of regression tests to examine the relationship between automatic or manual transmission on a set of statistical data on cars. While our final model shows that, keeping other variables constant, cars with a manual gearing have a slightly higher miles per gallon. The statistical test revealed that the effect of gearing is insignificant.Therefore, we cannot form a conclusion on whether automatic or manual gearing results in superior mileage. The reader is encouraged to consider cars of a weight and number of cylinders if they are seeking better mileage.

#Analysis

#The first step of our analysis is to simply explore the relationship between gearing and mpg. After loading the data, we draw a regression line.

data(mtcars)
plot(mtcars$am, mtcars$mpg)
explore <- lm(mpg ~ am, data = mtcars)
summary(explore)
## 
## Call:
## lm(formula = mpg ~ am, data = mtcars)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.392 -3.092 -0.297  3.244  9.508 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    17.15       1.12   15.25  1.1e-15 ***
## am              7.24       1.76    4.11  0.00029 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.9 on 30 degrees of freedom
## Multiple R-squared:  0.36,   Adjusted R-squared:  0.338 
## F-statistic: 16.9 on 1 and 30 DF,  p-value: 0.000285
abline(explore)

##See in Rplot.pdf attached below that graph hints at an increase in mpg when gearing was manual.

##This data may have other variables which may play a bigger role in determination of mpg.

##We perform an analysis of variance exercise on the data.

analysis <- aov(mpg ~ ., data = mtcars)
summary(analysis)
##             Df Sum Sq Mean Sq F value Pr(>F)    
## cyl          1    818     818  116.42  5e-10 ***
## disp         1     38      38    5.35 0.0309 *  
## hp           1      9       9    1.33 0.2610    
## drat         1     16      16    2.34 0.1406    
## wt           1     77      77   11.03 0.0032 ** 
## qsec         1      4       4    0.56 0.4617    
## vs           1      0       0    0.02 0.8932    
## am           1     14      14    2.06 0.1659    
## gear         1      1       1    0.14 0.7137    
## carb         1      0       0    0.06 0.8122    
## Residuals   21    147       7                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##Clearly important variables ( p-value below 0.05) which determine miles per gallen are the number of cylinders,displacement and weight. We need to refine our linear model by introducing these three confounding variables into the exercise.

##To enahncement the first model, we now perform a linear regression with weight, displacement and cylinders as confounding variables in the lm.

lm1 <- lm(mpg ~ cyl + disp + wt + am, data = mtcars)
summary(lm1)
## 
## Call:
## lm(formula = mpg ~ cyl + disp + wt + am, data = mtcars)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.318 -1.362 -0.479  1.354  6.058 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  40.8983     3.6015   11.36  8.7e-12 ***
## cyl          -1.7842     0.6182   -2.89   0.0076 ** 
## disp          0.0074     0.0121    0.61   0.5451    
## wt           -3.5834     1.1865   -3.02   0.0055 ** 
## am            0.1291     1.3215    0.10   0.9229    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.64 on 27 degrees of freedom
## Multiple R-squared:  0.833,  Adjusted R-squared:  0.808 
## F-statistic: 33.6 on 4 and 27 DF,  p-value: 4.04e-10
##Coefficient of Displacement has a p-value which allows is to keep the hypothesis that the coefficient is null. We will now observe to see if removing the displacement viable can increase the adjusted R-square of the test.

lm1 <- lm(mpg ~ cyl + wt + am, data = mtcars)
summary(lm1)
## 
## Call:
## lm(formula = mpg ~ cyl + wt + am, data = mtcars)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.173 -1.534 -0.539  1.586  6.081 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   39.418      2.641   14.92  7.4e-15 ***
## cyl           -1.510      0.422   -3.58   0.0013 ** 
## wt            -3.125      0.911   -3.43   0.0019 ** 
## am             0.176      1.304    0.14   0.8933    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.61 on 28 degrees of freedom
## Multiple R-squared:  0.83,   Adjusted R-squared:  0.812 
## F-statistic: 45.7 on 3 and 28 DF,  p-value: 6.51e-11
##Indeed, the adjusted r-squared is higher at 0.81. We choose this is our final model.

##Clearly after introducing the number of cylinders and weight as a confounding variable, the ceofficient of the am variable becomes very small with a large p-value. This means that we cannot reject the hypothesis the coefficient of am is 0.

##Now we plot the residuals to search for discernible patterns.

plot(fitted(lm1), resid(lm1))
abline(h = 0)

##See Rplot2.pdf file attched in below.There is no discernible pattern when residuals are plotted against the fitted values. This confirms that our model is a good fit.

##Conclusion

##While a subtle relationship exists between am and mpg, this is insignificant and we cannot concluded that shifting from automatic to manual gearing will result in a car which gives better mileage. The reader is encourage to consider the car's weight and number of cylinders instead to determine the mileage of the vehicle.
