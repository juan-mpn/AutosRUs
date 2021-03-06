# AutosRUs
Using R to create statistical linear regression t-test on datasets

##AutosRU's
##Data Analytics Team
##3/14/2020
##**_MechaCarChallengeRScript.R_**
##**_MechaCarWriteUp.txt_**
Analysis using R Studio multiple linear regresion, designed a linear model that can predict the mpg of MechaCar prototypes using the following variables: 
# **_MechaCar_mpg.csv_**
# **_MechaCar_mpg2.csv_**

- mpg
- length
- weight
- clearance
- angle
- AWD

After running the analysis and creating a matrix that compared all the variables. This is helping us to compare the ouput using the summary(), cor() and lm() functions creating metrics from our model. Below the output and analysis. 

# cor()
               weight         mpg   clearance      length       angle         AWD
weight     1.00000000  0.09068314  0.08511338 -0.12271790 -0.11307851 -0.03698098
mpg        0.09068314  1.00000000  0.32874886  0.60947984 -0.02083999 -0.14166977
clearance  0.08511338  0.32874886  1.00000000 -0.31663112 -0.21112057 -0.15214456
length    -0.12271790  0.60947984 -0.31663112  1.00000000  0.02577114  0.08565668
angle     -0.11307851 -0.02083999 -0.21112057  0.02577114  1.00000000 -0.09120266
AWD       -0.03698098 -0.14166977 -0.15214456  0.08565668 -0.09120266  1.00000000

We can clearly see there is direct correlation between mpg and lenght as shown in the below graphs. Using this information our analysis continous to take these two variables in our main analysis for linear regresion. 

Scatter![alt text](https://github.com/juan-mpn/AutosRUs/blob/master/Resources/mpgvslengthscatter.png)


LinearRegresion![alt text](https://github.com/juan-mpn/AutosRUs/blob/master/Resources/lengthmpg-LinearRegresion.png)


PSIvsDensity![alt text](https://github.com/juan-mpn/AutosRUs/blob/master/Resources/PSIDensityPlot.png)


Call:
lm(formula = mpg ~ length + AWD + clearance + weight + angle, 
    data = mpgm)

Residuals:
     Min       1Q   Median       3Q      Max 
-19.4701  -4.4994  -0.0692   5.4433  18.5849 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.040e+02  1.585e+01  -6.559 5.08e-08 ***
length       6.267e+00  6.553e-01   9.563 2.60e-12 ***
AWD         -3.411e+00  2.535e+00  -1.346   0.1852    
clearance    3.546e+00  5.412e-01   6.551 5.21e-08 ***
weight       1.245e-03  6.890e-04   1.807   0.0776 .  
angle        6.877e-02  6.653e-02   1.034   0.3069    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 8.774 on 44 degrees of freedom
Multiple R-squared:  0.7149,	Adjusted R-squared:  0.6825 
F-statistic: 22.07 on 5 and 44 DF,  p-value: 5.35e-11

This analysis shows the linear regresion, lm() returns our y intercept (Intercept) and slope (length) coefficients. We can find the linear regresion model for our dataset as shown below:

mpg = 6.267e+00length + -1.040e+02 

We can also see Pearson p-value 5.35e-11 and our r-squared 0.7149. This is sufficient evidence to reject our null hypotesis, which means that the slope of our linear model is not zero as we can visualize in the linear regresion model using ggplot2 being a positive correlation where x-axis(MPG) increases, the variable on the y-axis (Length) increases as well. 

##----------------------- Coil Suspension Analysis ------- t-test
##Min, Mean, Max, Median, Standard Deviation, Variance, Num of Vehicles per lot
**Manufacturing_Lot**
		Min_PSI - Mean_PSI - Maximum_PSI - Med_PSI - StandDev_PSI - 	Variance - Num_Vehicles
1	Lot1	1498	1500.00		1502	  1500.0	0.9897433	0.9795918	50
2	Lot2	1494	1500.20		1506	  1500.0	2.7330181	7.4693878	50
3	Lot3	1452	1496.14		1542	  1498.5	13.0493725	170.2861224	50
Showing 1 to 3 of 3 entries, 8 total columns

We can clearly see a much better product on Lot1, having a perfect Mean 1500 and very small variance. We need to look deeper on what change when Lot2 and Lot3 where produced and perform further analysis and more testing. The same is shown in the Paired t-test below. This is enough evidence to show that Lot 3 mean suspension coil PSI is out of specification and statistically it can be classified as different from the rest of the population mean. We could be within specification if we use the entire dataset that includes all three lots but we have to point out that **Lot 3 is statistically different and out of sepecifications**.


	Paired t-test

**data:  Lot 1 and Lot2**
t = -0.52031, df = 49, p-value = 0.6052
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.9724591  0.5724591
sample estimates:
mean of the differences 
                   -0.2 

**Lot3: PSI different from the rest of the population**
t = 2.0728, df = 49, **p-value = 0.04347**
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.1177828 7.6022172
sample estimates:
mean of the differences 
                   3.86 

##New Design Study**
A new study that will be very intersting to consumers would be to compare wheel size against engine size. It is well known that if we have the incorrect tire size there can be fuel effiency cost. Finding the correct pair it can bring savings for consumers and they will be very insterested in finding that marketing can show this correlation proven scietifically unsing mathematical/machine learning models how we achive this effiency. This data is already available and it could be easily obtained by MechaCar vehicles. The null hypothesis would be that there is no stistical difference between MechaCar fuel effiency and other makers of similar vehicles. The alternative hypothesis would be that there is a statistical difference between fuel efficency of MechaCar when compared to other makers. The population data that we will request MechaCar would be all comparable vehicles using one-sample t-test we can perform this analysis. It will be absolutely necessary that MechaCar provides tire size and engine size to arrive to the conclusion of what is the best combination for fuel effiency.
