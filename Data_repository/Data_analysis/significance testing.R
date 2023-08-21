###Significance testing

#we are using the Larry_results table to test for significance 
results_larry <- results[1:21,]

#Variance
anova_results <- aov(variance ~ event, data = results_larry)
summary(anova_results)
#Not significant 

#Skewness
anova_results <- aov(skewness ~ event, data = results_larry)
summary(anova_results)
#Not significant

#Autocorrelation 
anova_results <- aov(autocorrelation ~ event, data = results_larry)
summary(anova_results)
#Significant

#MCP
anova_results <- aov(mcp ~ event, data = results_larry)
summary(anova_results)
#not significant

#home-range
anova_results <- aov(log(home_range) ~ event, data = results_larry)
summary(anova_results)
#Not significant

#home-range ML
anova_results <- aov(home_range_ML ~ event, data = results_larry)
summary(anova_results)

#idicator
anova_results <- aov(indicator ~ event, data = finished)
summary(anova_results)

#inter q
anova_results <- aov(interq ~ event, data = finished)
summary(anova_results)
