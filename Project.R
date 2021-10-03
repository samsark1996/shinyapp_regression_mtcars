rm(list = ls(all.names = TRUE))
cat('\14')

library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(GGally)
library(data.table)
library(leaps)

data("mtcars")
summary(mtcars)

mtcars$am = factor(mtcars$am)
levels(mtcars$am) = c("automatic","manual")
mtcars$vs = factor(mtcars$vs)
levels(mtcars$vs) = c("V-shaped","straight")


# df_sum = mtcars %>%
#   group_by(am,vs) %>%
#   summarise(Mean = mean(mpg),
#             SD = sd(mpg),
#             Minimum = min(mpg),
#             
#             Maximum = max(mpg)) %>%
#   data.frame()
# print(df_sum)
# 
# 
# ggpairs(mtcars[,-c(1,8,9)])
# 
# f1 = ggplot(data = mtcars,aes(x = am,y = mpg,fill = am)) +
#   geom_boxplot()  +
#   scale_fill_brewer(palette="Dark2") +
#   theme_classic()
# 
# f2 = ggplot(data = mtcars,aes(x = vs,y = mpg,fill = vs)) +
#   geom_boxplot()  +
#   scale_fill_brewer(palette="Dark2") +
#   theme_classic()
# 
# grid.arrange(f1,f2,nrow = 1)
# 
# p1 = ggplot(data = mtcars) +
#   geom_boxplot(aes(x=am,y=mpg,fill = am)) +
#   geom_jitter(aes(x=am,y=mpg),position=position_jitter(0.05)) + 
#   facet_wrap(~vs,nrow = 1) + xlab("Transmission") + ylab("Miles/(US) gallon")
# print(p1)
# 
# lm_man = lm(mpg~., data = mtcars[mtcars$am=="manual",-9])
# summary(lm_man)
# lm_auto = lm(mpg~., data = mtcars[mtcars$am=="automatic",-9])
# summary(lm_auto)


var_list = colnames(mtcars)[-1]

lm_best = regsubsets(mpg~., data = mtcars,nvmax = 10)
results = summary(lm_best)
selection = results$which
Models = vector()

for(i in 1:10){
  current_sel = selection[i,][][-1]
  Models[i] = paste("mpg ~ ",paste(var_list[current_sel],collapse = " + "))
}
No_of_Variables = 1:10
R_squared = results$rsq
Adjusted_R_squared = results$adjr2
Residual_Sum_of_Squares = results$rss
Cp = results$cp
BIC = results$bic

Final_Summary = data.frame(No_of_Variables,Models,R_squared,Adjusted_R_squared,
                           Residual_Sum_of_Squares,Cp,BIC)
blm = Final_Summary$Models[Final_Summary$Adjusted_R_squared == max(Final_Summary$Adjusted_R_squared)]


blm_fit = lm(formula = as.formula(blm),data = mtcars)

summary(blm_fit)




f3 = ggplot(data = Final_Summary,aes(x = 1:10,y = Adjusted_R_squared)) + 
  geom_line() + 
  geom_point(show.legend = F)
print(f3)

Final_Model = lm(mpg ~  disp + hp + wt + qsec + am, data = mtcars)

diagPlot = function(model){
  p1 = ggplot(model, aes(.fitted, .resid))+geom_point()
  p1 = p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1 = p1+xlab("Fitted values")+ylab("Residuals")
  p1 = p1+ggtitle("Residual vs Fitted Plot")+theme_bw()
  
  p2 = ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
  p2 = p2+geom_abline()+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
  p2 = p2+ggtitle("Normal Q-Q")+theme_bw()
  
  p3 = ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
  p3 = p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
  p3 = p3+ylab(expression(sqrt("|Standardized residuals|")))
  p3 = p3+ggtitle("Scale-Location")+theme_bw()
  
  p4 = ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
  p4 = p4+xlab("Obs. Number")+ylab("Cook's distance")
  p4 = p4+ggtitle("Cook's distance")+theme_bw()
  
  p5 = ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
  p5 = p5+stat_smooth(method="loess", na.rm=TRUE)
  p5 = p5+xlab("Leverage")+ylab("Standardized Residuals")
  p5 = p5+ggtitle("Residual vs Leverage Plot")
  p5 = p5+scale_size_continuous("Cook's Distance", range=c(1,5))
  p5 = p5+theme_bw()+theme(legend.position="bottom")
  
  p6 = ggplot(model, aes(.hat, .cooksd))+geom_point(na.rm=TRUE)+stat_smooth(method="loess", na.rm=TRUE)
  p6 = p6+xlab("Leverage hii")+ylab("Cook's Distance")
  p6 = p6+ggtitle("Cook's dist vs Leverage hii/(1-hii)")
  p6 = p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
  p6 = p6+theme_bw()
  
  return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5, cvlPlot=p6))
}

diagPlts = diagPlot(fortify(Final_Model))
do.call(grid.arrange, c(diagPlts, top="Diagnostic Plots", nrow=2))

T1 = t.test(mtcars$mpg[mtcars$am=="manual"],
            mtcars$mpg[mtcars$am=="automatic"],
            alternative = "greater",
            paired = FALSE,
            var.equal = FALSE,
            conf.level = 0.95)
print(T1)
