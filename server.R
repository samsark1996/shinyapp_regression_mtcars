library(shiny)
library(shinyjs)
library(leaps)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)


# Preparing the data
data("mtcars")
mtcars$am = factor(mtcars$am)
levels(mtcars$am) = c("automatic","manual")
mtcars$vs = factor(mtcars$vs)
levels(mtcars$vs) = c("V-shaped","straight")
round_df = function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns = sapply(x, mode) == 'numeric'
    x[numeric_columns] =  round(x[numeric_columns], digits)
    return(x)
}


modelsearch = function(nvar){
    var_list = colnames(mtcars)[-1]
    
    lm_best = regsubsets(mpg~., data = mtcars,nvmax = nvar)
    results = summary(lm_best)
    selection = results$which
    Models = vector()
    
    for(i in 1:nvar){
        current_sel = selection[i,][][-1]
        Models[i] = paste("mpg ~ ",paste(var_list[current_sel],collapse = " + "))
    }
    No_of_Variables = 1:nvar
    R_squared = results$rsq
    Adjusted_R_squared = results$adjr2
    Residual_Sum_of_Squares = results$rss
    Cp = results$cp
    BIC = results$bic
    
    Final_Summary = data.frame(No_of_Variables,Models,R_squared,Adjusted_R_squared,
                               Residual_Sum_of_Squares,Cp,BIC)
    return(Final_Summary)
}



bestmodel = function(blm){
    blms = blm$Models[blm$Adjusted_R_squared == max(blm$Adjusted_R_squared)]
    blm_fit = lm(formula = as.formula(blms),data = mtcars)
    
    return(blm_fit)
}

yourmodel = function(variables){
    if(length(variables)==1){
        your_formual = paste("mpg ~ ",variables,collapse = "")
    }else if(length(variables)>=2){
        your_formual = paste("mpg ~ ",paste(variables,collapse = " + "))
    }
    your_formula = as.formula(your_formual)
    your_model = lm(your_formula,data = mtcars)
    return(your_model)
}

diagPlot = function(model){
    # Modified the code from https://rpubs.com/therimalaya/43190
    p1 = ggplot(model, aes(.fitted, .resid))+geom_point()
    p1 = p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
    p1 = p1+xlab("Fitted values")+ylab("Residuals")
    p1 = p1+ggtitle("Residual vs Fitted Plot")+theme_bw()
    
    p2 = ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
    p2 = p2+geom_abline()+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
    p2 = p2+ggtitle("Normal Q-Q plot")+theme_bw()
    
    p3 = ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
    p3 = p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
    p3 = p3+ylab(expression(sqrt("|Standardized residuals|")))
    p3 = p3+ggtitle("Scale-Location plot")+theme_bw()
    
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

shinyServer(function(input, output,session) {
    
    rv = reactiveValues()
    description = "This app creates custom regression model for mtcars dataset. It can fit any number of variables selected from the left panel. Alternatively it can select the best model for the most appropriate subset of data based on adjusted R squared value."
    output$description = renderText(description)
    
    observe({
            if(input$modelingtype=="bestop"){
                disable("variables")
                enable("varnumber")
                # print(input$varnumber)
            }else{
                disable("varnumber")
                enable("variables")
            }
    })
    # Model generation
    observe({
        if(input$gatherinput==1){
            isolate({
                if(input$modelingtype=="bestop"){
                    ModelSearchResult = modelsearch(input$varnumber)
                    bm = bestmodel(ModelSearchResult)
                    
                    fla = as.character(formula(bm))
                    fla = paste(fla[2],fla[1],fla[3],collapse = "")
                    output$Formula = renderText(fla)
                    coeftbl = summary(bm)$coefficients
                    coeftbl = round_df(coeftbl,3)
                    Variables = rownames(coeftbl)
                    coeftbl = cbind(Variables,coeftbl)
                    output$CoefTable = renderTable(coeftbl)
                    
                    output$distPlot = renderPlot({
                        diagPlts = diagPlot(fortify(bm))
                        do.call(grid.arrange, c(diagPlts, top="Diagnostic Plots", nrow=2))
                    })
                }else if(input$modelingtype=="customop"){
                    # print(input$variables)
                    if(length(input$variables)==1){
                        if(input$variables=="all"){
                            variables = c("mpg","cyl","disp","hp","drat","wt",
                                          "qsec","vs","am","gear","carb")
                        }else{
                            variables = input$variables
                        }
                    }else{
                        variables = input$variables
                    }
                    bm = yourmodel(variables)
                    fla = as.character(formula(bm))
                    fla = paste(fla[2],fla[1],fla[3],collapse = "")
                    output$Formula = renderText(fla)
                    coeftbl = summary(bm)$coefficients
                    coeftbl = round_df(coeftbl,3)
                    Variables = rownames(coeftbl)
                    coeftbl = cbind(Variables,coeftbl)
                    output$CoefTable = renderTable(coeftbl)

                    output$distPlot = renderPlot({
                        diagPlts = diagPlot(fortify(bm))
                        do.call(grid.arrange, c(diagPlts, top="Diagnostic Plots", nrow=2))
                    })
                }
            })
            R_squared = summary(bm)$r.squared
            Adjusted_R_squared = summary(bm)$adj.r.squared
            aic = AIC(bm)
            bic = BIC(bm)
            model_param_out = paste0("R squared = ",round(R_squared,3),
                                    ", Adjusted R squared = ",round(Adjusted_R_squared,3),
                                    ", AIC = ",round(aic,3),
                                    ", BIC = ", round(bic,3),collapse = "")
            print(model_param_out)
            output$modelparamout = renderText(model_param_out)
        }else if(input$gatherinput>1){
            msgstr = "Please press the reset button to reset the model before you proceed"
            print(msgstr)
        }
    })
    
    # Reset function
    observe({
        if(input$resetinput>0){
            reset("form")
            session$reload()
        }
    })
    
    
})
