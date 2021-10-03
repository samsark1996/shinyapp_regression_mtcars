library(shiny)
library(shinyjs)
library(leaps)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# rsconnect::setAccountInfo(name='samsark', token='1EECA09290CF73AD9E00D3F17A2DA468', secret='KTxHBvwB4xY2InYkyn27MoXhn2gpRG1YJZ0b7LZf')

jsResetCode = "shinyjs.reset = function() {history.go(0)}"
shinyUI(fluidPage(
    useShinyjs(),
    # Application title
    titlePanel("Custom linear model for mtcars data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 3,
            h2("Inputs"),
            actionButton("resetinput", "Reset Model", icon = icon("redo")),
            div(id = "form",
                radioButtons("modelingtype", "Regression option:",
                             c("Select best model" = "bestop",
                               "Select variables" = "customop")),
                numericInput("varnumber","How many variables do you need for the best model",
                             value = 10,min = 1,max = 10,step = 1),
                checkboxGroupInput("variables", "Select variables:",
                                   c("cyl - Number of cylinders" = "cyl",
                                     "disp - Displacement (cu.in.)" = "disp",
                                     "hp - Gross horsepower" = "hp",
                                     "drat - Rear axle ratio" = "drat",
                                     "wt - Weight (1000 lbs)" = "wt",
                                     "qsec - 1/4 mile time" = "qsec",
                                     "vs - Engine" = "vs",
                                     "am - Transmission" = "am",
                                     "gear - Number of forward gears" = "gear",
                                     "carb - Number of carburetors" = "carb",
                                     "All of the variables" = "all")),
                # checkboxGroupInput("reportoutput", "Report output:",
                #                    c("Statistical summary of the data" = "stat",
                #                      "Hypothesis testing" = "hypo",
                #                      "Model summary" = "modelsum",
                #                      "Residual diagnostics" = "resdiag",
                #                      "All of the above" = "all")),
                actionButton("gatherinput", "Generate Model", icon = icon("cogs"), class = "btn-success")
                )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2("Details of the model selected"),
            fluidRow(
                column(6,
                       h3("Brief description:"),
                       textOutput("description"),
                       h3("Formula:"),
                       textOutput("Formula"),
                       h3("Model fit parameters:"),
                       textOutput("modelparamout")
                ),
                column(6,
                       h3("Coefficients"),
                       tableOutput("CoefTable")
                )
            ),
            fluidRow(
                h2("Residual diagnostic plot"),
                plotOutput("distPlot")
            )#,
            # actionButton("genreport", "Generate report", icon = icon("book"))
        )
    )
))
# deployApp()