#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(encoding="UTF-8")

# load libraries
library(shiny)
library(datasets)
library(tidyverse)
library(extrafont)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(showtext)
library(scales)
library(DT)

powerbi_rEnableShowTextForCJKLanguages =  1
font_add_google("Noto Sans SC", "GSC")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Data for New York State and New York City"),

    wellPanel(
        fluidRow(
            column(3,
                   # selectInput("region", "选择区域：", choices = c("纽约州", "纽约市", "LIC-11101", "哥伦比亚大学-10027")),
                   helpText("首先请选择数据源：纽约州政府 / 纽约市政府；"),
                   helpText("州政府数据以病例的上报日期为准进行统计，包括全州的检测、确诊、住院和死亡人数，以及纽约市的检测和确诊人数。"),
                   helpText("市政府数据以病例的确诊日期为准进行统计，包括全市的检测、确诊、住院和死亡人数，以及各邮编对应地区的检测和确诊人数。"),
                   helpText("   我们提供LIC（11101）和哥伦比亚大学（10027）地区的数据；其他邮编地区请前往市政府网站查询。"),
                   helpText("因为统计口径的区别，我们认为市政府数据能更好的反映疫情变化趋势，但是有严重的数据滞后，因此最近3天数据不可靠。"),
                   selectInput("data_source", "选择数据源：", choices = c("州政府", "市政府")),
                   conditionalPanel(
                        condition = "input.data_source == 州政府", selectInput("region", "选择区域：", choices = c("纽约州", "纽约市")),
                        condition = "input.data_course == 市政府", selectInput("region", "选择区域：", choices = c("LIC-11101", "哥伦比亚大学-10027"))
                   )
            ),
            column(3,
                   # # these options will be added later
                   radioButtons("pos_rate", "选择第二纵轴显示数据", choiceNames = c("确诊率", "住院/死亡数"), choiceValues = c("YES", "NO")), # data source: NYS or NYC
                   helpText("    只有纽约州提供住院和死亡人数数据")
                   # textInput("data_length", "输入想要查看数据的天数", value = 20)
                   # actionButton("showbutton", label = "显示数据")
            )
            # colum(3,
            #       # To add new data. Password required.
            #       passwordInput("password", "Data Entry Password:"),
            #       textInput()
            # )
        )
    ),
    fluidRow(
        column(7, plotOutput("dualplot")),
        column(5, DT::dataTableOutput("recent_data", width = "100%"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
