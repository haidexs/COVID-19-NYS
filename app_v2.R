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

# Define UI for dataset viewer application
ui = fluidPage(
    wellPanel(
        fluidRow(
            column(3,
                   selectInput("region", "选择区域：", choices = c("纽约州", "纽约市", "LIC-11101", "哥伦比亚大学-10027")),
                   helpText("    纽约州和纽约市数据来自州政府；"),
                   helpText("    LIC和哥伦比亚大学数据来自纽约市政府;"),
                   helpText("    两者的记录方法不同，体现在纽约州以病例的上报日期为统计依据，而纽约市以确诊时间为依据。"),
            ),
            column(3,
                   # # these options will be added later
                   radioButtons("pos_rate", "选择第二纵轴显示数据", choiceNames = c("确诊率", "住院/死亡数"), choiceValues = c("YES", "NO")), # data source: NYS or NYC
                   helpText("    只有纽约州提供住院和死亡人数数据")
                   # textInput("data_length", "输入想要查看数据的天数", value = 20)
                   # actionButton("showbutton", label = "显示数据")
            )
        )
    ),
    fluidRow(
        column(7, plotOutput("dualplot")),
        column(5, DT::dataTableOutput("recent_data", width = "100%"))
    )

    # sidebarLayout(
    #     sidebarPanel(
    #         selectInput("region", "选择区域：", choices = c("纽约州", "纽约市", "LIC-11101", "哥伦比亚大学-10027")),
    #         helpText("    纽约州和纽约市数据来自州政府；"),
    #         helpText("    LIC和哥伦比亚大学数据来自纽约市政府;"),
    #         helpText("    两者的记录方法不同，体现在纽约州以病例的上报日期为统计依据，而纽约市以确诊时间为依据。"),
    # 
    #         # # these options will be added later
    #         radioButtons("pos_rate", "选择第二纵轴显示数据", choiceNames = c("确诊率", "住院/死亡数"), choiceValues = c("YES", "NO")), # data source: NYS or NYC
    #         helpText("    只有纽约州提供住院和死亡人数数据"),
    #         # textInput("data_length", "输入想要查看数据的天数", value = 30),
    #         # helpText("    若超过有数据")
    #         # textInput("zipcode", "输入邮编：", value = "11101"), # other zipcodes
    #         # radioButtons("data_type", NULL, choiceNames = c("新增", "累计"), choiceValues = c("New", "Total")), # new or total (total = cumulative)
    #         # radioButtons("data_source", "数据源", choiceNames = c("州政府", "市政府"), choiceValues = c("NYS", "NYC")), # data source: NYS or NYC
    #         width = 3
    #     ),
    #     mainPanel(
    #         plotOutput("dualplot"),
    #         DT::dataTableOutput("recent_data"),
    #         width = 9
    #     )
    # )
)

# Define server logic required to summarize and view the selected dataset
server = function(input, output) {
    # prepare data
    covid_data = read.csv("./dataset.csv", header=TRUE, sep=",")
    covid_data$Date = format(as.Date(as.character(covid_data$Date), "%m/%d/%y"), "%m/%d")
    long_data = covid_data %>% gather(key="data_att", value="counts", -Date)
    
    # to show Chinese, we use SimHei font with the help of package extrafont
    loadfonts()
    
    # define colors
    col_tested = rgb(0.95, 0.67, 0.33)
    col_positive = rgb(0.17, 0.49, 0.69)
    col_admitted = rgb(0.86, 0.17, 0.14)
    col_death = rgb(0.2, 0.2, 0.2)
    col_positive_rate = rgb(1, 0.1, 0.1)
    
    # define 2nd y-axis scale and limit of breaks
    coeff = 0.15

    data_to_plot = reactive({
        # if (input$data_length == 0) {
        #     return()
        # }
        region_code = switch(input$region,
                             "纽约州" = "NYS",
                             "纽约市" = "NYC",
                             "LIC-11101" = "LIC",
                             "哥伦比亚大学-10027" = "ColumbiaUniv")
        # "10019" = "ColumbusCircle_1",
        # "10044" = "ColumbusCircle_2")
        # print(region_code)
        
        data_tmp = select(covid_data, Date, contains(region_code)) %>%
            rename(MMDD = Date,
                   NewPositive = paste(region_code, "New.Positive", sep = "."), NewTested = paste(region_code, "New.Tested", sep = "."), 
                   NewAdmitted = paste(region_code, "New.Hospitolized", sep = "."), NewDeath = paste(region_code, "New.Death", sep = "."),
                   TotalPositive = paste(region_code, "Total.Positive", sep = "."), TotalTested = paste(region_code, "Total.Tested", sep = "."),
                   TotalAdmitted = paste(region_code, "Total.Hospitolized", sep = "."), TotalDeath = paste(region_code, "Total.Death", sep = "."))

        if (max(c(0, data_tmp$NewTested), na.rm = TRUE) == 0){
            select(data_tmp, MMDD, contains("Total")) %>%
                rename(Tested = TotalTested, Positive = TotalPositive, Death = TotalDeath, Admitted = TotalAdmitted) %>%
                mutate(data_att = "累计") %>%
                filter(!is.na(Tested))
            # data_att = "累计"
        } else {
            select(data_tmp, MMDD, contains("New")) %>%
                rename(Tested = NewTested, Positive = NewPositive, Death = NewDeath, Admitted = NewAdmitted) %>%
                mutate(data_att = "新增") %>%
                filter(!is.na(Tested))
            # data_att = "新增"
        }
        
        # if(identical(region_code, "NYS")) {
        #     select(covid_data, Date, contains(region_code)) %>%
        #         rename(MMDD = Date, NewPositive = NYS.New.Positive, NewTested = NYS.New.Tested,
        #                NewAdmitted = NYS.New.Hospitolized, NewDeath = NYS.New.Death,
        #                TotalPositive = NYS.Total.Positive, TotalTested = NYS.Total.Tested, TotalDeath = NYS.Total.Death)
        # } else if(identical(region_code, "NYC")) {
        #     select(covid_data, Date, contains(region_code)) %>%
        #         rename(日期 = Date, NewPositive = NYC.New.Positive, NewTested = NYC.New.Tested,
        #             TotalPositive = NYC.Total.Positive, TotalTested = NYC.Total.Tested)
        # } else if(identical(region_code, "LIC")) {
        #     select(covid_data, Date, contains(region_code)) %>%
        #         rename(日期 = Date, TotalPositive = LIC.Total.Positive, TotalTested = LIC.Total.Tested)
        # } else {
        #    stop("Check your region.")
        # }
    }
    )
    
    total_or_new = reactive({
        data_to_plot()$data_att[1]
    })
    
    region_name = reactive({
        input$region
    })

    pos_rate_option = reactive({
        input$pos_rate
    })
    
    # left_axis_max = reactive({
    #     max(covid_data$NYS.NewTested, na.rm = TRUE)
    # })
    # 
    # right_axis_max = reactive({
    #     max(covid_data$NYS.NewHospitolized1, na.rm = TRUE)
    # })
    
    output$dualplot = renderPlot({
        showtext_begin()
        data1 = data_to_plot()
        region = region_name()
        data_att = total_or_new()
        pos_rate = pos_rate_option()
        left_axis_max = max(c(0, data1$Tested), na.rm = TRUE)
        right_axis_max = max(c(0, data1$Admitted), na.rm = TRUE)
        
        plot_left_y = ggplot(data1, aes(x=as.Date(MMDD, format="%m/%d"))) +
            geom_bar(aes(y=Tested, fill=col_tested), stat="identity", size=0.1, alpha=0.9) +
            geom_bar(aes(y=Positive, fill=col_positive), stat="identity", size=0.1, alpha=1)
        
        if (identical(pos_rate, "NO")) {
            plot_right_y = plot_left_y + 
                geom_line(aes(y=Death/coeff, colour=col_death), size=2, na.rm = TRUE, group = 1) + # Divide by 10 to get the same range than the temperature
                geom_line(aes(y=Admitted/coeff, colour=col_admitted), size=2, na.rm=TRUE, group=1) +
                scale_y_continuous(
                    # Features of the first axis
                    name = "检测/确诊数",
                    n.breaks = 5,
                    # breaks = seq(0,left_axis_max,5000),
                    minor_breaks = seq(0,left_axis_max,2500),
                    # labels = seq(0,max(data_to_plot()$NYSNewTested, na.rm = TRUE),2500),
                    # name = "Axis 1",
                    
                    # Add a second axis and specify its features
                    sec.axis = sec_axis(~.*coeff, name="住院/死亡数", breaks = seq(0,right_axis_max/coeff,1000))
                    # sec.axis = sec_axis(~.*coeff, name="Axis 2")
                ) + 
                scale_color_manual(name = NULL,
                                   labels = paste(data_att, c("检测","确诊","死亡","住院"), sep=""),
                                   breaks = c(col_tested, col_positive, col_death, col_admitted),
                                   values = c(col_tested, col_positive, col_death, col_admitted),
                                   aesthetics = c("fill","colour"))
        } else {
            coeff_2 = 1/left_axis_max
            plot_right_y = plot_left_y + 
                geom_line(aes(y=(Positive/Tested)/coeff_2), colour=col_positive_rate, size=1, linetype="dotted", na.rm = TRUE, group = 1) + # Divide by 10 to get the same range than the temperature
                geom_point(aes(y=(Positive/Tested)/coeff_2, color = col_positive_rate, fill=col_positive_rate), shape = 23, size = 4) + 
                scale_y_continuous(
                    # Features of the first axis
                    name = "检测/确诊数",
                    n.breaks = 5,
                    # breaks = seq(0,left_axis_max,5000),
                    minor_breaks = seq(0,left_axis_max,2500),
                    # labels = seq(0,max(data_to_plot()$NYSNewTested, na.rm = TRUE),2500),
                    # name = "Axis 1",
                    
                    # Add a second axis and specify its features
                    sec.axis = sec_axis(~.*coeff_2, name="确诊率", breaks = seq(0,1,0.25))
                    # sec.axis = sec_axis(~.*coeff, name="Axis 2")
                ) + 
                scale_color_manual(name = NULL,
                                   labels = c(paste(data_att, c("检测","确诊"), sep=""), "确诊率"),
                                   breaks = c(col_tested, col_positive, col_positive_rate),
                                   values = c(col_tested, col_positive, col_positive_rate),
                                   aesthetics = c("fill", "colour"))
        }
        
        to_plot = plot_right_y +
            scale_x_date(date_breaks = "3 days", date_labels = "%m/%d") + 
            # labs(title = "NYS COVID-19") +
            labs(title = paste(region, "新冠疫情数据", sep = ""), x = "日期") +
            # scale_x_date(name = "日期", date_breaks = "3 days") +
            theme_bw() + 
            theme(text=element_text(family="GSC", size=10), legend.position = "top") + # legend.position = c(0.1, 0.83)
            theme(plot.title = element_text(face = "bold", size = 10)) +
            theme(legend.text = element_text(size = 8), 
                  axis.text.x = element_text(angle=0, hjust=1),
                  axis.ticks.length.y.left = unit(.05, "cm"), axis.ticks.length.y.right = unit(.05, "cm"),
                  aspect.ratio = 0.5)
        
        plot(to_plot)
        showtext_end()
    })
    
    # data_length = reactive({
    #     min(input$data_length, length(data_to_plot()[ ,1]))
    # })

    output$recent_data = DT::renderDataTable({
        # renderTable({
        data1 = data_to_plot()
        # nrow = length(data1[ , 1])
        region = region_name()
        data_att = total_or_new()
        # data_nrow = as.numeric(data_length())
        # data_nrow = nrow
        data_region_att = paste(region, data_att, sep="\n")
        data1$pos_rate = format(round(data1$Positive / data1$Tested, 3), nsmall = 3)
        colnames(data1) = c("日期", paste(data_region_att, c("确诊","检测","住院","死亡"), sep=""), "data_att", "确诊率")
        # return(data1[(nrow-data_nrow+1):nrow, 1:5])
        return(data1[ , c(1:5,7)])
    }
    # spacing = "s", width = "auto", align = "c",
    # rownames = FALSE, colnames = TRUE
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
