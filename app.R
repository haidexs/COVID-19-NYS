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

powerbi_rEnableShowTextForCJKLanguages =  1
font_add_google("Noto Sans SC", "GSC")

# Define UI for dataset viewer application
ui = fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("region", "选择区域：", choices = c("纽约州", "纽约市", "11101", "10027")),
            # # these options will be added later
            # textInput("zipcode", "输入邮编：", value = "11101"), # other zipcodes
            # radioButtons("data_type", NULL, choiceNames = c("新增", "累计"), choiceValues = c("New", "Total")), # new or total (total = cumulative)
            # radioButtons("data_source", "数据源", choiceNames = c("州政府", "市政府"), choiceValues = c("NYS", "NYC")), # data source: NYS or NYC
            width = 3,
            helpText("纽约州和纽约市数据来自州政府；",
                "LIC，哥大，哥圈数据来自纽约市政府;",
                "两者的记录方法不同，体现在纽约州以病例的上报日期为统计依据，而纽约市以确诊时间为依据。")
        ),
        mainPanel(
            plotOutput("dualplot"),
            # plotOutput("")
            width = 8
        )
    )
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
    coeff = 0.23

    data_to_plot = reactive({
        region_code = switch(input$region,
                             "纽约州" = "NYS",
                             "纽约市" = "NYC",
                             "11101" = "LIC",
                             "10027" = "ColumbiaUniv")
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
                mutate(data_att = "累计")
            # data_att = "累计"
        } else {
            select(data_tmp, MMDD, contains("New")) %>%
                rename(Tested = NewTested, Positive = NewPositive, Death = NewDeath, Admitted = NewAdmitted) %>%
                mutate(data_att = "新增")
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
        left_axis_max = max(c(0, data1$Tested), na.rm = TRUE)
        right_axis_max = max(c(0, data1$Admitted), na.rm = TRUE)

        to_plot = ggplot(data1, aes(x=MMDD)) +
            geom_bar(aes(y=Tested, fill=col_tested), stat="identity", size=0.1, alpha=0.9) + 
            geom_bar(aes(y=Positive, fill=col_positive), stat="identity", size=0.1, alpha=1) +
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
                sec.axis = sec_axis(~.*coeff, name="住院/死亡数", breaks = seq(0,right_axis_max/coeff,500))
                # sec.axis = sec_axis(~.*coeff, name="Axis 2")
            ) +
            
            # labs(title = "NYS COVID-19") +
            labs(title = paste(region, "新冠疫情数据", sep = ""), x = "日期") +
            # scale_x_date(name = "日期", date_breaks = "3 days") +
            theme_bw() +
            theme(text=element_text(family="GSC", size=10), legend.position = c(0.1, 0.83)) +
            theme(plot.title = element_text(face = "bold", size = 10)) +
            scale_color_manual(name = NULL,
                               labels = paste(data_att, c("检测","确诊","死亡","住院"), sep=""),
                               breaks = c(col_tested, col_positive, col_death, col_admitted),
                               values = c(col_tested, col_positive, col_death, col_admitted),
                               aesthetics = c("fill","colour")) +
            theme(axis.text.x = element_text(angle=45, hjust=0.8), aspect.ratio = 0.5)

        plot(to_plot)
        showtext_end()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
