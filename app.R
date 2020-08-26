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
                   # helpText("首先请选择数据源：纽约州政府 / 纽约市政府；"),
                   radioButtons("data_source", "选择数据源：", choiceNames = c("州政府", "市政府"), choiceValues = c("NYS", "NYC")),
                   conditionalPanel(
                       condition = "input.data_source == 'NYS'",
                       radioButtons("region", "选择区域：", choiceNames = c("纽约州", "纽约市"), choiceValues = c("NYS", "NYC"))
                   ),
                   conditionalPanel(
                       condition = "input.data_source == 'NYC'",
                       radioButtons("region2", "选择区域：", choiceNames = c("纽约市", "LIC-11101", "哥伦比亚大学-10027"), choiceValues = c("NYC", "LIC", "ColumbiaUniv")),
                       helpText("市政府数据中，死亡人数仅包括在死亡前新冠检测结果为阳性的病例；未进行过检测，但是死亡记录为新冠的病例另行统计。")
                   )
                   # helpText("关于数据源的说明："),
                   # helpText("州政府数据以病例的上报日期为准进行统计，包括全州的检测、确诊、住院和死亡人数，以及纽约市的检测和确诊人数。"),
                   # helpText("市政府数据以病例的确诊日期为准进行统计，包括全市的检测、确诊、住院和死亡人数，以及各邮编对应地区的检测和确诊人数。"),
                   # helpText("我们提供LIC（11101）和哥伦比亚大学（10027）地区的历史数据；其他邮编地区仅提供累计检测和确诊数。"),
                   # helpText("因为统计口径的区别，我们认为市政府的住院和死亡数据能更好的反映疫情变化趋势，但是有严重的数据滞后，因此最近3天数据不可靠。同时，市政府的检测数非常不准确，因此确诊率不可靠。")
            ),
            column(4,
                   # selectInput("data_source", "选择数据源：", choices = c("州政府", "市政府")),
                   
                   radioButtons("pos_rate", "选择第二纵轴显示数据：", choiceNames = c("确诊率", "住院/死亡数"), choiceValues = c("YES", "NO")), # data source: NYS or NYC
                   helpText("住院和死亡人数若没有被报告，则不显示。"),
                   
                   textInput("zip_code", "输入邮编查看累计检测和确诊数：", value = 11101),
                   # actionButton("goButtion", "提交")
            )
            # colum(3,
            #       # To add new data. Password required.
            #       passwordInput("password", "Data Entry Password:"),
            #       textInput()
            # )
        )
    ),
    fluidRow(
        column(7, textOutput("positve_tested_zipcode")),
        hr()
    ),
    fluidRow(
        column(7, plotOutput("dualplot")),
        column(5, DT::dataTableOutput("recent_data", width = "100%"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # prepare data
    # covid_data_from_NYS = read.csv("./NYS_dataset.csv", header=TRUE, sep=",")
    # covid_data_from_NYS$Date = format(as.Date(as.character(covid_data_from_NYS$Date), "%m/%d/%y"), "%m/%d")
    # covid_data_from_NYC = read.csv("./NYC_dataset.csv", header=TRUE, sep=",")
    # covid_data_from_NYC$Date = format(as.Date(as.character(covid_data_from_NYC$Date), "%m/%d/%y"), "%m/%d")
    # covid_data_zip_code = read.csv("./tests-by-zcta.csv", header=TRUE, sep=",")
    
    # long_data = covid_data %>% gather(key="data_att", value="counts", -Date)
    
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

    data_source = reactive({
        input$data_source
               # "州政府" = "NYS",
               # "市政府" = "NYC")
    })
    
    region_code = reactive({
        data_source = data_source()
        if (identical(data_source, "NYS"))
            input$region
        else
            input$region2
        # data_source = data_source()
        # region_code = "NYC"
        # if (identical(data_source, "NYS")) {
        #     region_code = switch(input$region,
        #            "纽约州" = "NYS",
        #            "纽约市" = "NYC")
        #     # "LIC-11101" = "LIC",
        #     # "哥伦比亚大学-10027" = "ColumbiaUniv")
        # } else {
        #     region_code = switch(input$region,
        #            "LIC-11101" = "LIC",
        #            "哥伦比亚大学-10027" = "ColumbiaUniv")
        # }
        # # "10019" = "ColumbusCircle_1",
        # # "10044" = "ColumbusCircle_2")
        # return(region_code)
    })

    total_or_new = reactive({
        data_to_plot()$data_att[1]
    })
    
    region_name = reactive({
        region_code = region_code()
        switch(region_code, 
               "NYS" = "纽约州",
               "NYC" = "纽约市",
               "LIC" = "11101",
               "ColumbiaUniv" = "10027")
    })
    
    pos_rate_option = reactive({
        input$pos_rate
    })
    
    data_to_plot = reactive({
        data_source = data_source()
        region_code = region_code()
        if (identical(data_source, "NYC") & identical(region_code, "NYS")){
            data_source = "NYC"
            region_code = "NYC"
        }
        
        data_file = paste(data_source, "_dataset.csv", sep="")
        covid_data = read.csv(data_file, header=TRUE, sep=",")
        
        data_tmp = select(covid_data, Date, contains(region_code)) %>%
            rename(MMDD = Date,
                   NewPositive = paste(region_code, "New.Positive", sep = "."), NewTested = paste(region_code, "New.Tested", sep = "."), 
                   NewAdmitted = paste(region_code, "New.Hospitolized", sep = "."), NewDeath = paste(region_code, "New.Death", sep = "."),
                   TotalPositive = paste(region_code, "Total.Positive", sep = "."), TotalTested = paste(region_code, "Total.Tested", sep = "."),
                   TotalAdmitted = paste(region_code, "Total.Hospitolized", sep = "."), TotalDeath = paste(region_code, "Total.Death", sep = "."))
        
        if (max(c(0, data_tmp$NewTested), na.rm = TRUE) == 0){
            data_return = select(data_tmp, MMDD, contains("Total")) %>%
                rename(Tested = TotalTested, Positive = TotalPositive, Death = TotalDeath, Admitted = TotalAdmitted) %>%
                mutate(data_att = "累计") %>%
                filter(!is.na(Positive))
            # data_att = "累计"
        } else {
            data_return = select(data_tmp, MMDD, contains("New")) %>%
                rename(Tested = NewTested, Positive = NewPositive, Death = NewDeath, Admitted = NewAdmitted) %>%
                mutate(data_att = "新增") %>%
                filter(!is.na(Positive))
            # data_att = "新增"
        }
        data_return$Tested[data_return$Tested < 0] = 0
        return(data_return)
    })
    
    # plot
    output$dualplot = renderPlot({
        
        data1 = data_to_plot()
        data_source = data_source()
        region = region_name()
        region_code = region_code()
        data_att = total_or_new()
        pos_rate = pos_rate_option()
        if (identical(data_source, "NYC") & identical(region_code, "NYS")){
            data_source = "NYC"
            region_code = "NYC"
        }
        left_axis_max = max(c(0, data1$Tested), na.rm = TRUE)
        right_axis_max = max(c(0, data1$Admitted), na.rm = TRUE)
        
        showtext_begin()
        
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
            theme(text=element_text(family="GSC")) +
            theme_bw() +
            theme(legend.position = "top") + # legend.position = c(0.1, 0.83)
            theme(plot.title = element_text(face = "bold", size = 9)) +
            theme(legend.text = element_text(size = 8), 
                  axis.text.x = element_text(angle=330, hjust=0),
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
        data_source = data_source()
        # nrow = length(data1[ , 1])
        region = region_name()
        data_att = total_or_new()
        # data_nrow = as.numeric(data_length())
        # data_nrow = nrow
        data_region_att = paste(region, data_att, sep="\n")
        data1$pos_rate = format(round(data1$Positive / data1$Tested, 3), nsmall = 3)
        colnames(data1) = c("日期", paste(data_region_att, c("确诊","检测","住院","死亡"), sep=""), "data_att", "确诊率")
        # return(data1[(nrow-data_nrow+1):nrow, 1:5])
        return(data1[ , c(1:5,7)] %>% map_df(rev))
    }
    # spacing = "s", width = "auto", align = "c",
    # rownames = FALSE, colnames = TRUE
    )
    
    zip_code = reactive(
        as.character(input$zip_code)
    )
    
    output$positve_tested_zipcode = renderText({
        zip_code = zip_code()
        zip_code = as.integer(zip_code)
        zip_data = read.csv("today.csv", header = FALSE, sep = ",")
        # colnames(zip_data) = c("Zipcode", "Positive", "Tested", "Percent")
        colnames(zip_data) = c("Zipcode", "Positive", "Tested")
        positive = zip_data[zip_data$Zipcode == zip_code, ]$Positive
        tests = zip_data[zip_data$Zipcode == zip_code, ]$Tested
        return_info = c("邮编为", zip_code, "的地区累计确诊 ", as.character(positive), 
                        "，累计检测 ", as.character(tests), 
                        "，确诊率为 ", format(round(positive/tests, 3), nsmall = 3),
                        "。")
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
