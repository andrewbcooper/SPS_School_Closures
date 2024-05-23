library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(htmlTable)
library(lubridate)
library(stringr)
library(stringi)
library(scales)

load('Closure.RData')

SpannerNames <- c("","2020 Building Info (Lower condition score is better)","2023 School Enrollment","2023 School Demographics","2023 Climate Survey (% Favorable)",
                  "2023 Special Ed Services","Seattle Racial & Social Equity Composite Index","% Area in Redlining Categories"
)

SpannerNames2 <- c("2020 Building Info (Lower condition score is better)","2023 School Enrollment","2023 School Demographics","2023 Climate Survey (% Favorable)",
                  "2023 Special Ed Services","Seattle Racial & Social Equity Composite Index","% Area in Redlining Categories"
)

mytargetschool = sort(NearestSchools$FocusSchool)
mycustomschool = sort(NearestSchools$FocusSchool)


myTrendMetric = TrendMetrics                                
names(myTrendMetric) <- c(TrendMetrics[1:2],paste("%",TrendMetrics[3:21]))


# Define server logic 
server <- function(input, output,session) {
  
  session <- sessionInfo()
  version <- paste0(session$R.version$major,".",session$R.version$minor)
  
  
  
  NearestNeighborData <- reactive({
    MyFocus = NearestSchools[which(NearestSchools$FocusSchool==input$school_name),][1,] %>% 
      as.vector() %>% unlist() %>% str_replace_all(.,"[^[:alnum:]]","") 
    
    School_table<- SchoolSummary2 %>% filter(MatchName %in% MyFocus) %>% 
      mutate(MatchName = ordered(MatchName,MyFocus)) %>% arrange(MatchName)
    
    School_table[-2] %>% t() %>% as.data.frame() %>% setNames(.,c("Target School","1st Nearest","2nd Nearest","3rd Nearest"))
  })
  
  SchoolSummaryv2 <- reactive({SchoolSummary_fancy %>% 
      select(School,input$district_variable_x,input$district_variable_y)
  })
  
  NearestNeighborTable <- reactive({
    css.cell.NN <- matrix('padding-left: 2.5em; padding-right: 1em;', nrow(NearestNeighborData()), ncol(NearestNeighborData()))
    css.cell.NN[1,] <- 'padding-left: 2.5em; padding-right: 1em; font-size: 1.25em;;'
    # Create the table (using table from htmlTables doc as example)
    HTML(
      htmlTable(NearestNeighborData(),css.cell = css.cell.NN,
                tspanner = SpannerNames,
                n.tspanner = c(1,2,4,20,12,6,5),
                col.rgroup = c("none", "#d8d8d8"),
                css.tspanner = "color: #007BED; font-size: 1.5em;text-align: left;",
                css.tspanner.sep = "border-top: 3px solid;")
    )
    
  })
  
  output$NNTable <- renderUI({NearestNeighborTable()})
  
  
  CustomCompareData <- reactive({
    
   ColumnNames = c(input$school_name,input$custom_school_name) %>%
      gsub("Elementary","",.) %>%
     gsub("School","",.)
    
    MyFocus2 = c(input$school_name,input$custom_school_name) %>% str_replace_all(.,"[^[:alnum:]]","") 
    
    CustomSchool_table<- SchoolSummary2 %>% filter(MatchName %in% MyFocus2) %>% 
      mutate(MatchName = ordered(MatchName,MyFocus2)) %>% arrange(MatchName)
    
    CustomSchool_table[-c(1,2)] %>% t() %>% as.data.frame() %>% 
      setNames(.,ColumnNames)
  })
    

  CustomCompareTable <- reactive({
    css.cell.CC <- matrix('padding-left: 2.5em; padding-right: 1em;', nrow(CustomCompareData()), ncol(CustomCompareData()))
    HTML(
      htmlTable(CustomCompareData(),css.cell = css.cell.CC,
                tspanner = SpannerNames2,
                n.tspanner = c(2,4,20,12,6,5),
                col.rgroup = c("none", "#d8d8d8"),
                css.tspanner = "color: #007BED; font-size: 1.5em;text-align: left;",
                css.tspanner.sep = "border-top: 3px solid;")
    )
    
  })
  
  output$CCTable <- renderUI({CustomCompareTable()})
  
  output$TrendsNN <- renderPlot({
    
    MyFocus = NearestSchools[which(NearestSchools$FocusSchool==input$school_name),][1,] %>% 
      as.vector() %>% unlist() %>% str_replace_all(.,"[^[:alnum:]]","") 
    
    MyFocusLabels = NearestSchools[which(NearestSchools$FocusSchool==input$school_name),][1,] %>% 
      as.vector() %>% unlist() %>% unname() %>% gsub("Elementary","",.) %>% gsub("School","",.)
    
    
    TrendYFormat <- ifelse(input$trend_variable != "Enrollment",label_percent(),label_number_auto())
  
    School_trends <- Schools %>% filter(MatchName %in% MyFocus) %>% mutate(MatchName = ordered(MatchName,MyFocus)) %>% arrange(MatchName)
  
  School_trends %>% ggplot(aes(x=SchoolYr,y=get(input$trend_variable),color=MatchName,linewidth=MatchName)) + 
    geom_line() +
    scale_x_continuous(name="School Year",breaks=seq(2010,2023,2))+
    scale_y_continuous(name = input$trend_variable,labels=TrendYFormat)+
    scale_color_discrete(name="",labels=MyFocusLabels) +
    scale_linewidth_manual(name="",values=c(4,2,1,.5),labels=MyFocusLabels)+
    theme_bw() + 
    theme(text = element_text(size = 20))
  
  })
  
  output$TrendsCC <- renderPlot({
    
    MyFocus3 = c(input$school_name,input$custom_school_name)  %>% 
      str_replace_all(.,"[^[:alnum:]]","") 
    
    MyFocusLabels3 = c(input$school_name,input$custom_school_name) %>% gsub("Elementary","",.) %>% gsub("School","",.)
    
    
    TrendYFormat3 <- ifelse(input$trend_variable != "Enrollment",label_percent(),label_number_auto())
    
    School_trends3 <- Schools %>% filter(MatchName %in% MyFocus3) %>% mutate(MatchName = ordered(MatchName,MyFocus3)) %>% arrange(MatchName)
    
    School_trends3 %>% ggplot(aes(x=SchoolYr,y=get(input$trend_variable),color=MatchName,linewidth=MatchName)) + 
      geom_line() +
      scale_x_continuous(name="School Year",breaks=seq(2010,2023,2))+
      scale_y_continuous(name = input$trend_variable,labels=TrendYFormat3)+
      scale_color_discrete(name="",labels=MyFocusLabels3) +
      scale_linewidth_manual(name="",values=c(4,2,1,.5),labels=MyFocusLabels3)+
      theme_bw() + 
      theme(text = element_text(size = 20))
    
  })
  
  output$DistrictPlot <- renderPlot({
  
    TrendYFormat.x <- ifelse(input$district_variable_x %in% c("Building Condition Score","Enrollment","Socio-economic Disparity Score","Health Disparity Score"),label_number_auto(),label_percent())
    TrendYFormat.y <- ifelse(input$district_variable_y %in% c("Building Condition Score","Enrollment","Socio-economic Disparity Score","Health Disparity Score"),label_number_auto(),label_percent())
    
    SchoolSummaryv2() %>% ggplot(aes(x=SchoolSummaryv2()[[2]],y=SchoolSummaryv2()[[3]])) + geom_point() + 
      scale_y_continuous(name=input$district_variable_y, labels=TrendYFormat.y)+
      scale_x_continuous(name=input$district_variable_x,labels=TrendYFormat.x)+
      theme_bw() + 
      theme(text = element_text(size = 20))
  })
  
  output$click_info2 <- renderTable({
  #  SchoolSummaryv2 <- SchoolSummary %>% select(ElementarySchool,input$district_variable_x,input$district_variable_y)
    silly1 <- nearPoints(SchoolSummaryv2(),input$plot_click2,xvar = names(SchoolSummaryv2())[2], yvar = names(SchoolSummaryv2())[3]) #%>% select(ElementarySchool,input$district_variable_x,input$district_variable_y)
    silly2 <- brushedPoints(SchoolSummaryv2(),input$plot_brush,xvar = names(SchoolSummaryv2())[2], yvar = names(SchoolSummaryv2())[3]) #%>% select(ElementarySchool,input$district_variable_x,input$district_variable_y)
    silly <- rbind(silly1,silly2)
    silly
  })
}

# Define UI for application
ui <- fluidPage(
  titlePanel('SPS Elementary & K-8 School Comparison Tool'),
  sidebarLayout(
    sidebarPanel(width = 5,
      
      h5("This tool allows users to compare Seattle Public Schools Elementary and K-8 Schools across a range of school- and attendance-area-specific variables. 
      These variables include school demographics and enrollment, school building condition and 'right-sized' capacity, student opinions about each school's climate and
      environment (Climate Survey), Special Education resources at each school, along with community-level metrics applied to attendance areas
      for Socio-economic disparity, health disparity, and demographics. The final set of variables include the neighborhood redlining classifications
      from the 1930s and 1940s applied to school attendance areas. This set of  data is included so that
      users can explore the how historic racist and discriminatory housing policies continue to shape our neighborhoods and educational landscape. 
      Variable names are those used in the original datasource.
         Links to data sources and downloads, along with more detailed descriptions, are available on the 'Read Me' tab"),
  
      h4("Nearest-neighbor Summary Tab:"),
      
      h6("After users choose a focal school, this tab displays comparison data for the 3 schools closest to that focal school (based on 
      linear distance, not actual travel times."),
      
 
      pickerInput("school_name", "Select a target / focal school",
                  mytargetschool,selected=mytargetschool[1],
                  multiple=FALSE,
                  options = pickerOptions(
                    title = 'Click to see options'
                  )
      ),
      
      h4("Nearest-neighbor Trends Tab:"),
         
      h6("After users choose a variable, this tab displays that variable over time for the 4 schools shown in the 'Nearest-neighbor 
         Summary Tab'.  Note, only the variables provided in OSPI's annual report cards are available for trending over time."),
      
      pickerInput("trend_variable", "Select a variable for comparing trends",
                  myTrendMetric, selected=myTrendMetric[1],
                  multiple=FALSE,
                  options = list(
                    "title" = 'Click to see options'
                  )
      ),   
      
      h4("Customized Summary Tab:"),
      h6("After users choose a focal school (above) and up to 3 schools in the selector (below), this tab displays comparison data for these schools."),
      
         pickerInput("custom_school_name", "Select up to 3 schools for Customized Comparisons",
                     mytargetschool,
                     multiple=TRUE,
                     options = list(
                       "title" = 'Click to see options',
                       "max-options" = 3
                     )
         ),
    
      
      h4("Customized Trends Tab:"),
      
      h6("This tab shows the trends for the variable selected above for the schools selected for customized comparisons.
         Note, only the variables provided in OSPI's annual report cards are available for trending over time."),
      
      h4("District-wide patterns:"),
      
      h6("This tab allows users to compare specific metrics for all schools across the district.  Comparing school and attendance-area demographic
         data with Climate Survey and Redlining data may be of particular interest to some users."),
      
      pickerInput("district_variable_x", "Select a variable for district-wide comparison (x-axis)",
                  DistrictMetrics, selected=DistrictMetrics[1],
                  multiple=FALSE,
                  options = list(
                    "title" = 'Click to see options'
                  )
      ),
      
      pickerInput("district_variable_y", "Select a variable for district-wide comparison (y-axis)",
                  DistrictMetrics, selected=DistrictMetrics[2],
                  multiple=FALSE,
                  options = list(
                    "title" = 'Click to see options'
                  )
      ),
      br(),
      HTML("Please direct questions, comments, and suggestions to <a href='mailto:andrew_b_cooper@yahoo.com'>Andy Cooper</a>")
      
      
  ),
    mainPanel(width = 7,
      tabsetPanel(type="tabs",
                  
        tabPanel("Nearest Neighbor Summary",
            tableOutput('NNTable')
        ),
        
        tabPanel("Nearest Neighbor Trends",
             plotOutput("TrendsNN")
        ),
        
        tabPanel("Custom Summary",
                 tableOutput('CCTable')
        ),
        
        tabPanel("Custom Trends",
                 plotOutput("TrendsCC")
        ),
        
        tabPanel("District-wide patterns",
                 plotOutput("DistrictPlot",click="plot_click2",brush="plot_brush"),
                 h4("Click a point or click-and-drag to select multiple points for more information"),
                 tableOutput("click_info2")
        ),
        tabPanel("Read Me",
                 HTML("This tool allows users to compare Seattle Public Schools Elementary and K-8 Schools across a range of school- and attendance-area-specific variables. 
      <p>
      <p>Data for enrollment and school demographics are taken from the <a href='https://washingtonstatereportcard.ospi.k12.wa.us/'>2010-2023 Washington Office of Superintendent of Public Instruction's (OSPI) annual report cards</a>. Annual
      percent change in enrollment was calculated using Sens Slope applied to the natural-log of Enrollment.  Sens Slope is appropriate for time-series data and is robust to outliers.
      <p>
      <p> School building condition and 'right-sized capacity' are taken from the <a href = 'https://www.seattleschools.org/departments/capital-projects-and-planning/facilities-master-plan/'>SPS '2021 Facilities Master Plan Update'</a>. Because this information has not been updated since 2021,
      not all buildings are listed and data for some buildings may not reflect their current contion or 'right-sized' capacity.  Note, '% Capacity' is calculated
      by diving Enrollment by this 'right-sized' capacity and therefore may not refect the true percent capacity over time.
      <p>
      <p> 'Climate Survey' data are taken from <a href = 'https://www.seattleschools.org/departments/rea/district-surveys/'>SPS's 2023 Student Survey of School Climate</a>. This survey asks a range of questions to students at each school and then categorizes those responses
      into specific topic areas.  The reported score is the percent of students at that school who gave 'favorable' scores to 
      those questions. Higher values imply the school is doing a more favorable job in those areas.
      <p>
      <p>Special Education services data is taken from <a href = 'https://www.seattleschools.org/enroll/about-our-schools/service-and-program-locations/#b1dc1fccb780'>SPS's 2023-2024 'Linked Elementary Schools and Location of Student Services' report</a>. 
      <p>
      <p>Community-level data on Socio-economic disparity, health disparity, and demographics are taken from the <a href='https://data-seattlecitygis.opendata.arcgis.com/datasets/SeattleCityGIS::racial-and-social-equity-composite-index-current/about'>Racial and Social Equity Composite Index created by
      City of Seattle Office of Planning & Community Development</a>.  Attendance-area values were created by intersecting the attendance area with
      the reporting areas of the report, and then calculating a weighted average score where weights were equal to the population size in those City of Seattle reporting areas.
      Due to partial overlaps of some reporting areas with the attendance boundaries, this weighted average is only an approximation of true, but unknown, score for the attendance area.
      <p>
      <p>The final set of variables are taken from <a href='https://dsl.richmond.edu/panorama/redlining/data/WA-Seattle'>'Mapping Inequality: Redlining in New Deal America'</a>
      which uses data from the City Survey Files, 1935-1940 at the National Archives.  This data contains neighborhood classifications used by
      the Home Owners' Loan Corporation (HOLC) which explicitly downgraded ratings of neighborhoods based on the prevalence of non-white people and immigrants (among other considerations). 
      This classification resulted in racist and discriminatory home loan and redlining policies.  Values for these variables were created by 
      intersecting the school attendance areas with these HOLC classifications, calculating the area of each intersection, and then dividing by the sum
      of the areas for all intersections within the school attendance area.  Because not all areas were classified, this estimate is only an approximation of how the HOLC would have
      classified the attendance area. The data is included here so that
      users can see the how these policies from the 1930's and 1940's continue to shape our neighborhood and educational landscape. 
      <p>
      <p>Geographic data for
         schools and school attendance areas are taken from the <a href='https://data.seattle.gov/browse?q=School&sortBy=relevance'>City of Seattle's Open Data Portal</a>.  
         
       <p>
       <p> Data and code can be found at <a href='https://github.com/andrewbcooper/SPS_School_Closures'>Andy's GitHub Page</a>.
      <p>Please direct questions, comments, and suggestions to <a href='mailto:andrew_b_cooper@yahoo.com'>Andy Cooper</a>"
      
                      
                      
                      )
                 )
        
    )

)))

shinyApp(ui = ui, server = server)
