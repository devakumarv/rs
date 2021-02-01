library(shiny)
library(shinythemes)

ui<-navbarPage("Ayurvedic Measurements", position = c("static-top"), theme = shinytheme("darkly"),
    tabPanel("Weight",
             tags$style(
                 HTML('
                  #sx{height: 39px}
                  #qx{height: 39px}
                  #ex{height: 39px}')),
    sidebarPanel(
          numericInput(inputId = "sx",label = NULL,value = 0,width = 600,),
          selectInput(inputId = "sy",label ="Ayurvedhic",choices = c("PARAMANU","DHAVANSHI","MARICHI"), 
          )
 ),
         
    sidebarPanel(Position = "right",
                 verbatimTextOutput("rx"),
                 selectInput(inputId = "ry",label = "Modern Metric" ,choices = c("mg","g","kg"))
        
),
mainPanel(
    h5("In this project we introduce a converter that will convert one unit into another form as per the requirement. It Includes conversion from .Ayurvedic classical texts have described weights and measures in ancient system. Many people are now not known with them."),
    tags$br(),
    h5("* Ayurvedic units to modern units of weight,length and time"),
    tags$br(),
    h5("* Conversion in between the units of Ayurvedic, Scientific"),
    tags$br(),
    tags$br(),
    h3("About Unit"),
    tags$br(),
    h5("A unit is a measurement of a quantity that is defined or adopted by tradition or law. Other
       quantities can be expressed as a multiple of the unit. In human history, various unit systems 
       were developed and used in different regions and cultures. Currently, the global standard of 
       measurement is the International System of Units (SI), which is a modern form of the metric system.
       Although SI is intended for global use, it has not been fully adopted, and some other systems of measurement are still used in parts of the world.")
    
)
),
tabPanel("Length",
         sidebarPanel(
             numericInput(inputId = "qx",label = NULL,value = 0),
             selectInput(inputId = "qy",label ="Ayurvedhic",choices = c("YAVODARA",
                                                                        "ANGULA",	
                                                                       "BITAHASTI",	
                                                                        "ARATNI"), 
             )
         ),
         sidebarPanel(Position = "right",
                      verbatimTextOutput("wx"),
                      selectInput(inputId = "wy",label = "Modern Metric" ,choices = c("mm","cm","m","km"))),
         mainPanel(
             h5("In this project we introduce a converter that will convert one unit into another form as per the requirement. It Includes conversion from .Ayurvedic classical texts have described weights and measures in ancient system. Many people are now not known with them."),
             tags$br(),
             h5("* Ayurvedic units to modern units of weight,length and time"),
             tags$br(),
             h5("* Conversion in between the units of Ayurvedic, Scientific"),
             tags$br(),
             tags$br(),
             h3("About Unit"),
             tags$br(),
             h5("A unit is a measurement of a quantity that is defined or adopted by tradition or law. Other
       quantities can be expressed as a multiple of the unit. In human history, various unit systems 
       were developed and used in different regions and cultures. Currently, the global standard of 
       measurement is the International System of Units (SI), which is a modern form of the metric system.
       Although SI is intended for global use, it has not been fully adopted, and some other systems of measurement are still used in parts of the world.")
             
         )
        
),
tabPanel("Time",
         sidebarPanel(
             numericInput(inputId = "ex",label = NULL,value = 0),
             selectInput(inputId = "ey",label ="Ayurvedhic",choices = c("KSANA",
                                                                        "LAVA",	
                                                                        "GHATI",	
                                                                        "YAMA"), 
             )
         ),
         sidebarPanel(Position = "right",
                      verbatimTextOutput("zx"),
                      selectInput(inputId = "zy",label = "Modern Metric" ,choices = c("seconds","minutes","hours"))),
         mainPanel(
             h5("In this project we introduce a converter that will convert one unit into another form as per the requirement. It Includes conversion from .Ayurvedic classical texts have described weights and measures in ancient system. Many people are now not known with them."),
             tags$br(),
             h5("* Ayurvedic units to modern units of weight,length and time"),
             tags$br(),
             h5("* Conversion in between the units of Ayurvedic, Scientific"),
             tags$br(),
             tags$br(),
             h3("About Unit"),
             tags$br(),
             h5("A unit is a measurement of a quantity that is defined or adopted by tradition or law. Other
       quantities can be expressed as a multiple of the unit. In human history, various unit systems 
       were developed and used in different regions and cultures. Currently, the global standard of 
       measurement is the International System of Units (SI), which is a modern form of the metric system.
       Although SI is intended for global use, it has not been fully adopted, and some other systems of measurement are still used in parts of the world.")
             
         )
)
)


server<-function(input,output){
    output$rx<-renderText({
        if (input$sy == "PARAMANU" && input$ry == "mg"){
              (input$sx*0.0016)
        }else if (input$sy == "PARAMANU" && input$ry == "g"){
            (input$sx*1.6e-6)
        }
        else if (input$sy == "PARAMANU" && input$ry == "kg"){
            (input$sx*1.6e-9)
        }
        else if (input$sy == "DHAVANSHI" && input$ry == "mg"){
            (input$sx*0.05)
        }else if (input$sy == "DHAVANSHI" && input$ry == "g"){
            (input$sx*5e-5)
        }
        else if (input$sy == "DHAVANSHI" && input$ry == "kg"){
            (input$sx*5e-8)
        }
        else if (input$sy == "MARICHI" && input$ry == "mg"){
            (input$sx*0.32)
        }else if (input$sy == "MARICHI" && input$ry == "g"){
            (input$sx*0.00032)
        }
        else if (input$sy == "MARICHI" && input$ry == "kg"){
            (input$sx*3.2e-7)
        }
    })
    output$wx<-renderText({
        if (input$qy == "YAVODARA" && input$wy == "mm"){
              (input$qx*2.4)
        }
        else if (input$qy == "YAVODARA" && input$wy == "cm"){
            (input$qx*0.24)
        }
        else if (input$qy == "YAVODARA" && input$wy == "m"){
            (input$qx*0.0024)
        }
        else if(input$qy == "YAVODARA" && input$wy == "km"){
            (input$qx*2.4e-6)
        }
        else if (input$qy == "ANGULA" && input$wy =="mm"){
            (input$qx*19.5)
        }
        else if (input$qy == "ANGULA" && input$wy =="cm" ){
            (input$qx*1.95)
        }
        else if(input$qy == "ANGULA" && input$wy =="m"){
            (input$qx*0.0195)
        }
        else if(input$qy == "ANGULA" && input$wy =="km"){
            (input$qx*1.95e-5)
        }
        else if (input$qy == "BITAHASTI" && input$wy =="mm"){
            (input$qx*228.6)
        }
        else if (input$qy == "BITAHASTI" && input$wy =="cm"){
            (input$qx*22.86)
        }
        else if (input$qy == "BITAHASTI" && input$wy =="m"){
            (input$qx*0.2286)
        }
        else if (input$qy == "BITAHASTI" && input$wy =="km"){
            (input$qx*0.0002286)
        }
        else if (input$qy == "ARATNI" && input$wy =="mm"){
            (input$qx*419.1)
        }
        else if (input$qy == "ARATNI" && input$wy =="cm"){
            (input$qx*41.91)
        }
        else if (input$qy == "ARATNI" && input$wy =="m"){
            (input$qx*0.4191)
        }
        else if (input$qy == "ARATNI" && input$wy =="km"){
            (input$qx*0.0004191)
        }
        })
    output$zx<-renderText({
        if (input$ey == "KSANA" && input$zy == "seconds"){
            (input$ex*0.38)
        }
        else if (input$ey == "KSANA" && input$zy == "minutes"){
            (input$ex*0.006333333)
        }
        else if (input$ey == "KSANA" && input$zy == "hours"){
            (input$ex*0.00010555555)
        }
        else if (input$ey == "LAVA" && input$zy =="seconds"){
            (input$ex*0.77)
        }
        else if (input$ey == "LAVA" && input$zy =="minutes" ){
            (input$ex*0.01283333)
        }
        else if(input$ey == "LAVA" && input$zy =="hours"){
            (input$ex*0.0002138889)
        }
        else if (input$ey == "GHATI" && input$zy =="seconds"){
            (input$ex*1440)
        }
        else if (input$ey == "GHATI" && input$zy =="minutes"){
            (input$ex*24)
        }
        else if (input$ey == "GHATI" && input$zy =="hours"){
            (input$ex*0.4)
        }
        else if (input$ey == "YAMA" && input$zy =="seconds"){
            (input$ex*10800)
        }
        else if (input$ey == "YAMA" && input$zy =="minutes"){
            (input$ex*180)
        }
        else if (input$ey == "YAMA" && input$zy =="hours"){
            (input$ex*3)
        }
        
        
    })
}

shinyApp(ui = ui,server = server)
