sd_tab <- tabItem(tabName = "compare_sd", 
                  fluidRow(
                    box(width = 12, background = "blue",solidHeader = T,headerBorder = F,
                        span(style="font-size:2rem", "How do different school districts and counties in Pennsylvania stack up against each other? Select two counties or school districts and choose a variable to compare them on. Please note that not all data are available for all school districts or counties."))
                  ),
                  fluidRow(
                  tabBox(
                    width = 12,
                    tabPanel("Compare two Districts", 
                             box(width=12,
                               pickerInput(width=NULL,
                                 inputId = "compare_1",
                                 label = "Select School District #1", 
                                 choices = split(lea_plus$schoolname, lea_plus$county) %>% map_if(~length(.)==1,~list(.)),
                                 options = list(`live-search` = TRUE)
                               ),
                               pickerInput(width=NULL,
                                 inputId = "compare_2",
                                 label = "Select School District #2", 
                                 choices =split(lea_plus$schoolname, lea_plus$county) %>% map_if(~length(.)==1,~list(.)),
                                 selected = "PITTSBURGH SD",
                                 options = list(`live-search` = TRUE)
                               ),
                               pickerInput(
                                 inputId = "sd_var_of_interest",
                                 label = "Variable of Interest", 
                                 choices = map_vars[map_vars!="num_sds"],
                                 options = list(
                                   style = "btn-success")
                               )), 
                             fluidRow(
                               box(title = htmlOutput("sds_compare_header",inline = T),
                                 width = 12,
                                 highchartOutput("sd_compare_plot")
                               )
                             )
                             ),
                    tabPanel("Compare two Counties", 
                             box(width=12,
                                 pickerInput(width=NULL,
                                             inputId = "compare_1_count",
                                             label = "Select County #1", 
                                             choices = sort(unique(lea_counties$county)),
                                             options = list(`live-search` = TRUE)
                                 ),
                                 pickerInput(width=NULL,
                                             inputId = "compare_2_count",
                                             label = "Select County #2", 
                                             choices =sort(unique(lea_counties$county)),
                                             selected = "Beaver",
                                             options = list(`live-search` = TRUE)
                                 ),
                                 pickerInput(
                                   inputId = "count_var_of_interest",
                                   label = "Variable of Interest", 
                                   choices = map_vars[map_vars!="num_sds"],
                                   options = list(
                                     style = "btn-success")
                                 )), 
                             fluidRow(
                               box(title = htmlOutput("counts_compare_header",inline = T),
                                   width = 12,
                                   highchartOutput("counts_compare_plot")
                               )
                             )
                    )
                  )))