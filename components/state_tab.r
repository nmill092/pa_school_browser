state_tab <- tabItem("state",
                     fluidRow(
                       box(
                         title = "Pennsylvania School District Explorer (County-Level Summaries)",
                         collapsible = T,
                         solidHeader = T,
                         status = "primary",
                         footer = p(
                           icon("info-circle"),
                           strong(
                             "Data represent average values across all LEAs in a given county. Click on a county to filter the table below, and click on it a second time to view data for all counties."
                           )
                         ),
                         width = 12,
                         sidebarLayout(
                           sidebarPanel(
                           pickerInput(
                             "varr",
                             strong("Color counties by:"),
                             choices = map_vars
                           ),
                           
                           h5(strong("Summary Stats for Selected Variable"), style="text-align:center"),
                           withSpinner(tableOutput("statsTbl") %>% tagAppendAttributes(class="table"),type = 6, color.background = "white")
                         ), mainPanel(
                           withSpinner(
                             leafletOutput("map1"),
                             type = 6,
                             color.background = "white"
                           )
                         ))
                       )),
                       tabBox(
                         width = 12,
                         myPanel("user","Demographic Profile","demoDT"),
                         myPanel("line-chart","Socioeconomic Profile","socioDT"),
                         myPanel("toolbox","Student/Teacher Support","moneyDT"),
                         myPanel("school","Student Performance","perfDT")
                       )
                     )