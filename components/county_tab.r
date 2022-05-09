source("components/text_modules.r")

county_tab <- tabItem(
  "county",
  fluidRow(
    box(
      width = 12,
      status = "primary",
      headerBorder = F,
      style = "background-color:#fff; color:#002A86",
      pickerInput(
        "county_drilldown_sel",
        label = h2(style = "display:inline-block", span(icon("list-alt")), strong("Select a county: ")),
        choices = sort(unique(counties@data$COUNTY_NAM)),
        options = list(`live-search` = TRUE)
      )
    )
  ),
  fluidRow(valueBoxOutput("selected_county", width = 12)),
  fluidRow(
    box(
      width = 12,
      collapsible = T,
      title = "County School District Map (click on a district to view details)",
      withSpinner(
        leafletOutput("county_tab_map"),
        type = 6,
        color.background = "white"
      )
    )
  ),
  fluidRow(
    box(
      width = 12,
      icon = icon("search"),
      solidHeader = T,
      title = HTML(
        paste(
          "<h3 style='display:inline; text-transform:uppercase;'><strong>",
          textOutput("county_drilldown_sel", inline = T),
          "County Schools at a glance",
          "</h3><strong>"
        )
      ),
      box(
        width = 4,
        title = "Demographic Breakdown",
        highchartOutput("raceChart")
      ),
      valueBoxOutput("county_pop_vb", width =
                       8),
      valueBoxOutput("county_medincome_vb", width =
                       8),
      valueBoxOutput("county_ptratio_vb", width =
                       8),
      valueBoxOutput("county_grad_vb", width =
                       8)
    )
  ),
  fluidRow(tabBox(
    width = 12,
    tabPanel(
      myPanel("user", "Demographic Profile", "counties_demoDT"),
      myPanel("line-chart", "Socioeconomic Profile", "counties_socioDT"),
      myPanel("toolbox", "Student/Teacher Support", "counties_moneyDT"),
      myPanel("school", "Student Performance", "counties_perfDT")
    )
  ))
)
