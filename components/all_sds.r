all_sd <- tabItem(tabName = "all_sd",
                  fluidRow(
                    box(
                      width = 12,
                      title = "All School Districts",
                      pickerInput(
                        "selected_sd_var",
                        strong("Color school districts by:"),
                        choices = map_vars[map_vars != "num_sds"]
                      )
                    ),
                    box(
                      width = 12,
                      withSpinner(
                        leafletOutput("all_sds_map"),
                        type = 6,
                        color.background = "white"
                      )
                    )
                  ),
                  fluidRow(tabBox(
                    width = 12,
                    myPanel("user", "Demographic Profile", "all_sds_demoDT"),
                    myPanel("line-chart", "Socioeconomic Profile", "all_sds_socioDT"),
                    myPanel("toolbox", "Student/Teacher Support", "all_sds_moneyDT"),
                    myPanel("school", "Student Performance", "all_sds_perfDT")
                  )))