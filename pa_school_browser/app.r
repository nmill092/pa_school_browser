# load("prep.Rds")

source("global.r", local = T)
source("components/state_tab.r", local = T)
source("components/county_tab.r", local = T)
source("components/text_modules.r", local = T)
source("components/sd_comparison.r", local = T)
source("components/all_sds.r", local = T)
source("components/about.r", local = T)


db_body <-
  dashboardBody(tabItems(about, state_tab, sd_tab, all_sd, county_tab),
                includeCSS("styles.css"))

db_sideBar = dashboardSidebar(
  id = "menu",
  width = 300,
  sidebarMenu(
    menuItem("About this App", tabName = "about", icon = icon("info")),
    menuItem(
      "State Overview",
      selected = T,
      tabName = "state",
      icon = icon("map-marked-alt")
    ),
    menuItem(
      "County Drilldown",
      tabName = "county",
      icon = icon("search-location")
    ),
    menuItem(
      "School District Detail",
      startExpanded = T,
      menuItem(
        "Compare School Districts",
        tabName = "compare_sd",
        icon = icon("compress-alt")
      ),
      menuItem(
        "School District Explorer",
        tabName = "all_sd",
        icon = icon("chart-bar")
      )
    )
  )
)




ui <-
  dashboardPage(
    title = "PA School Data Browser",
    skin = "blue-light",
    header = dashboardHeader(
      title = strong("PA School Data Browser"),
      titleWidth = 300
    ),
    sidebar = db_sideBar,
    controlbar = dashboardControlbar(skinSelector()),
    body = db_body,
    footer = dashboardFooter(left = "Created by Nile Miller", right = strong(
      icon("linkedin"),
      a(href = "https://www.linkedin.com/in/nile-miller/", target = "_blank", "in/nile-miller")
    ))
  )


server <- function(input, output, session) {
  modal <- modalDialog(
    title = "About this App",
    size = 'm',
    state_app_info,
    easyClose = T
  )
  
  showModal(modal)
  
  observeEvent(input$btn, {
    showModal(modal)
  })
  # STATE server code -------------------------------------------------------
  
  selected_county_var <- reactive({
    varr <- as.numeric(unlist(counties@data[, input$varr]))
    varr
  })
  
  clicked_county <- reactiveVal(unique(counties$COUNTY_NAM))
  
  observeEvent(input$map1_shape_click, {
    clicked_county(input$map1_shape_click$id)
    print(clicked_county())
  })
  
  observeEvent(input$map1_click, {
    clicked_county(unique(counties$COUNTY_NAM))
    print(clicked_county())
  })
  
  
  ## state map ---------------------------------------------------------------
  
  
  
  output$map1 <- renderLeaflet({
    pal <- colorBin(
      "YlGnBu",
      domain = counties@data$medincome,
      bins = 4,
      pretty = T
    )
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      addPolygons(
        layerId = ~ COUNTY_NAM,
        group = "Counties",
        data = counties,
        fill = T,
        fillOpacity = 1,
        color = "#000",
        fillColor = pal(counties@data$medincome),
        highlightOptions = highlightOptions(fillOpacity = 1),
        label = ~ COUNTY_NAM,
        popup = ~ state_county_popup,
        weight = 1,
        popupOptions = list(closeOnClick = T)
      ) %>%
      addLegend(
        title = "Median Income",
        "topright",
        pal = pal,
        values = counties@data$medincome,
        data = counties
      ) %>% addControl(html = "<img src='https://www.pa.gov/wp-content/themes/PA.Gov/img/logo/pa-keystone-large.png' height='20'/>")
    
  })
  
  observe({
    print(selected_county_var())
    
    pal <- colorBin("YlGnBu",
                    domain = selected_county_var(),
                    bins = 4,
                    pretty = T)
    print("re-rendering...")
    
    leafletProxy("map1") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        layerId = ~ COUNTY_NAM,
        group = "Counties",
        data = counties,
        fill = T,
        fillOpacity = 1,
        color = "#000",
        highlightOptions = highlightOptions(fillOpacity = 1),
        label = ~ paste(COUNTY_NAM, "County"),
        fillColor = pal(selected_county_var()),
        weight = 1,
        popup = ~ state_county_popup,
        popupOptions = list(closeOnClick = T)
      ) %>% addLegend(
        "topright",
        title = names(map_vars)[map_vars == input$varr],
        pal = pal,
        values = selected_county_var(),
        data = counties
      )
  })
  
  
  ## state datatables --------------------------------------------------------
  
  
  output$demoDT <- renderDT({
    data <- lea_counties %>%
      select(
        "County" = county,
        "White, %" = enroll_allgrades_white_pct_ttl,
        "Black %" =  enroll_allgrades_black_pct_ttl,
        "Hispanic %" = enroll_allgrades_hisp_pct_ttl,
        "Asian %" = enroll_allgrades_asian_pct_ttl,
        "Two or More Races %" = enroll_allgrades_tr_pct_ttl,
        "American Indian %" = enroll_allgrades_ame_pct_ttl
      ) %>% filter(`County` %in% clicked_county())
    
    
    datatable(
      data,
      caption = "Percent of total enrollment by race.",
      style = "bootstrap",
      class = 'table-hover table-striped',
      options = list(
        dom = "frtip",
        scrollX = T,
        processing = FALSE
      ),
      rownames = FALSE
    )
  })
  
  output$statsTbl <- renderTable({
    state_avg <-
      data.frame(Value = c("Mean" = format(
        mean(selected_county_var(), na.rm = T),
        big.mark = ",",
        digits = 2,
        scientific = F
      )))
    state_med <-
      data.frame(Value = c("Median" = format(
        median(selected_county_var(), na.rm = T),
        big.mark = ",",
        digits = 2,
        scientific = F
      )))
    state_min <-
      data.frame(Value = c("Lowest" = format(
        min(selected_county_var(), na.rm = T),
        big.mark = ",",
        digits = 2,
        scientific = F
      )))
    
    state_max <-
      data.frame(Value = c("Highest" = format(
        max(selected_county_var(), na.rm = T),
        big.mark = ",",
        digits = 2,
        scientific = F
      )))
    rbind(state_avg, state_med, state_min, state_max)
  }, width = "100%", striped = T, hover = T, colnames = F, rownames =
    T)
  
  
  output$socioDT <- renderDT({
    data <- lea_counties %>%
      select(
        "County" = county,
        "Median Income" = medincome,
        "% Eligible for Free/Reduced Lunch" = ccd_free_reduced_lunch_eligible_pct,
        "% Limited English Proficiency" = enrollment_lep_pct
      ) %>% filter(`County` %in% clicked_county())
    
    datatable(
      data,
      style = "bootstrap",
      class = 'table-hover table-striped',
      options = list(
        dom = "frtip",
        scrollX = T,
        processing = FALSE
      ),
      rownames = FALSE
    ) %>% formatCurrency(2, '\U0024', digits = 0)
  })
  
  
  output$moneyDT <- renderDT({
    data <- lea_counties %>%
      select(
        "County" = county,
        "Expenditure per Student" = exp_per_student,
        "Total Revenue" = rev_total,
        "Average Teacher Salary" = prof_teach_salary,
        "Student-Teacher Ratio" = ccd_pupil_teacher_ratio,
        "Support Staff/Student Ratio" = support_per_student
      ) %>% filter(`County` %in% clicked_county())
    
    datatable(
      data,
      style = "bootstrap",
      class = 'table-hover table-striped',
      options = list(
        dom = "frtip",
        scrollX = T,
        processing = FALSE
      ),
      rownames = FALSE
    ) %>% formatCurrency(2:4, '\U0024', digits = 0)
  })
  
  output$perfDT <- renderDT({
    data <- lea_counties %>%
      select(
        "County" = county,
        "4-Year Graduation Rate" = gradrate_4year_total,
        "College-Bound Graduates %" = grads_college_pct,
        "Total Suspensions/Expulsions" = exp_sus
      ) %>%
      filter(`County` != "Philadelphia") %>% filter(`County` %in% clicked_county())
    
    datatable(
      data,
      caption = "Note: College-bound graduate percentage missing for Philadelphia School District.",
      style = "bootstrap",
      class = 'table-hover table-striped',
      options = list(
        dom = "frtip",
        scrollX = T,
        processing = FALSE
      ),
      rownames = FALSE
    )
  })
  
  
  
  # COUNTY server code ------------------------------------------------------
  
  output$county_drilldown_sel <-
    renderText({
      input$county_drilldown_sel
    })
  
  
  
  ## county map --------------------------------------------------------------
  
  
  output$county_tab_map <- renderLeaflet({
    leaflet(shp %>% subset(cty_name == "ADAMS"),
            options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      addPolygons(
        fillOpacity = .7,
        fillColor = "dodgerblue",
        color = "#696969",
        popup = county_drilldown_popup,
        weight = 1,
        highlightOptions = highlightOptions(fillOpacity = 1),
        label = ~ school_dis
      ) %>% addMiniMap()
  })
  
  observe({
    pal <-
      colorBin(
        "YlGnBu",
        domain =  shp@data$medincome,
        bins = 5,
        pretty = T
      )
    
    selected_county <- toupper(input$county_drilldown_sel)
    
    lat <-
      mean(purrr::map_dbl(shp[shp@data$cty_name == selected_county, ]@polygons, ~
                            .@labpt[2]))
    lng <-
      mean(purrr::map_dbl(shp[shp@data$cty_name == selected_county, ]@polygons, ~
                            .@labpt[1]))
    
    data <- shp[shp@data$cty_name == selected_county, ]
    
    leafletProxy("county_tab_map") %>%
      clearShapes() %>%
      addPolygons(
        fillOpacity = .7,
        fillColor = "dodgerblue",
        color = "#696969",
        popup = county_drilldown_popup,
        data = data,
        weight = 1,
        highlightOptions = highlightOptions(fillOpacity = 1),
        label = ~ school_dis
      ) %>%
      flyTo(lng = lng,
            lat = lat,
            zoom = 9) %>% addMiniMap()
    
  })
  
  ## county datatables -------------------------------------------------------
  
  
  output$counties_demoDT <- renderDT({
    data <- lea_plus %>% ungroup() %>%
      filter(county %in% input$county_drilldown_sel) %>%
      select(
        "District" = schoolname,
        "White, %" = enroll_allgrades_white_pct_ttl,
        "Black %" =  enroll_allgrades_black_pct_ttl,
        "Hispanic %" = enroll_allgrades_hisp_pct_ttl,
        "Asian %" = enroll_allgrades_asian_pct_ttl,
        "Two or More Races %" = enroll_allgrades_tr_pct_ttl,
        "American Indian %" = enroll_allgrades_ame_pct_ttl
      )
    
    
    datatable(
      data,
      caption = "Percent of total enrollment by race.",
      style = "bootstrap",
      class = 'table-hover table-striped',
      options = list(
        dom = "frtip",
        scrollX = T,
        processing = FALSE
      ),
      rownames = FALSE
    )
  })
  
  
  
  output$counties_socioDT <- renderDT({
    data <- lea_plus %>% ungroup() %>%
      filter(county %in% input$county_drilldown_sel) %>%
      select(
        "District" = schoolname,
        "Median Income" = medincome,
        "% Eligible for Free/Reduced Lunch" = ccd_free_reduced_lunch_eligible_pct,
        "% Limited English Proficiency" = enrollment_lep_pct
      )
    
    datatable(
      data,
      style = "bootstrap",
      class = 'table-hover table-striped',
      options = list(
        dom = "tip",
        scrollX = T,
        processing = FALSE
      ),
      rownames = FALSE
    ) %>% formatCurrency(2, '\U0024', digits = 0)
  })
  
  
  output$counties_moneyDT <- renderDT({
    data <- lea_plus %>% ungroup() %>%
      filter(county %in% input$county_drilldown_sel) %>%
      select(
        "District" = schoolname,
        "Expenditure per Student" = exp_per_student,
        "Total Revenue" = rev_total,
        "Average Teacher Salary" = prof_teach_salary,
        "Student-Teacher Ratio" = ccd_pupil_teacher_ratio,
        "Support Staff/Student Ratio" = support_per_student
      )
    
    datatable(
      data,
      style = "bootstrap",
      class = 'table-hover table-striped',
      options = list(
        dom = "frtip",
        scrollX = T,
        processing = FALSE
      ),
      rownames = FALSE
    ) %>% formatCurrency(2:4, '\U0024', digits = 0)
  })
  
  output$counties_perfDT <- renderDT({
    data <- lea_plus %>% ungroup() %>%
      filter(county %in% input$county_drilldown_sel) %>%
      select(
        "District" = schoolname,
        "4-Year Graduation Rate" = gradrate_4year_total,
        "College-Bound Graduates %" = grads_college_pct,
        "Total Suspensions/Expulsions" = exp_sus
      )
    
    datatable(
      data,
      caption = "Note: College-bound graduate percentage missing for Philadelphia School District.",
      style = "bootstrap",
      class = 'table-hover table-striped',
      options = list(
        dom = "frtip",
        scrollX = T,
        processing = FALSE
      ),
      rownames = FALSE
    )
  })
  
  
  ## county value boxes ------------------------------------------------------
  
  output$selected_county <- renderValueBox({
    valueBox(
      value = paste(input$county_drilldown_sel, "County"),
      subtitle = paste("Number of School Districts:", counties@data$num_sds[counties@data$COUNTY_NAM ==                                                   input$county_drilldown_sel]),
      icon = icon("map-marked"),
      color = "light-blue"
    )
    
  })
  
  output$county_pop_vb <- renderValueBox({
    data = subset(lea_plus, county == input$county_drilldown_sel)
    valueBox(
      value = format(sum(data$enroll_allgrades_total), big.mark = ","),
      subtitle = "Total Enrollment, All School Districts, All Grades",
      color = "teal",
      icon = icon("child")
    )
  })
  
  
  output$county_medincome_vb <- renderValueBox({
    data = subset(lea_counties, county == input$county_drilldown_sel)
    valueBox(
      value = scales::dollar_format(prefix = "$", big.mark = ",")(data$medincome),
      subtitle = "Median Income, All School Districts",
      color = "maroon",
      icon = icon("wallet")
    )
    
  })
  
  output$county_ptratio_vb <- renderValueBox({
    data = subset(lea_counties, county == input$county_drilldown_sel)
    valueBox(
      value = paste(data$ccd_pupil_teacher_ratio[1], "to 1"),
      subtitle = "Average Student-Teacher Ratio",
      color = "aqua",
      icon = icon("percent")
    )
    
  })
  
  output$county_grad_vb <- renderValueBox({
    data = subset(lea_counties, county == input$county_drilldown_sel)
    valueBox(
      value = paste0(data$gradrate_4year_total[1], "%"),
      subtitle = "Average 4-Year Graduation Rate",
      color = "light-blue",
      icon = icon("user-graduate")
    )
    
  })
  
  output$raceChart <- renderHighchart({
    highchart(google_fonts = T) %>%
      hc_chart(style = list(fontFamily = "Roboto")) %>%
      hc_add_series(
        subset(racepvted,
               county == input$county_drilldown_sel),
        "pie",
        hcaes(x = race, y = round(meanpct, 2)),
        name = "Total Enrollment"
      ) %>% hc_add_theme(hc_theme_smpl()) %>%
      hc_plotOptions(
        series = list(showInLegend = TRUE),
        
        pie = list(
          allowPointSelect = T,
          dataLabels = list(enabled = F),
          showInLegend = TRUE
        )
      ) %>% hc_legend(
        layout = "horizontal",
        floating = F,
        padding = 15,
        backgroundColor = "white",
        borderRadius = "10"
      )
    
  })
  
  
  
  # SD comparison server code -----------------------------------------------
  
  
  ## School district comparison ----------------------------------------------
  
  
  
  output$sds_compare_header <- renderText({
    HTML(
      paste(
        "<div style='margin:auto; display:block; width:100%; text-align:center'><strong>",
        names(map_vars)[map_vars == input$sd_var_of_interest],
        "</strong><br>",
        "<em>Comparing",
        input$compare_1,
        "and",
        input$compare_2,
        "</em></div>"
      )
    )
  })
  
  output$sd_compare_plot <- renderHighchart({
    selected_sds <- c(input$compare_1, input$compare_2)
    variable_name <-
      names(map_vars)[map_vars == input$sd_var_of_interest]
    print(variable_name)
    data <-
      lea_plus %>% ungroup() %>% filter(schoolname %in% selected_sds)
    
    
    highchart() %>% hc_add_series(data,
                                  "column",
                                  hcaes(x = "schoolname",
                                        y = !!input$sd_var_of_interest),
                                  name = variable_name) %>% hc_add_theme(hc_theme_economist()) %>% hc_plotOptions(column = list(dataLabels = list(enabled = T))) %>% hc_xAxis(categories = data$schoolname)
  })
  
  
  ## County comparison ----------------------------------------------
  
  
  output$counts_compare_plot <- renderHighchart({
    selected_counties <- c(input$compare_1_count, input$compare_2_count)
    variable_name <-
      names(map_vars)[map_vars == input$count_var_of_interest]
    
    data <- lea_counties %>% filter(county %in% selected_counties)
    
    
    highchart() %>% hc_add_series(data,
                                  "column",
                                  hcaes(x = "county",
                                        y = !!input$count_var_of_interest),
                                  name = variable_name) %>% hc_add_theme(hc_theme_economist()) %>% hc_plotOptions(column = list(dataLabels = list(enabled = T))) %>% hc_xAxis(categories = data$county)
  })
  
  
  # All SDs -----------------------------------------------------------------
  
  selected_sd_var <- reactive({
    varr <- as.numeric(unlist(shp@data[, input$selected_sd_var]))
    varr
  })
  
  ## All SDs map -------------------------------------------------------------
  
  
  output$all_sds_map <- renderLeaflet({
    pal <- colorBin(
      "YlGnBu",
      domain = shp@data$medincome,
      bins = 4,
      pretty = T
    )
    
    leaflet(shp, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      addPolygons(
        fillOpacity = .9,
        fillColor = ~ pal(medincome),
        color = "#696969",
        popup = county_drilldown_popup,
        weight = 1,
        highlightOptions = highlightOptions(fillOpacity = 1),
        label = ~ school_dis
      ) %>% addLegend(
        "topright",
        title = "Median Resident Income",
        pal = pal,
        values = ~ medincome,
        data = shp
      )
  })
  
  observe({
    pal <-
      colorBin("YlGnBu",
               domain =  selected_sd_var(),
               bins = 5,
               pretty = T)
    
    selected_county <- toupper(input$county_drilldown_sel)
    
    lat <-
      mean(purrr::map_dbl(shp[shp@data$cty_name == selected_county, ]@polygons, ~
                            .@labpt[2]))
    lng <-
      mean(purrr::map_dbl(shp[shp@data$cty_name == selected_county, ]@polygons, ~
                            .@labpt[1]))
    
    
    leafletProxy("all_sds_map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillOpacity = .9,
        fillColor = pal(selected_sd_var()),
        color = "#696969",
        popup = county_drilldown_popup,
        data = shp,
        weight = 1,
        highlightOptions = highlightOptions(fillOpacity = 1),
        label = ~ school_dis
      ) %>% addLegend(
        "topright",
        title = names(map_vars)[map_vars == input$selected_sd_var],
        pal = pal,
        values = selected_sd_var(),
        data = shp
      )
    
  })
  
  
  
  ## All SDs datatables ------------------------------------------------------
  
  output$all_sds_demoDT <- renderDT(server = FALSE, {
    data <- lea_plus %>% ungroup() %>%
      select(
        "District" = schoolname,
        "County" = county,
        "White, %" = enroll_allgrades_white_pct_ttl,
        "Black %" =  enroll_allgrades_black_pct_ttl,
        "Hispanic %" = enroll_allgrades_hisp_pct_ttl,
        "Asian %" = enroll_allgrades_asian_pct_ttl,
        "Two or More Races %" = enroll_allgrades_tr_pct_ttl,
        "American Indian %" = enroll_allgrades_ame_pct_ttl
      ) %>% mutate_if(is.character, as.factor)
    
    
    datatable(
      data,
      caption = "Percent of total enrollment by race.",
      style = "bootstrap",
      class = 'table-hover table-striped',
      filter = "top",
      extensions = c('Buttons', 'AutoFill'),
      options = list(
        dom = "Bfrtip",
        buttons = c('csv', 'excel', 'pdf'),
        autofill = TRUE,
        scrollX = T,
        filter = list(position = 'top', clear = FALSE)
      ),
      rownames = FALSE
    )
  })
  
  
  
  output$all_sds_socioDT <- renderDT(server = FALSE, {
    data <- lea_plus %>% ungroup() %>%
      select(
        "District" = schoolname,
        "County" = county,
        "Median Income" = medincome,
        "% Eligible for Free/Reduced Lunch" = ccd_free_reduced_lunch_eligible_pct,
        "% Limited English Proficiency" = enrollment_lep_pct
      ) %>% mutate_if(is.character, as.factor)
    
    datatable(
      data,
      style = "bootstrap",
      class = 'table-hover table-striped',
      filter = "top",
      extensions = c('Buttons', 'AutoFill'),
      options = list(
        dom = "Bfrtip",
        buttons = c('csv', 'excel', 'pdf'),
        autofill = TRUE,
        scrollX = T,
        filter = list(position = 'top', clear = FALSE)
      ),
      rownames = FALSE
    ) %>% formatCurrency(3, '\U0024', digits = 0)
  })
  
  
  output$all_sds_moneyDT <- renderDT(server = FALSE, {
    data <- lea_plus %>% ungroup() %>%
      select(
        "District" = schoolname,
        "County" = county,
        "Expenditure per Student" = exp_per_student,
        "Total Revenue" = rev_total,
        "Average Teacher Salary" = prof_teach_salary,
        "Student-Teacher Ratio" = ccd_pupil_teacher_ratio,
        "Support Staff/Student Ratio" = support_per_student
      ) %>% mutate_if(is.character, as.factor)
    
    datatable(
      data,
      style = "bootstrap",
      filter = "top",
      extensions = c('Buttons', 'AutoFill'),
      class = 'table-hover table-striped',
      options = list(
        dom = "Bfrtip",
        buttons = c('csv', 'excel', 'pdf'),
        autofill = TRUE,
        scrollX = T,
        filter = list(position = 'top', clear = FALSE)
      ),
      rownames = FALSE
    ) %>% formatCurrency(3:5, '\U0024', digits = 0)
  })
  
  output$all_sds_perfDT <- renderDT(server = FALSE, {
    data <- lea_plus %>% ungroup() %>%
      select(
        "District" = schoolname,
        "County" = county,
        "4-Year Graduation Rate" = gradrate_4year_total,
        "College-Bound Graduates %" = grads_college_pct,
        "Total Suspensions/Expulsions" = exp_sus
      ) %>% mutate_if(is.character, as.factor)
    
    datatable(
      data,
      caption = "Note: College-bound graduate percentage missing for Philadelphia School District.",
      style = "bootstrap",
      class = 'table-hover table-striped',
      filter = "top",
      extensions = c('Buttons', 'AutoFill'),
      options = list(
        dom = "Bfrtip",
        buttons = c('csv', 'excel', 'pdf'),
        autofill = TRUE,
        scrollX = T,
        filter = list(position = 'top', clear = FALSE)
      ),
      rownames = FALSE
    )
  })
  renderDT
  
}

shinyApp(ui = ui, server = server)
