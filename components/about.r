about <- tabItem("about",
                 fluidRow(
                   box(
                     width = 12, 
                     title = "App Overview",
                     icon = icon("door-open"),
                     status = "teal", 
                     HTML("
                          <h2><strong>How to Use this App</strong></h2>
<p>The <strong style='color:dodgerblue'>State Overview</strong> tab provides state and county-level metrics on school demographics, performance, and other socioeconomic factors. The <strong style='color:dodgerblue'>'County Drilldown'</strong> tab provides a more granular look at county-level stats, while also allowing users to browse through a list of school districts in each county. Under <strong style='color:dodgerblue'>'School District Detail,'</strong> you'll find a tool that allows you to compare school districts or counties on a specific variable of interest. Finally, the <strong style='color:dodgerblue'>'School District Explorer'</strong> provides a map and full listing of all of Pennsylvania's 499 school districts (excluding Bryn Athyn School District, which does not contain any schools and was not included anywhere in this dashboard).</p>
<h2><strong>Data Sources</strong></h2>
      <p>This app uses data from the Pennsylvania School Data Project led by <a href='https://www.researchforaction.org/pa-school-data-project/' target='_blank'>Research for Action</a>. The dataset is compiled from a variety of sources, including:</p>
      <ul>
         <li>Pennsylvania Department of Education</li>
         <li>Civil Rights Data Collection (CRDC)</li>
         <li>Common Core of Data (CCD)</li>
         <li>PA Safe Schools</li>
      </ul>
<p>Additional data (median income, school district population) comes from the United States Census Bureau's 2016-2020 American Community Survey (ACS).</p>
")
                   ),
                   box(
                     width = 12,
                     title = "Additional Information",
                     HTML(readLines("components/about-text.html"), "<hr/>"),
                     "Github repo:",
                     socialButton(href = "https://github.com/nmill092/pa_school_browser",
                                  icon = icon("github"))
                   )
                 ))