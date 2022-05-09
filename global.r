library(shiny)
library(leaflet)
library(shinydashboard)
library(shinycssloaders)
library(shinydashboardPlus)
library(shinyWidgets)
library(htmltools)
library(glue)
library(rgdal)
library(tidyverse)
library(tidycensus)
library(highcharter)
library(purrr)
library(DT)





# LEA Data Cleaning -------------------------------------------------------

lea <- read_csv("data/LEA_data.csv")

lea_sel <- lea %>% 
  select("county", "schoolname", starts_with("enroll_allgrades"),  "ccd_pupil_teacher_ratio", "exp_total", "rev_total", "prof_total", "ccd_free_reduced_lunch_eligible", "prof_teach_salary", "enrollment_lowinc_pct", "enrollment_lep", "gradrate_4year_total", "grads_college_pct", "prof_teach_salary", "staff_total_ft", starts_with("safesch"))


lea_sel <- lea_sel %>% mutate(exp_sus = rowSums(select(.,starts_with("safesch"))),
                              exp_per_student = exp_total / enroll_allgrades_total, 
                              support_per_student = enroll_allgrades_total/ staff_total_ft) %>% 
  select(-starts_with("safesch"))

lea_sel$support_per_student[is.infinite(lea_sel$support_per_student)] <- NA


## Census Data  ------------------------------------------------------------

census_data <- get_acs(geography = "school district (unified)", variables = c(medincome = "B19013_001", population = "B01003_001"), state="PA", year = 2020)

census_data <- census_data %>% select(-moe) %>% pivot_wider(names_from = variable, values_from = estimate)

census_data <- census_data[-501,] # Remove "school district not defined" row 

census_data <- census_data %>% mutate(NAME = toupper(gsub("School District, Pennsylvania",replacement = "SD",.$NAME))) # Prepare SD names for join 


## Join Census and LEA Data ------------------------------------------------

# Identify records that don't match

unmatched_lea <- lea_sel %>% anti_join(census_data, by = c("schoolname" = "NAME"))
unmatched_census <- census_data %>% anti_join(lea_sel, by = c("NAME" = "schoolname"))

# Bryn Athyn SD apparently doesn't actually contain any schools but people still technically live in the district so it's included in the census data
census_data <- census_data[!census_data$NAME=="BRYN ATHYN SD",] 

lea_sel <- lea_sel %>% mutate(schoolname = case_when(
  schoolname == "CHELTENHAM SD" ~ "CHELTENHAM TOWNSHIP SD",
  schoolname == "HEMPFIELD  SD" ~ "HEMPFIELD SD",
  schoolname == "KEYSTONE  SD" ~ "KEYSTONE SD",
  schoolname == "MT LEBANON SD" ~ "MOUNT LEBANON SD",
  schoolname == "NORTHWESTERN  SD" ~ "NORTHWESTERN SD",
  schoolname == "OWEN J ROBERTS SD" ~ "OWEN J. ROBERTS SD",
  schoolname == "SAINT CLAIR AREA SD" ~ "ST. CLAIR AREA SD", 
  schoolname == "SAINT MARYS AREA SD" ~ "ST. MARYS AREA SD", 
  schoolname == "UPPER SAINT CLAIR SD" ~ "UPPER ST. CLAIR SD",
  schoolname == "WILSON  SD" ~ "WILSON SD",
  TRUE ~ schoolname
))

lea_plus <- lea_sel %>% left_join(census_data, by = c("schoolname" = "NAME"))
lea_plus <- lea_plus %>% relocate(GEOID,.after = medincome)
lea_plus <- lea_plus %>% mutate(support_per_student = round(support_per_student,2))

# Fix character columns 

lea_plus <- lea_plus %>% mutate(ccd_free_reduced_lunch_eligible = as.numeric(ccd_free_reduced_lunch_eligible), 
                                ccd_free_reduced_lunch_eligible_pct = ccd_free_reduced_lunch_eligible/enroll_allgrades_total * 100, 
                                enrollment_lep_pct = enrollment_lep/enroll_allgrades_total * 100,
                                gradrate_4year_total = as.numeric(gradrate_4year_total),
                                grads_college_pct = as.numeric(grads_college_pct),
                                GEOID = as.numeric(GEOID))

lea_plus <-  lea_plus %>% 
  group_by(county) %>% 
  mutate_at(vars(contains("enroll_allgrades")), funs(pct_ttl=round(./enroll_allgrades_total*100,2))) %>% mutate_if(is.numeric, round,digits=2)  %>% filter(schoolname!="BRYN ATHYN SD") %>% mutate_if(is.numeric, round,digits=2)


## county-level summaries --------------------------------------------------

roundedMean <- function(val) { 
  return(round(mean(val,na.rm=T),2))
}

lea_counties <- lea_plus %>% 
  group_by(county) %>%
  summarise_at(vars(-c("schoolname","GEOID","population","enroll_allgrades_total","enroll_allgrades_asian","enroll_allgrades_ame","enroll_allgrades_black","enroll_allgrades_hisp","enroll_allgrades_white","enroll_allgrades_tr",ends_with("pct_ttl"))), roundedMean) %>% mutate_if(is.numeric, round,digits=2)

# Since I can't use summarise_at with multiple functions, but i want to show the total county SD population and number of school districts, i have to make another dataset and then do a join

lea_counties_2 <- lea_plus %>% 
  group_by(county) %>%
  summarise(population = sum(population,na.rm=T))

lea_counties_3 <- lea_plus %>% 
  group_by(county) %>%
  count(name = "num_sds")

lea_counties_4 <- lea_plus %>%
  select(county, ends_with("pct_ttl")) %>% 
  group_by(county) %>% summarise_all(roundedMean)

lea_counties <- lea_counties %>% left_join(lea_counties_2, by = "county")
lea_counties <- lea_counties %>% left_join(lea_counties_3, by = "county")
lea_counties <- lea_counties %>% left_join(lea_counties_4, by = "county") %>% mutate_if(is.numeric, round,digits=2)

## Join in map data --------------------------------------------------------

counties <- readOGR("data/PaCounty2022_04","PaCounty2022_04")


counties$COUNTY_NAM <- str_to_title(counties$COUNTY_NAM)
counties$COUNTY_NAM[counties$COUNTY_NAM=="Mckean"] <- "McKean"


counties@data <- counties@data %>% left_join(lea_counties,by=c("COUNTY_NAM" = "county"))




shp <- readOGR("data/geo_export_a978505f-e2a6-4942-9816-2fb5f567f17d/",
               "geo_export_a978505f-e2a6-4942-9816-2fb5f567f17d")

shp <- subset(shp, school_dis!="Bryn Athyn SD")

shp@data <- shp@data %>% 
  mutate(school_dis = toupper(school_dis)) %>% 
  mutate(school_dis = case_when(
    school_dis == "MT LEBANON SD" ~ "MOUNT LEBANON SD",
    school_dis == "NORTHWESTERN  SD" ~ "NORTHWESTERN SD",
    school_dis == "OWEN J ROBERTS SD" ~ "OWEN J. ROBERTS SD",
    school_dis == "SAINT CLAIR AREA SD" ~ "ST. CLAIR AREA SD", 
    school_dis == "SAINT MARYS AREA SD" ~ "ST. MARYS AREA SD", 
    school_dis == "UPPER SAINT CLAIR SD" ~ "UPPER ST. CLAIR SD",
    TRUE ~ school_dis
  )) %>% left_join(lea_plus,by = c("school_dis" = "schoolname")) 




## race pivot data ---------------------------------------------------------

racebrkdown <- select(lea_plus,
                      c(county,schoolname,starts_with("enroll_allgrades"))) %>% 
  select(-enroll_allgrades_total)

racepvted <- racebrkdown %>% 
  pivot_longer(cols = starts_with("enroll"),names_to = "race") %>% 
  mutate(race = case_when(
    race == "enroll_allgrades_white" ~ "White",
    race == "enroll_allgrades_black" ~ "Black",
    race == "enroll_allgrades_hisp" ~ "Hispanic",
    race == "enroll_allgrades_ame" ~ "American Native",
    race == "enroll_allgrades_asian" ~ "Asian",
    race == "enroll_allgrades_tr" ~ "Two or More Races",
    TRUE ~ "Other"
  )) %>% group_by(county,race) %>% 
  summarise(meanpct = mean(value)) %>% 
  mutate(meanpct = round(meanpct,2))




# custom functions --------------------------------------------------------

myPanel <- function(icon,title,chart) {
  tabPanel( 
    icon = icon(icon), 
    title = title, 
    withSpinner(
      DTOutput(chart), 
      type = 6, 
      color.background = "white"
    ))
}



# text labels -------------------------------------------------------------



state_county_popup <-
  glue(
    "<h3>County: <b style='color:#002A86'>{counties@data$COUNTY_NAM}</b></h3>
              Number of LEAs: <b>{counties@data$num_sds}</b><br/>
              Average Student-Teacher Ratio (all grades): <b>{counties@data$ccd_pupil_teacher_ratio}</b> to 1<br/>
              Average Median Income (all LEAs): <b>{scales::dollar_format(prefix = '$', big.mark= ',')(counties@data$medincome)}</b> <br/>
              Average Expenditure per Student: <b>{scales::dollar_format(prefix = '$',big.mark = ',')(counties@data$exp_per_student)} </b><br>
     Average 4-Year Graduation Rate: <b>{counties@data$gradrate_4year_total} </b><br>
    Average % of College-Bound Graduates: <b>{counties@data$grads_college_pct} </b><br>"
  )


county_drilldown_popup <-
  ~glue(
    "SD Name: <b>{school_dis}</b> <br/>
    Student-Teacher Ratio (all grades):<b>{ccd_pupil_teacher_ratio}<b><br/>
    Median Income: <b>{scales::dollar_format(prefix = '$', big.mark= ',')(medincome)}</b><br/>
    Expenditure per Student: <b>{scales::dollar_format(prefix = '$',big.mark = ',')(exp_per_student)} </b><br/>
    4-Year Graduation Rate: <b>{gradrate_4year_total}</b><br/>
    % of College-Bound Graduates: <b>{grads_college_pct}</b><br/>"
  )


state_app_info <- HTML(
  "<img src='https://source.unsplash.com/7K_agbqPqYo/640x423' width = '100%' /><h2><strong>PA School Data Browser</strong></h2>
  <p>The data used in this app come from the July 2018 <a href='https://www.researchforaction.org/pa-school-data-project/' target='_blank'><strong>PA School Data Project</strong></a> led by Research for Action, which sought to 'bring together comprehensive Pennsylvania education data from various public sources into one spot.' Additional school district-level data come from the United States Census Bureau's 2016-2020 5-year American Community Survey (ACS) (via Kyle Walker's <a href='https://walker-data.com/tidycensus/' target='_blank'>tidycensus R package</a>).</p>"
)

map_vars <- list(
  "Median Resident Income" = "medincome",
  "Student-Teacher Ratio" = "ccd_pupil_teacher_ratio",
  "4-Year Graduation Rate" = "gradrate_4year_total",
  "Expenditure per Student" = "exp_per_student",
  "Percent College-Bound Graduates" = "grads_college_pct",
  "Number of School Districts" = "num_sds",
  "Teacher Salary" = "prof_teach_salary",
  "% Eligible for Free/Reduced Lunch" = "ccd_free_reduced_lunch_eligible_pct",
  "% from Low Income Families" = "enrollment_lowinc_pct",
  "Student-Support Staff Ratio" = "support_per_student",
  "% Students with Limited English Proficiency" = "enrollment_lep_pct"
)