about <- tabItem("about",
  fluidRow(
    box(width = 12,
        title = "About this App",
        HTML(readLines("about-text.html"),"<hr/>"),
        socialButton(
          href = "https://dropbox.com",
          icon = icon("dropbox")
        ),
        socialButton(
          href = "https://github.com",
          icon = icon("github")
        )
    )
  )
)