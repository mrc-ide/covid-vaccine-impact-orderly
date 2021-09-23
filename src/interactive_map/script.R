dir.create("web_page", showWarnings = FALSE)
#render the map
rmarkdown::render("make_map.Rmd",
                  output_file = "web_page/web-map.html")
