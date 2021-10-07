#render the report
rmarkdown::render("generate_report.Rmd",
                  output_file = "report_main.pdf")
