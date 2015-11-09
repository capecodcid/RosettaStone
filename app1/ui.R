shinyUI(fluidPage(
  
  titlePanel("Rosetta Stone Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Data and Options",align="center"),
      
      # Need to be able to upload data
      # eventually might want to make that automated
      fileInput("File2022", label = h5("2022 .csv")),
      fileInput("File2023", label = h5("2023 .csv")),
      
      # Need to be able to select teacher (drop down)
      selectInput("proctor", label = h5("Proctor"),
                  choices = list("epstein" = "epstein", 
                                 "sinha"    ="sinha"  ,
                                 "olivo"    ="olivo"  ,
                                 "delanos"  ="delanos",
                                 "caraza"   ="caraza" ,
                                 "rupani"   ="rupani" ,
                                 "allcock"  ="allcock",
                                 "gibson"   ="gibson" ,
                                 "payette"  ="payette"), selected = "gibson"),
     # Need to be able to select grade  
      selectInput("grade", label = h5("Grade"),
                  choices = list("9th" = '9',
                                 "10th" = '10'), selected = '9'),
     
     # Need to be able to select week (could make this automated)
     sliderInput("week", label = h5("Week of School Year"), min=1, max=weeksPerYear, value=10)
    ),
    mainPanel(
      h1("Tracker:"),
      plotOutput("trackerPlot")
    )
  )
))