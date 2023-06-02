# https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
# https://zhulab.ucsc.edu/
library(shiny)
library(shinythemes)

ui <-fluidPage(theme = shinytheme("cerulean"),
               
               titlePanel("PhenoInfo"),
               navbarPage("",
                          tabPanel(icon("home"),
                                   fluidRow(column(tags$img(src="What is Phenology.jpg",width="800px",height="800px"),width=8))
                          ),
                          tabPanel("Pheno Intro",
                                   fluidRow(column(tags$img(src="phenology intro 1.png",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                                   fluidRow(column(tags$img(src="phenology intro 2.png",width="600px",height="360px"),width=8))
                          ),
                          
                          tabPanel("Phenology Personal Effect",
                                   fluidRow(column(tags$img(src="Phenology personal effect.png",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                                   fluidRow(column(tags$img(src="Phenology personal effect 2.png",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                                   fluidRow(column(tags$img(src="phenology personal effect 3.png",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                                   fluidRow(column(tags$img(src="phenology personal effect 4.png",width="600px",height="700px"),width=8))
                          ),
                          
                          tabPanel("Climate Change Relation",
                                   fluidRow(column(tags$img(src="climate change relation 1.png",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                                   fluidRow(column(tags$img(src="climate change relation 2.png",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                                   fluidRow(column(tags$img(src="climate change relation 3.png",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                                   fluidRow(column(tags$img(src="climate change relation 4.png",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                                   fluidRow(column(tags$img(src="climate change relation 5.png",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                                   fluidRow(column(tags$img(src="climate change relation 6.png",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                          ),
                          
                          tabPanel("Seen On Our Social Media",
                                   fluidRow(column(tags$img(src="What is Phenology.jpg",width="600px",height="360px"),width=8)),
                                   br(),
                                   fluidRow(column(tags$img(src="What is phenology 2.jpg",width="600px",height="360px"),width=8)),
                                   br(),
                                   fluidRow(column(tags$img(src="What is phenology 3.jpg",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                                   br(),
                                   fluidRow(column(tags$img(src="Hottentot Fig.jpg",width="600px",height="360px"),width=8)),
                                   br(),
                                   fluidRow(column(tags$img(src="Hottentot Fig 2.jpg",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                                   br(),
                                   fluidRow(column(tags$img(src="Scarlet Monkeyflower.jpg",width="600px",height="360px"),width=8)),
                                   br(),
                                   fluidRow(column(tags$img(src="Scarlet Monkeyflower 2.jpg",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                                   br(),
                                   fluidRow(column(tags$img(src="Mountain Lion.jpg",width="600px",height="360px"),width=8)),
                                   br(),
                                   fluidRow(column(tags$img(src="Mountain Lion 2.jpg",width="600px",height="360px"),width=8)),
                                   br(),
                                   br(),
                                   br(),
                                   fluidRow(column(tags$img(src="Black Bear.jpg",width="600px",height="360px"),width=8)),
                                   br(),
                                   fluidRow(column(tags$img(src="Black Bear 2.jpg",width="600px",height="360px"),width=8)),
                                   br(),
                          ),
                          
                          tabPanel("Getting To Know Us",
                                   fluidRow(column(tags$img(src="ucsc-entrance-sign.jpeg",width="400px",height="260px"),width=8)),
                                   br(),
                                   br(),
                                   h1("Yiluan Song"),
                                   fluidRow(column(tags$img(src="Pic of Yiluan.png",width="400px",height="360px"),width=8)),
                                   p("Yiluan is broadly interested in the impacts of climate change and disturbance regime shift on vegetation. Funded by the Hammett Fellowship, she currently focuses on comparing the pace of plant phenological shift to that of climate change using satellite remote sensing data. She is also interested in using statistical and mechanistic models to forecast the future of the earth’s vegetation under global change, in terms of phenology, carbon storage, and distribution. She graduated from the National University of Singapore in 2016 with a Bachelor of Science (Honors) in Life Sciences, with a specialization in Environmental Biology."),
                                   br(),
                                   br(),
                                   h1("Kai Zhu"),
                                   fluidRow(column(tags$img(src="Pic of Kai.jpeg",width="400px",height="360px"),width=8)),
                                   p("Kai is interested in global change ecology, ecological modeling, and environmental data science, where he enjoys integrating ecological theory with advanced tools in statistics and computer science. His current research focuses on plant and soil responses to environmental change, both spatially and temporally. Recent projects include quantifying climate change impacts on forest geographic distribution and growth in North America; synthesizing a multi-factor global change experiment in California grassland; understanding soil fungi and tree mutualisms across geographical gradients in the United States; detecting land surface phenology change in Northern Hemisphere.

Kai is a recipient of the National Science Foundation CAREER Award, an elected Ecological Society of America Early Career Fellow, and a winner of the New Phytologist Tansley Medal. Kai completed his postdoc with Chris Field and Tad Fukami at Stanford University and received his PhD in ecology with Jim Clark and MS in statistics with Alan Gelfand from Duke University."),
                                   br(),
                                   br(),
                                   h1("Marco Conci"),
                                   fluidRow(column(tags$img(src="Pic of marco",width="400px",height="260px"),width=8)),
                                   p("Stuggling to work with technology"),
                                   br(),
                                   br(),
                                   h1("Luke Lastname"),
                                   fluidRow(column(tags$img(src="pic of Luke",width="400px",height="260px"),width=8)),
                                   p("Good at stuff. Good person. Currently interested in life things. NEED TO FILL IN"),
                                   br(),
                                   br(),
                                   
                                   h1("Contact Us"),
                                   p("Email: kai dot zhu at ucsc dot edu")
                          )#,
                          #should do another tab here with hashtage campaign jpgs
               )
)

server <-function(input, output) {}

shinyApp(ui = ui, server = server)