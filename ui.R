if (!require("devtools"))
  install.packages("devtools")


if (!require("shinysky")) devtools::install_github("ShinySky","AnalytixWare")
library(shinysky)
if (!require("lattice"))
  install.packages("lattice")


library(lattice)
shinyUI(navbarPage("Estimação de parâmetros",
                            tabPanel("Tópicos",
                            tags$head(tags$link(rel = "icon", type = "image/x-icon", href = 
                                                  "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),  
                            
                            
                            fluidRow(column(3,selectInput("select", label = h3("Escolha uma Distribuição"), 
                                                          choices = list("Geométrica" = 1, "Normal" = 2,
                                                                         "Beta" = 3), selected = 1))),
                            
                            
                            
                            
                            
                            fluidRow(
                              column(3,
                                     wellPanel(
                                       
                                       helpText("Defina um tamanho da amostra e o parâmetro p"),
                                       numericInput("n", label="tamanho da amostra", value=10),
                                       #shinyalert("shinyalert2", TRUE, auto.close.after=10),
                                       numericInput("prob", label="valor de p entre (0,1)", value=0.5, min=0,max=1)
                                       
                                       
                                     )),
                              column(9,
                                     wellPanel(h3("Máxima Verossimilhança para Distribuição Geometrica"),
                                               withMathJax(p("Os gráficos abaixo representam a função de probabilidade e a função de log-verossimlhaça respectivamente
                                                              da distribuição geométrica. Sua densidade é dada por: $$P(X=x) = p(1-p)^x ,   x=0,1,...$$"),
                                                           p("O gráfico de log-verossimilhança é criado a partir de uma amostra aleatória de tamanho n, a linha vermelha indica
                                                             o valor verdadeiro do parâmetro e a linha azul indica o valor do estimador de máxima verossimilhança obtido a partir da amostra.")),
                                               plotOutput("pmf"),
                                               br(),
                                               p("Pode-se notar que quando o tamanho da amostra aumenta o estimador de máxima verossimilhança se aproxima do valor verdadeiro do parâmetro.")
                                               
                                               ))
                              
                              
                                               ))))