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
                                                          choices = list("Geometrica" = "Geometrica", "Normal" = "Normal",
                                                                         "Beta" = "Beta"), selected = "Geometrica"))),
                            
                            
                            conditionalPanel(condition="input.select=='Geometrica'",
                            
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
                              
                              
                                               )),
                            
                            
                            conditionalPanel(condition="input.select=='Normal'",
                                             
                                             fluidRow(
                                               column(3,
                                                      wellPanel(
                                                        
                                                        helpText("Defina um tamanho da amostra, média e variância"),
                                                        numericInput("n1", label="tamanho da amostra", value=100),
                                                        numericInput("mean", label="média", value=0),
                                                        numericInput("var", label="variância", value=1,min=0),
                                                        helpText("Defina x,y,z ângulos do gráfico"),
                                                        numericInput("x", label="x", value=45),
                                                        numericInput("y", label="y", value=45),
                                                        numericInput("z", label="z", value=90)
                                                        
                                                      )),
                                               column(9,
                                                      wellPanel(h3("Máxima Verossimilhança para Distribuição Normal"),
                                                                withMathJax(p("O gráfico abaixo representa a função de log-verossimlhaçada distribuição Normal dada por $$l(\\mu,\\sigma|x_1,x_2,...,x_n) = -\\frac{n}{2}log(2\\pi) - \\frac{n}{2}log(\\sigma^2) -\\frac{\\sum_{i=1}^{n}(x_i-\\mu)^2}{2\\sigma^2}$$"),
                                                                            p("O gráfico de log-verossimilhança é criado a partir de uma amostra aleatória de tamanho n, com média e variância especificada ao lado. Os estimadores de \\mu e \\sigma^2 são respectivamentes dados pela média e a variância amostral")),
                                                                plotOutput("norm"),
                                                                br(),
                                                                p("Pode-se notar que quando o tamanho da amostra aumenta o estimador de máxima verossimilhança se aproxima do valor verdadeiro do parâmetro.")
                                                                
                                                                            ))
                                               
                                               
                                               )),
                            
                            
                            
                            conditionalPanel(condition="input.select=='Beta'",
                                             
                                             fluidRow(
                                               column(3,
                                                      wellPanel(
                                                        
                                                        helpText("Defina um tamanho da amostra, e os parâmetros da Beta"),
                                                        numericInput("n2", label="tamanho da amostra", value=10),
                                                        numericInput("Alpha", label="Alpha", value=2,min=0),
                                                        numericInput("Beta", label="Beta", value=2,min=0),
                                                        helpText("Defina x,y,z ângulos do gráfico"),
                                                        numericInput("x1", label="x", value=45),
                                                        numericInput("y1", label="y", value=45),
                                                        numericInput("z1", label="z", value=90)
                                                      )),
                                               column(9,
                                                      wellPanel(h3("Máxima Verossimilhança para Distribuição Geometrica"),
                                                                withMathJax(p("Os gráficos abaixo representam a função de probabilidade e a função de log-verossimlhaça respectivamente
                                                              da distribuição geométrica. Sua densidade é dada por: $$P(X=x) = p(1-p)^x ,   x=0,1,...$$"),
                                                                            p("O gráfico de log-verossimilhança é criado a partir de uma amostra aleatória de tamanho n, a linha vermelha indica
                                                             o valor verdadeiro do parâmetro e a linha azul indica o valor do estimador de máxima verossimilhança obtido a partir da amostra.")),
                                                                plotOutput("beta"),
                                                                br(),
                                                                p("Pode-se notar que quando o tamanho da amostra aumenta o estimador de máxima verossimilhança se aproxima do valor verdadeiro do parâmetro.")
                                                                
                                                      ))
                                               
                                               
                                             ))
                            
                            
                            
                            
                            
                            
                            )))