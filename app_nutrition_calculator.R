#Librerías

library(shiny)
library(plyr)
library(readxl)
require(shiny)
require(DT)
require(D3partitionR)
require(magrittr)
require(shinyWidgets)
require(ggplot2)
require(stringr)
require(shinydashboard)
require(data.table)
library(readr)
library(dplyr)


# dataset para el menu personalizado

alimentos_tipo <- fread("./data/alimentos_tipo.csv",sep = ";")

# dataset para Top en propiedad

alimentos <- read_excel("./data/alimentos.xlsx")

# dataset para preguntas

questions <- read.csv(file = './data/preguntas.csv', colClasses = 'character', 
                      header = TRUE, sep = ";")

# Puntuación maxima para cada pregunta

tscore1 <- 1 # Cada pregunta como mucho obtendra un punto
tscore2 <- 1
tscore3 <- 1


# Función para obtener el resultado de la pregunta

chkQuestion <- function(answer, correct, index) {
  mensaje <- if (answer == correct) 'Correct' else 'False'
  return(mensaje)}

# Funcion para calcular la puntuacion de las preguntas

scrQuestion1 <- function(result1) {
  vscore1 <- (result1 == 'False') * -1
  tscore1 <- tscore1 + vscore1
  return(as.numeric(max(0,tscore1)))}


ui <- dashboardPage(
  skin = "green",dashboardHeader(title = "App Nutrientes"), # formato y título del dashboard
                    dashboardSidebar(
                      sidebarMenu( # Menu con las distintas ventanas
                        menuItem("Menu Personalizado", tabName = "Menu", 
                                 icon = icon("fas fa-utensils")), # icono y título de cada pestaña
                        menuItem("Top en Nutrientes", tabName = "Top", 
                                 icon = icon("fas fa-sort-numeric-up")),#icono y título de cada pestaña
                        menuItem("Quiz", tabName = "Quiz", 
                                 icon = icon("fas fa-question")),#icono y título de cada pestaña
                        menuItem("Sobre Nosotros", tabName = "SobreNosotros", 
                                 icon = icon("address-card"))#icono y título de cada pestaña
                        
                      )
                    ),
                    
                    dashboardBody( # cuerpo del dashboard
                      tabItems( # tabItems y dentro cada tabItem
                        
                        tabItem(tabName = "Quiz",h1("Quiz 🧠"), # nombre y título que verá el usuario
                                fluidRow(
                                  imageOutput("picture", height = "auto"),
                                  box(width = 8,h4("Pruebe aquí su conocimiento de las propiedades de los alimentos. ¡Recuerde que
                                     es importante conocerlos para tomar mejores decisiones!")),# caja con la explicación de lo que peude hacer la pestaña
                                  uiOutput("ui"), # guardar una expresión en una variable
                                  
                                  box(width = 5,list(h4('Resultados'), # caja con los resultados del quiz por pregunta
                                           verbatimTextOutput('result1'), # Respuesta de la pregunta 1
                                           verbatimTextOutput('result2'), # Respuesta de la pregunta 2
                                           verbatimTextOutput('result3'), # Respuesta de la pregunta 3
                                           verbatimTextOutput('total'), # Resultado final obtenido
                                           actionButton("update", label = "Nuevo test")))) # botón reactividad para hacer un nuevo test con nuevas preguntas
                                 ),
                        
                        tabItem(
                          tabName = "Top",h1("Alimentos con un macronutriente destacado 🥚🍅🍝🦐"),# nombre y título que verá el usuario
                                  fluidRow(
                                    box(width = 8,
                                        h4("Explore aquí los alimentos ricos en un macronutriente")), # caja con la explicación de lo que peude hacer la pestaña
                                    box(selectInput("propiedad", #inputID
                                                  label = "Elige una propiedad del alimento", # Explicacion para el usuario
                                                  choices = list("Proteinas" = "proteinas", 
                                                                 "Grasas" = "grasas",
                                                                 "Carbohidratos" = "carbohidratos"),
                                                  selected = "unif")),# ninguna seleccion predeterminada
                                    box(width = 10,tableOutput('table')))), # output de la tabla de datos
                        
                        
                        
                        tabItem(tabName = "Menu", h1("Alimentos consumidos hoy 😋"),# nombre y título que verá el usuario
                                fluidRow(
                                  
                                  box(width = 8,h4("Complete con los alimentos hoy ingeridos. ¡Hay muchos tipos!")),
                                  valueBoxOutput("calories"), # caja con la explicación de lo que peude hacer la pestaña
                                  
                                  tabPanel("Menu selection",
                                     column(3, 
                                            uiOutput('seleccion_tipo')), # En esta columna de la pantalla se encuentra el boton reactivo a la seleccion de tipo de alimento
                                     column(5,
                                            uiOutput('seleccion_alimento')), # En esta columna de la pantalla se encuentra el boton reactivo a la seleccion del alimento
                                     column(12,
                                            dataTableOutput('selected_items')) # Output de la tabla
                            )
                          )
                        ),
                        tabItem(tabName = "SobreNosotros",
                                h1("Sobre Nosotros 👥"),# nombre y título que verá el usuario
                                fluidRow(
                                  imageOutput("foto", height = "auto"),
                                  box(width = 8,
                                             h4("Sergio Cañón Laiz e Ignacio Ruiz de Zuazu Echevarría.
                                                          Proyecto para la asignatura 'Técnicas de Visualización' en el Máster 
                                                          de Data Science. Profesor: Alejandro Vidal Mata")), # Descripcion
                                  box(width = 8,
                                      h4(" Ignacio Ruiz de Zuazu Echevarría: 
                                         https://www.linkedin.com/in/ignacio-ruiz-de-zuazu-echevarr%C3%ADa-7a7191183/")), #Linkedin
                                  
                                  box(width = 8,
                                      h4(" Sergio Cañón Laiz: 
                                         https://www.linkedin.com/in/sergio-ca%C3%B1%C3%B3n-laiz/"))                                  ))
                        
                        )))
                        
                        
server <- function(input, output) {
  
  
  ###### Menu personalizado #####
  
  
  vals <- reactiveValues(switch_origin_cal = F) 
  # Cuando lee un valor de él, la expresión reactiva que llama toma una 
  # dependencia reactiva de ese valor
  
  
  output$seleccion_tipo <- renderUI( # Los componentes de la interfaz de usuario 
    # se generan como HTML en el servidor dentro de un bloque renderUI () 
    # y se envían al cliente, que los muestra con uiOutput (). 
    # Cada vez que se envía un nuevo componente al cliente, reemplaza completamente 
    # al componente anterior.
    
    selectizeInput('seleccion_tipo','Selecciona el tipo de ingrediente', 
                   # Tabla para el menú personalizado
                   c('All',unique(alimentos_tipo[['Category']])), 
                   # Se pueden elegir todas y por categoria de cada alimento
                   multiple = T, # permitimos elegir múltiples valores
                   selected = 'All') # Todos como predeterminado
  )
  
  output$seleccion_alimento <- renderUI( 
    # El mismo caso que el anterior, en este caso se realiza para la selección de los alimentos
    selectizeInput('seleccion_alimento','Añade un ingrediente',
                   split(alimentos_tipo[Category %in% input$seleccion_tipo | input$seleccion_tipo == 'All']$Item, 
                         # Cuando se selecciona un tipo de alimentos, unicamente se muestran esos elementos salvo si se selecciona All que son todos los tipos de alimentos
                         alimentos_tipo[Category %in% input$seleccion_tipo | input$seleccion_tipo == 'All',Category]),multiple = T)
  )
  
  
  output$selected_items <- renderDataTable( # Output de la tabla
    {
      alimentos_tipo[Item %in% input$seleccion_alimento,colnames(alimentos_tipo), 
                     # Para todos aquellos items en el boton obtiene todos los datos con el nombre de las columnas
                     with = F]
    },options = list(scrollX = T,dom = 't'),rownames = FALSE 
    # scroll activado para ver toda la tabla
    
  )
  
  

  
  ###### Parte del Quiz ########
  
  pregunta <- reactive({# reactivo y el objeto pregunta pasa a función como "pregunta()"
    input$update # botón activo para hacer un nuevo conjunto de r
    questions[sample(nrow(questions), size = 3),]}) 
  # mezlamos las posibles preguntas y eligimos 3
  
  output$ui <- renderUI({box(
    
    radioButtons('answ1', paste('Q1 -',pregunta()[[1,3]]), # pregunta, fila 1 de la muestra
                 c(pregunta()[[1,5]], # alternativa
                   pregunta()[[1,6]],# alternativa
                   pregunta()[[1,7]],# alternativa
                   pregunta()[[1,4]]), selected = 0),# respuesta
    
    radioButtons('answ2', paste('Q2 -',pregunta()[[2,3]]),# pregunta, fila 2 de la muestra
                 c(pregunta()[[2,5]],# alternativa
                   pregunta()[[2,6]],# alternativa
                   pregunta()[[2,7]],# alternativa
                   pregunta()[[2,4]]), selected = 0),# respuesta
    
    radioButtons('answ3', paste('Q3 -',pregunta()[[3,3]]),# pregunta, fila 3 de la muestra
                 c(pregunta()[[3,5]],# alternativa
                   pregunta()[[3,6]],# alternativa
                   pregunta()[[3,7]],# alternativa
                   pregunta()[[3,4]]), selected = 0),# respuesta
    
    actionButton('enviar', 'Enviar') # boton reactibo para enviar resultados a la función chkQuestion()
  )
  })
  
  rltInput1 <- reactive({input$enviar 
    # boton para comprobar el input del usuario con la respuesta correcta (columna 4)
    isolate(try_default(chkQuestion(input$answ1, pregunta()[[1,4]],3), 
                        # aplicamos la funcion para ver si es correcta o no
                        default = 'Choose', quiet = TRUE))
  })
  rltInput2 <- reactive({input$enviar 
    # boton para comprobar el input del usuario con la respuesta correcta (columna 4)
    isolate(try_default(chkQuestion(input$answ2, pregunta()[[2,4]],3),
                        default = 'Choose', quiet = TRUE))
  })
  rltInput3 <- reactive({input$enviar 
    #nboton para comprobar el input del usuario con la respuesta correcta (columna 4)
    isolate(try_default(chkQuestion(input$answ3, pregunta()[[3,4]],3),
                        default = 'Choose', quiet = TRUE))
  })
  
  
  output$result1 <- renderText({paste('Q1:',rltInput1())}) 
  # damos formato a la salida del resultado que verá el usuario
  
  output$result2 <- renderText({paste('Q2:',rltInput2())})
  
  output$result3 <- renderText({paste('Q3:',rltInput3())})
  
  totalInput1 <- reactive({ 
    scrQuestion1(rltInput1())*(rltInput1() == 'Correct')}) 
  # si es correcta sumamos un punto. Usamos funcion scrQuestion
  totalInput2 <- reactive({
    scrQuestion1(rltInput2())*(rltInput2() == 'Correct')}) 
  # si es correcta sumamos un punto . Usamos funcion scrQuestion
  totalInput3 <- reactive({
    scrQuestion1(rltInput3())*(rltInput3() == 'Correct')}) 
  # si es correcta sumamos un punto . Usamos funcion scrQuestion
  
  output$total <- renderText(paste0("Tu puntuación es: ",totalInput1() + totalInput2() + totalInput3())) # resultado total
  
  output$picture <- renderImage({# Salida de la imagen
    return(list(src = "./imagenes/Foto.jpg",contentType = "image/jpg", alt = "Alignment"))
  }, deleteFile = FALSE) 
  

  ############ Parte de Top en propiedad ##########
  
  dataset <- alimentos[,1:5] 
  # tabla solamente con macronutrientes, calorias y nombre del alimento
  
  output$table <- renderTable({# salida de tabla
    
    if (input$propiedad == "proteinas") { 
      
      head(dataset[order(dataset[,5], decreasing = TRUE),], 100)
      # muestra los 100 alimentos con mayor cantidad del macronutriente elegido

    } else if (input$propiedad == "grasas") { 
      # En el caso en el que la opcion seleccionada no sea la de proteinas, la siguiente opción pasará a ser o grasas o carbohidratos
      
      head(dataset[order(dataset[,4], decreasing = TRUE),], 100)

    } else if (input$propiedad == "carbohidratos") {
      
      head(dataset[order(dataset[,3], decreasing = TRUE),], 100)
    }
    
  })
  
  ######### Sobre nosotros #########
  
  output$foto <- renderImage({ # Salida de la imagen
    return(list(src = "./imagenes/cunef.jpg",contentType = "image/jpg", alt = "Alignment")) 
    # output imagen sobre nosotros
  }, deleteFile = FALSE) 
}

shinyApp(ui = ui, server = server)


