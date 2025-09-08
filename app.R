library(shiny)
library(DT)
library(shinyjs)
library(dplyr)
library(digest)

# directory where responses get stored ####
# getwd()
responsesDir <- file.path("C:/Users/USUARIO/OneDrive/Documentos/Shiny apps/Anteproyectos Share/data")



# variables o preguntas a guardar (Variables) #### 
fieldsAll <-c("name", "pregunta", "replicar_pub", "califique", "info_dta")


# campos obligatorios (*) ####
# para  habilitar boton de envio estos campos son obligarorios 
fieldsMandatory <- c("name", "pregunta")
# Necesitamos usar shinyjspara eso, así que debes agregar una llamada a 
# shinyjs::useShinyjs() cualquier parte de la interfaz de USUARIO UI


# asterisco rojo ####
# en las preguntas o campos obligatorios
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# color rojo del asterisco .mandatory_star { color: red; } 
# color rojo de los mensajes de error error { color: red; }
appCSS <-
  ".mandatory_star { color: red; } 
   #error { color: red; }"


# tiempo de envio de la respuesta ####
epochTime <- function() {
  as.integer(Sys.time())
}

# tiempo nombre archivo ####
# este tiempo es mas que todo para ponerle un nombre unico al archivo
# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
# el tiempo epochTime  guarda en la data la hora de diligenciamiento del formulario
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# archivo csv para exportar ####
# se genera un nombre unico para cada archivo que contiene cada respuesta
# para que sea unico se hace por segundo y hash MD5 del envio
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}


# USUARIO  Interfaz ####

# La mayor parte de este código simplemente configura una aplicación brillante y agrega algunos 
# campos de entrada y un botón a un elemento div llamado FORM

shinyApp(
  
  ui = fluidPage(
    shinyjs::useShinyjs(), # esta linea coordina los campos obligatorios; fieldsMandatory
    shinyjs::inlineCSS(appCSS),# coordina el asperisco rojo para campos obligatorios
    titlePanel("Anteproyectos de investigación UTD - Shiny app"),
    
    div(
      id = "form", # FORM dvi ####
      ## variables dentro de la ui (Variables)####
      textInput("name", labelMandatory("Nombre"), ""), # importante poner el labelMandatory para las que son obligarorias
      textInput("pregunta", labelMandatory("Pregunta de investigación")),
      checkboxInput("replicar_pub", "¿Desea replicar una investigación publicada en otro país para la población colombiana ?", F),
      sliderInput("califique", "Califique su conocimiento en el tema principal de la investigación. Siendo 1 muy poco y 5 especializado", 0, 5, 1, ticks = FALSE),
      selectInput("info_dta", "¿Dispone de información de la población de interés tabulada en hojas de Excel o en algún otro formato estructurado como base de datos u otro?",
                  c("Si",  "No", "Dispongo de información textual en formato pdf .doc o similar")),
     
      
      actionButton("submit", "Enviar respuestas", class = "btn-primary"),
      
      # mensaje de enviaidno respuesta y mensajes de error 
      shinyjs::hidden(
        span(id = "submit_msg", "Enviando a UTD ..."),
        div(id = "error",
            div(br(), tags$b("Error: "), span(id = "error_msg"))
        )
      )
    ),
    
    ## Mensaje de gracias! ####
    shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Gracias, su respuesta se ha guardado satisfactoriamente"),
        actionLink("submit_another", "Envia otra respuesta")
      )
    )  
  ),
  
# SERVIDOR ####  
  server = function(input, output, session) {
    
    # usar la toggleStatefunción para habilitar o deshabilitar el botón de envío según una condición.
    # Esta condición es si se han completado todos los campos obligatorios. Para calcularlo, 
    # podemos recorrer los campos obligatorios y comprobar sus valores. 
    ## check if all mandatory fields have a value ####
    observe({
      
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      # enable/disable the submit button
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    
    ## Recopilar todas las entradas en una data ####
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })
    
  
    # Accion del boton de envio ####
    # ACCION QUE OCURRE CUENDO SE DA CLICK EN EL BOTON DE ENVIO DE RESPUESTAS
    observeEvent(input$submit, {
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      tryCatch({ # este tryCatch entro para funcinamiento al detalle de los mensajes 
        # cuando se oprime el boton 
        saveData(formData()) ### guarda datos en el csv  ####
        shinyjs::reset("form") ### restablece el formulario cuando se envio la respuesta  ####
        shinyjs::hide("form") ### ocultar el formulario   ####
        shinyjs::show("thankyou_msg") ### muestra mensaje de agradecimiento ####
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    # mensaje de enviar otra respuesta ####
    # que hará lo contrario: ocultar el mensaje de agradecimiento y mostrar el formulario
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    
  }
)
