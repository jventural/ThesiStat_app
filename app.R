library(shiny)
library(shinythemes)
library(ThesiStats)
library(tidyverse)
library(readxl)
library(pwr)
library(openxlsx)
library(rlang)

ui <- navbarPage("DataClean",
                 theme = shinytheme("cerulean"),

                 tabPanel("Carga y Renombrado",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Carga y Renombrado de Columnas"),
                              fileInput("datafile", "Cargar archivo Excel", accept = c(".xlsx")),
                              textInput("new_names", "Nuevos nombres (separados por coma)", value = ""),
                              textInput("columns", "Índices de columnas (ej: 1:13)", value = ""),
                              textInput("filter_value", "Filtro: Consentimiento (ej: Si)", value = "")
                            ),
                            mainPanel(
                              h4("Datos Procesados"),
                              tableOutput("table")
                            )
                          )
                 ),

                 tabPanel("Renombramiento de Ítems",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Renombramiento de Ítems"),
                              textInput("prefix1", "Prefijo 1", value = ""),
                              textInput("prefix2", "Prefijo 2", value = ""),
                              textInput("inicio", "Inicio", value = ""),
                              textInput("final", "Final", value = ""),
                              # Se usan textInput para los números y luego se convierten a numéricos
                              textInput("n_items1", "Número de ítems 1", value = ""),
                              textInput("n_items2", "Número de ítems 2", value = "")
                            ),
                            mainPanel(
                              h4("Datos con Ítems Renombrados"),
                              tableOutput("table2")
                            )
                          )
                 ),

                 tabPanel("Expresiones Likert y Mapeo",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Detección de Expresiones Likert"),
                              h5("Escala 1"),
                              textInput("scale1_columns", "Columnas (ej: SATQ1:SATQ22)", value = ""),
                              checkboxInput("start_zero1", "Start zero escala 1", value = FALSE),
                              textInput("likert_levels1", "Niveles Likert (separados por coma)", value = ""),
                              tags$hr(),
                              h5("Escala 2"),
                              textInput("scale2_columns", "Columnas (ej: IC1:IC14)", value = ""),
                              checkboxInput("start_zero2", "Start zero escala 2", value = TRUE),
                              textInput("likert_levels2", "Niveles Likert (separados por coma)", value = ""),
                              tags$hr(),
                              h4("Mapeo de Respuestas Alternativas"),
                              textInput("map1_first", "Primer ítem mapeo escala 1", value = ""),
                              textInput("map1_last", "Último ítem mapeo escala 1", value = ""),
                              textInput("map2_first", "Primer ítem mapeo escala 2", value = ""),
                              textInput("map2_last", "Último ítem mapeo escala 2", value = "")
                            ),
                            mainPanel(
                              h4("Datos Finales"),
                              downloadButton("downloadData", "Descargar Excel"),
                              tableOutput("table3")
                            )
                          )
                 )
)

server <- function(input, output) {

  ## 1. Carga de datos y renombrado de columnas ----
  rawData <- reactive({
    req(input$datafile)
    read_excel(input$datafile$datapath)
  })

  processedData <- reactive({
    req(rawData())
    new_names_vector <- strsplit(input$new_names, ",")[[1]] %>% trimws()
    columns_vector <- eval(parse(text = input$columns))

    df_new <- ThesiStats::rename_columns(rawData(),
                                         new_names = new_names_vector,
                                         columns = columns_vector) %>%
      filter(Consentimiento == input$filter_value)
    df_new
  })

  ## 2. Renombramiento de ítems ----
  processedDataRenombrado <- reactive({
    req(processedData())
    df_new <- processedData()

    df_new_renombrado <- rename_items2(df_new,
                                       prefix1 = input$prefix1,
                                       prefix2 = input$prefix2,
                                       inicio = input$inicio,
                                       final = input$final,
                                       n_items1 = as.numeric(input$n_items1),
                                       n_items2 = as.numeric(input$n_items2))
    df_new_renombrado
  })

  ## 3. Detección de expresiones Likert y mapeo de respuestas ----
  finalData <- reactive({
    req(processedDataRenombrado())
    df_new_renombrado <- processedDataRenombrado()

    # Escala 1
    likert_levels1_vector <- strsplit(input$likert_levels1, ",")[[1]] %>% trimws()
    data_scale1 <- df_new_renombrado %>% select(!!parse_expr(input$scale1_columns))
    exp_df_scale1 <- detect_expression_Likert(data_scale1,
                                              start_zero = input$start_zero1,
                                              likert_levels = likert_levels1_vector)
    exp_df1 <- ThesiStats::convert_to_expresions(exp_df_scale1)

    # Escala 2
    likert_levels2_vector <- strsplit(input$likert_levels2, ",")[[1]] %>% trimws()
    data_scale2 <- df_new_renombrado %>% select(!!parse_expr(input$scale2_columns))
    exp_df_scale2 <- detect_expression_Likert(data_scale2,
                                              start_zero = input$start_zero2,
                                              likert_levels = likert_levels2_vector)
    exp_df2 <- ThesiStats::convert_to_expresions(exp_df_scale2)

    # Mapeo de respuestas alternativas
    column_mappings <- list(
      list(c(input$map1_first, input$map1_last), exp_df1),
      list(c(input$map2_first, input$map2_last), exp_df2)
    )

    final_df <- remplace_alternative_response(df_new_renombrado, column_mappings)
    final_df
  })

  # Salidas en cada pestaña
  output$table <- renderTable({
    processedData()
  })

  output$table2 <- renderTable({
    processedDataRenombrado()
  })

  output$table3 <- renderTable({
    finalData()
  })

  # Botón para descargar Datos Finales en Excel
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Datos_Finales_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      final_df <- finalData()
      openxlsx::write.xlsx(final_df, file)
    }
  )
}

shinyApp(ui, server)
