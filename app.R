#-------------------------------------------------------------------------------
#
#
#
# SOURCE THE GLOBAL VARIABLES
#
#
#
#-------------------------------------------------------------------------------
source("global.R")

#-------------------------------------------------------------------------------
#
#
#
# USER INTERFACE
#
#
#
#-------------------------------------------------------------------------------
ui <- fluidPage(
  # Initiate shinyJS
  shinyjs::useShinyjs(),
  
  # Create a custom command to unbind the radio buttons on re-draw of the table
  tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
          Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
        })")),
  
  fluidRow(class = "panel-main",
           column(1),
           column(10,
                  uiOutput("user_interface"),
                  shinyWidgets::actionBttn(inputId = "next_alt",
                                           label = "Reveal another alternative",
                                           style = "material-flat",
                                           color = "success")
                  ),
           column(1)
           ),
  fluidRow(class = "panel-next-page",
           column(9),
           column(1, 
                  shinyWidgets::actionBttn(inputId = "next_page", label = "NULL",
                                    style = "material-circle", color = "success",
                                    icon = icon("arrow-right"))
                  ),
           column(1)
           )
  
)

#-------------------------------------------------------------------------------
#
#
#
# SERVER SIDE
#
#
#
#-------------------------------------------------------------------------------
server <- function(input, output, session) {
  #-----------------------------------------------------------------------------
  # Define a set of reactive values. Note that we start the question counter
  # at zero to correctly index the questions when the page counter increases
  #-----------------------------------------------------------------------------
  current <- reactiveValues(
    page = 1, 
    alt = 1, 
    question = 0
  )
  
  #-----------------------------------------------------------------------------
  # Define what happens when the session begins
  #-----------------------------------------------------------------------------
  
  #-----------------------------------------------------------------------------
  # Define what happens when the session ends
  #-----------------------------------------------------------------------------
  session$onSessionEnded(
    function () {
      time_end <- Sys.time()
      # save_db(survey_output, "test_db", db_config)
    }
  )
  
  #-----------------------------------------------------------------------------
  # Define treatments and randomly allocate respondents
  #-----------------------------------------------------------------------------
  # treatment <- sample(seq_len(10), 1)
  treatment <- 1
  
  if (treatment == 1) {
    nalts <- 3L
    sequential <- FALSE
  }
  
  #-----------------------------------------------------------------------------
  # Get the choice tasks and prepare to send all choice task data to the
  # data base
  #-----------------------------------------------------------------------------
  profiles <- sample(seq_len(nrow(design)), (tasks * nalts), prob = weights)
  choice_tasks <- design %>%
    slice(profiles)
  
  # Reorder the alternatives of the choice tasks to ensure no ordering effects
  choice_tasks <- choice_tasks %>%
    mutate(ct_order = rep(sample(seq_len(tasks)), each = nalts),
           alt_order = rep(sample(seq_len(nalts)), times = tasks)) %>%
    arrange(ct_order, alt_order) %>%
    select(-ct_order, -alt_order)
  
  names_attributes <- tools::toTitleCase(names(choice_tasks))
  
  # Get the data ready to submit to the database
  data_attributes <- as.vector(t(choice_tasks))
  names(data_attributes) <- str_c(
    rep(str_c("ct", seq_len(tasks), sep = "_"), each = (nalts * nattr)),
    rep(names(choice_tasks), times = (nalts * tasks)),
    rep(rep(seq_len(nalts), each = nattr), times = tasks), sep = "_"
  )
  
  #-----------------------------------------------------------------------------
  # Get the possible responses to each question
  #-----------------------------------------------------------------------------
  responses <- reactive({
    outline %>%
      slice(current$page) %>%
      select(starts_with("option")) %>%
      select_if(not_all_na) %>%
      unlist(., use.names = FALSE)
  })
  
  #-----------------------------------------------------------------------------
  # Get the questions for the likert batteries
  #-----------------------------------------------------------------------------
  battery_questions <- reactive({
    outline %>%
      slice(current$page) %>%
      select(starts_with("battery")) %>%
      select_if(not_all_na) %>%
      unlist(., use.names = FALSE)
  })
  
  #-----------------------------------------------------------------------------
  # When the 'next_page' button is clicked, increase the page and question
  # counters, and reset the alt counter.
  #-----------------------------------------------------------------------------
  observeEvent(input[["next_page"]], {
    current$page <- current$page + 1
    
    if (dplyr::pull(outline, page_type)[current$page] == "question") {
      current$question <- current$question + 1
    }
    
    current$alt <- 1
  })
  
  #-----------------------------------------------------------------------------
  # When the 'next_alternative' button is clicked, increase the 'alt' counter
  #-----------------------------------------------------------------------------
  observeEvent(input[["next_alt"]], {
    # Manually trigger unbind-DT when the next alternative button is clicked
    response_id <- paste0("response_", current$question)
    session$sendCustomMessage('unbind-DT', response_id)
    
    # Increase the alternative counter
    current$alt <- current$alt + 1
  })
  
  #-----------------------------------------------------------------------------
  # When the question counter increases, re-draw the question. We need to create
  # two outputs: i) The question text, and ii) the response option.
  #-----------------------------------------------------------------------------
  observeEvent(current$question, {
    local({
      # Render the question text
      text_id <- paste0("text_", current$question)
      output[[text_id]] <- renderText({
        paste0("Question ", current$question, ": ",
               dplyr::pull(outline, question)[current$page])
      })
      
      # Get the response id
      response_id <- paste0("response_", current$question)
      question_type <- dplyr::pull(outline, question_type)[current$page]
      if (is.na(question_type)) question_type <- "not a question page"
      
      # Render a likert scale question
      if (question_type == "likert") {
        output[[response_id]] <- renderUI({
          shinyWidgets::awesomeRadio(inputId = response_id,
                                     label = "Please select: ",
                                     choices = c(responses()),
                                     selected = "None",
                                     width = "100%",
                                     status = "success")
        })
      } 
      
      # Render the battery of likert scales
      if (question_type == "battery") {
        output[[response_id]] <- DT::renderDataTable({
          rows <- length(battery_questions())
          cols <- length(responses())
          
          battery <- matrix(as.character(seq_len(cols)), nrow = rows,
                            ncol = cols, byrow = TRUE,
                            dimnames = list(battery_questions(),
                                            responses()))
          
          for (i in seq_len(rows)) {
            battery[i, ] <- sprintf('<input type = "radio", name = "%s", value = "%s"/>',
                                    battery_questions()[i], battery[i, ])
          }
          
          # Return the battery of likert questions
          battery
          
        }, escape = FALSE, server = FALSE, selection = "none", 
        options = list(
          dom = "t", paging = FALSE, ordering = FALSE
        ),
        callback = DT::JS("table.rows().every(function(i, tab, row) {
                            var $this = $(this.node());
                            $this.attr('id', this.data()[0]);
                            $this.addClass('shiny-input-radiogroup');
                          });
                          Shiny.unbindAll(table.table().node());
                          Shiny.bindAll(table.table().node());")
        )
      }
    })
  })
  
  #-----------------------------------------------------------------------------
  # When the question counter OR alternative counter changes, we need to
  # re-draw the choice task
  #-----------------------------------------------------------------------------
  observeEvent({
    current$question
    current$alt}, {
      local({
        # Render the question text
        text_id <- paste0("text_", current$question)
        output[[text_id]] <- renderText({
          paste0("Question ", current$question, ": ",
                 dplyr::pull(outline, question)[current$page])
        })

        # Get the response id
        response_id <- paste0("response_", current$question)
        question_type <- dplyr::pull(outline, question_type)[current$page]
        if (is.na(question_type)) question_type <- "not a question page"
        
        # Render the choice tasks
        if (question_type == "choice_task") {
          output[[response_id]] <- DT::renderDataTable({
            task_index <- as.integer(
              str_remove(
                dplyr::pull(outline, "question_id")[current$page], "ct_"
                )
              )

            if (sequential == FALSE) {
              current$alt <- nalts
            }
            
            the_rows <- ((1 + (task_index - 1) * nalts):(task_index * nalts))[seq_len(current$alt)]

            # Subset the choice_tasks to only the current choice task
            
            task_matrix <- choice_tasks %>%
              slice(the_rows)

            radio_choice <- matrix(0, nrow = current$alt, ncol = 1L)
            for (i in seq_len(current$alt)) {
              radio_choice[i, ] <- sprintf('<input type = "radio", name = "%s", value = "%s"/>',
                                           response_id,
                                           responses()[i])
            }

            # Combine with radio buttons and set dimension names
            task_matrix <- cbind(task_matrix, radio_choice)
            colnames(task_matrix) <- c(names_attributes, "I choose")
            rownames(task_matrix) <- paste0("Wine ", seq_len(current$alt))

            # Return the matrix
            t(task_matrix)

          }, escape = FALSE, server = FALSE, selection = "none",
          options = list(
            dom = "t", paging = FALSE, ordering = FALSE,
            preDrawCallback = DT::JS(
            'function() { 
              Shiny.unbindAll(this.api().table().node()); }'),
            drawCallback = DT::JS(
                'function() { ',
                paste0('var $radio_row = $(\'tr:has(input[name=', response_id, '])\'); '),
                'var $row = this.api().table().rows($radio_row); ',
                'var $this = $($row.nodes(0)); ',
                paste0('this.attr("id", ', response_id, '); '),
                '$this.addClass(\"shiny-input-radiogroup\"); ',
                'Shiny.bindAll(this.api().table().node()); ',
                '}'
            )
          )
        )
      }
    })
  })
  

#   'function() {
#                            var $radio_row = $(\'tr:has(input[name="choice"])\');
#                            var $row = this.api().table().rows($radio_row);
#                            var $this = $($row.nodes(0));
#                            $this.attr("id", "choice");
#                            $this.addClass("shiny-input-radiogroup");
#                            Shiny.bindAll(this.api().table().node());
#                          }'
  
  #-----------------------------------------------------------------------------
  # When the page counter increases, store the current responses to the 
  # questions and meta-data gathered so far
  #-----------------------------------------------------------------------------
  observeEvent(current$page, {
    
  })
  
  #-----------------------------------------------------------------------------
  # Define the reactive function 'user_interface()'
  #-----------------------------------------------------------------------------
  user_interface <- reactive({
    # Define the page and question types
    page_type <- dplyr::pull(outline, page_type)[current$page]
    question_type <- dplyr::pull(outline, question_type)[current$page]
    if (is.na(question_type)) question_type <- "not a question page"
    
    # Define the output ids inside the 'user_interface()'
    text_id <- paste0("text_", current$question)
    response_id <- paste0("response_", current$question)
    
    # Show/hide reveal next alternative
    if (question_type == "choice_task" && sequential == TRUE) {
      shinyjs::showElement("next_alt")
    } else {
      shinyjs::hideElement("next_alt")
    }
    
    if (page_type == "first_page") {
      return(
        shiny::withTags(
          div(
            h1("Welcome")
          )
        )
      )
    }
    
    if (page_type == "question") {
      # Check the answer
      observe({
        if (question_type == "likert") {
          # Check whether (all) questions are answered
          toggle_condition <- length(input[[response_id]]) > 0
          
          # Check the output
          output[["check"]] <- renderPrint({
            str(input[[response_id]])
          })
        }
        
        if (question_type == "battery") {
          # Check whether (all) questions are answered
          filled <- vapply(battery_questions(), function (x) {
            length(input[[x]]) > 0
          }, logical(1))
          toggle_condition <- all(filled)
          
          # Check the output
          output[["check"]] <- renderPrint(
            str(sapply(battery_questions(), function (i) input[[i]]))
          )
        }
        
        if (question_type == "choice_task") {
          output[["check"]] <- renderPrint({
            str(input[[response_id]])
          })
        }
        
      })
      
      # Render the question
      return(
        shiny::withTags(
          div(
            h3(textOutput(text_id)),
            if (question_type == "battery" || question_type == "choice_task") {
              div(DT::dataTableOutput(response_id))
            },
            if (question_type == "likert") {
              div(uiOutput(response_id))
            },
            verbatimTextOutput("check"),
            p(question_type),
            p(response_id),
            p(is.character(response_id))
          )
        )
      )
    }

  })
  
  #-----------------------------------------------------------------------------
  # Define the UI as an output of the reactive function 'user_interface()'
  #-----------------------------------------------------------------------------
  output[["user_interface"]] <- renderUI({
    user_interface()
  })
}


#-------------------------------------------------------------------------------
#
#
#
# COMBINE INTO AN APPLICATION
#
#
#
#-------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
