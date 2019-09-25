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
ui <- fluidPage(theme = "master.css",
  # Initiate shinyJS
  shinyjs::useShinyjs(),
  
  # Create a custom command to unbind the radio buttons on re-draw of the table
  tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                   Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                   })")),
  
  # Add the loading screen
  div(id = "loading-screen",
    div(class = "loader")
  ),
  
  # Wrap the rest of the visible user interface in the hidden() environment
  shinyjs::hidden(
    div(id = "survey",
        
      # Title page
      fluidRow(class = "top-panel",
               column(8),
               column(4,
                      img(src = "mono-reverse-logo.png", class = "funder-panel-image", style = "border:0;"))
      ),
      
      # Progress bar
      fluidRow(class = "progress-bar-panel",
               shinyWidgets::progressBar(id = "progress_bar", value = 0, range_value = c(0, (pages - 1)), display_pct = FALSE, title = NULL, striped = TRUE, status = "success")
      ),
      
      # Main survey panel
      fluidRow(class = "panel-main",
               column(1),
               column(10,
                      uiOutput("user_interface"),
                      # Button to reveal the next alternative
                      shinyWidgets::actionBttn(inputId = "next_alt",
                                               label = "Reveal another alternative",
                                               style = "material-flat",
                                               color = "success"),
                      
                      # Downlaod buttton for participant information sheet
                      shinyWidgets::downloadBttn(outputId = "download_info",
                                                 label = "Participant information sheet",
                                                 style = "material-flat", color = "success")
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
    question = 0,
    time = 1000,
    task = 0
  )
  
  checked <- reactiveValues()
  battery_randomized <- reactiveVal(FALSE)
  
  #-----------------------------------------------------------------------------
  # Define what happens when the session begins
  #-----------------------------------------------------------------------------
  
  url_vars <- NULL
  observe({
    url_vars <<- parseQueryString(session$clientData$url_search)
  })
  

  # Generate a survey specific ID number
  resp_id <- paste0(sample(c(letters, LETTERS, 0:9), 10), collapse = "")
  output$resp_id <- renderText(resp_id)
  
  # Add exit URL
  exit_url <- paste0("https://inspire-project.info/?id=", resp_id, "&?test=", 8)
  
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
  # treatment <- 6
  treatment <- sample(1:10, 1)
  
  # Enforce that your chosen alt has to be among your most preferred. 
  # Add popup if this is not the case. 
  
  # Standard choice task with 3 alternatives
  if (treatment == 1) {
    nalts <- 3L
    sequential <- FALSE
    current_best <- FALSE
    consideration_set <- FALSE
    consideration_set_all <- FALSE
    search_cost <- FALSE
  }
  
  # Standard choice task with 6 alternatives
  if (treatment == 2) {
    nalts <- 6L
    sequential <- FALSE
    current_best <- FALSE
    consideration_set <- FALSE
    consideration_set_all <- FALSE
    search_cost <- FALSE
  }
  
  # Standard choice tasks with 9 alternatives
  if (treatment == 3) {
    nalts <- 9L
    sequential <- FALSE
    current_best <- FALSE
    consideration_set <- FALSE
    consideration_set_all <- FALSE
    search_cost <- FALSE
  }
  
  # Sequential choice tasks
  if (treatment == 4) {
    nalts <- 9L
    sequential <- TRUE
    current_best <- FALSE
    consideration_set <- FALSE
    consideration_set_all <- FALSE
    search_cost <- FALSE
  }
  
  # Sequential choice tasks with the current best selected
  if (treatment == 5) {
    nalts <- 9L
    sequential <- TRUE
    current_best <- TRUE
    consideration_set <- FALSE
    consideration_set_all <- FALSE
    search_cost <- FALSE
  }
  
  # Sequential choice task with the current consideration set (max 3)
  if (treatment == 6) {
    nalts <- 9L
    sequential <- TRUE
    current_best <- FALSE
    consideration_set <- TRUE
    consideration_set_all <- FALSE
    search_cost <- FALSE
  }
  
  # Sequential choice task with the current consideration set (full)
  if (treatment == 7) {
    nalts <- 9L
    sequential <- TRUE
    current_best <- FALSE
    consideration_set <- FALSE
    consideration_set_all <- TRUE
    search_cost <- FALSE
  }
  
  # Sequential choice task with fixed time cost across alts and tasks
  if (treatment == 8) {
    nalts <- 9L
    sequential <- TRUE
    current_best <- FALSE
    consideration_set <- FALSE
    consideration_set_all <- FALSE
    search_cost <- TRUE
    time_delay <- rep(sample(seq(0, 5000, 250), 1), times = (tasks * nalts))
  }
  
  # Sequential choice tasks with fixed cost across alts, but variable across tasks
  if (treatment == 9) {
    nalts <- 9L
    sequential <- TRUE
    current_best <- FALSE
    consideration_set <- FALSE
    consideration_set_all <- FALSE
    search_cost <- TRUE
    time_delay <- rep(sample(seq(0, 3000, 250), tasks, replace = TRUE), each = nalts)
  }
  
  # Sequential choice tasks with variable time cost across alts and tasks
  if (treatment == 10) {
    nalts <- 9L
    sequential <- TRUE
    current_best <- FALSE
    consideration_set <- FALSE
    consideration_set_all <- FALSE
    search_cost <- TRUE
    time_delay <- sample(seq(0, 3000, 250), tasks * nalts, replace = TRUE)
  }
  
  if (treatment %in% c(8, 9, 10)) {
    names(time_delay) <- paste0("time_delay_task_",
                                rep(seq_len(tasks), each = nalts),
                                "_alt_",
                                rep(seq_len(nalts), times = tasks))
    
    # Initiate the reactive values for the timer and active
    timer <- reactiveVal(time_delay[1])
    active <- reactiveVal(TRUE)
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
  colnames(choice_tasks) <- names_attributes
  
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
    tmp <- outline %>%
      slice(current$page) %>%
      select(starts_with("battery")) %>%
      select_if(not_all_na) %>%
      unlist(., use.names = FALSE)

    if (battery_randomized()) {
      tmp <- sample(tmp, length(tmp))
    }
    
    return(tmp)
  })
  
  #-----------------------------------------------------------------------------
  # When the 'next_page' button is clicked, increase the page and question
  # counters, and reset the alt counter.
  #-----------------------------------------------------------------------------
  observeEvent(input[["next_page"]], {
    #   Update the progressbar
    shinyWidgets::updateProgressBar(session = session, id = "progress_bar",
                                    value = current$page,
                                    range_value = c(0, (pages - 1)), title = NULL)
    
    current$alt <- 1
    
    # Reset the checked values
    if (current_best || consideration_set || consideration_set_all) {
      lapply(seq_len(nalts), function (x) {
        checked[[paste0("alt_", x)]] <- ""
      })
    }
    
    battery_randomized(FALSE)
    current$page <- current$page + 1
    
    # Define the page and question types
    page_type <- dplyr::pull(outline, page_type)[current$page]
    question_type <- dplyr::pull(outline, question_type)[current$page]
    if (is.na(question_type)) question_type <- "not a question page"
    
    # Update the question and task counters
    if (page_type == "question") {
      current$question <- current$question + 1
      
      if (question_type == "choice_task") {
        current$task <- current$task + 1
      }
      
      if (question_type == "battery_randomized") {
        battery_randomized(TRUE)
      } 
    }
  })
  
  #-----------------------------------------------------------------------------
  # When the 'next_alternative' button is clicked, increase the 'alt' counter
  #-----------------------------------------------------------------------------
  observeEvent(input[["next_alt"]], {
    # Get the response_id
    response_id <- paste0("response_", current$question)
    
    # Update the reactive value for the time_index
    if (search_cost) {
      current$time <- as.integer(time_delay[(current$task - 1) * nalts + current$alt])
      timer(current$time)
      active(TRUE)
    }
    
    # Get the checked values at each click of the next page button
    if (current_best || consideration_set || consideration_set_all) {
      checkbox_names <- paste("considered", current$task, seq_len(current$alt), sep = "_")
      checked_values <- vapply(checkbox_names, function (x) {
        isTRUE(input[[x]])
      }, logical(1))
      for (i in seq_along(checked_values)) {
        checked[[paste0("alt_", i)]] <- ifelse(isTRUE(checked_values[i]), "checked", "")
      }
    }
    
    # Manually trigger unbind-DT when the next alternative button is clicked
    session$sendCustomMessage('unbind-DT', response_id)
    
    # Increase the alternative counter
    if (search_cost) {
      shinyjs::disable("next_alt")
      
      shinyjs::delay(current$time, {
        current$alt <- current$alt + 1
        shinyjs::enable("next_alt")})
      
    } else {
      current$alt <- current$alt + 1
    }
    
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
      
      # Render a dropdown menu question
      if (question_type == "dropdown") {
        output[[response_id]] <- renderUI({
          shiny::selectInput(inputId = response_id,
                             label = "Please select: ",
                             choices = c("", responses()),
                             selected = character(0),
                             width = "100%")
        })
      }
      
      # Render a text input field
      if (question_type == "text") {
        output[[response_id]] <- renderUI({
          shiny::textInput(inputId = response_id,
                           label = "Please input your answer: ",
                           value = "",
                           width = "380px")
        })
      }
      
      # Render the battery of likert scales
      if (question_type == "battery" || question_type == "battery_randomized") {
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
          dom = "t", paging = FALSE, ordering = FALSE,
          columnDefs = list(list(className = "dt-center", targets = seq_along(responses())))
        ),
        callback = DT::JS("table.rows().every(function(i, tab, row) {
                          var $this = $(this.node());
                          $this.attr('id', this.data()[0]);
                          $this.addClass('shiny-input-radiogroup');
      });
                          Shiny.unbindAll(table.table().node());
                          Shiny.bindAll(table.table().node());")
        )
    } # End question type battery question
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
          # Render the choice task
          output[[response_id]] <- DT::renderDataTable({
            if (sequential == FALSE) {
              current$alt <- nalts
            }
            
            the_rows <- ((1 + (current$task - 1) * nalts):(current$task * nalts))[seq_len(current$alt)]
            
            # Subset the choice_tasks to only the current choice task
            task_matrix <- choice_tasks %>%
              slice(the_rows)
            
            # Add checkboxes if the respondent is in the consideration-set or current best
            if (current_best || consideration_set || consideration_set_all) {
              checkboxes <- matrix(0, nrow = current$alt, ncol = 1L)
              for (i in seq_len(current$alt)) {
                checkboxes[i, ] <- sprintf('<input type = "checkbox" value = "%s" id = "%s" %s/>',
                                           i,
                                           paste("considered", current$task, i, sep = "_"),
                                           checked[[paste0("alt_", i)]])
              }
              names_tmp <- colnames(task_matrix)
              task_matrix <- cbind(task_matrix, checkboxes)
              colnames(task_matrix) <- c(names_tmp, "I actively consider")
            }
            
            # Add the choice response
            radio_choice <- matrix(0, nrow = current$alt, ncol = 1L)
            for (i in seq_len(current$alt)) {
              radio_choice[i, ] <- sprintf('<input type = "radio" name = "%s" value = "%s"/>',
                                           response_id,
                                           responses()[i])
            }
            
            # Combine with radio buttons and set dimension names
            names_tmp <- colnames(task_matrix)
            task_matrix <- cbind(task_matrix, radio_choice)
            colnames(task_matrix) <- c(names_tmp, "I choose")
            rownames(task_matrix) <- paste0("Wine ", seq_len(current$alt))
            
            # Return the matrix
            t(task_matrix)
            
          }, escape = FALSE, server = FALSE, selection = "none",
          options = list(
            dom = "t", paging = FALSE, ordering = FALSE,
            columnDefs = list(list(className = "dt-center", targets = seq_len(current$alt))),
            preDrawCallback = DT::JS(
              'function() { 
              Shiny.unbindAll(this.api().table().node()); }'),
            drawCallback = DT::JS(
              paste0("function() {",
                     paste0("var $radio_row = $(\"tr:has(input[name =", paste0("\'", response_id, "\'") ," ])\");"), 
                     "var $row = this.api().table().rows($radio_row);
                     var $this = $($row.nodes(0));",
                     paste0("$this.attr('id', ", paste0("\'", response_id, "\'"),");"),
                     paste0("$this.prop('checked', false);"),
                     "$this.addClass('shiny-input-radiogroup');
                     Shiny.bindAll(this.api().table().node());}"
              ))
            )
          ) # End renderDT
          
          # Define the renderText() for the count down timer.
          output$time_left <- renderText({
            left_on_timer <- as.character(timer() / 1000)
            if (nchar(left_on_timer) == 1) left_on_timer <- paste0(left_on_timer, ".")
            paste0("Time left until you can reveal another alternative: ",
                   stringr::str_pad(left_on_timer, 4, "right", "0"), "s")
          })
        }
      })
    })
  
  #-----------------------------------------------------------------------------
  # When the page counter increases, store the current responses to the 
  # questions and meta-data gathered so far
  #-----------------------------------------------------------------------------
  observeEvent(current$page, {
    # Grab the question inputs AND the conditional inputs
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
    
    # JS for buttons
    shinyjs::hideElement("next_alt")
    shinyjs::hideElement("download_info_bttn")
    
    if (page_type == "first_page") {
      return(
        shiny::withTags(
          div(
            h1("Welcome"),
            p(paste0("You are in treatment: ", treatment))
          )
        )
      )
    } # End first page
    
    if (page_type == "info_page") {
      # Show the download button
      shinyjs::showElement("download_info_bttn")
      
      # Define the download handler for the button
      output$download_info <- downloadHandler(
        filename = "participant-information-sheet.pdf",
        content = function(file) {
          file.copy(file.path("www", "participant-information-sheet-survey.pdf"), file)
        }
      )
      
      return(
        shiny::withTags({
          div(h2("Participant information sheet"),
              
              h3("Your unique ID number: "),
              p(tags$b(resp_id)),
              
              h3("Research Project"),
              p("The Influence of Information Search on Preference Formation and Choice (INSPiRE)"),
              
              h3("Background and aims of project"),
              p("We would like to invite you to take part in this survey. This study aims to improve our understanding of how people make choices. When people make choices, they also make trade-offs. To get more of something, you might have to give up something else. For example, how much money you are willing to give up to help conservation efforts. A better understanding of these choices and trade-offs may help researchers provide policy makers with more accurate information."),
              
              h3("Why have I been invited to take part?"),
              p("You have been invited to take part because you are a UK resident aged 18 or over and are registered with [INSERT PANEL COMPANY HERE]."),
              
              h3("Do I have to take part?"),
              p("Participation is voluntary. If you do decide to take part, you can withdraw your participation at any time without needing to explain and without penalty by closing your browser window.  If you withdraw we will not collect any more data from you. However, any data collected up until the point that you withdraw will be kept and used in the data analysis. You will be given this information sheet to keep and be asked to complete an electronic consent form."),
              
              h3("What will happen if I take part?"),
              p("You will have to complete one online survey. The survey will take approximately 15 minutes to complete. We ask that you complete the survey in one sitting as it is not possible to rejoin later."),
              
              h3("Are there any potential risks in taking part?"),
              p("There are no forseeable risks in taking part?"),
              
              h3("Are there any benefits in taking part?"),
              p("The compensation for taking part in the survey is standard and stipulated by the survey company."),
              
              h3("Legal basis for processing personal data"),
              p("As part of the project we will be recording personal data relating to you.  This will be processed in accordance with the General Data Protection Regulation (GDPR).  Under GDPR the legal basis for processing your personal data will be public interest/the official authority of the University. All responses will be treated confidenially and all data collected anonymized."),
              
              h3("What happens to the data I provide?"),
              p("The research data will be anonymized by removing any personal identifiers such as names, e-mail addresses or IP-addresses, so nothing about you will be identifiable. Your  data will be kept for two years on Research Drive, a secure data centre on the Stirling campus, and then will be lodged in DataSTORRE. The data will be kept for a minimum of 10 years after last publication or access of the data in accordance with the University of Stirling’s research data policy."),
              
              h3("Future uses of the data"),
              p("Due to the nature of this research, it is possible that other researchers may find the data useful to answering other research questions. We will ask for your explicit consent for your data to be shared in this way and, if you agree, we will ensure that the data collected is untraceable back to you before letting others use it."),
              
              h3("Will the research be published?"),
              p("The research will be published in academic journals. You will not be identifiable in any publication. The University of Stirling is committed to making the outputs of research publicly accessible and supports this commitment through their online open access repository STORRE. Unless funder/publisher requirements prevent us, this research will be publicly disseminated through this open access repository."),
              
              h3("Who is organizing and funding the research?"),
              p("This project is funded by the European Union’s Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant agreement No 793163."),
              
              h3("Who has reviewed this research project?"),
              p("The ethical approaches of this project have been approved via The University of Stirling  General University Ethics Panel."),
              
              h3("Your rights"),
              p("You have the right to request to see a copy of the information we hold about you. You have the right to withdraw from the survey at any time without giving reasons and without consequences to you, and we will not collect any more data from you. However, any data you have provided up to the point of withdrawal may be used for analysis."),
              
              h3("Who do I contact if I have concerns about htis study or I wish to complain?"),
              p("If you would like to discuss the research with someone, please feel free to contact Dr Erlend Dancke Sandorf via e-mail: e.d.sandorf@stir.ac.uk. If you would like to discuss the research with someone not directly involved with the project, please contact Dr. Mirko Moro via e-mail: mirko.moro@stir.ac.uk. You have the right to lodge a complaint against the University regarding data protection issues with the Information Commissioner’s Office (https://ico.org.uk/concerns/). The University’s Data Protection Officer is Joanna Morrow, Deputy Secretary.  If you have any questions relating to data protection these can be addressed to data.protection@stir.ac.uk in the first instance."),
              
              p("You can download a copy of this information by clicking the button below. If you wish to keep up to date with the research, please keep an eye on the project website: https://inspire-project.info")
          )
        })
      )
    } # End participant information page
    
    if (page_type == "consent_page") {
      # Define the mandatory consent fields
      mandatory <- c("consent_item_one", "consent_item_two", "consent_item_three",
                     "consent_item_four", "consent_item_five", "consent_item_six")
      
      # Check that all the boxes have been ticked
      observe({
        ticked <- vapply(mandatory, function(x){
          isTRUE(input[[x]])
        },
        logical(1))
        
        toggle_condition <- all(ticked)
        
        shinyjs::toggleState(id = "next_page", condition = toggle_condition)
      })
      
      return(
        shiny::withTags({
          div(
            h3("Consent form"),
            p("Please confirm that you have read and understood each of the items listed below. If you do wish to give consent you will be directed away from the survey."),
            shinyWidgets::materialSwitch(inputId = "consent_item_one", label = label_mandatory("I confirm that I have read and understood the information sheet explaining the research project."), value = FALSE, status = "success", right = TRUE, width = "100%"),
            shinyWidgets::materialSwitch(inputId = "consent_item_two", label = label_mandatory("I understand that my participation is voluntary and that I am free to withdraw at any time during the survey, but that any data collected up until this point may be used in analysis."), value = FALSE, status = "success", right = TRUE, width = "100%"),
            shinyWidgets::materialSwitch(inputId = "consent_item_three", label = label_mandatory("I have been given a unique identifying number and know whom to contact should I wish to obtain a copy of the data the researchers hold about me."), value = FALSE, status = "success", right = TRUE, width = "100%"),
            shinyWidgets::materialSwitch(inputId = "consent_item_four", label = label_mandatory("I understand that my responses will be kept anonymous and I give permission for members of the research team to have access to my anonymised responses."), value = FALSE, status = "success", right = TRUE, width = "100%"),
            shinyWidgets::materialSwitch(inputId = "consent_item_five", label = label_mandatory("I agree for research data collected in the study to be made available to researchers, including those working outside the EU to be used in other research studies. I understand that any data that leave the research group will be fully anonymised so that I cannot be identified."), value = FALSE, status = "success", right = TRUE, width = "100%"),
            shinyWidgets::materialSwitch(inputId = "consent_item_six", label = label_mandatory("I agree to take part in this study."), value = FALSE, status = "success", right = TRUE, width = "100%")
          )
        })
      )
    } # End consent page
    
    if (page_type == "section_1") {
      return(
        shiny::withTags(
          div(
            h1("Section 1: Your consumption and purchasing behavior"),
            p("In the next section we will ask some questions relating to your consumption and purchasing of wine.")
          )
        )
      )
    } # End section 1 info page
    
    if (page_type == "section_2") {
      return(
        shiny::withTags(
          div(
            h1("Section 2: Your stated purchases of wine"),
            p("[INSERT VIDEOS HERE DPENDING ON THE TREATMENT]")
          )
        )
      )
    } # End section 2 info page
    
    if (page_type == "question") {
      if (question_type == "choice_task") {
        # Show the next_alt button if we are in a sequential treatment
        if (sequential) {
          shinyjs::showElement("next_alt")
        }
      }
      
      # Toggle conditions and output observer
      observe({
        if (question_type == "likert") {
          # Check whether (all) questions are answered
          toggle_condition <- length(input[[response_id]]) > 0
          
          output[["check"]] <- renderPrint({
            str(input[[response_id]])
          })
        } # End Likert scale question
        
        if (question_type == "dropdown" || question_type == "text") {
          # Check whether (all) questions are answered
          toggle_condition <- input[[response_id]] != ""
          
          output[["check"]] <- renderPrint({
            str(input[[response_id]])
          })
        } # End dropdown menu question
        
        if (question_type == "battery" || question_type == "battery_randomized") {
          # Check whether (all) questions are answered
          filled <- vapply(battery_questions(), function (x) {
            length(input[[x]]) > 0
          }, logical(1))
          toggle_condition <- all(filled)
          
          # Check the output
          output[["check"]] <- renderPrint(
            str(sapply(battery_questions(), function (i) input[[i]]))
          )
        } # End battery question
        
        if (question_type == "choice_task") { 
          #   Make the button next alternative inactive when all alternatives are revealed
          shinyjs::toggleState(id = "next_alt", condition = current$alt < nalts)
          
          # Set the current_best and consideration_set observers
          if (current_best || consideration_set || consideration_set_all) {
            checkbox_names <- paste("considered", current$task, seq_len(current$alt), sep = "_")
            checked <- vapply(checkbox_names, function (x) {
              isTRUE(input[[x]])
            }, logical(1))
            sum_checked <- sum(checked)
            
            # Current best condition
            if (current_best) toggle_input_condition <- 1
            if (consideration_set) toggle_input_condition <- 3
            if (consideration_set_all) toggle_input_condition <- 9
            
            if (sum_checked >= toggle_input_condition) {
              for (i in seq_len(current$alt)) {
                if (isFALSE(checked[i])){
                  shinyjs::disable(checkbox_names[i])  
                }
              }
            } else {
              for (i in seq_len(current$alt)) {
                shinyjs::enable(checkbox_names[i])  
              }
            }
            
            consideration_check <- as.integer(input[[response_id]]) %in% which(checked %in% TRUE)
            
            output[["considered"]] <- renderPrint({
              str(sapply(checkbox_names, function (i) input[[i]]))
            })

          } else {
            consideration_check <- TRUE
          } # End of if (current_best || consideration_set)
          
          shinyjs::reset(response_id)
          
          # Check whether (all) questions are answered
          toggle_condition <- length(input[[response_id]]) > 0 && consideration_check
          
          output[["check"]] <- renderPrint({
            str(input[[response_id]])
          })
        } # End choice_task
        
        shinyjs::toggleState("next_page", condition = toggle_condition)
      }) # End JS and output observer
      
      
      
      # Set up a second observer for the count down timer
      observe({
        if (search_cost && question_type == "choice_task") {
          invalidateLater(time_inc, session)
          isolate({
            if (active()) {
              timer(timer() - time_inc)
              if (timer() < time_inc) {
                active(FALSE)
              }
            }
          })

        }
      }) #  End time observer
      
      # Render the question
      return(
        shiny::withTags(
          div(
            h3(textOutput(text_id)),
            if (question_type == "battery" || question_type == "battery_randomized" || question_type == "choice_task") {
              div(DT::dataTableOutput(response_id))
            },
            if (question_type == "likert" || question_type == "dropdown" || question_type == "text") {
              div(uiOutput(response_id))
            },
            if (search_cost && question_type == "choice_task") {
              textOutput("time_left")
            },
            verbatimTextOutput("check"),
            verbatimTextOutput("considered")
          )
        )
      )
    } # End question page
    
    if (page_type == "final_page") {
      # Hide the 'next_page' button
      shinyjs::hideElement("next_page")
      
      return(
        shiny::withTags(
          div(
            p("Thank you very much for participating."),
            p("To claim your reward for participating, please click the button below to be redirected."),
            a(h4("Exit the survey", class = "btn btn-default action-button" , 
                 style = "fontweight:600"),
              href = exit_url)
          )
        )
      )
    } # End final page
    
  })
  
  #-----------------------------------------------------------------------------
  # Define the UI as an output of the reactive function 'user_interface()'
  #-----------------------------------------------------------------------------
  output[["user_interface"]] <- renderUI({
    user_interface()
  })
  
  #-----------------------------------------------------------------------------
  # Hide the loading message when the survey function is done
  #-----------------------------------------------------------------------------
  shinyjs::hide(id = "loading-screen", anim = TRUE, animType = "fade")
  shinyjs::show("survey")
  
} # End server


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
