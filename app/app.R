# credit: dynamic table selection adapted from https://mgei.github.io/post/dynamic-shinydashboard/
source(
  "helpers.R"
)
source(
  "open_python.R"
)
<<<<<<< HEAD
<<<<<<< HEAD
source_python(
  "sbml_to_sbtab.py"
  )
=======
>>>>>>> 7834f37 (continuous saving of xml working)
=======
>>>>>>> 1217cc9 (continuous saving of xml working)

options(stringsAsFactors = FALSE)

# dynamic sub menu
update_submenu <- function(local) {
  lapply(split(local$subitems, seq(nrow(local$subitems))), function(x) {
    menuSubItem(x$name, tabName = paste0("tab_", x$id))
  })
}

# ui
ui <- dashboardPage(
  dashboardHeader(title = tags$p(tags$a(href='https://ontox-project.eu/',
                                 tags$img(src='ontox_logo.png',height='40',width='60')),
                                 " - Physiological Maps Data Entry Portal"), 
                  titleWidth = 500,
                  tags$li(a(onclick = "onclick =window.open('https://github.com/xxx/xxx')",
                            href = NULL,
                            icon("github"),
                            title = "GitHub",
                            style = "cursor: pointer;"
                            ),
                          class = "dropdown"
                          )
                  ),
  dashboardSidebar(
    sidebarMenuOutput("mysidebar")
  ),
  dashboardBody(
    tab_list_ui(),
    useShinyjs()
  )
)

# server
server <- function(input, output, session) {
  # dynamic sidebar menu 
  output$mysidebar <- renderMenu({
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Setup", tabName = "setup",
        icon = icon("gear"), selected = TRUE
      ),
      menuItem(
        "Select tables", tabName = "select_tables", 
        icon = icon("table")
      ),
      menuItem(
        "Tables", id = "subs", tabName = "subs", 
        icon = icon("database"), startExpanded = TRUE,
        update_submenu(local)
      ),
      menuItem(
        "Help", tabName = "help",
        icon = icon("question")
      )
    )
  })
  
  # debugging
  observe({
    print(paste0("current tabs = ", 
                 paste0(unlist(local$current_tabs), collapse = " ")))
    print(paste0("empty tabs = ", 
                 paste0(unlist(local$empty_tabs), collapse = " ")))
  })
  
  # This is to get the desired menuItem selected initially. 
  # selected=T seems not to work with a dynamic sidebarMenu.
  observeEvent(session, {
    updateTabItems(session, "tabs", selected = "setup")
  })
  
  # store dynamic tab list and dynamic contents to use in app
  local <- reactiveValues(
    empty_tabs = as.list(table_names),
    current_tabs = list(),
    subitems = data.frame(id = integer(), name = character()),
    choices = table_names,
    # create empty list for table headers (for exporting)
    headers = list(),
    # make reactive dataframes out of table choices
    data = sbtab_tables_list,
    # create empty list for data upload
    sbtabfile = list()
  )
  
  
  ## render setup screen
  output$mysetup <- renderUI({
    bsCollapse(id = "homescreen", open = "Homescreen",
      # create home- choicescreen       
      bsCollapsePanel("Homescreen",
        htmlOutput("welcome"),
        actionButton("new_sbtab", "Create new SBtab"),
        actionButton("upload_sbtab", "Upload an SBtab object"),
        actionButton("upload_sbml", "Upload an SBML object")
      ),
      # upload screen for SBtab file
      bsCollapsePanel("Upload SBtab",
        fileInput("sbtabfile_in", "Upload SBtab file",
                  multiple = FALSE,
                  accept = c("text/tsv",
                             "text/tab-separated-values,text/plain",
                             ".tsv")),
        actionButton("set_sbtab", "Click here to continue (required)")
      ),
      bsCollapsePanel("Upload SBML",
        fileInput("sbmlfile_in", "Upload SBML file",
                  multiple = FALSE,
                  accept = c("text/xml",
                             "text/plain",
                             ".xml")),
        actionButton("set_sbml", "Click here to continue (required)")
      ),
      bsCollapsePanel("First setup",
        textInput("set_documentname", "Please name your document", placeholder = "Documentname"),
        selectInput("sbtab_version", "Please enter which SBtab Version you need (1.0 default)", 
                    c("0.8", "0.9", "1.0"), selected = "1.0"),
        actionButton("set", "Save input")
      ),
      bsCollapsePanel("Save and download",
        htmlOutput("text_download"),
        downloadButton("download_tsv", "Download tsv"),
        downloadButton("download_xml", "Download xml"),
        actionButton("open_minerva", "Open MINERVA", icon(progressBar(id = "open_m", value = 0, total = 5)))
      )
    )
  })
  
  # Render homescreen text
  output$welcome <- renderText({paste("<b>What would you like to do?</b>")})
  
  # Open "First setup" panel when "Create new SBtab" is selected
  observeEvent(input$new_sbtab, {
    updateCollapse(session, "homescreen", open = "First setup")
  })
  
  # Open "Upload SBtab" panel when "Upload an SBtab object" is selected
  observeEvent(input$upload_sbtab, {
    updateCollapse(session, "homescreen", open = "Upload SBtab")
  })
  
  # Open "Upload SBML" panel when "Upload an SBML object" is selected
  observeEvent(input$upload_sbml, {
    updateCollapse(session, "homescreen", open = "Upload SBML")
  })
  
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
  # Open "First setup" panel after SBtab is uploaded
  observeEvent(input$set_sbtab, {
    updateCollapse(session, "homescreen", open = "First setup")
  })
  
  # Open "First setup" panel after SBML is uploaded
  observeEvent(input$set_sbml, {
    updateCollapse(session, "homescreen", open = "First setup")
  })
  
=======
>>>>>>> 07ca341 (SBML uploading functional)
  # Open "configure map" panel when document name is set
  observeEvent(input$set, {
    updateCollapse(session, "homescreen", open = "Save and download")
    updateTabItems(session, "tabs", selected = "select_tables")
  })
  
  output$text_download <- renderText({
    paste("<b>Download your file to .tsv or .xml format</b>")
  })
  
  # render select_tables
  output$mytables <- renderUI({
    tagList(
      selectInput("add_subitem", "Select table to add",
                  choices = local$choices),
      actionButton("add", "Add"),
      br(), br(),
      selectInput("rm_subitem", "Select table to remove",
                  choices = local$subitems$name),
      actionButton("rm", "Remove")
    )
  })
  
  # store dynamic tab list and dynamic contents
  local <- reactiveValues(
    empty_tabs = as.list(table_names),
    current_tabs = list(),
    subitems = data.frame(id = integer(), name = character()),
<<<<<<< HEAD
    choices = table_names
  )
  
=======
    choices = table_names,
    # create empty list for table headers (for exporting)
    headers = list(),
    # make reactive dataframes out of table choices
    data = sbtab_tables_list,
    # create empty list for data upload
    sbtabfile = list()
  )
  
=======
>>>>>>> f7bab8a (MINERVA map visualisation implemented and working)
=======
>>>>>>> 432e189 (MINERVA map visualisation implemented and working)
  # read input sbtab to dashboard
  observeEvent(input$sbtabfile_in, {
    local$sbtabfile <- suppressWarnings(read_sbtab(input$sbtabfile_in$datapath))
    # print names of tables in the file to console
    print(paste("File", paste0("'",input$sbtabfile_in$name, "'"), "contains tabs:"))
    print(names(local$sbtabfile))
    local$data[names(local$sbtabfile)] <- lapply(names(local$sbtabfile), function(name){
      # make sure all columns start with uppercase letter
      colnames(local$sbtabfile[[name]]) <- 
        gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
             colnames(local$sbtabfile[[name]]),
             perl = TRUE)
      local$data[[name]] <- add_row(local$data[[name]], local$sbtabfile[[name]], .after = 0)
    })
  })
  
  # read input sbml to dashboard
  observeEvent(input$sbmlfile_in, {
    sbml_to_sbtab(input$sbmlfile_in$datapath)
    local$sbtabfile <- suppressWarnings(read_sbtab(sbtab_string))
    # print names of tables in the file to console
    print(paste("File", paste0("'",input$sbmlfile_in$name, "'"), "contains tabs:"))
    print(names(local$sbtabfile))
    local$data[names(local$sbtabfile)] <- lapply(names(local$sbtabfile), function(name){
      # make sure all columns start with uppercase letter
      colnames(local$sbtabfile[[name]]) <- 
        gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
             colnames(local$sbtabfile[[name]]),
             perl = TRUE)
      local$data[[name]] <- add_row(local$data[[name]], local$sbtabfile[[name]], .after = 0)
    })
  })  
  
  # open table tabs from uploaded files
  observeEvent(input$set_sbtab|input$set_sbml, {
    req(input$set_sbtab|input$set_sbml)
    # open tabs included in sbtab in the dashboard
    lapply(names(local$sbtabfile), function(table){
      # update empty/current tab lists if the table is not open yet
      if(!(table %in% local$current_tabs)){
        local$empty_tabs <- local$empty_tabs[local$empty_tabs!=table]
        local$current_tabs <- append(local$current_tabs, table)
        
        # tab name
        local$subitems <- rbind(local$subitems,
                                data.frame(id = table, name = table))
        
        # write table header for file
        local$headers <- append(local$headers,
                                paste0('!!SBtab TableID="t_', table, '"', ' SBtabVersion="', input$sbtab_version, '"',' Document="', input$set_documentname, '"',' TableType="', table, '"',' TableName="', table, '"')
        )
        
        # remove name of table from choices
        local$choices <- local$choices[local$choices!=table]
        
        # render dynamic table and description corresponding to tab name
        output[[paste0("sub_", table)]] <- renderUI({
          upload_tableUI(table, local$sbtabfile)
        })
        
        # save hot values to reactive dataframe
        observeEvent(input[[paste0(table, "_hot")]], {
          local$data[[table]] <- hot_to_r(input[[paste0(table, "_hot")]])
        })
        
        # update dynamic content in the created table
        output[[paste0(table, "_hot")]] <- renderRHandsontable({
          rhandsontable(local$data[[table]], rowHeaders = NULL) %>%
            hot_cols(colWidths = 0.1) %>%
            hot_col(col = input[[paste0(table, "_cols")]], colWidths = "100%")
        })
        
        # Head to save and download tab
        observeEvent(input[[paste0("goto_download_", table)]], {
          updateTabItems(session, "tabs", selected = "setup")
          updateCollapse(session, "homescreen", open = "Save and download")
        })
        
        # output description table
        output[[paste0("Description", table)]] <- outputTableDescription(table)
      }
      
      # If table was opened previously, open filled columns
      updateCheckboxGroupInput(session, paste0(table, "_cols"),
                               selected = c("ReferenceDOI",
                                            "ID",
                                            "ReactionID",
                                            names(local$sbtabfile[[table]][which(local$sbtabfile[[table]][1,] != "")]))
      )
    })
    names(local$headers) <- local$current_tabs
    
    # open "First setup" panel after SBtab or SBML is uploaded and the continue button is pressed
    updateCollapse(session, "homescreen", open = "First setup")
  })
  
<<<<<<< HEAD
<<<<<<< HEAD
>>>>>>> 48dc8b5 (Uploading TSV files works and opens the corresponding tabs in the app)
  # dynamic sidebar menu #
  output$mysidebar <- renderMenu({
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Setup", tabName = "setup",
        icon = icon("gear"), selected = TRUE
      ),
      menuItem(
        "Select tables", tabName = "select_tables", 
        icon = icon("table")
      ),
      menuItem(
        "Tables", id = "subs", tabName = "subs", 
        icon = icon("database"), startExpanded = TRUE,
        update_submenu(local)
      )
    )
  })
  
  # debugging
  observe({
    print(paste0("current tabs = ", 
                 paste0(unlist(local$current_tabs), collapse = " ")))
    print(paste0("empty tabs = ", 
                 paste0(unlist(local$empty_tabs), collapse = " ")))
  })
  
  # render dynamic table and description corresponding to tab name
  for(n in 1:12){
    output[[ paste0("sub_", n) ]] <- renderUI ({
      list(
        bsCollapsePanel("Select columns to include",
                        checkboxGroupInput(paste0(table_names[n], "_cols"),
                                           "Choose from:",
                                           choices = names(sbtab_tables_list[[n]]),
                                           selected = c("ReferenceDOI", "ID"),
                                           inline = TRUE)
        ),
        tabItem(
          tabName = table_names[n],
          fluidRow(
            column( 10,
                    rHandsontableOutput(paste0(table_names[n], "_hot"), 
                                        height = 400, 
                                        width = "100%"),
                    offset = 0
            ),
          )
        ),
        actionButton("goto_download", "Click here to go to the download screen" ),
        br(), br(),
        bsCollapsePanel("Description of table elements",
                        DT::dataTableOutput(paste0("Description", table_names[n]), 
                                            width = "100%")
        )
      )
    })
  }
  
  # make reactive dataframes out of table choices
  for(i in 1:12){
    df <- sbtab_tables_list[i]
    values <- reactiveValues(data = df)
  
    # save hot values to reactive dataframe
    observeEvent(input[[paste0(table_names[i], "_hot")]], {
     values$data <- hot_to_r(input[[paste0(table_names[i], "_hot")]])
    })
  }
  
=======
  # Open "configure map" panel when document name is set
  observeEvent(input$set, {
    updateCollapse(session, "homescreen", open = "Save and download")
    updateTabItems(session, "tabs", selected = "select_tables")
  })
  
  # render select_tables
  output$mytables <- renderUI({
    tagList(
      selectInput("add_subitem", "Select table to add",
                  choices = local$choices),
      actionButton("add", "Add"),
      br(), br(),
      selectInput("rm_subitem", "Select table to remove",
                  choices = local$subitems$name),
      actionButton("rm", "Remove")
    )
  })

>>>>>>> f7bab8a (MINERVA map visualisation implemented and working)
=======
  # Open "configure map" panel when document name is set
  observeEvent(input$set, {
    updateCollapse(session, "homescreen", open = "Save and download")
    updateTabItems(session, "tabs", selected = "select_tables")
  })
  
  # render select_tables
  output$mytables <- renderUI({
    tagList(
      selectInput("add_subitem", "Select table to add",
                  choices = local$choices),
      actionButton("add", "Add"),
      br(), br(),
      selectInput("rm_subitem", "Select table to remove",
                  choices = local$subitems$name),
      actionButton("rm", "Remove")
    )
  })

>>>>>>> 432e189 (MINERVA map visualisation implemented and working)
  # add a tab
  observeEvent(input$add, {
    req(input$add_subitem)
    req(length(local$empty_tabs) > 0)
    # id of next tab to fill
    id <- table_names[which(input$add_subitem == table_names)]
    # update empty/current tab lists
    local$empty_tabs <- local$empty_tabs[-which(local$empty_tabs == id)]
    local$current_tabs <- append(local$current_tabs, id)
    # tab name
    subitem <- input$add_subitem
    local$subitems <- rbind(local$subitems,
                            data.frame(id = id, name = subitem))

    # write table header for file
    local$headers <- append(local$headers,
                            paste0('!!SBtab TableID="t_', subitem, '"', ' SBtabVersion="', input$sbtab_version, '"',' Document="', input$set_documentname, '"',' TableType="', subitem, '"',' TableName="', subitem, '"')
                            )
    names(local$headers) <- local$current_tabs

    # remove name of table from choices
    local$choices <- local$choices[local$choices!=subitem]
    updateTabItems(session, "tabs", selected = "select_tables")
<<<<<<< HEAD
    
<<<<<<< HEAD
=======

>>>>>>> 48dc8b5 (Uploading TSV files works and opens the corresponding tabs in the app)
    # render dynamic table and description corresponding to tab name
    output[[ paste0("sub_", subitem)]] <- renderUI ({
      add_tableUI(subitem)
    })
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
    
    # make reactive dataframes out of table choices
    df <- sbtab_tables_list[[subitem]]
    values <- reactiveValues(data = df)
    
=======
    #   list(
    #     bsCollapsePanel("Select columns to include",
    #                     checkboxGroupInput(paste0(subitem, "_cols"),
    #                                        "Choose from:",
    #                                        choices = names(sbtab_tables_list[[subitem]]),
    #                                        selected = c("ReferenceDOI", "ID", "ReactionID"),
    #                                        inline = TRUE)
    #     ),
    #     tabItem(
    #       tabName = subitem,
    #       fluidRow(
    #         column( 10,
    #                 rHandsontableOutput(paste0(subitem, "_hot"), 
    #                                     height = 400, 
    #                                     width = "100%"),
    #                 offset = 0
    #         ),
    #       )
    #     ),
    #     actionButton("goto_download", "Click here to go to the download screen" ),
    #     br(), br(),
    #     bsCollapsePanel("Description of table elements",
    #                     DT::dataTableOutput(paste0("Description", subitem), 
    #                                         width = "100%")
    #     )
    #   )
    # })

>>>>>>> 48dc8b5 (Uploading TSV files works and opens the corresponding tabs in the app)
=======
    
>>>>>>> 1025e35 (open columns get updated on file upload)
=======
    
>>>>>>> d864e55 (open columns get updated on file upload)
    # save hot values to reactive dataframe
    observeEvent(input[[paste0(subitem, "_hot")]], {
      values$data <- hot_to_r(input[[paste0(subitem, "_hot")]])
    })
<<<<<<< HEAD
=======
    # # render dynamic table and description corresponding to tab name
    # output[[ paste0("sub_", id) ]] <- renderUI ({
    #   list(
    #     bsCollapsePanel("Select columns to include",
    #       checkboxGroupInput(paste0(subitem, "_cols"),
    #                          "Choose from:",
    #                          choices = names(sbtab_tables_list[[subitem]]),
    #                          selected = c("ReferenceDOI", "ID"),
    #                          inline = TRUE)
    #     ),
    #     tabItem(
    #       tabName = subitem,
    #       fluidRow(
    #         column( 10,
    #           rHandsontableOutput(paste0(subitem, "_hot"), 
    #                               height = 400, 
    #                               width = "100%"),
    #           offset = 0
    #         ),
    #       )
    #     ),
    #     actionButton("goto_download", "Click here to go to the download screen" ),
    #     br(), br(),
    #     bsCollapsePanel("Description of table elements",
    #         DT::dataTableOutput(paste0("Description", subitem), 
    #                             width = "100%")
    #     )
    #   )
    # })
    # 
    # # make table a reactive dataframe
    # for(i in sbtab_tables_list){
    #   df <- i
    #   values <- reactiveValues(data = df)
    # }
    # 
    # # save hot values to reactive dataframe
    # observeEvent(input[[paste0(subitem, "_hot")]], {
    #   values$data <- hot_to_r(input[[paste0(subitem, "_hot")]])
    # })
>>>>>>> 6b16831 (created function for reading sbtab files)
    
=======

>>>>>>> 48dc8b5 (Uploading TSV files works and opens the corresponding tabs in the app)
    # update dynamic content in the created table
    output[[paste0(subitem, "_hot")]] <- renderRHandsontable({
      rhandsontable(values$data, rowHeaders = NULL) %>%
        hot_cols(colWidths = 0.1) %>%
        hot_col(col = input[[paste0(subitem, "_cols")]], colWidths = "100%")
    })
    
    # Head to save and download tab
    observeEvent(input[[paste0("goto_download_", subitem)]], {
      updateTabItems(session, "tabs", selected = "setup")
      updateCollapse(session, "homescreen", open = "Save and download")
    })

    # output description table
    output[[paste0("Description", subitem)]] <- outputTableDescription(subitem)
<<<<<<< HEAD
    
    # write in table header
    tableheader <- 
      paste0('!!SBtab TableID="t_', subitem, '"', ' SBtabVersion="', input$sbtab_version, '"',' Document="', input$set_documentname, '"',' TableType="', subitem, '"',' TableName="', subitem, '"')
      
    # write table header and columns to file
    observeEvent(input$save_hot, {
      write_lines(tableheader, file = "physmap.tsv", append = TRUE)
      write_tsv(set_cols(values$data), file = "physmap.tsv", col_names = TRUE, append = TRUE, na = "")
      write_lines(" ", file = "physmap.tsv", append = TRUE)
      source_python("sbtab_to_sbml.py")
    })
    
    # 
    observeEvent({input$sbtabfile_in 
      input$set_sbtab}, {
        sbtabdata <- read_sbtab(input$sbtabfile_in)
    })
=======
>>>>>>> 48dc8b5 (Uploading TSV files works and opens the corresponding tabs in the app)
  })
  
  # write tsv and xml documents actively
  observeEvent(local$data, {
    documentname_set <-
      paste0('!!!SBtab Document="', 
             if(is_empty(input$set_documentname)){
               "Documentname"
               }else{
                 input$set_documentname
                 }, 
             '"') %>% 
      as.character()
    write_lines(documentname_set, file = "physmap.tsv")
    for(table in local$current_tabs){
      write_lines(local$headers[[table]], file = "physmap.tsv", append = TRUE)
      write_tsv(set_cols(local$data[[table]]), file = "physmap.tsv", col_names = TRUE, append = TRUE, na = "")
      write_lines(" ", file = "physmap.tsv", append = TRUE)
    }
    # make it so that sbtab conversion errors don't crash the app 
    # (incomplete sbtab document will cause the .py script to return error)
    tryCatch({
      sbtab_to_sbml("physmap.tsv")
      },
      warning = function(warn){
        print("py.warn")
      },
      error = function(err){
        print("py.err")
      })
    })
  
  # remove a tab
  observeEvent(input$rm, {
    req(input$rm_subitem)
    # add name of table back to choices
    local$choices <- local$choices %>% c(paste(input$rm_subitem))
    # id of tab to fill
    subitem_ind <- which(local$subitems$name == input$rm_subitem)
    subitem <- local$subitems[subitem_ind,]
    # update empty/current tab lists
    local$empty_tabs <- append(local$empty_tabs, subitem$id)
    local$current_tabs <- local$current_tabs[-which(local$current_tabs == subitem$id)]
    # # reset deleted tab and tab content
    # shinyjs::reset(paste0("sub_", subitem$id))
    # local$data[which(names(local$data) == subitem$id)] <- sbtab_tables_list[which(names(sbtab_tables_list) == subitem$id)]
    # print(local$data[which(names(local$data) == subitem$id)])
    # updateCheckboxGroupInput(session, paste0(input$rm_subitem, "_cols"),
    #                          selected = c("ReferenceDOI",
    #                                       "ID",
    #                                       "ReactionID")
    #                          )
    # tab name
    local$subitems <- local$subitems[-subitem_ind,]
    updateTabItems(session, "tabs", selected = "select_tables")
  })
  
  # render text in download tab
  output$text_download <- renderText({
    paste("<b>Download your file to .tsv or .xml format</b>")
  })
  
  # download tab content to .tsv and .xml
  output$download_tsv <- downloadHandler(
    filename = "physmap.tsv",
    content = function(file) {
      file.copy("physmap.tsv", file)
    })
  
  output$download_xml <- downloadHandler(
    filename = "physmap.xml",
    content = function(file) {
      write_file(sbml, file)
    })
  
  # open minerva on click
  observeEvent(input$open_minerva, {
    showNotification("Please wait a few seconds for the page to load")
    source_python("minerva_upload.py")
<<<<<<< HEAD
<<<<<<< HEAD
    #delay(2000, js$browseURL(paste0("http://localhost:8080/minerva/index.xhtml?id=", project_id)))
=======
    delay(2000, browseURL(paste0("http://localhost:8080/minerva/index.xhtml?id=", project_id)))
>>>>>>> 432e189 (MINERVA map visualisation implemented and working)
=======
  })
  
  ## render help screen
  output$myhelp <- renderUI({
    includeMarkdown("README_copy.md")
>>>>>>> 0563296 (Added help tab in the app)
  })
  
}

shinyApp(ui, server,
onStart = function() {
  rcmd_bg("docker-compose", "up", wd = "./docker",  supervise = TRUE)
  
  onStop(function() {
    rcmd_bg("docker", c("stop", "docker_minerva_1"))
    rcmd_bg("docker", c("stop", "docker_db_1"))
  })
})