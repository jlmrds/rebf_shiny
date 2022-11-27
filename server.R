
server <- function(input, output) {
  ### === Core/Data Logic === ###
# When the session ends, the list of scenarios is written to google drive.
  onSessionEnded(
    fun = writeScen2Drive
  )

  ### === Scenario Maker Logic === ###
  output$maker_scenario <- renderUI({ # This is a dynamic UI selectInput as the list of scenarios is dynamic.
    scenarios_short <- list.files(here::here('data/scenarios'))
    selectInput('maker_scenario','Select Scenario', choices = scenarios_short)
  })

  output$maker_current_scenario <- renderText({ # This renders the currently selected scenario
    paste0('Current Scenario: ', input$maker_scenario)
  })

  output$maker_renderscenario <- DT::renderDataTable({ #This renders the list of interventions in the current BUILDER scenario
    cs()
  }, server = FALSE, options = list(
    pageLength=50, scrollX='400px'))

  output$viewer_renderscenario <- DT::renderDataTable({ #This renders the list of interventions in the current VIEWER scenario
    csview()
  }, server = FALSE, options = list(
    pageLength=50, scrollX='400px'))

  maker_rscen_proxy <- DT::dataTableProxy(outputId = 'maker_renderscenario') # This is a proxy for the BUILDER scenario table so that it can be updated dynamically as the intervention list changes

  # This executes when a new scenario is created, it creates an empty data frame to hold the new values, saves the scenario locally and adds it to the list of selectable scenarios
  observeEvent(input$maker_new_scenario, {
    emptyScen <- tibble(lan = NULL,
                        ss = NULL,
                        type = NULL,
                        start_year = NULL,
                        end_year = NULL,
                        value = NULL,
                        unit = NULL)
    saveRDS(emptyScen, file  = paste0(here::here('data/scenarios/'), input$maker_new_scenario_name))
    updateSelectInput(inputId = 'maker_scenario',
                      choices = list.files(here::here('data/scenarios')),
                      selected = input$maker_new_scenario_name)

  })


  # = Intervention List = #


  current_scenario <- reactive({
    t <- readRDS(paste0(here::here('data/scenarios/'), input$maker_scenario))
    return(t)
  })

  current_scenario_view <- reactive({
    t <- readRDS(paste0(here::here('data/scenarios/'), input$viewer_scenario))
    return(t)
  })

  cs <- reactiveVal()
  csview <- reactiveVal()

  observe({
    shiny::req(input$maker_scenario)
    cs(current_scenario()) #Sets cs to the current scenario. Confusing, but cs is a reactiveVal which is easier to pass around than a reactive object.
    print(input$maker_authority)

  })

  observeEvent(input$deleterow_button, { #Triggers when delete row button is pressed on builder tab
     t <- cs()[-input$deleterow,]
     cs(t)
     saveRDS(cs(), file = paste0(here::here('data/scenarios/'), input$maker_scenario))
  })

  observe({ #Always updates csview to the value of current_scenario_view()
    shiny::req(input$viewer_scenario)
    csview(current_scenario_view())
          })
  ir_new <- reactive({ #New intervention row, updates as user inputs are updated but isn't used until the user 'saves' the intervention
    if('All' %in% input$newint_ss){
      ssinter <- list('All')
    }
      else{
        ssinter <- list(input$newint_ss)
      }

    if('All' %in% input$newint_authority){
      laninter <- list('All')
    }
    else{
      laninter <- list(input$newint_authority)
    }
    ir <- tibble(lan = laninter,
                 ss = ssinter,
                 type = input$newint_type,
                 start_year = input$newint_startyear,
                 end_year = input$newint_endyear,
                 value = input$newint_value,
                 unit = input$newint_unit)

    return(ir)
  })

  observeEvent(input$newint_save, { # Activates when the user saves an intervention, adds the new intervention to the dataframe, sets the value of cs to the new list of interventions and saves the scenario to the local disk.

    t <- rbind(cs(), ir_new())
    cs(t)
    saveRDS(cs(), file = paste0(here::here('data/scenarios/'), input$maker_scenario))
  })








  ### === Viewer Logic === ###

  output$viewer_scenario <- renderUI({ #This is a dynamic input showing the list of selectable scenarios
    scenarios_short <- list.files(here::here('data/scenarios'))
    selectInput('viewer_scenario','Select Scenario', choices = scenarios_short)
  })



  output$viewer_current_scenario <- renderText({#This is a dynamic text output showing which scenario is selected
    paste0('Current Scenario: ', input$viewer_scenario)
  })



  # = ggplotly Logic = #

  mod_data <- reactiveVal()

  observe({
    req(input$viewer_scenario) #Only triggers code in this observe() chunk when input$viewer_scenario has a value, this stops crashing due to NA values.
    scen_file <- readRDS(paste0(here::here('data/scenarios/'), input$viewer_scenario)) #Loads the data for the currently selected scenario
    mod_data(apply_scenario(data_0, scenario_df = scen_file)) # This is where the actual data manipulation happens! data_0 is declared in global (it's the beis data)
  })

  view_by <- reactiveVal()
##### ==== This is a horrendous bit of code which changes the way data is grouped for plotting depending on how the view_by parameter is set ====== ######

    viewer_data <- reactive({
    if(input$view_by == 'Total - By Authority'){
      view_by('lan')
      mod_data() %>%
        group_by(lan, year) %>%
        summarise(emissions = mean(emissions))

    }


    else if(input$view_by == 'Total - By Sector'){
      view_by('s')
      mod_data() %>%
        group_by(s, year) %>%
        summarise(emissions = mean(emissions))


    }

    else if (input$view_by == 'Commercial - Sub-sectors'){
      view_by('ss')
      mod_data()[mod_data()$s == 'Commercial',] %>%
        group_by(ss, year) %>%
        summarise(emissions = mean(emissions))

    }
    else if (input$view_by == 'Domestic - Sub-sectors'){
      view_by('ss')
      mod_data()[mod_data()$s == 'Domestic',] %>%
        group_by(ss, year) %>%
        summarise(emissions = mean(emissions))

    }
    else if (input$view_by == 'Industry - Sub-sectors'){
      view_by('ss')
      mod_data()[mod_data()$s == 'Industry',] %>%
        group_by(ss, year) %>%
        summarise(emissions = mean(emissions))

    }
    else if (input$view_by == 'LULUCF - Sub-sectors'){
      view_by('ss')
      mod_data()[mod_data()$s == 'LULUCF',] %>%
        group_by(ss, year) %>%
        summarise(emissions = mean(emissions))

    }
    else if (input$view_by == 'Transport - Sub-sectors'){
      view_by('ss')
      mod_data()[mod_data()$s == 'Transport',] %>%
        group_by(ss, year) %>%
        summarise(emissions = mean(emissions))

    }
    else if (input$view_by == 'Public Sector - Sub-sectors'){
      view_by('ss')
      mod_data()[mod_data()$s == 'Public Sector',] %>%
        group_by(ss, year) %>%
        summarise(emissions = mean(emissions))

    }
    else if (input$view_by == 'Sheffield - Sectors'){
      view_by('s')
      mod_data()[mod_data()$lan == 'Sheffield',] %>%
        group_by(s, year) %>%
        summarise(emissions = mean(emissions))

    }
    else if (input$view_by == 'Rotherham - Sectors'){
      view_by('s')
      mod_data()[mod_data()$lan == 'Rotherham',] %>%
        group_by(s, year) %>%
        summarise(emissions = mean(emissions))

    }
    else if (input$view_by == 'Barnsley - Sectors'){
      view_by('s')
      mod_data()[mod_data()$lan == 'Barnsley',] %>%
        group_by(s, year) %>%
        summarise(emissions = mean(emissions))

    }
    else if (input$view_by == 'Doncaster - Sectors'){
      view_by('s')
      mod_data()[mod_data()$lan == 'Doncaster',] %>%
        group_by(s, year) %>%
        summarise(emissions = mean(emissions))

    }


  })

  ##### ===== The above could probably be replaced with a dplyr::case_when() statement or something.===== ######



  output$viewer_plot <- plotly::renderPlotly({ #Plotly is used for interactivity, the actual plot is a ggplot2 object.
    req(input$viewer_scenario)
    plotly::ggplotly( # Code for the main plot window
      p = ggplot2::ggplot(data = viewer_data(), aes_string(x = 'year', y = 'emissions',fill = view_by())) +
        geom_col(col = 'black', width = 0.9) +
        geom_vline(aes(xintercept = 2019.5), linetype = 2) +
        theme_minimal() +
        ggplot2::scale_fill_brewer(type = 'qual', palette = 8) +
        ylab('C02 Emissions (kt)') + xlab('Year') +
        labs(fill = input$view_by)

    )
  }
  )

  output$viewer_plot_facet <- plotly::renderPlotly({ # Code for the smaller, facet plots.
    req(input$viewer_scenario)
    plotly::ggplotly(
      p = ggplot2::ggplot(data = viewer_data(), aes_string(x = 'year', y = 'emissions', fill = view_by())) +
        geom_col(col = 'black', width = 0.9) +
        geom_vline(aes(xintercept = 2019.5), linetype = 2) +
        theme_minimal() +
        ggplot2::scale_fill_brewer(type = 'qual', palette = 8) +
        ylab('C02 Emissions (kt)') + xlab('Year') +
        labs(fill = input$view_by) +
        facet_wrap(as.formula(paste("~", view_by())))

    )
  }
  )

  # = Export Logic = #
  # Exporting is currently not implemented, the downloadHandler() function should work but this particular attempt did not!
#
#   observeEvent(input$export_scenario,{
#     file_name <- paste0(input$viewer_scenario, '.csv')
#     output$export_scenario <- downloadHandler(
#       filename = file_name,
#       content = function(){readRDS(paste0(here::here('data/scenarios/'), input$viewer_scenario))}
#
#     )
#   })
#
#   output$download_data <- downloadHandler()
#   output$download_plot <- downloadHandler()
}
