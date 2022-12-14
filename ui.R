#Define ui as a navbarPage
ui <- navbarPage('Regional Emissions Budgeting Framework',
                 tabPanel('About',
                          column(2),
                          column(8,
                          h1('About this tool'),
                          h4('This tool allows users to build and share emissions scenarios until 2050 using BEIS emissions data for the South Yorkshire region.
                             Construct or edit a scenario in the `Builder` tab, then visualise and export the result in the `Viewer` tab. '),
                          h2('Builder Tab'),
                          h4('In this tab, you can create or edit emissions scenarios'),
                          h4('View existing scenario'),
                          p('To view scenarios which are available in the current application session,
                          simply use the drop-down menu to select one by name. The list of interventions which the scenario
                             contains will be automatically loaded into the dynamic dataframe at the bottom of the page.'),
                          h4('Create a new scenario'),
                          p('To create a new scenario, enter a unique scenario name into the `enter new Scenario name` text input box
                           and click on `Create a new Scenario`. This will add a new, empty scenario into the list of available scenarios in the
                            drop-down menu at the top of the page.'),
                          h4('Add interventions to an existing scenario or delete interventions'),
                          p('A scenario is a list of interventions - specific emissions effects applied to the various subsectors/local authorities.
                          To add an intervention to the currently selected scenario, supply the relevant values to the intervention builder inputs and click `Save`'),
                          p('Each intervention has 7 fields associated with it:
                                   Affected local authorities,
                                   Affected (sub)sectors,
                                   Start year, End year, Type of change (step or gradient),
                                   unit of change (absolute or percentage)
                                   and the value of change (positive or negative)'),
                          p('A dynamic datatable shows you the list of interventions included in the currently selected scenario,
                            you can use the Delete function below the table to remove an intervention by referencing its row number'),
                          h2('Viewer Tab'),
                          h4('In this tab you can explore a range of views on the data generated by emissions scenarios'),
                          p('Using the control panel on the left hand side of the screen, select a scenario to view and a perspective on the data.'),
                          p('You can view total emissions across all local authorities, across all sectors, or within specific local authorities or sectors using the `View by` drop down menu.'),
                          p('The figures generated are `Plotly` plots, which means they are interactive. You can hover over specific areas to get more information, or use the various Plotly tools to inspect the data.'),

                          )
                 ),# end of 'about'
                 tabPanel('Builder',

                          titlePanel('Create a Scenario'),
                          fluidPage(
                            fluidRow(
                              column(4,
                                            uiOutput('maker_scenario'),
                                            textInput('maker_new_scenario_name', label = 'or, enter new Scenario name'),
                                            actionButton('maker_new_scenario', label = "Create a new Scenario"),

                                            h3('Add a new intervention to the currently selected scenario'),

                                            inputPanel(id = 'newint',
                                                       tags$style(HTML(
                                                         '#newint {background-color: #F5F5F5;}')),





                                                       selectizeInput('newint_authority', label = 'Affected Authority',
                                                                      choices = c('All','Sheffield','Doncaster','Barnsley','Rotherham'),
                                                                      multiple = TRUE),



                                                       selectizeInput('newint_ss', label = 'Choose affected subsectors', choices = subsector_choices,
                                                                      multiple = TRUE),


                                                       selectInput('newint_type', label = 'Type',
                                                                   choices = c('Step','Gradient')),


                                                       numericInput('newint_startyear', label = 'Start Year',
                                                                    value = 2020),



                                                       numericInput('newint_endyear', label = 'End Year',
                                                                    value = NULL),


                                                       numericInput('newint_value', label = 'Change in Emissions',
                                                                    value = 0),


                                                       selectInput('newint_unit', label = 'Unit',
                                                                   choices = c('%','ktCO2')),


                                                       actionButton('newint_save', label = 'Save',
                                                                    style = 'margin-top:25px;'),


                                            ),
                            ),
                            column(8, h3(textOutput('maker_current_scenario')),
                                   DT::dataTableOutput('maker_renderscenario'),
                                   numericInput('deleterow',
                                                label = 'Delete Intervention by entering row index:',
                                                value = NULL),
                                   actionButton('deleterow_button', label = 'Delete')
                                   )
                            )# eol column

                          ) # eol fluidpage

                 ),# end of 'scenario'
                 tabPanel('Viewer',
                          fluidPage(
                            column(4,
                                   uiOutput('viewer_scenario'),
                                   h3('Plot options'),
                                   selectInput('view_by', label = 'View by: ', choices = c('Total - By Authority', 'Total - By Sector','Sheffield - Sectors','Rotherham - Sectors',
                                                                                           'Barnsley - Sectors','Doncaster - Sectors',
                                                                                           'Domestic - Sub-sectors','Commercial - Sub-sectors',
                                                                                           'Industry - Sub-sectors','Transport - Sub-sectors','LULUCF - Sub-sectors',
                                                                                           'Public Sector - Sub-sectors')),
                                  # h3('Export options'),
                                   #downloadButton('export_scenario', label = 'Export Scenario'),
                                   #downloadButton('export_plot', label = 'Export Plot'),
                                   #downloadButton('export_data', label = 'Export Data'),
                                   h3('Scenario Interventions: '),
                                   DT::dataTableOutput('viewer_renderscenario')
                            ),# eol column
                            column(8,
                                   h3(textOutput('viewer_current_scenario')),
                                   plotly::plotlyOutput('viewer_plot'),
                                   plotly::plotlyOutput('viewer_plot_facet')

                            ) # eol column

                          ) #eol fixedpage

                 ),# end of 'plot'


) # End of ui object
