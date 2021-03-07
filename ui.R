ui=fluidPage(theme=shinytheme('lumen'),
            titlePanel('Fred Backtest Index'),
            sidebarLayout(
              sidebarPanel(
                textInput('freddt','Fred Data:',value='UNRATE',placeholder = 'Enter The Fred series ID'),
                dateRangeInput('minmaxdate','Date Range',start=as.Date('1900-01-01'),
                               end=as.Date(Sys.Date()-1),min=as.Date('1900-01-01'),max=as.Date(Sys.Date()-1),
                               ),
                actionButton('submit','Submit'),
                fluidRow(paste('If you leave the starting date as is, you get the first data ever collected on it.')),
                fluidRow(paste('If you change the starting date to a closer present date, you are now including re-statements'))
              ),
              mainPanel(
                #MainPanel Here
                plotlyOutput('plot'),
                dataTableOutput('table')
              )
            ))