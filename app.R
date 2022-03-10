
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(shinymanager)
library(ggExtra)
library(mongolite)
library(shinyhelper)
library(shinycssloaders)
library(shinyWidgets)
library(circlize)


#Run this if you have an issue deploying to shinyapps.io with Bioconducter packages
# library(BiocManager)
# options(repos = BiocManager::repositories())


#javascript that changes color of some of the tabs
js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #d73925;
}"'



#javascript that closes out when inactive
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


  
ui <- fluidPage(head_auth = tags$script(inactivity),
                 dashboardPage(title="ViReMa",skin = "black",
                     dashboardHeader(tags$li(class = "dropdown",
                                             tags$style(".main-header {max-height: 50px}"),
                                             tags$style(".main-header .logo {height: 50px}")
                     ),
                       title = tags$a(href='https://www.utmb.edu/routhlab/home',
                                                            tags$img(height=50, weight=70,src='viremaLogo.png'))),
                     dashboardSidebar(sidebarMenu(
                         menuItem("Introduction", tabName = "intro", icon = icon("th")),
                         menuItem("Analyze Local Files", tabName = "dashboard", icon = icon("dna")),
                         menuItem("Deposit Sequences", tabName = "depseq", icon = icon("plus-circle"))
                     )
                     ),
                     dashboardBody( #the javascript colors the tabs in the ui
                         tags$style(js), #creates tabs for the sidebar
                         tabItems(
                             # First tab content
                             tabItem(tabName = "dashboard",
                                     fluidRow(#creates a tabbox for the plot area
                                     tabBox(
                                         title = "",
                                         # The id lets us use input$tabset1 on the server to find the current tab
                                         id = "tabset1", width = "800px",height = "720px",
                                         tabPanel("Sequence Explorer", 
                                                  
                                                  
                                                  
                                                  fluidRow(
                                                      column(2, style = "border: 2px solid #666666;",
                                                             br(), #upload multiple BED6 files to compare multiple sequenced samples
                                                             actionButton('exampleData', 'Load Example Data'),
                                                             hr(),
                                                             fileInput("file1", "1. Upload .BED Files",
                                                                       multiple = TRUE,
                                                                       accept = c(".bed",".txt"))%>% 
                                                               helper(type = "inline",
                                                                      title = "Upload .BED File(s)",
                                                                      content = c("<h3>This file input is where users will upload their main data file(s). <br><br> Acceptable file types include .BED and .TXT files with a specific format.</h3>",
                                                                                  "<h4><b>Your files need to have at least 6 columns</b> - these correspond to the reference sequence, the donor site (start), the acceptor site (end), a description field, reads, and strandedness. If you selected other options during ViReMa analysis, you may have extra fields like reads at the acceptor site/donor site or nucleotide sequences at those sites.</h4>"),
                                                                      size = "l"),
                                                             tags$b("2. Drag Over Plots to Interact"),
                                                             dropdown(
                                                                selectInput("scatterColor", label = "Palette", choices=c("Jaworski et al. 2021","Viridis"), selected = "Jaworski et al. 2021", multiple = FALSE, selectize = TRUE),
                                                                switchInput(inputId = "hl1", label = "Highlight Table Sequences", value = FALSE),
                                                             label = "Options", icon = icon("gear")),
                                                             br(),br(),
                                                             #download the main 'ssPlot'
                                                             selectInput("fformat", "3. Export Plots", choices=c(".png",".tiff",".jpeg",".pdf"), selected = "png", multiple = FALSE, selectize = TRUE),
                                                             downloadButton('downloadPlot','Download Plots'),br(),br()
                                                             
                                                             
                                                      ),
                                                      column(5, #generate a plot for start/stop of sequences in BED6 file
                                                             plotOutput('ssPlot',
                                                                        brush = brushOpts( id = "plot_brush"))%>% withSpinner(color="#d73925") 
                                                      ),
                                                      column(5, #zoomed in plot for start/stop of sequences in BED6 file based on scrubbing of ssPlot
                                                             plotOutput('ssPlotZoom',click='plot_click1',
                                                                        brush = brushOpts( id = "plot_brush1"))%>% withSpinner(color="#d73925") 
                                                      ),
                                                      box(
                                                        
                                                        solidHeader = T,
                                                        width=12,collapsed=F,
                                                        fluidRow(
                                                          column(8,
                                                                 selectInput("fileSubset", "Subset Uploaded Samples", choices = NULL, multiple = TRUE),
                                                          ),
                                                          column(4,
                                                                 selectInput("RefSeqFilter", "Subset Reference Seqs (Column 1)", choices = NULL, multiple = TRUE),
                                                          )
                                                        )

                                                      )
                                                  )
                                                
                                                  
                                                  
                                                  ), #plots for information concerning all included samples (BED6 files)
                                         tabPanel("Overview", 
                                                  fluidRow(
                                                    box(
                                                        
                                                        solidHeader = T,
                                                        width=12,collapsed=F,
                                                        checkboxInput("tableSubset", "Sequence Information Table Events Only", value = FALSE)
                                                    
                                                        ),
                                                    box(
                                                      solidHeader = T,
                                                      width=12,collapsed=F,
                                                      tabsetPanel(
                                                        tabPanel("Summary", #Plot of nucleotides at positions near recombination site; for brushed area in ssPlotZoom
                                                                 column(2,
                                                                        br(),
                                                                        h4(tags$b("Details")),
                                                                        h5(tags$b("This table provides a quick breakdown of files included in analysis.")),
                                                                        hr()

                                                                 ),
                                                                 column(5,
                                                                        DTOutput('totalReads'),
                                                                        DTOutput('uniqueEvents')
                                                                        
                                                                 ),
                                                                 column(5,
                                                                        
                                                                 )
                                                        ),
                                                        tabPanel("Nucleotide Usage", #Plot of nucleotides at positions near recombination site; for brushed area in ssPlotZoom
                                                                 column(2,
                                                                        br(),
                                                                        h4(tags$b("Details")),
                                                                        h5(tags$b("These plots visualize nucleotide frequencies near all recombination sites for included samples.")),
                                                                        h5("An error will occur if any samples were missing columns for the sequences at donor and acceptor sites in the original .BED file."),
                                                                        hr(),
                                                                        downloadButton('downloadOverviewPlot1','Download Plots'),
                                                                        br(),br()
                                                                        
                                                                        
                                                                 ),
                                                                 column(5,
                                                                        plotOutput('donorPlot')%>% withSpinner(color="#d73925") ,
                                                                 ),
                                                                 column(5,
                                                                        plotOutput('acceptPlot')%>% withSpinner(color="#d73925"),

                                                                 )
                                                        ),
                                                        tabPanel("Histograms", #Plot of nucleotides at positions near recombination site; for brushed area in ssPlotZoom
                                                                 column(2,
                                                                        br(),
                                                                        h4(tags$b("Details")),
                                                                        h5(tags$b("These plots visualize read coverage of deletion and duplication events from included samples.")),
                                                                        h5("The reads are added spanning the deleted or duplicated regions respectively for each event."),
                                                                        hr(),
                                                                        downloadButton('downloadOverviewPlot2','Download Plots'),
                                                                        br(),br()
                                                                        
                                                                        
                                                                 ),
                                                                 column(5,
                                                                        plotOutput('manhattanDelPlot',height = '200px') %>% withSpinner(color="#d73925"),
                                                                 ),
                                                                 column(5,
                                                                        plotOutput('manhattanDupPlot',height = '200px') %>% withSpinner(color="#d73925")                                                       
                                                                        
                                                                 )
                                                        )
                                                      )
                                                    )
                                                  )
                                                        
                                                  ),
                                         tabPanel("Genomic Viewer",
                                                  # genome viewer
                                                  fluidRow(
                                                    column(5, style = "border: 2px solid #666666;",
                                                           br(),
                                                           h4(tags$b("The Sequence Information Table Must Be Filled First.")) %>% 
                                                             helper(type = "inline",
                                                                    title = "Filling the 'Sequence Information' Table",
                                                                    content = c("<h3>Either brush both scatterplots in the 'Sequence Explorer' tab or use the 'Advanced Filtering' text box. <br> </h3>",
                                                                                "<h4>The information in the table will populate a Circos plot. Annotations of genes or other genomic features can be added to see the spatial relationship between events and features.</h4>"),
                                                                    size = "l"),
                                                           hr(),
                                                           fileInput("file2", "1.1 Upload Annotaton .BED/.TXT File",
                                                                     multiple = F,
                                                                     accept = c(".bed",".txt")) %>% 
                                                                     helper(type = "inline",
                                                                     title = "Upload Annotation .BED/.TXT File",
                                                                     content = c("<h3>This file input allows users to add annotations to the genomic plot. <br><br> Acceptable file types include .BED and .TXT files with a specific format.</h3>",
                                                                                 "<h4><b>Necessary columns include Reference Sequence, Start, End, and Name.</b></h4>",
                                                                                 "<h4>'Reference Sequence' must match a Reference Sequence present in your uploaded ViReMa ouput .BED files.</h4>",
                                                                                 "<h4>The annotations can be different colors by including hex codes WITHOUT the # (example - E58601) in the last column.</h4>",
                                                                                 '<img src="annoColumns.png", align="center", height="300px", width="703px"</img>'),
                                                                     size = "l"),
                                                           dropdown(
                                                             uiOutput("xLimCircosSliders"),
                                                             hr(),
                                                             sliderInput("annoTextSize", "Annotation Label Size:",
                                                                         min = 0, max = 2,
                                                                         value = 0.5, step = 0.1),
                                                             sliderInput("axisTextSize", "Axis Label Size:",
                                                                         min = 0, max = 2,
                                                                         value = 0.9, step = 0.1),
                                                             sliderInput("gapDist", "Gap Distance (Base Pairs):",
                                                                         min = 0, max = 1000,
                                                                         value = 100, step = 50),
                                                             sliderInput("cex", "Arrow Transparency:",
                                                                         min = 0, max = 1,
                                                                         value = 0.2, step = 0.05),
                                                             selectInput("circosColor", "Arrow Color:", choices=c("black","red","blue","green","orange","purple"), selected = "black", multiple = FALSE, selectize = TRUE),
                                                             label = "Options", icon = icon("gear")),
                                                           br(),
                                                           tags$b("1.2 Fast Annotations (DOUBLE CLICK to Edit / CTRL + ENTER to Confirm)"),
                                                           DTOutput('annotate'),
                                                           actionButton('annoAddRow', 'Add Row'),
                                                           actionButton('annoPlotCircos', 'Plot'),
                                                           actionButton('reset', 'Reset'),
                                                           downloadButton('downloadCircos','Download Plot'),
                                                           
                                                           br(),br()
                                                    ),
                                                    column(7,
                                                           plotOutput('circosPlot', width="600px",height="600px")%>% withSpinner(color="#d73925") 
                                                    )
                                                  )
                                         )
                                     )
                                     ),
                                     fluidRow(
                                         box(title = "Sequence Information Table",width = "800px",
                                         textInput("advFilter", "Populate Table Using Advanced Filter Expressions (e.g. Col6 == \"+\" & Col3 > Col2 | Col5 > 50)")%>% 
                                           helper(type = "inline",
                                                  title = "Advanced Filtering",
                                                  content = c("<h3>This text box supports advanced filtering expressions. <br><br> It applies these expressions to the full, aggregated dataset and serves the filtered results in the data table below.</h3>",
                                                              "<h4>Columns are referenced by 'Col' + the index of the column (e.g. 'Col1'). These correspond to the columns in your original .BED files.</h4>",
                                                              "<h4>Multiple expressions can be chained together using '&' and/or '|'.</h4>",
                                                              "<h4>For numeric values, all standard operators and operations like '+', '-', '*', '>', '<', or '=' work.</h4>",
                                                              "<h4>For character values, exact matching requires '==' and for strings to be in parentheses (e.g. ' Col4==\"Deletion\" ').</h4>",
                                                              "<h4><b>If you know R,</b> this text box supports any base functions or those included in the 'dplyr' package. Boolean statements are being passed to the 'dplyr' package 'filter' function to subset the data.</h4>"),
                                                  size = "l"),
                                         br(),
                                         DT::dataTableOutput("mytable"),
                                         downloadLink('downloadData', 'Download Sequence Data (.csv)'),br(),br(),br()
                                         )
                                     )
                                     
), #Forms for users to deposit their own BED6 files for curation and inclusion in the ViReMa MongoDB database
tabItem(tabName = "depseq",
        box(title=tags$b("Deposit Virus Sample .BED Files into ViReMa Database"), width = "800px",
            tags$h4(p(tags$b("In Progress"))),
            tags$h5(tags$p("Deposited data is curated and made available for all researchers to use through the ViReMa Database.")),
            textInput("name", "Name", ""),
            textInput("lab", "Name of Lab"),
            textInput("email", "Email Address"),
            textInput("info", "Additional Info") %>% 
              helper(type = "inline",
                     title = "Additional Info",
                     content = c("Include information about the deposited files that could be useful for other researchers.",
                                 "This is where information like <b>whether the sample is from cell culture/animal model/clinical isolate, geographical location, vaccination status, and additional context goes</b>."),
                     size = "l"),
            selectizeInput(
              inputId = 'virusType',
              label = 'Virus in Sample(s)',
              choices = NULL,
              selected = NULL,
              multiple = FALSE, # allow for multiple inputs
              options = list(create = TRUE) # if TRUE, allows newly created inputs
            ),
            fileInput("file3", "Upload .BED File(s)",
                      multiple = TRUE,
                      accept = c(".bed",".txt")),
            actionButton("submit", "Submit", class = "btn-primary")
            ),
), #Introductory information detailing the app and it's usage
tabItem(tabName = "intro",
        box(width = "800px",
            tags$h1("Documentation"),
            tags$h3("Tutorials and Vignettes -",tags$a(href="https://jayeung12.github.io/", "Analyzing ViReMa Output")),
            tags$h3("Open an issue on our ",tags$a(href="https://jayeung12.github.io/", "GitHub Page"),"for troubleshooting or suggestions"),
            tags$h3("Download the 'ViReMa' package from ", tags$a(href="https://bioconda.github.io/recipes/virema/README.html", "Bioconda")),
            br(),
            tags$h1("What is ViReMa?"),
            tags$blockquote(tags$h3(tags$strong("\"We developed an algorithm named ViReMa (Viral-Recombination-Mapper) to provide a versatile platform for rapid, sensitive and nucleotide-resolution detection of recombination junctions in viral genomes using next-generation sequencing data.\"")), cite = "Andrew Routh, John E. Johnson"),
            tags$h4(tags$p("ViReMa has been used to illuminate viral recombination events from both experimental and clinical isolates across several viral families. Check out the papers linked below for examples of ViReMa-supported projects.")),
            tags$h4(tags$p("This Shiny-based web application provides a user-friendly interface for exploration of ViReMa output files (in .BED file format). Analyze your own local files using the 'Analyze Local Files' tab or analyze sequences from our database under 'Analyze ViReMa Database'. Here you can:")),
            tags$h4(tags$strong(tags$ul(
              tags$li("Interact with plots to intuitively explore data,"), 
              tags$li("Apply advanced filters to subset data,"), 
              tags$li("Generate and export standard plots,"),
              tags$li("Deposit sequence files for multi-lab collaboration"),br(),
              tags$img(src = "plotExIntro.png", width = "100%", height = "100%")
            ))),
            tags$h1("Credits"),
            tags$h3(tags$a(href="https://www.utmb.edu/routhlab/home", "Visit the Routh Lab Website")),
            br(),hr(),"Read the original ViReMa paper:",br(),
            "Andrew Routh, John E. Johnson, Discovery of functional genomic motifs in viruses with ViReMa-a Virus Recombination Mapper-for analysis of next-generation sequencing data, Nucleic Acids Research, Volume 42, Issue 2, 1 January 2014, Page e11, https://doi.org/10.1093/nar/gkt916"),
)

),br(),br(),br(),br(),
tags$style(HTML(
  "html {
             position: relative;
             min-height: 100%;
           }
           body {
             margin-bottom: 30px; /* Margin bottom by footer height */
           }
           .footer {
             position: absolute;
             bottom: 0;
             text-align:center;
             width: 100%;
             height: 120px; /* Set the fixed height of the footer here */
           }")),
tags$footer(HTML('<footer>
                      <img src="UTMBHealth_logo___no_tagline.175ab92b.svg", align="left", height="120", width="120"</img>  <img src="nih-logo-300x300.png", align="left", style="vertical-align:middle;margin:25px 20px;", height="70", width="75"</img> <br><br><br> Created by Jason Yeung 2021.
                    </footer>'), class = "footer"),

)
)
)


server <- function(input, output, session){
  
    observe_helpers(withMathJax = TRUE)
    
    #All of the reactiveValues
    dataFilter <- reactiveVal()
    groupings = reactiveVal()
    data <- reactiveValues(upload_state = NULL, plot = NULL,example_counter = 0, example_loaded = FALSE)
    exportDF <- reactiveVal()

    #Debouncing prevents the plots from updating too frequently
    dataFilter_deb <- debounce(dataFilter, 3000)
    exportDF_deb <- debounce(exportDF, 2000)
    
    
    
    #reads info from the BED files uploaded by the user
    #aggregates read counts based on start and stop position of recombination event
    #sums the number of samples an event appears in based on start, stop, and type of event
    observeEvent(c(input$exampleData,input$file1),{
      
      if(input$exampleData == data$example_counter){
        
        filelist=list()
        for(i in 1:length(input$file1[,1])){
          filename=input$file1$name[i]
          filelist[[i]] = read.table(input$file1[[i, 'datapath']],skip=1, sep="\t",stringsAsFactors=FALSE, quote="") %>% mutate(File=filename)
        }
        boundDF = do.call(rbind, filelist) %>% group_by(V1,V2,V3,V4) %>% mutate(V11=n()) %>% mutate(V5=sum(V5 %>% as.numeric()))
        data$example_loaded = FALSE
        
      }else{
        
        filelist=list()
        filelist[[1]] = read.table("www/Example1.txt",skip=1, sep="\t",stringsAsFactors=FALSE, quote="") %>% mutate(File="Example1.txt")
        filelist[[2]] = read.table("www/Example2.txt",skip=1, sep="\t",stringsAsFactors=FALSE, quote="") %>% mutate(File="Example2.txt")
        boundDF = do.call(rbind, filelist) %>% group_by(V1,V2,V3,V4) %>% mutate(V11=n()) %>% mutate(V5=sum(V5 %>% as.numeric()))
        data$example_counter = input$exampleData
        data$example_loaded = TRUE
        
      }
      

      
      #Set our reactive value dataFilter to the aggregated dataframe 'boundDF'
      dataFilter(boundDF)
      
      
      availableRefSeq = boundDF$V1 %>% unique()
      updateSelectizeInput(session, 'RefSeqFilter', selected="", choices = availableRefSeq, server = TRUE)
      
      #Updates options for the 'fileSubset' selection box
      #Accounts for if example files button is clicked or using user uploaded files
      if(input$file1$name %>% is.null() == FALSE){
        filename= input$file1$name
      }else{
        filename = c("Example1.txt","Example2.txt")
      }
      updateSelectizeInput(session, 'fileSubset', selected="", choices = filename, server = TRUE)
      
      #This avoids an error message that occurs because of debouncing
      data$debounce = 'pause'
      
      
      #This resets the Circos annotation file input when you upload different files
      #This avoids plotting annotations on the wrong genome
      data$upload_state <- 'reset'
      annotations=data.frame(RefSeq=c(dataFilter()$V1[1],"","","",""), Start=c(10,"","","",""), End=c(500,"","","",""),Name=c("Ex","","","",""))
      data$annotations = annotations
    },ignoreInit = TRUE)
    
    
    
    #Responds to both users subsetting by reference seq and by file name
    observeEvent(c(input$RefSeqFilter,input$fileSubset),{
      req(dataFilter() )
      
      #This accounts for the example data button being hit
      if(data$example_loaded==TRUE){
        
        
        if(input$fileSubset %>% is.null == FALSE)
        {
          #Find the indexes of the file names selected by the user in the 'fileSubset' select box
          fileIndexes=which(c("Example1.txt","Example2.txt")%in%input$fileSubset)
          filelist=list()
          filelist[[1]] = read.table("www/Example1.txt",skip=1, sep="\t",stringsAsFactors=FALSE, quote="") %>% mutate(File="Example1.txt")
          filelist[[2]] = read.table("www/Example2.txt",skip=1, sep="\t",stringsAsFactors=FALSE, quote="") %>% mutate(File="Example2.txt")
          filelist=filelist[fileIndexes]
          boundDF = do.call(rbind, filelist) %>% group_by(V1,V2,V3,V4) %>% mutate(V11=n()) %>% mutate(V5=sum(V5 %>% as.numeric()))
          #Accounts for subsetting by ref seqs
          if(is.null(input$RefSeqFilter) == FALSE ){
            boundDF = boundDF %>% filter(V1 %in% input$RefSeqFilter)
          }
          dataFilter(boundDF)
          
          
        }else{
          
          #Accounts for if the user deletes all of the options after selection
          filelist=list()
          filelist[[1]] = read.table("www/Example1.txt",skip=1, sep="\t",stringsAsFactors=FALSE, quote="") %>% mutate(File="Example1.txt")
          filelist[[2]] = read.table("www/Example2.txt",skip=1, sep="\t",stringsAsFactors=FALSE, quote="") %>% mutate(File="Example2.txt")
          boundDF = do.call(rbind, filelist) %>% group_by(V1,V2,V3,V4) %>% mutate(V11=n()) %>% mutate(V5=sum(V5 %>% as.numeric()))
          
          #Accounts for subsetting by ref seqs
          if(is.null(input$RefSeqFilter) == FALSE ){
            boundDF = boundDF %>% filter(V1 %in% input$RefSeqFilter)
          }
          dataFilter(boundDF) 
          
        }
        
      }else{
        
        if(input$fileSubset %>% is.null == FALSE)
        {
          #Find the indexes of the file names selected by the user in the 'fileSubset' select box
          fileIndexes=which(input$file1$name%in%input$fileSubset)
          filelist=list()
          for(i in fileIndexes){
            filename=input$file1$name[i]
            filelist[[i]] =   read.table(input$file1[[i, 'datapath']],skip=1, sep="\t",stringsAsFactors=FALSE, quote="")%>% mutate(File=filename)
          }
          boundDF = do.call(rbind, filelist) %>% group_by(V1,V2,V3,V4) %>% mutate(V11=n()) %>% mutate(V5=sum(V5 %>% as.numeric()))
          #Accounts for subsetting by ref seqs
          if(is.null(input$RefSeqFilter) == FALSE ){
            boundDF = boundDF %>% filter(V1 %in% input$RefSeqFilter)
          }
          dataFilter(boundDF)
          
          
        }else{
          
          #Accounts for if the user deletes all of the options after selection
          filelist=list()
          for(i in 1:length(input$file1[,1])){
            filename=input$file1$name[i]
            filelist[[i]] =   read.table(input$file1[[i, 'datapath']],skip=1, sep="\t",stringsAsFactors=FALSE, quote="")%>% mutate(File=filename)
          }
          boundDF = do.call(rbind, filelist) %>% group_by(V1,V2,V3,V4) %>% mutate(V11=n()) %>% mutate(V5=sum(V5 %>% as.numeric()))
          #Accounts for subsetting by ref seqs
          if(is.null(input$RefSeqFilter) == FALSE ){
            boundDF = boundDF %>% filter(V1 %in% input$RefSeqFilter)
          }
          dataFilter(boundDF) 
          
        }
      }
        
      
    }, ignoreNULL = FALSE)
    

    
    #makes the start/stop plot
    output$ssPlot <- renderPlot({ 
      req(dataFilter() )
      #Having this function wait avoids a situation where dataFilter_deb() hasn't loaded before the function is called
      #It's because of the debouncing function
      if(data$debounce == "pause"){
        Sys.sleep(3)
        data$debounce = "go"
      }
      
      #This removes that error message flash
      validate(
        need(nrow(dataFilter_deb()) > 0, message = FALSE)
      )      

      fill_limit=c(1,max(dataFilter_deb()$V11))
      
      #Don't worry about this being called V11 - the initial creation of the variable calls it V11 regardless of the position
        p=ggplot(dataFilter_deb(),aes(x=V3, y=V2, color=V11)) +
            geom_point(aes(size = V5),alpha = 0.6) +
            scale_size_continuous("Read Count",range = c(0.4, 3))+
            coord_cartesian() +
          ylab("Donor Site") +
          xlab("Acceptor Site") +
            theme_classic(base_size = 18) +
            scale_color_gradient("# of Isolates",low = "#87f6ff", high = "#f786ff", limits = fill_limit)+
          geom_rug(col=rgb(.5,.6,.7,alpha=.1))+
          guides(color = guide_colorbar(order=1), size=guide_legend(order = 2))
        
        
        if(input$scatterColor == "Viridis"){
          p=ggplot(dataFilter_deb(),aes(x=V3, y=V2, color=V11)) +
            geom_point(aes(size = V5),alpha = 0.6) +
            scale_size_continuous("Read Count",range = c(0.4, 3))+
            coord_cartesian() +
            ylab("Donor Site") +
            xlab("Acceptor Site") +
            theme_classic(base_size = 18) +
            scale_color_viridis_c("# of Isolates", limits = fill_limit)+
            geom_rug(col=rgb(.5,.6,.7,alpha=.1))+
            guides(color = guide_colorbar(order=1), size=guide_legend(order = 2))
          
        }
        
        if(input$hl1 == TRUE & length(input$mytable_rows_all)!=0){
          #This if statement allows the following: if a filter is applied using the built in table filter functions, it only highlights the sequences in the table (not all in the brush space)
          if(length(input$mytable_rows_all) != nrow(exportDF()) )
          {
            
            p <- p + geom_point(data = exportDF_deb()[input$mytable_rows_all,], aes(x=V3, y=V2), colour = "red") + geom_point(data = exportDF()[input$mytable_rows_selected,], aes(x=V3, y=V2), colour = "#FFB81C")
            
          }else{
            
            p <- p + geom_point(data = exportDF_deb(), aes(x=V3, y=V2), colour = "red")+ geom_point(data = exportDF()[input$mytable_rows_selected,], aes(x=V3, y=V2), colour = "#FFB81C")
            
          }

        }
        data$plotMS=p
        p
        
    })
    

    #get the brushed points
    searched <- reactive({
        req(dataFilter() )
        if(input$advFilter!=""){
          query=input$advFilter
          query=rlang::parse_expr(query)
          advFilterDF=dataFilter()
          colnames(advFilterDF)=paste0("Col",seq(1,ncol(advFilterDF)))
          error <- try( advFilterDF%>% as.data.frame()%>% filter(!!query) )
          if(inherits(error,"try-error"))
          {
            stop("There is an issue with the syntax. Hit the question mark for detailed instructions.")
          }else{
            queryDF=advFilterDF %>% as.data.frame()%>% filter(!!query)
            #Plotting functions (the scatterplots) rely on the variables being called V# so you need to change the names before changing the value of exportDF
            colnames(queryDF)=paste0("V",seq(1,ncol(queryDF)))
            exportDF(queryDF)
            #We change the colnames to something more readable for the table output
            colnames(queryDF)=c("Reference", "From (Donor)", "To (Acceptor)", "Details", "Read Count", "Strandedness")
            colnames(queryDF)[length(queryDF %>% colnames())-1]="File"
            colnames(queryDF)[length(queryDF %>% colnames())]="Number of Isolates"
            queryDF  %>% return
            
          }
        }else if(length(input$plot_brush1)!=0) {
          brush=brushedPoints(dataFilter(), input$plot_brush1, xvar = "V3", yvar = "V2")
          exportDF(brush)
          colnames(brush)=c("Reference", "From (Donor)", "To (Acceptor)", "Details", "Read Count", "Strandedness")
          #change last column to Number of Isolates
          colnames(brush)[length(brush %>% colnames())-1]="File"
          colnames(brush)[length(brush %>% colnames())]="Number of Isolates"
          brush %>% as.data.frame() %>% return
        }


    })
    

    
    
    output$ssPlotZoom <- renderPlot({ 
      req(dataFilter() )
      
      validate(
        need(nrow(dataFilter_deb()) > 0, message = FALSE)
      )      
      
      fill_limit=c(1,max(dataFilter_deb()$V11))
      
        p=ggplot(brushedPoints(dataFilter_deb(), input$plot_brush, xvar = "V3", yvar = "V2"),aes(x=V3, y=V2,color=V11)) +
          geom_point(aes(size = V5),alpha = 0.6) +
          scale_size_continuous("Read Count",range = c(0.75, 2.5))+
          coord_cartesian() +
          ylab("Donor Site") +
          xlab("Acceptor Site") +
          theme_classic(base_size = 18)+
          scale_color_gradient("# of Isolates",low = "#87f6ff", high = "#f786ff", limits = fill_limit)+
          geom_rug(col=rgb(.5,.6,.7,alpha=.1))+
          guides(color = guide_colorbar(order=1), size=guide_legend(order = 2))
        
        
        #Allows for the viridis color palette
        if(input$scatterColor == "Viridis"){
          p=ggplot(brushedPoints(dataFilter_deb(), input$plot_brush, xvar = "V3", yvar = "V2"),aes(x=V3, y=V2,color=V11)) +
            geom_point(aes(size = V5),alpha = 0.6) +
            scale_size_continuous("Read Count",range = c(0.75, 2.5))+
            coord_cartesian() +
            ylab("Donor Site") +
            xlab("Acceptor Site") +
            theme_classic(base_size = 18)+
            scale_color_viridis_c("# of Isolates", limits = fill_limit)+
            geom_rug(col=rgb(.5,.6,.7,alpha=.1))+
            guides(color = guide_colorbar(order=1), size=guide_legend(order = 2))
        }
          
        
        #assign it to a reactive value for the download button
        data$plotSS=p
        p
    })
    
    
    #output those points into a table
    output$mytable = DT::renderDataTable(filter = list(position = 'top', clear = FALSE),options = list(scrollX = TRUE,dom = 'ltpir'),{
      searched()
    })
    
    
    fileSuffix = function(plotname){
      if(input$fformat==".png") filename <- paste0(plotname,"Plot.png",collapse = "")
      if(input$fformat==".tiff") filename <- paste0(plotname,"Plot.tiff",collapse = "")
      if(input$fformat==".jpeg") filename <- paste0(plotname,"Plot.jpg",collapse = "")
      if(input$fformat==".pdf") filename <- paste0(plotname,"Plot.pdf",collapse = "")
      return(filename)
    }
    
    output$downloadPlot <- downloadHandler(
        
        filename = 'viremaplots.zip',
        content = function(file){
          #Match the file type with what the user selected in input$fformat
          if(input$fformat==".png") filesuff <- "png"
          if(input$fformat==".tiff") filesuff <- "tiff"
          if(input$fformat==".jpeg") filesuff <- "jpg"
          if(input$fformat==".pdf") filesuff <- "pdf"
          
          # Save the histograms (a loop can be used here for a bunch of plots)
          ggsave( fileSuffix("mainscatter"), plot = data$plotMS, device = filesuff, width=7.5, height=4.5)
          ggsave( fileSuffix("subscatter"), plot = data$plotSS, device = filesuff, width=7.5, height=4.5)
          
          # Zip them up
          zip( file, c( fileSuffix("mainscatter"), fileSuffix("subscatter")))
        }
        
    )
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(exportDF()[input$mytable_rows_all,], con)
      }
    )
    
    output$downloadCircos <- downloadHandler(
      filename = function(){fileSuffix("circos") }, # variable with filename
      content = function(file) {
        if(input$fformat==".png") png(file = file, width = 600, height = 600, units = "px")
        if(input$fformat==".tiff") tiff(file = file, width = 600, height = 600, units = "px")
        if(input$fformat==".jpeg") jpeg(file = file, width = 600, height = 600, units = "px")
        if(input$fformat==".pdf") pdf(file = file)
        
        print(data$plotC)
        dev.off()
      })
    
    
    output$downloadOverviewPlot1 <- downloadHandler(
      
      filename = 'viremaOVplots.zip',
      content = function(file){
        #Match the file type with what the user selected in input$fformat
        if(input$fformat==".png") filesuff <- "png"
        if(input$fformat==".tiff") filesuff <- "tiff"
        if(input$fformat==".jpeg") filesuff <- "jpg"
        if(input$fformat==".pdf") filesuff <- "pdf"
        
        # Save the histograms (a loop can be used here for a bunch of plots)
        ggsave( fileSuffix("donor"), plot = data$plotDP, device = filesuff, width=7.5, height=4.5)
        ggsave( fileSuffix("acceptor"), plot = data$plotAP, device = filesuff, width=7.5, height=4.5)

        # Zip them up
        zip( file, c( fileSuffix("donor"), fileSuffix("acceptor") ))
      }
      
    )
    
    output$downloadOverviewPlot2 <- downloadHandler(
      
      filename = 'viremaOVplots.zip',
      content = function(file){
        #Match the file type with what the user selected in input$fformat
        if(input$fformat==".png") filesuff <- "png"
        if(input$fformat==".tiff") filesuff <- "tiff"
        if(input$fformat==".jpeg") filesuff <- "jpg"
        if(input$fformat==".pdf") filesuff <- "pdf"
        
        # Save the histograms (a loop can be used here for a bunch of plots)
        ggsave( fileSuffix("deletion"), plot = data$plotDEL, device = filesuff, width=7.5, height=4.5)
        ggsave( fileSuffix("duplication"), plot = data$plotDUP, device = filesuff, width=7.5, height=4.5)
        
        # Zip them up
        zip( file, c( fileSuffix("deletion"), fileSuffix("duplication")))
      }
      
    )
    
    
    output$manhattanDelPlot <- renderPlot({ 
      req(dataFilter() )
      mDel=dataFilter()%>% select(V2,V3,V4,V5,V6,V7,V9) %>% filter(V2<V3) %>% mutate(id=paste0(V2,V3,V4,collapse = ",")) %>% group_by(id) %>% filter(row_number()==1) %>% ungroup()

      if(input$tableSubset == TRUE & length(input$mytable_rows_all) > 0){
        
        mDel=exportDF_deb()%>% select(V2,V3,V4,V5,V6,V7,V9) %>% filter(V2<V3) %>% mutate(id=paste0(V2,V3,V4,collapse = ",")) %>% group_by(id) %>% filter(row_number()==1) %>% ungroup()
        
      }
      
      #The 'binwidth' for the frequency is 100 - this is for speed purposes
      inSeq=sapply(seq(from=100,to=max(mDel$V3),by=100),function(i) sapply(1:nrow(mDel), function(x) mDel$V2[x]<i&i<mDel$V3[x] ))
      plotDF=sapply(1:ncol(inSeq),function(x) c(x*100,mDel[which(inSeq[,x]==TRUE),]$V5 %>% sum)) %>% t() %>% as.data.frame()
      colnames(plotDF) = c("V1","n")
      
      p=ggplot(data=plotDF, aes(x=V1, y=n)) + geom_area(fill="lightblue")+
        geom_step()+
        ylab("Reads") +
        xlab("Base Position of Deletion Events") +
        theme_classic(base_size = 18)
      

       # mDel=dataFilter_deb()%>% select(V1,V2,V3,V4,V5) %>% filter(V2<V3) %>% mutate(id=paste0(V1,V2,V3,V4,collapse = ",")) %>% group_by(id) %>% filter(row_number()==1) %>% ungroup()  %>% rowwise%>% mutate(seq = list(seq(from=V2,to=V3,by=1))) %>%
       #   unnest(cols = seq) %>% group_by(seq) %>% mutate(n=sum(V5))
       # 
       # p=ggplot(mDel,aes(x=seq)) +
       #   geom_histogram(binwidth = 100, fill = "lightblue")+
       #   ylab("Reads") +
       #   xlab("Base Position of Deletion Events") +
       #   theme_classic(base_size = 18)
       data$plotDEL=p
       p
    })
    
    output$manhattanDupPlot <- renderPlot({ 
      req(dataFilter() )
      
      mDup=dataFilter()%>% select(V2,V3,V4,V5,V6,V7,V9) %>% filter(V2>V3) %>% mutate(id=paste0(V2,V3,V4,collapse = ",")) %>% group_by(id) %>% filter(row_number()==1) %>% ungroup()
      
      if(input$tableSubset == TRUE & length(input$mytable_rows_all) > 0){
        
        mDup=exportDF_deb()%>% select(V2,V3,V4,V5,V6,V7,V9) %>% filter(V2>V3) %>% mutate(id=paste0(V2,V3,V4,collapse = ",")) %>% group_by(id) %>% filter(row_number()==1) %>% ungroup()
        
      }
      
      inSeq=sapply(seq(from=0,to=max(mDup$V3),by=100),function(i) sapply(1:nrow(mDup), function(x) mDup$V2[x]>i&i>mDup$V3[x] ))
      plotDF=sapply(1:ncol(inSeq),function(x) c(x*100,mDup[which(inSeq[,x]==TRUE),]$V5 %>% sum)) %>% t() %>% as.data.frame()
      colnames(plotDF) = c("V1","n")
      
      # 
      # mDup=dataFilter_deb()%>% select(V1,V2,V3,V4,V5) %>% filter(V2>V3) %>% mutate(id=paste0(V1,V2,V3,V4,collapse = ",")) %>% group_by(id) %>% filter(row_number()==1) %>% ungroup()  %>% rowwise%>% mutate(seq = list(seq(from=V2,to=V3,by=-1))) %>%
      #   unnest(cols = seq) %>% group_by(seq) %>% mutate(n=sum(V5))

      p=ggplot(data=plotDF, aes(x=V1, y=n)) + geom_area(fill="red")+
        geom_step()+
        ylab("Reads") +
        xlab("Base Position of Duplication Events") +
        theme_classic(base_size = 18)
      
      data$plotDUP=p
      p
    })
    
    output$donorPlot <- renderPlot({ 
      req(dataFilter() )
      
      
      donor=dataFilter_deb()%>% select(V2,V3,V4,V5,V6,V7,V9)%>% mutate(letters = str_split(V9, "")) %>%
        unnest(cols = letters) %>%filter(letters!="|") %>% 
        group_by(V2,V3,V4,V5,V6,V7, V9) %>%
        mutate(position = row_number()-25) %>% ungroup()%>% count(letters,position)
      
      #If the option to limit the plots to sequences in the table only is selected, it subsets
      if(input$tableSubset == TRUE & length(input$mytable_rows_all) > 0){
        donor=exportDF_deb()[input$mytable_rows_all,]%>% select(V2,V3,V4,V5,V6,V7,V9)%>% mutate(letters = str_split(V9, "")) %>%
          unnest(cols = letters) %>%filter(letters!="|") %>% 
          group_by(V2,V3,V4,V5,V6,V7, V9) %>%
          mutate(position = row_number()-25) %>% ungroup()%>% count(letters,position)
        
      }
      
      p=ggplot(donor,aes(x=position, y=n, group=letters)) +
        geom_line(aes(color=letters),size=1)+ylim(c(0,NA))+
        geom_vline(xintercept = 0, linetype="dashed", 
                     color = "gray", size=1)+
        ylab("# of Sequences") +
        xlab("Base Position Relative to Donor") +
        theme_classic(base_size = 18)
      data$plotDP=p
      p
    })
    
    output$acceptPlot <- renderPlot({ 
      req(dataFilter() )

      accept=dataFilter_deb()%>% select(V2,V3,V4,V5,V6,V7,V10)%>% mutate(letters = str_split(V10, "")) %>%
        unnest(cols = letters) %>%filter(letters!="|") %>%
        group_by(V2,V3,V4,V5,V6,V7,V10) %>%
        mutate(position = row_number()-25) %>% ungroup()%>% count(letters,position)
      
      #If the option to limit the plots to sequences in the table only is selected, it subsets
      if(input$tableSubset == TRUE & length(input$mytable_rows_all) > 0){
        
        accept=exportDF_deb()[input$mytable_rows_all,]%>% select(V2,V3,V4,V5,V6,V7,V10)%>% mutate(letters = str_split(V10, "")) %>%
          unnest(cols = letters) %>%filter(letters!="|") %>% 
          group_by(V2,V3,V4,V5,V6,V7, V10) %>%
          mutate(position = row_number()-25) %>% ungroup()%>% count(letters,position)
        
        
      }
      
      
      p=ggplot(accept,aes(x=position, y=n, group=letters)) +
        geom_line(aes(color=letters),size=1)+ylim(c(0,NA))+
        geom_vline(xintercept = 0, linetype="dashed", 
                   color = "gray", size=1)+
        ylab("# of Sequences") +
        xlab("Base Position Relative to Acceptor") +
        theme_classic(base_size = 18) 
      data$plotAP=p
      p
    })
    
    
    #Summary table for individual and aggregated samples
    output$uniqueEvents = DT::renderDataTable(rownames = FALSE,selection = 'none',options = list(scrollX = TRUE,dom = 'tp',pageLength = 5),{
      req(dataFilter() )
      #If statement is for when the user has selected a subset of samples to focus on with the dropdown
      if(input$tableSubset == TRUE & length(input$mytable_rows_all) > 0){
        idDF = exportDF_deb()%>% select(V1,V2,V3,V4,V5,V11,File) %>% mutate(id=paste0(V1,V2,V3,V4,collapse = ",")) %>% group_by(id) %>% filter(row_number()==1) %>% ungroup()
        uniqueDF = idDF %>% select(File,V11) %>% filter(V11==1) %>% group_by(File) %>% summarise("Unique Events" = sum(V11))
        filesDF = exportDF_deb() %>% group_by(File) %>% summarise("Total # of Reads Mapping to Recombination Events" = sum(V5),"Total Events" = n())
        individualFiles=merge(filesDF,uniqueDF,by="File")
        totalRow = c("Total", sum(filesDF$"Total # of Reads Mapping to Recombination Events"), idDF %>% nrow(), which(idDF$V11==1) %>% length)
        rbind( totalRow , individualFiles )
      }else{
        idDF = dataFilter_deb()%>% select(V1,V2,V3,V4,V5,V11,File) %>% mutate(id=paste0(V1,V2,V3,V4,collapse = ",")) %>% group_by(id) %>% filter(row_number()==1) %>% ungroup()
        uniqueDF = idDF %>% select(File,V11) %>% filter(V11==1) %>% group_by(File) %>% summarise("Unique Events" = sum(V11))
        filesDF = dataFilter_deb() %>% group_by(File) %>% summarise("Total # of Reads Mapping to Recombination Events" = sum(V5),"Total Events" = n())
        individualFiles=merge(filesDF,uniqueDF,by="File")
        totalRow = c("Total", sum(filesDF$"Total # of Reads Mapping to Recombination Events"), idDF %>% nrow(), which(idDF$V11==1) %>% length)
        rbind( totalRow , individualFiles )
      }
    })
    
    #Produces the table for quick annotations on the Circos plot
    output$annotate = DT::renderDataTable(editable="row",rownames = FALSE,selection = 'none',options = list(scrollX = TRUE,dom = 'tpi',pageLength = 3),{
      req(dataFilter() )
      data$annotations
    })
    
    observeEvent(input$annotate_cell_edit, {
      
      addEdit = data$annotations
      addEdit[input$annotate_cell_edit$row[1],]=input$annotate_cell_edit$value
      data$annotations = addEdit
    })
    
    observeEvent(input$annoAddRow, {
      newRow=rbind(data$annotations,c("","","","")) %>% as.data.frame()
      data$annotations = newRow
    })
    
    #This is a work around to allow resetting of the Circos annotation fileInput
    #See https://stackoverflow.com/questions/44203728/how-to-reset-a-value-of-fileinput-in-shiny/44206615
    observeEvent(input$file2, {
      data$upload_state <- 'uploaded'

    })
    

    #This allows for removal of annotations from the plot when the 'reset' button is hit
    observeEvent(input$reset, {
      data$upload_state <- 'reset'
    })
    
    #Changes the value of anno_input when the 'plot' button is hit
    observeEvent(input$annoPlotCircos, {
      data$upload_state <- 'custom'

    })
    
    
    #A reactive value that is affected by uploading new BED files and the reset button
    #Is used to when generating the Circos plot
    #the value of anno_input also changes with pressing the 'Plot' button 'annoPlotCircos'
    anno_input <- reactive({
      if (is.null(data$upload_state)) {
        return(NULL)
      } else if (data$upload_state == 'uploaded') {
        anno= read.table(input$file2$datapath, sep="\t",stringsAsFactors=FALSE, quote="")
        return(anno)
      } else if (data$upload_state == 'reset') {
        return(NULL)
      } else if (data$upload_state == 'custom') {
        
        #Takes data$annotations from the Fast Annotations and coerces it into a usable form
        annoColNam=data$annotations
        annoColNam$Start = annoColNam$Start %>% as.numeric()
        annoColNam$End = annoColNam$End %>% as.numeric()
        colnames(annoColNam)=paste(rep("V",ncol(annoColNam)),seq(1:ncol(annoColNam)), sep="")
        #filter to only filled out rows
        annoColNam[annoColNam==""]=NA
        annoColNam=annoColNam %>% na.omit %>% as.data.frame()
        return(annoColNam)
      }
    })
    
    #Need to generate sliders based on the number of unique reference sequences
    output$xLimCircosSliders <- renderUI({
      req(dataFilter() )
      

      #in order to plot the data's xlim correctly, the maximum value in either the start or end needs to be found
      xUpperLims=dataFilter() %>% group_by(V1) %>% mutate(maxV2 = max(V2, na.rm=TRUE))%>% mutate(maxV3 = max(V3, na.rm=TRUE)) %>% summarise(max=ifelse(maxV2>maxV3,maxV2,maxV3) ) %>% slice(which.max(max))
      #need to make a new set of xlims for each reference seq in the dataset
      xLims=as.matrix(cbind(rep(0, unique(dataFilter()$V1) %>% length ), xUpperLims$max ))
      
      
      # First, create a list of sliders each with a different name
      
      sliders <- lapply(1:length( unique(dataFilter()$V1) ), function(i) {
        inputName <- unique(dataFilter()$V1)[i]
        numericRangeInput(inputName, inputName, min=0, value=c(xLims[i,1],xLims[i,2]), step = 100)
      })
      
      # Create a tagList of sliders (this is important)
      do.call(tagList, sliders)
      
    })
    

    #Generate the Circos plot
    output$circosPlot <- renderPlot({ 
      req(dataFilter() )
      req(input$mytable_rows_all)

      circos.clear()
      
      inputName=unique(dataFilter()$V1)[1]
      holdVal=str2expression(text=paste0("input$",inputName,collapse = "")) %>% eval()
      
      #Different plots will be produced if annotations are available or not
      if(is.null(anno_input()))
      {
        
        #need to make a new set of xlims for each reference seq in the dataset
        #holdVal will check if the xlim slider has been instantiated
        if(is.null(holdVal))
        {
          xUpperLims=dataFilter() %>% group_by(V1) %>% mutate(maxV2 = max(V2, na.rm=TRUE))%>% mutate(maxV3 = max(V3, na.rm=TRUE)) %>% summarise(max=ifelse(maxV2>maxV3,maxV2,maxV3) ) %>% slice(which.max(max))
          #need to make a new set of xlims for each reference seq in the dataset
          xLims=as.matrix(cbind(rep(0, unique(dataFilter()$V1) %>% length ), xUpperLims$max ))
          rownames(xLims)=unique(dataFilter()$V1)
          
        }else{
          xLims=data.frame()
          for(i in 1:length(unique(dataFilter()$V1)) )
          {
            inputName=unique(dataFilter()$V1)[i]
            #Need to an expression and evaluate it to access the slider variables for each reference seq
            holdVal=str2expression(text=paste0("input$",inputName,collapse = "")) %>% eval()
            xLims=rbind(xLims,holdVal)
          }
          xLims=xLims %>% as.matrix()
          rownames(xLims)=unique(dataFilter()$V1)
        }
        
        
        b1=exportDF()[input$mytable_rows_all,] %>% ungroup() %>% select(V1,V2)
        b2=exportDF()[input$mytable_rows_all,] %>% ungroup() %>% select(V1,V3)
        
        #Remove all genomicLinks that fall outside of the xlims
        b1bool=vector()
        b2bool=vector()
        b1$id = 1:nrow(b1)
        b2$id = 1:nrow(b2)
        for(i in unique(b1$V1))
        {
          hold = b1 %>% dplyr::filter(V1==i) %>% dplyr::filter( between(V2,xLims[which(rownames(xLims)==i),1],xLims[which(rownames(xLims)==i),2]) )
          b1bool = c(b1bool,hold$id)
        }
        
        for(i in unique(b2$V1))
        {
          hold = b2 %>% dplyr::filter(V1==i) %>% dplyr::filter( between(V3,xLims[which(rownames(xLims)==i),1],xLims[which(rownames(xLims)==i),2]) )
          b2bool = c(b2bool,hold$id)
        }
        
        b1=b1[intersect(b1bool,b2bool),1:2]
        b2=b2[intersect(b1bool,b2bool),1:2]
        
        
        circos.par("track.height" = 0.2)
        circos.initialize(dataFilter()$V1,xlim=xLims)
        circos.track(sectors=dataFilter()$V1, ylim = c(1,110),
                     panel.fun = function(x, y) {
      
                       circos.axis(labels.cex = input$axisTextSize)
                       circos.genomicLink(b1,b2, col = add_transparency(input$circosColor, 1-input$cex), arr.type = "triangle",directional = 1,
                                          border = NA)
                     })
        data$plotC=recordPlot()
        
      }else
      {
        #Else statement if some annotation is present - either file upload or fast annotations
        anno = anno_input()
        b1=exportDF()[input$mytable_rows_all,] %>% ungroup() %>% select(V1,V2)
        b2=exportDF()[input$mytable_rows_all,] %>% ungroup() %>% select(V1,V3)

        #in order to plot the data's xlim correctly, the maximum value in either the start or end needs to be found
        #need to make a new set of xlims for each reference seq in the dataset
        #the inputs from the sliders under the Options dropdown have the reference sequences as their names
        #we need to access these names in order to have the sliders affect the Circos plot
        if(is.null(holdVal))
        {
          xUpperLims=dataFilter() %>% group_by(V1) %>% mutate(maxV2 = max(V2, na.rm=TRUE))%>% mutate(maxV3 = max(V3, na.rm=TRUE)) %>% summarise(max=ifelse(maxV2>maxV3,maxV2,maxV3) ) %>% slice(which.max(max))
          #need to make a new set of xlims for each reference seq in the dataset
          xLims=as.matrix(cbind(rep(0, unique(dataFilter()$V1) %>% length ), xUpperLims$max ))
          rownames(xLims)=unique(dataFilter()$V1)
          
        }else{
          xLims=data.frame()
          for(i in 1:length(unique(dataFilter()$V1)) )
          {
            inputName=unique(dataFilter()$V1)[i]
            #Need to evaluate an expression to access the slider variables for each reference seq
            holdVal=str2expression(text=paste0("input$",inputName,collapse = "")) %>% eval()
            xLims=rbind(xLims,holdVal)
          }
          xLims=xLims %>% as.matrix()
          rownames(xLims)=unique(dataFilter()$V1)
        }
        
        #Need to make a row variable for pretty placement of annotations

         annoOrd=anno %>% select(V1,V2,V3,V4) %>% arrange(V2)
         #you can't read in the # symbol so reading in custom hex codes has to exclude that symbol
         #if the last column in the annotation data frame has hex codes, carry those colors forward
         if( all(nchar( as.vector(anno[,ncol(anno)]) ) == 6 ) ){
           annoOrd=anno %>% select(V1,V2,V3,V4) 
           annoOrd$color=as.vector(anno[,ncol(anno)])
           annoOrd = annoOrd %>% arrange(V2)
         }
         annoOrd$row=NA
         annoOrd$row[1]=1
         
         #This assigns annotations to rows based on if they overlap
         #Account for if there is only one annotation
         if(nrow(anno)>1)
         {
           for(i in 2:nrow(annoOrd)){
             
             #groups existing rows and evaluates whether the member segments overlap with the segment to be placed. 
             #'input$gapDist' is linked to a slider under the options dropdown. It controls the bp distance between adjacent segments.
             noOverlaps=annoOrd %>% na.omit %>% group_by(row) %>% mutate(Overlap=any( V3+ input$gapDist >= annoOrd$V2[i] ) ) %>% dplyr::filter(Overlap==F) %>% arrange(row,desc())
             
             if(nrow(noOverlaps!=0)){
               annoOrd$row[i]=noOverlaps$row[1]
             }else{
               annoOrd$row[i]=max(annoOrd$row,na.rm = T)+1
             }
             
           }
         }
         annoOrd$row[1]=1
         
         #Set anno limits to the x axis limits if they are outside their bounds
         #First remove all the ones that are completely outside
         
         finalAnno=data.frame()
         for(i in unique(dataFilter()$V1) )
         {
             #identifies annotations with starts or ends in between the xlimits
             hold = annoOrd %>% dplyr::filter(V1==i) %>% dplyr::filter( between(V2,xLims[which(rownames(xLims)==i),1],xLims[which(rownames(xLims)==i),2])|between(V3,xLims[which(rownames(xLims)==i),1],xLims[which(rownames(xLims)==i),2]) ) %>% mutate(V2=ifelse(V2<xLims[which(rownames(xLims)==i),1],xLims[which(rownames(xLims)==i),1],V2)) %>% mutate(V3=ifelse(V3>xLims[which(rownames(xLims)==i),2],xLims[which(rownames(xLims)==i),2],V3))
             finalAnno=rbind(finalAnno,hold)
             #identifies annotations with starts and ends more extreme than xlimits
             hold = annoOrd %>% dplyr::filter(V1==i) %>% dplyr::filter(V2<xLims[which(rownames(xLims)==i),1]&V3>xLims[which(rownames(xLims)==i),2]) %>% mutate(V2 = xLims[which(rownames(xLims)==i),1]) %>% mutate(V3 = xLims[which(rownames(xLims)==i),2])
             finalAnno=rbind(finalAnno,hold)
         }

         #Remove all genomicLinks that fall outside of the xlims
         b1bool=vector()
         b2bool=vector()
         b1$id = 1:nrow(b1)
         b2$id = 1:nrow(b2)
         for(i in unique(dataFilter()$V1))
         {
           hold = b1 %>% dplyr::filter(V1==i) %>% dplyr::filter( between(V2,xLims[which(rownames(xLims)==i),1],xLims[which(rownames(xLims)==i),2]) )
           b1bool = c(b1bool,hold$id)
         }
         
         for(i in unique(dataFilter()$V1))
         {
           hold = b2 %>% dplyr::filter(V1==i) %>% dplyr::filter( between(V3,xLims[which(rownames(xLims)==i),1],xLims[which(rownames(xLims)==i),2]) )
           b2bool = c(b2bool,hold$id)
         }
         
         b1=b1[intersect(b1bool,b2bool),1:2]
         b2=b2[intersect(b1bool,b2bool),1:2]
         
        #This is a scaling factor to generate appropriate heights for annotation rectangles
        #It's based on the number of rows
        scaleFact=100/max(annoOrd$row,na.rm = T)
        

        circos.par("track.height" = 0.2)
        circos.initialize(sectors=dataFilter()$V1, dataFilter()$V3, xlim=xLims)
        circos.track(dataFilter()$V1, ylim = c(1,110),
                     panel.fun = function(x, y) {
                       circos.axis(labels.cex = input$axisTextSize)
                       circos.genomicLink(b1,b2, col = add_transparency(input$circosColor, 1-input$cex), arr.type = "triangle",directional = 1,
                                          border = NA)
                     })
        for(i in unique(finalAnno$V1) )
        {
          seqs=finalAnno %>% filter(V1==i)
          yLower=(seqs$row-1)*scaleFact
          yUpper=(seqs$row)*scaleFact
          
          #this only applies if the annotation file had hexcodes in the last column
          if("color"%in%colnames(seqs)){
            circos.rect(sector.index = seqs$V1[1], xleft=seqs$V2, ybottom=yLower, xright=seqs$V3, ytop=yUpper,col=paste("#",seqs$color, sep = ""))
          }else{
            circos.rect(sector.index = seqs$V1[1], xleft=seqs$V2, ybottom=yLower, xright=seqs$V3, ytop=yUpper,col="#FF0000")
          }
          circos.text(sector.index = seqs$V1[1], x=(seqs$V2+seqs$V3)/2, y=yLower+(scaleFact/2), seqs$V4, cex=input$annoTextSize, facing = "bending.inside")
          
        }
        data$plotC=recordPlot()
        
      }
     

    })
    

    
    
    
}

shinyApp(ui, server)