

library("shinythemes")


dataset <- list('Upload a file'=c(1))

shinyUI(fluidPage(theme = shinytheme("flatly"),pageWithSidebar(
  

  headerPanel("Data and Text Mining"),
  
  sidebarPanel(
   
    
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 selected = NULL),
    
    numericInput(inputId ="obs",label = "Number of observations to view:", 100),
    hr(),
    
    checkboxInput(inputId ="plotting", label ="Plotting",
                  value = FALSE),
    
    
    conditionalPanel(  
      condition = "input.plotting == true" ,
      
      
      selectInput(inputId ="plot",label=strong("Plot Type "),
                  choices=list("None"="none","Histogram"="hist", "Boxplot"="box", "Barplot"="bar", "Density Chart"="dens",
                               "Pointchart"="point","Heatmap"="heat","Pairs Plot"="pair")),
    
    
    conditionalPanel(
      condition = "input.plot == 'hist'",
      
      selectInput(inputId = "attributes",
                  label = "ATTRIBUTES",
                  choices=names(dataset)),
      
      
      sliderInput(inputId ="bin",label="Binwidth",
                  min=0,max=200,value=1,step=0.1),
      
      actionButton(inputId ="goButton1", label =strong("Update View", style = "color:blue")),
      
      
      selectInput(inputId ="fil", label ="Fill",
                  choices=c('None'='.',names(dataset))),
      
      selectInput(inputId ='facet_row', label ='Facet Row', 
                  choices= c('None'='.',names(dataset))),
      selectInput(inputId ='facet_col', label ='Facet Column', 
                  choices= c('None'='.',names(dataset)))
      
    ),
    
    conditionalPanel(
      condition = "input.plot == 'box'",
      
      selectInput(inputId = "attributesx.box",
                  label = "X",
                  choices=names(dataset),selected='None'),
      
      selectInput(inputId = "attributesy.box",
                  label = "Y",
                  choices = names(dataset),selected='None'),
      
      actionButton(inputId ="goButton6", label =strong("Update View", style = "color:blue")),
      
      
      selectInput(inputId ="filbox", label ="Fill",
                  choices=c('None'='.',names(dataset))),
      
      selectInput(inputId ='facet_row.box', label ='Facet Row', 
                  choices= c('None'='.',names(dataset))),
      selectInput(inputId ='facet_col.box', label ='Facet Column', 
                  choices= c('None'='.',names(dataset)))
      
    ),
    
    conditionalPanel(
      condition = "input.plot == 'bar'",
      
      selectInput(inputId = "attributes.bar",
                  label = "ATTRIBUTES",
                  choices=names(dataset)),
      
      
      checkboxInput(inputId ="identity", label ="Stat.Identity",
                    value = FALSE),  
      
      conditionalPanel(  
        condition = "input.identity == true" ,
        
        selectInput(inputId = "attributes.ident",
                    label = "ATTRIBUTES",
                    choices=names(dataset))
        
      ),
      
      
      actionButton(inputId ="goButton3", label =strong("Update View", style = "color:blue")),
      
      
      selectInput(inputId ="filbar", label ="Fill",
                  choices=c('None'='.',names(dataset))),
      
      selectInput(inputId ='facet_row.bar', label ='Facet Row', 
                  choices= c('None'='.',names(dataset))),
      selectInput(inputId ='facet_col.bar', label ='Facet Column', 
                  choices= c('None'='.',names(dataset)))
      
    ),
    
    conditionalPanel(
      condition = "input.plot == 'dens'",
      
      selectInput(inputId = "attributes.dens",
                  label = "ATTRIBUTES",
                  choices= names(dataset)),
      
      
      actionButton(inputId ="goButton2", label =strong("Update View", style = "color:blue")),
      
      
      selectInput(inputId ="fil.dens", label ="Fill",
                  choices= c('None'='.',names(dataset))),
      
      selectInput(inputId ='facet_row.dens', label ='Facet Row', 
                  choices= c('None'='.',names(dataset))),
      selectInput(inputId ='facet_col.dens', label ='Facet Column', 
                  choices= c('None'='.',names(dataset)))
      
    ),

    conditionalPanel(
      condition = "input.plot == 'point'" ,
      selectInput(
        inputId = "attrx",
        label = "X",
        choices = names(dataset),
      ),
      selectInput(
        inputId = "attry",
        label = "Y",
        choices = names(dataset),
      ),
      actionButton(inputId ="goButton5", label =strong("Update View", style = "color:blue")),
      
      selectInput(inputId ="filpoint", label ="Fill",
                  choices=c('None'='.',names(dataset))),
      
      selectInput(inputId ="sizepoint", label ="Size",
                  choices=c('None'='.',names(dataset))),
      
      selectInput(inputId ='facet_rowpoint', label ='Facet Row', 
                  choices= c('None'='.',names(dataset))),
      selectInput(inputId ='facet_colpoint', label ='Facet Column', 
                  choices= c('None'='.',names(dataset)))
      
    ),
    
    conditionalPanel(
      condition = "input.plot == 'heat'" 
 
    )
    
  ),
  hr(),
  
  checkboxInput(inputId ="pred_mod", label ="Data Mining",
                value = FALSE),
  
  conditionalPanel(  
    condition = "input.pred_mod == true" ,
    
  # start supervised modelling
    checkboxInput(inputId ="supervised", label ="Supervised Methods",
                  value = FALSE),
                       
  conditionalPanel(  
    condition = "input.supervised == true" ,
    
    #Data imputation choice
    checkboxInput(inputId ="imput", label ="Data Imputation",
                  value=FALSE),
    helpText("Perform Data Imputation before modelling. May take some time!"),
    
    radioButtons(inputId ="supervisedradio", label =strong("Methods"),
                 choices= c("Random Forests" = "rf",
                   "Trees" = "tree", "Support Vector Machines"="svm"),selected=""),
  
    #for fandom Forests
  conditionalPanel(
    condition = "input.supervisedradio == 'rf'"  ,
    selectInput(inputId='rf_y',
                label=strong('Response Variable'),
                choices=names(dataset)),
    
    radioButtons(
      inputId="radio",
      label=strong("Predictor variable Selection Type:"),
      choices=list(
        "All",
        "Manual Select"
      ),
      selected="Manual Select"),
    
    conditionalPanel(
      condition = "input.radio != 'All'",
      checkboxGroupInput(
        inputId ="rf_x", 
        label = strong("Choose Predictor variables"),
        choices=names(dataset) 
        
      )
    ),
   uiOutput("slider"),
    
    radioButtons(inputId='plot_rf_var',
                 label=strong('Choose Plot'),
                 choices=list(
                   "Random Forest Plot"="rfplot",
                   "Variance Importance Plot"="varimpplot"
                 ),
                 selected="rfplot"),
    
    actionButton(inputId ="goButtonrf", label =strong("Update Model", style = "color:blue"))
    ),
  
  #for trees
  conditionalPanel(
    condition = "input.supervisedradio == 'tree'"  ,
    selectInput(inputId='tree_y',
                label=strong('Response Variable'),
                choices=names(dataset)),
    
    radioButtons(
      inputId="radiotree",
      label=strong("Predictor variable Selection Type:"),
      choices=list(
        "All",
        "Manual Select"
      ),
      selected="Manual Select"),
    
    conditionalPanel(
      condition = "input.radiotree != 'All'",
      checkboxGroupInput(
        inputId ="tree_x", 
        label = strong("Choose Predictor variables"),
        choices=names(dataset) 
      )
    ),
    
   uiOutput("treeslider"),
    actionButton(inputId ="goButtontree", label =strong("Update Model", style = "color:blue"))
  ),
  
  #for svm
  conditionalPanel(
    condition = "input.supervisedradio == 'svm'"  ,
    selectInput(inputId='svm_y',
                label=strong('Response Variable'),
                choices=names(dataset)),
    
    radioButtons(
      inputId="radiosvm",
      label=strong("Predictor variable Selection Type:"),
      choices=list(
        "All",
        "Manual Select"
      ),
      selected="Manual Select"),
    
    conditionalPanel(
      condition = "input.radiosvm != 'All'",
      checkboxGroupInput(
        inputId ="svm_x", 
        label = strong("Choose Predictor variables"),
        choices=names(dataset) 
        )
    ),
    
    radioButtons(
      inputId="radiosvmkernel",
      label=strong("Choose SVM kernel:"),
      choices=list(
        "Linear",
        "Polynomial",
        "Radial basis",
        "Sigmoid"
      ),
      selected="Linear"),
    helpText("Scatter plot of the input data of a svm fit for classification models"),
    
    selectInput(inputId='svm_y_plot',
                label=strong('Y Variable for Plotting'),
                choices=names(dataset)),
    selectInput(inputId='svm_x_plot',
                label=strong('X Variable for Plotting'),
                choices=names(dataset)),
    
    actionButton(inputId ="goButtonsvm", label =strong("Update Model", style = "color:blue"))
  )

  ),
 
  hr(),

  checkboxInput(inputId ="unsupervised", label ="UnSupervised Methods",
                value = FALSE),
  
  conditionalPanel(  
    condition = "input.unsupervised == true" ,
    
    #Data imputation choice
    checkboxInput(inputId ="imput_un", label ="Data Imputation",
                  value=FALSE),
    helpText("Perform Data Imputation before modelling. May take some time!"),
    
    radioButtons(inputId ="unsupervisedradio", label =strong("Methods"),
                choices= c("PCA" = "pca",
                           "Kmeans" = "kmeans",
                           "Hierarchical Clustering" = "clust"),selected=""
    ),
    
    #for PCA
    conditionalPanel(
      condition = "input.unsupervisedradio == 'pca'"  ,
      radioButtons(inputId='plot_pca_cum',
                  label=strong('Choose Plot'),
                  choices=list(
                    "PCA Plot"="pcaplot",
                    "Cumulative PCA Plot"="cumpcaplot"
                  ),
                  selected="pcaplot"),
      
      actionButton(inputId ="goButtonpca", label =strong("Run PCA", style = "color:blue"))
    
    ),
    
    #for kmeans
    conditionalPanel(
      condition = "input.unsupervisedradio == 'kmeans'"  ,
      
      selectInput(inputId='kmeans_y',
                  label=strong('First Variable'),
                  choices=names(dataset)),
      selectInput(inputId='kmeans_x',
                  label=strong('Second Variable'),
                  choices=names(dataset)),
      
      numericInput('clusters', 'Cluster count', 3,
                   min = 1, max = 20),
      
      actionButton(inputId ="goButtonkmeans", label =strong("Run Kmeans", style = "color:blue"))
      
    ),
    
    #for clustering
    conditionalPanel(
      condition = "input.unsupervisedradio == 'clust'"  ,
      
      radioButtons(inputId='clust_method',
                   label=strong('Choose Clustering Method'),
                   choices=list(
                     "Complete"="complete",
                     "Ward"="ward",
                     "Single"="single",
                     "Average"="average",
                     "Median"="median",
                     "Centroid"="centroid"),
                   selected="complete"),
      
      actionButton(inputId ="goButtonclust", label =strong("Run Hierarchical Clustering", style = "color:blue"))
    )
  )
   ) ,
  
  tags$hr(),

##################################################################################
  checkboxInput(inputId ="text_mining", label ="Text Mining",
                value = FALSE),
  conditionalPanel(  
    condition = "input.text_mining == true" ,
    
    radioButtons(inputId='text_mining_method',
                 label=strong('Choose Text Mining Method'),
                 choices=list(
                   "SVM"="svm",
                   "MAXENT"="maxent",
                   "Random Forest"="rf",
                   "GLMNET"="glmnet",
                   "BAGGING"="bagging",
                   "BOOSTING"="boosting"),
                 selected=""),
    
    #svm text
    conditionalPanel( 
      condition = "input.text_mining_method == 'svm'"  ,
    
    selectInput(inputId='text_y_svm',
                label=strong('Choose Target Label'),
                choices=names(dataset)),
    
    radioButtons(
      inputId="radio_text_svm",
      label=strong("Predictor variable Selection Type:"),
      choices=list(
        "All",
        "Manual Select"
      ),
      selected="Manual Select"),
    
    conditionalPanel(
      condition = "input.radio_text_svm != 'All'",
      checkboxGroupInput(
        inputId ="text_x_svm", 
        label=strong('Choose Predictor'),
        choices=names(dataset))
    ),
    
    numericInput(inputId ="textnum_svm",label = "Size of Train set:", 100),
    #print analytics
    checkboxInput(inputId ="pr_analytics_svm", label =strong("Print Analytics"),
                  value=FALSE),
    
    actionButton(inputId ="goButtontext_corp_svm", label =strong("Run", style = "color:blue"))
    ),
    
    #maxent text
    conditionalPanel( 
      condition = "input.text_mining_method == 'maxent'"  ,
      
      selectInput(inputId='text_y_maxent',
                  label=strong('Choose Target Label'),
                  choices=names(dataset)),
      
      radioButtons(
        inputId="radio_text_maxent",
        label=strong("Predictor variable Selection Type:"),
        choices=list(
          "All",
          "Manual Select"
        ),
        selected="Manual Select"),
      
      conditionalPanel(
        condition = "input.radio_text_maxent != 'All'",
        checkboxGroupInput(
          inputId ="text_x_maxent", 
          label=strong('Choose Predictor'),
          choices=names(dataset))
      ),
      
      numericInput(inputId ="textnum_maxent",label = "Size of Train set:", 100),
      #print analytics
      checkboxInput(inputId ="pr_analytics_maxent", label =strong("Print Analytics"),
                    value=FALSE),
      
      #uiOutput("textslider"),
      actionButton(inputId ="goButtontext_corp_maxent", label =strong("Run", style = "color:blue"))
    ),
    
    #random forests text
    conditionalPanel( 
      condition = "input.text_mining_method == 'rf'"  ,
      
      selectInput(inputId='text_y_rf',
                  label=strong('Choose Target Label'),
                  choices=names(dataset)),
      
      radioButtons(
        inputId="radio_text_rf",
        label=strong("Predictor variable Selection Type:"),
        choices=list(
          "All",
          "Manual Select"
        ),
        selected="Manual Select"),
      
      conditionalPanel(
        condition = "input.radio_text_rf != 'All'",
        checkboxGroupInput(
          inputId ="text_x_rf", 
          label=strong('Choose Predictor'),
          choices=names(dataset))
      ),
      
      numericInput(inputId ="textnum_rf",label = "Size of Train set:", 100),
      #print analytics
      checkboxInput(inputId ="pr_analytics_rf", label =strong("Print Analytics"),
                    value=FALSE),
      
      #uiOutput("textslider"),
      actionButton(inputId ="goButtontext_corp_rf", label =strong("Run", style = "color:blue"))
    ),
    
    #glmnet text
    conditionalPanel( 
      condition = "input.text_mining_method == 'glmnet'"  ,
      
      selectInput(inputId='text_y_glmnet',
                  label=strong('Choose Target Label'),
                  choices=names(dataset)),
      
      radioButtons(
        inputId="radio_text_glmnet",
        label=strong("Predictor variable Selection Type:"),
        choices=list(
          "All",
          "Manual Select"
        ),
        selected="Manual Select"),
      
      conditionalPanel(
        condition = "input.radio_text_glmnet != 'All'",
        checkboxGroupInput(
          inputId ="text_x_glmnet", 
          label=strong('Choose Predictor'),
          choices=names(dataset))
      ),
      
      numericInput(inputId ="textnum_glmnet",label = "Size of Train set:", 100),
      #print analytics
      checkboxInput(inputId ="pr_analytics_glmnet", label =strong("Print Analytics"),
                    value=FALSE),
      
      actionButton(inputId ="goButtontext_corp_glmnet", label =strong("Run", style = "color:blue"))
    ),
    
    #bagging text
    conditionalPanel( 
      condition = "input.text_mining_method == 'bagging'"  ,
      
      selectInput(inputId='text_y_bagg',
                  label=strong('Choose Target Label'),
                  choices=names(dataset)),
      
      radioButtons(
        inputId="radio_text_bagg",
        label=strong("Predictor variable Selection Type:"),
        choices=list(
          "All",
          "Manual Select"
        ),
        selected="Manual Select"),
      
      conditionalPanel(
        condition = "input.radio_text_bagg != 'All'",
        checkboxGroupInput(
          inputId ="text_x_bagg", 
          label=strong('Choose Predictor'),
          choices=names(dataset))
      ),
      
      numericInput(inputId ="textnum_bagg",label = "Size of Train set:", 100),
      #print analytics
      checkboxInput(inputId ="pr_analytics_bagg", label =strong("Print Analytics"),
                    value=FALSE),
      
      actionButton(inputId ="goButtontext_corp_bagg", label =strong("Run", style = "color:blue"))
    ),
    
    #boosting text
    conditionalPanel( 
      condition = "input.text_mining_method == 'boosting'"  ,
      
      selectInput(inputId='text_y_boost',
                  label=strong('Choose Target Label'),
                  choices=names(dataset)),
      
      radioButtons(
        inputId="radio_text_boost",
        label=strong("Predictor variable Selection Type:"),
        choices=list(
          "All",
          "Manual Select"
        ),
        selected="Manual Select"),
      
      conditionalPanel(
        condition = "input.radio_text_boost != 'All'",
        checkboxGroupInput(
          inputId ="text_x_boost", 
          label=strong('Choose Predictor'),
          choices=names(dataset))
      ),
      
      numericInput(inputId ="textnum_boost",label = "Size of Train set:", 100),
      #print analytics
      checkboxInput(inputId ="pr_analytics_boost", label =strong("Print Analytics"),
                    value=FALSE),
      
      actionButton(inputId ="goButtontext_corp_boost", label =strong("Run", style = "color:blue"))
    ),
    
    radioButtons(inputId='text_plot_method',
                 label=strong('Choose Text Plotting'),
                 choices=list(
                   "Word Cloud"="wordcloud",
                   "Top Words"="topwords",
                   "Hierarchical Clustering"="text_hclust",
                   "K means"="text_kmeans",
                   "Neural Networks"="network"),
                 selected=""),
    actionButton(inputId ="goButtontext", label =strong("Run", style = "color:blue"))
  ) 
  ),
  
  mainPanel(tags$head(tags$style(type="text/css", ".tab-content {overflow: visible;}")),
            
            tabsetPanel(
              tabPanel("Data", tableOutput("contents")),
              tabPanel("Summary", verbatimTextOutput("summary")),
              tabPanel("EDA", plotOutput("Plot", width = "1000px", height = "800px")),
              tabPanel("Supervised Modelling",selectInput(inputId = "help_page",
                                                          label = strong("Choose Method"),
                                                          choices = list('None'='.',
                                                                         "Random Forest"="rfs",
                                                                         "Trees"="trees",
                                                                         "Support Vector Machine"="svms")),
                       verbatimTextOutput(outputId = "pred_s"),
                       plotOutput("s.Plot", width = "1000px", height = "800px")
                      ),
              tabPanel("UnSupervised Modelling",selectInput(inputId = "help_page_un",
                                                          label = strong("Choose Method"),
                                                          choices = list('None'='.',
                                                                         "PCA"="pca_un",
                                                                         "Kmeans"="kmeans_un",
                                                                         "Clustering"="clust_un")),
                       verbatimTextOutput(outputId = "pred_un"),
                       plotOutput("un.Plot", width = "1000px", height = "800px")),
              
              tabPanel("Text Mining",selectInput(inputId = "help_page_text",
                                                 label = strong("Choose Method"),
                                                 choices = list('None'='.',
                                                                "SVM"="svm_text",
                                                                "MAXENT"="maxent_text",
                                                                "Random Forests"="rf_text",
                                                                "GLMNET"="glmnet_text",
                                                                "BAGGING"="bagging_text",
                                                                "BOOSTING"="boosting_text")),
                       
                       verbatimTextOutput(outputId = "text_mn"),
                       plotOutput("text.Plot", width = "1000px", height = "1000px")),
              
              tabPanel("Help", selectInput(inputId = "general_page",
                                           label = strong("Search Help"),
                                           choices = list('None'='.',
                                                          "General Help"="gen_help",
                                                          "Plotting Help"="plot_help",
                                                          "Data Mining Help"="mod_help",
                                                          "Text Mining Help"="text_mod_help")),
                       verbatimTextOutput(outputId = "help_text"))
              
            )          
  )
  ))) 
    
