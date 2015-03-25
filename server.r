


library(randomForest)
library(ggplot2)
library(plyr)
library(MASS)
library(reshape2)
library(tree)
library(data.table)
library(e1071)
library(Amelia)
library(class)
library(tm)
library(SnowballC)
library(wordcloud)
library(cluster)
library(RTextTools)
library(igraph)


options(shiny.maxRequestSize=30*1024^2)
shinyServer(function(input, output,session) {
  
  mydata <- reactive({
    if (is.null(input$file1)) {
      return(NULL)
    }
    d <- read.csv(input$file1$datapath, header=input$header, sep=input$sep,stringsAsFactors = F)    # read the file
    return(d)     
    
  })   

  observe({
    df <- mydata()
    
    if (!is.null(df)) {
      updateSelectInput(session, 'attributes', choices = names(df))
      updateSelectInput(session, 'attributes.dens', choices = names(df))
      updateSelectInput(session, 'attributes.bar', choices = names(df))
      updateSelectInput(session, 'attributes.ident', choices = names(df))
      updateSelectInput(session, 'attributesx.box', choices =c(None='.', names(df)))
      updateSelectInput(session, 'attributesy.box', choices =c(None='.', names(df)))
      
      updateSelectInput(session, 'attributx', choices = names(df))
      updateSelectInput(session, 'attributy', choices = names(df))
      
      updateSelectInput(session, 'attrx', choices = names(df))
      updateSelectInput(session, 'attry', choices = names(df))
      
      updateSelectInput(session, 'sizepoint', choices = c(None='.', names(df)))
      
      updateSelectInput(session, 'fil', choices = c(None='.', names(df)))
      updateSelectInput(session, 'fil.dens', choices = c(None='.', names(df)))
      updateSelectInput(session, 'filpoint', choices = c(None='.', names(df)))
      updateSelectInput(session, 'filbar', choices = c(None='.', names(df)))
      updateSelectInput(session, 'filbox', choices = c(None='.', names(df)))
      
      updateSelectInput(session, 'facet_col', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_row', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_rowpoint', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_colpoint', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_col.dens', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_row.dens', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_col.bar', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_row.bar', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_col.box', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_row.box', choices = c(None='.', names(df)))
      updateCheckboxGroupInput(session, 'rf_x', choices = c(names(df)))
      updateSelectInput(session, 'rf_y', choices = c(None='.', names(df)))
      updateCheckboxGroupInput(session, 'tree_x', choices = c(names(df)))
      updateSelectInput(session, 'tree_y', choices = c(None='.', names(df)))
      updateCheckboxGroupInput(session, 'svm_x', choices = c(names(df)))
      updateSelectInput(session, 'svm_y', choices = c(None='.', names(df)))
      updateCheckboxGroupInput(session, 'clust_x', choices = c(names(df)))
      updateSelectInput(session, 'clust_y', choices = c(None='.', names(df)))
      updateSelectInput(session, 'knn_y', choices = c(None='.', names(df)))
      updateSelectInput(session, 'knn_x', choices = c(None='.', names(df)))
      updateSelectInput(session, 'kmeans_y', choices = c(None='.', names(df)))
      updateSelectInput(session, 'kmeans_x', choices = c(None='.', names(df)))
      updateRadioButtons(session, 'radiocl', choices = c(names(df)))
      updateSelectInput(session, 'text_y', choices = c(None='.', names(df)))
      updateSelectInput(session, 'text_x', choices = c(None='.', names(df)))
      updateSelectInput(session, 'text_y_svm', choices = c(None='.', names(df)))
      updateCheckboxGroupInput(session, 'text_x_svm', choices = c(None='.', names(df)))
      updateSelectInput(session, 'text_y_maxent', choices = c(None='.', names(df)))
      updateCheckboxGroupInput(session, 'text_x_maxent', choices = c(None='.', names(df)))
      updateSelectInput(session, 'text_y_rf', choices = c(None='.', names(df)))
      updateCheckboxGroupInput(session, 'text_x_rf', choices = c(None='.', names(df)))
      updateSelectInput(session, 'text_y_glmnet', choices = c(None='.', names(df)))
      updateCheckboxGroupInput(session, 'text_x_glmnet', choices = c(None='.', names(df)))
      updateSelectInput(session, 'text_y_bagg', choices = c(None='.', names(df)))
      updateCheckboxGroupInput(session, 'text_x_bagg', choices = c(None='.', names(df)))
      updateSelectInput(session, 'text_y_boost', choices = c(None='.', names(df)))
      updateCheckboxGroupInput(session, 'text_x_boost', choices = c(None='.', names(df)))
      updateRadioButtons(session, "radiosvmkernel", choices=list(
        "Linear",
        "Polynomial",
        "Radial basis",
        "Sigmoid"
      ))
      updateSelectInput(session, 'svm_x_plot', choices = c(None='.', names(df)))
      updateSelectInput(session, 'svm_y_plot', choices = c(None='.', names(df)))
    }
  })
  
#############################
  make_model <- function(model_type, formula,subset=NULL,...) {
    
    # In order to get the output to print the formula in a nice way, we'll
    # use do.call here with some quoting.
    
    do.call(model_type, args = list(formula = formula,data = quote(mydata()), ...))
  }
###############################
  #Imputation function
  #Factors are not used for imputation at the moment!
  stat.infill<-function(df.data){
    
    x<-sapply(df.data, class)
    x<-sapply(x, "[[", 1)
    x.f<-x=='factor' #find factors
    which.f<-which(x=='factor')
    which.f<-as.vector(which.f)
    d<-any(grep('POS',x))
    colClasses <- which(x=="POSIXct" )
    if(d)  df.data[,colClasses] <- (sapply(df.data[,colClasses], function(x) as.Date(x, format="%Y-%m-%d")))/365+1970 
    
    am.out <- amelia(df.data, m = 5,p2s = 0,idvars=which.f,
                     
                     #noms=which.f,
                     emppri=0.01*nrow(df.data),
                     parallel ="multicore",ncpus = parallel::detectCores())
    
    #various stop 
    if(any(grep('One or more of the imputations',am.out$message)) | any(grep('You have a variable in your dataset that does not vary', am.out$message)) | 
         any(grep('The time series and cross sectional variables cannot be transformed',am.out$message)) | any(grep('The square root transformation',am.out$message)) | 
         any(grep('One of the variable names in the options list',am.out$message)) | any(grep("The following variable", am.out$message))) stop()
    
    # extract numeric columns from each element of am.out$impuations  
    num <- llply(am.out$imputations, function(df.dat) df.dat[,sapply(df.dat, is.numeric)]) 
    
    # sum them up and divide by length to get mean
    mean.find = Reduce("+", num)/length(num)
    df.data <- cbind(df.data[x.f],mean.find)
    
    return(df.data)
    
  }
  
  output$slider <- renderUI({
    sliderInput("rf_train", "Size of Train set", min=1, max=nrow(mydata()), value=1)
  })
  output$treeslider <- renderUI({
    sliderInput("tree_train", "Size of Train set", min=1, max=nrow(mydata()), value=1)
  })
 
  datakmeans <- reactive({
    df <- mydata()
    df<-data.frame(df[, c(input$kmeans_x,input$kmeans_y)],stringsAsFactors = FALSE)
    sapply(na.omit(df), as.numeric)
  })
  
  clusters <- reactive({
    kmeans(datakmeans(), input$clusters)
  })
  
  datakmeans_imp <- reactive({
    df.mydata<-stat.infill(mydata())
    df.mydata<-data.frame(df.mydata[, c(input$kmeans_x,input$kmeans_y)],stringsAsFactors = FALSE)
    sapply(na.omit(df.mydata), as.numeric)
  })
  
  clusters_imp <- reactive({
    kmeans(datakmeans_imp(), input$clusters)
  })
  
  
  output$Plot <- renderPlot({
    if (is.null(mydata()))
      return(NULL) 
    input$goButton
    input$goButton1
    input$goButton2
    input$goButton3
    input$goButton4
    input$goButton5
    input$goButton6
    
    if ( input$plot == "none" ) {
      return(NULL)
    }
    
    #plotting parameters
    isolate({
      
      if ( input$plot == "hist" ) {
        
        if(input$fil!="." ){
          aes_mapping <- aes_string(x = input$attributes,group=input$fil,fill=input$fil) }
        
        else{
          
          aes_mapping <- aes_string(x =input$attributes)  }
        
        #plot
        p <- ggplot(mydata(),mapping=aes_mapping ) +
          geom_histogram(binwidth=input$bin)
        
        facets <- paste(input$facet_row, '~', input$facet_col)
        if (facets != '. ~ .')
          p <- p + facet_grid(facets,scales='free')  +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
        
      }
      
    }) 
    
    isolate({
      
      if ( input$plot == "box" ) {
        
        if(input$filbox!="." ){
          aes_mapping <- aes_string(x = input$attributesx.box,y = input$attributesy.box,group=input$filbox,fill=input$filbox) }
        
        else{
          
          aes_mapping <- aes_string(x =input$attributesx.box,y = input$attributesy.box)  }       
        
        #plot
        p <- ggplot(mydata(),mapping=aes_mapping ) +
          geom_boxplot()
        
        facets <- paste(input$facet_row.box, '~', input$facet_col.box)
        if (facets != '. ~ .')
          p <- p + facet_grid(facets,scales='free')  +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
        
      }
      
    })
    
    isolate({
      
      if ( input$plot == "bar" ) {
        
        if(input$filbar!="." ){
          if (input$identity == TRUE ) {
            aes_mapping <- aes_string(x = input$attributes.bar, y=input$attributes.ident ,group=input$filbar,fill=input$filbar)}
          else {
            aes_mapping <- aes_string(x = input$attributes.bar,group=input$filbar,fill=input$filbar) }
        }
        else if(input$identity == TRUE){
          aes_mapping <- aes_string(x = input$attributes.bar, y=input$attributes.ident)
        }
        else {
          
          aes_mapping <- aes_string(x =input$attributes.bar)  }
              
        #plot
        p <- ggplot(mydata(),mapping=aes_mapping )
        
        if (input$identity ==TRUE ){
          p<- p+geom_bar(stat='identity') }
        
        else{
          p<- p+ geom_bar()
        }
        
        facets <- paste(input$facet_row.bar, '~', input$facet_col.bar)
        if (facets != '. ~ .')
          p <- p + facet_grid(facets,scales='free')  +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
        
      }
      
    }) 

    isolate({
      
      if ( input$plot == "dens" ) {
        
        if(input$fil.dens!="." ){
          aes_mapping <- aes_string(x = input$attributes.dens,group=input$fil.dens,fill=input$fil.dens) } 
           
        else{
          
          aes_mapping <- aes_string(x =input$attributes.dens)  }
               
        #plot
        p <- ggplot(mydata(),mapping=aes_mapping ) +
          geom_density(alpha=.3)
        
        facets <- paste(input$facet_row.dens, '~', input$facet_col.dens)
        if (facets != '. ~ .')
          p <- p + facet_grid(facets,scales='free')  +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
        
      } 
    }) 
    
    isolate({
      
      if ( input$plot == "line" ) {
        
        if(input$filline!='.' ){
          if(input$sizeline!='.'){
            aes_mapping <- aes_string(x =input$attributx,y=input$attributy,color=input$filline,size=input$sizeline) }       
          else{
            aes_mapping <- aes_string(x =input$attributx,y=input$attributy,color=input$filline) }
        }
        
        else{
          aes_mapping <- aes_string(x =input$attributx,y=input$attributy)   }
        
        #plot
        p <- ggplot(mydata(),mapping=aes_mapping ) +
          geom_line(group=input$filline)              
        
        facets <- paste(input$facet_rowline, '~', input$facet_colline)
        if (facets != '. ~ .')
          p <- p + facet_grid(facets)  +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
        
      }
    })
    
    isolate({
      if ( input$plot == "point" ) {
        
        if(input$filpoint!='.' ){
          if(input$sizepoint!='.'){
            aes_mapping <- aes_string(x =input$attrx,y=input$attry,color=input$filpoint,size=input$sizepoint) }     
          else{
            aes_mapping <- aes_string(x =input$attrx,y=input$attry,color=input$filpoint) }
        }
        
        else{
          aes_mapping <- aes_string(x =input$attrx,y=input$attry)  }
        
        #plot
        p <- ggplot(mydata(),mapping=aes_mapping ) +
          geom_point()
        
        facets <- paste(input$facet_rowpoint, '~', input$facet_colpoint)
        if (facets != '. ~ .')
          p <- p + facet_grid(facets)  +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
        
      }   
    })
    
    if ( input$plot == "heat" ) {
      
      d<-as.data.frame(sapply(mydata(),as.numeric))
      cc<-cor(d,use="pairwise.complete.obs")
      z<-melt(cc)
      
      p<-ggplot(z, aes(Var1, Var2, fill = value)) + geom_tile() + theme(axis.text.x  = element_text(angle=90,hjust=0.9,vjust=0.5))+
             scale_fill_gradient2(low = "blue",  high = "red")+xlab('') +ylab('')+labs(fill='Correlation')    
    }   
    
    if ( input$plot == "pair" ) {
      
      d<-as.data.frame(sapply(mydata(),as.numeric))
      
      panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
      {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r = abs((cor(x, y,use="pairwise.complete.obs")))
        txt <- format(c(r, 0.123456789), digits=digits)[1]
        txt <- paste(prefix, txt, sep="")
        if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
        text(0.5, 0.5, txt, cex = cex * abs(r))
      }
      panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 18, 
                              cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) 
      {
        points(x, y, pch = pch, col = col, bg = bg, cex = cex)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok)) 
          lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
                col = col.smooth, ...)
      }
      panel.hist <- function(x, ...)
      {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks; nB <- length(breaks)
        y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
      }
      
      p<-pairs(d, lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist) 
    }  
    
    print(p)
  })
  

######################################## 
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    summary(mydata())
  })
  
  # Show the first "n" observations
  output$contents <- renderTable({
    head(mydata(), n = input$obs)
  }) 
################################
#print random forest, tree, svm models
  output$pred_s <- renderPrint({ 
    if(input$imput==TRUE){

      df.mydata<-stat.infill(mydata())

      if(input$help_page=='rfs'){
        make_model <- function(model_type, formula,subset=NULL,...) {
          do.call(model_type, args = list(formula = formula,data = quote(df.mydata),subset=train, ...))
        }
        
        input$goButtonrf
        if(input$goButtonrf > 0){
          isolate({
            
            train <- sample(input$rf_train)
            df.rf<-as.data.table(df.mydata)
            rf.test=df.rf[,input$rf_y][-train]
            
            if(input$radio == "All"){    
              formula <- as.formula(paste(input$rf_y, "~."))
            } 
            else{
              formula <- as.formula(paste(input$rf_y ,"~",paste(input$rf_x, collapse= "+")))            
            }
            rf_mod<-make_model("randomForest", formula,na.action=na.omit,importance =TRUE)
            
            yhat.rf <- predict(make_model("randomForest", formula,na.action=na.omit,importance =TRUE),newdata =df.rf[-train,])
            
            rf_imp<-importance(make_model("randomForest",formula,na.action=na.omit,importance =TRUE))
            print(list(rf_mod,rf_imp),row.names = FALSE )  
          })     
        } 
      }
      
      if(input$help_page=='trees'){
        make_model <- function(model_type, formula,subset=NULL,...) {
          do.call(model_type, args = list(formula = formula,data = quote(df.mydata),subset=train_tr, ...))
        }
        
        input$goButtontree
        if(input$goButtontree > 0){ 
          isolate({
            
            train_tr <- sample(input$tree_train)
            df_tree<-as.data.table(df.mydata)
            tree_test=df_tree[-train_tr,]
            test_set=df.mydata[,input$tree_y][-train_tr]
            
            if(input$radiotree == "All"){
              formula <- as.formula(paste(input$tree_y, "~.")) 
            } 
            else{  
              formula <- as.formula(paste(input$tree_y ,"~",paste(input$tree_x, collapse= "+")))
            }
            
            if(!is.numeric(df.mydata[,input$tree_y])){
              tree_predict <- predict(make_model("tree", formula),tree_test,type='class')
              print(list(summary(make_model("tree", formula,subset=train_tr)),table(tree_predict,test_set)))
            }
            else{
              print(summary(make_model("tree", formula)))
            }      
          })     
        } 
        else{
          return(cat('Must choose one or more predictor variables and hit Update Model \n'))
        }   
      }
      
      if(input$help_page=='svms'){
        
        input$goButtonsvm
        if(input$goButtonsvm > 0){
          isolate({
            
            if(input$radiosvm== "All"){    
              formula <- as.formula(paste(input$svm_y, "~.")) 
            } 
            else{
              formula <- as.formula(paste(input$svm_y ,"~ ",paste(input$svm_x, collapse= "+")))
            }
            if(input$radiosvmkernel== "Linear"){
              list( summary(make_model("svm", formula,kernel ="linear", cost =10,
                                       scale =FALSE)),
                    Predictions=predict(make_model("svm", formula,kernel ="linear", cost =10,
                                                   scale =FALSE),newdata=mydata()))}
            else if(input$radiosvmkernel== "Polynomial"){
              list( summary(make_model("svm", formula,kernel ="polynomial", cost =10,
                                       scale =FALSE)),
                    Predictions=predict(make_model("svm", formula,kernel ="polynomial", cost =10,
                                                   scale =FALSE),newdata=mydata()))}
            else if(input$radiosvmkernel== "Radial basis"){
              list(summary(make_model("svm", formula,kernel ="radial", cost =10,
                                      scale =FALSE)),
                   Predictions=predict(make_model("svm", formula,kernel ="radial", cost =10,
                                                  scale =FALSE),newdata=mydata()))}
            else if(input$radiosvmkernel== "Sigmoid"){
              list( summary(make_model("svm", formula,kernel ="sigmoid", cost =10,
                                       scale =FALSE)),
                    Predictions=predict(make_model("svm", formula,kernel ="sigmoid", cost =10,
                                                   scale =FALSE),newdata=mydata()))}      
          })      
        } 
        else{
          return(cat('Must choose one or more predictor variables and hit Update Model \n'))
        }   
      } 
      
    }
    else{
    
    if(input$help_page=='rfs'){
      make_model <- function(model_type, formula,subset=NULL,...) {
        do.call(model_type, args = list(formula = formula,data = quote(mydata()),subset=train,...))
      }
    
    input$goButtonrf
    if(input$goButtonrf > 0){
      isolate({
        
        train <- sample(input$rf_train)
        df.rf<-as.data.table(mydata())
        rf.test=df.rf[,input$rf_y][-train]
        
        if(input$radio == "All"){    
            formula <- as.formula(paste(input$rf_y, "~."))
        } 
        else{
              formula <- as.formula(paste(input$rf_y ,"~",paste(input$rf_x, collapse= "+")))            
          }
        rf_mod<-make_model("randomForest", formula,na.action=na.omit,importance =TRUE)
        
        yhat.rf <- predict(make_model("randomForest", formula,na.action=na.omit,importance =TRUE),newdata =df.rf[-train,])
        
        rf_imp<-importance(make_model("randomForest",formula,na.action=na.omit,importance =TRUE))
        print(list(rf_mod,rf_imp),row.names = FALSE )      
      })     
    } 
    else{
      return(cat('Must choose one or more predictor variables and hit Update Model \n'))
    }
    }
    
    if(input$help_page=='trees'){
      make_model <- function(model_type, formula,subset=NULL,...) {
        do.call(model_type, args = list(formula = formula,data = quote(mydata()),subset=train_tr,...))
      }
    
      input$goButtontree
      if(input$goButtontree > 0){ 
        isolate({
          
          train_tr <- sample(input$tree_train)
          df_tree<-as.data.table(mydata())
          tree_test=df_tree[-train_tr,]
          test_set=mydata()[,input$tree_y][-train_tr]
          
          if(input$radiotree == "All"){
            formula <- as.formula(paste(input$tree_y, "~.")) 
          } 
          else{  
            formula <- as.formula(paste(input$tree_y ,"~",paste(input$tree_x, collapse= "+")))
          }
          
         if(!is.numeric(mydata()[,input$tree_y])){
             tree_predict <- predict(make_model("tree", formula),tree_test,type='class')
             print(list(summary(make_model("tree", formula,subset=train_tr)),table(tree_predict,test_set)))
           }
           else{
            print(summary(make_model("tree", formula)))
          }      
        })     
      } 
      else{
        return(cat('Must choose one or more predictor variables and hit Update Model \n'))
      }   
    }
    
    if(input$help_page=='svms'){
      
      input$goButtonsvm
      if(input$goButtonsvm > 0){
        isolate({
          
          if(input$radiosvm== "All"){    
            formula <- as.formula(paste(input$svm_y, "~.")) 
          } 
          else{
            formula <- as.formula(paste(input$svm_y ,"~ ",paste(input$svm_x, collapse= "+")))
          }
          if(input$radiosvmkernel== "Linear"){
           list( summary(make_model("svm", formula,kernel ="linear", cost =10,
                               scale =FALSE)),
          Predictions=predict(make_model("svm", formula,kernel ="linear", cost =10,
                             scale =FALSE),newdata=mydata()))}
          else if(input$radiosvmkernel== "Polynomial"){
           list( summary(make_model("svm", formula,kernel ="polynomial", cost =10,
                               scale =FALSE)),
            Predictions=predict(make_model("svm", formula,kernel ="polynomial", cost =10,
                                           scale =FALSE),newdata=mydata()))}
          else if(input$radiosvmkernel== "Radial basis"){
            list(summary(make_model("svm", formula,kernel ="radial", cost =10,
                               scale =FALSE)),
            Predictions=predict(make_model("svm", formula,kernel ="radial", cost =10,
                                           scale =FALSE),newdata=mydata()))}
          else if(input$radiosvmkernel== "Sigmoid"){
           list( summary(make_model("svm", formula,kernel ="sigmoid", cost =10,
                               scale =FALSE)),
            Predictions=predict(make_model("svm", formula,kernel ="sigmoid", cost =10,
                                           scale =FALSE),newdata=mydata()))}       
        })      
      } 
      else{
        return(cat('Must choose one or more predictor variables and hit Update Model \n'))
      }   
    } 
  }
  })
#################################  
#plot random Forest,trees,svm
output$s.Plot <- renderPlot({ 
  
  if(input$imput==TRUE){
    
    df.mydata<-stat.infill(mydata())
    
    make_model <- function(model_type, formula,subset=NULL,...) {
      do.call(model_type, args = list(formula = formula,data = quote(df.mydata), ...))
    }
  
  if(input$help_page=='rfs'){
    
  input$goButtonrf
  if(input$goButtonrf > 0){
  isolate({
    
  # plot random forest model
    
    if(input$radio == "All"){ 
      formula <- as.formula(paste(input$rf_y, "~."))
    } 
    else{ 
      formula <- as.formula(paste(input$rf_y ,"~ ",paste(input$rf_x, collapse= "+")))
    }
    
    if(input$plot_rf_var=='rfplot'){
    plot(make_model("randomForest", formula,subset=train,importance =TRUE),main="")
    }
    else if(input$plot_rf_var=='varimpplot'){
      #plot importance measures
      varImpPlot(make_model("randomForest", formula,subset=train,importance =TRUE),main="Dotchart of variable importance as measured by a Random Forest")
    }
  })
 }
}

   if(input$help_page=='trees'){
  
      input$goButtontree
      if(input$goButtontree > 0){
      isolate({
      
      # plot tree model
      
      if(input$radiotree == "All"){
        formula <- as.formula(paste(input$tree_y, "~."))
      } 
      else{
        formula <- as.formula(paste(input$tree_y ,"~ ",paste(input$tree_x, collapse= "+")))
      }
      plot(make_model("tree", formula))
      text(make_model("tree", formula))
    })
  }
}

   if(input$help_page=='svms'){
  
    input$goButtonsvm
    if(input$goButtonsvm > 0){
    isolate({
      
      # plot svm model
      
      if(input$radiosvm == "All"){
        formula <- as.formula(paste(input$svm_y, "~."))
      } 
      else{ 
        formula <- as.formula(paste(input$svm_y ,"~ ",paste(input$svm_x, collapse= "+")))
      }
      formula_plot <- as.formula(paste(input$svm_y_plot ,"~ ",paste(input$svm_x_plot, collapse= "+")))
      
      if(input$radiosvmkernel== "Linear"){
        plot(make_model("svm", formula,kernel ="linear", cost =10,
                        scale =FALSE),data=df.mydata,formula=formula_plot)}
      else if(input$radiosvmkernel== "Polynomial"){
        plot(make_model("svm", formula,kernel ="polynomial", cost =10,
                        scale =FALSE),data=df.mydata,formula=formula_plot)}
      else if(input$radiosvmkernel== "Radial basis"){
        plot(make_model("svm", formula,kernel ="radial", cost =10,
                        scale =FALSE),data=df.mydata,formula=formula_plot)}
      else if(input$radiosvmkernel== "Sigmoid"){
        plot(make_model("svm", formula,kernel ="sigmoid", cost =10,
                        scale =FALSE),data=df.mydata,formula=formula_plot)}   
    })
  }
}
}

else{
  
  if(input$help_page=='rfs'){
    
    input$goButtonrf
    if(input$goButtonrf > 0){
      isolate({
        
        # plot random forest model
        
        if(input$radio == "All"){ 
          formula <- as.formula(paste(input$rf_y, "~."))
        } 
        else{ 
          formula <- as.formula(paste(input$rf_y ,"~ ",paste(input$rf_x, collapse= "+")))
        }
        
        if(input$plot_rf_var=='rfplot'){
          plot(make_model("randomForest", formula,subset=train,na.action=na.omit,importance =TRUE),main="")
        }
        else if(input$plot_rf_var=='varimpplot'){
          #plot importance measures
          varImpPlot(make_model("randomForest", formula,subset=train,na.action=na.omit,importance =TRUE),main="Dotchart of variable importance as measured by a Random Forest")
        }
      })
    }
  }


    if(input$help_page=='trees'){
  
      input$goButtontree
      if(input$goButtontree > 0){
      isolate({
      
      # plot tree model
      
      if(input$radiotree == "All"){
        formula <- as.formula(paste(input$tree_y, "~."))
      } 
      else{
        formula <- as.formula(paste(input$tree_y ,"~ ",paste(input$tree_x, collapse= "+")))
      }
      plot(make_model("tree", formula,subset=train_tr))
      text(make_model("tree", formula,subset=train_tr))
    })
  }
}

  if(input$help_page=='svms'){
  
    input$goButtonsvm
    if(input$goButtonsvm > 0){
    isolate({
      
      # plot svm model
      
      if(input$radiosvm == "All"){
        
        formula <- as.formula(paste(input$svm_y, "~."))
      } 
      else{
        
        formula <- as.formula(paste(input$svm_y ,"~ ",paste(input$svm_x, collapse= "+")))
      }
      formula_plot <- as.formula(paste(input$svm_y_plot ,"~ ",paste(input$svm_x_plot, collapse= "+")))
      
      if(input$radiosvmkernel== "Linear"){
        plot(make_model("svm", formula,kernel ="linear", cost =10,
                      scale =FALSE),data=mydata(),formula=formula_plot)}
      else if(input$radiosvmkernel== "Polynomial"){
        plot(make_model("svm", formula,kernel ="polynomial", cost =10,
                      scale =FALSE),data=mydata(),formula=formula_plot)}
      else if(input$radiosvmkernel== "Radial basis"){
        plot(make_model("svm", formula,kernel ="radial", cost =10,
                      scale =FALSE),data=mydata(),formula=formula_plot)}
      else if(input$radiosvmkernel== "Sigmoid"){
        plot(make_model("svm", formula,kernel ="sigmoid", cost =10,
                      scale =FALSE),data=mydata(),formula=formula_plot)} 
    })
  }
}
}
})
###############################
#PCA,Kmeans, Hierarchical Clustering
output$pred_un <- renderPrint({ 
  
  if(input$imput_un==TRUE){
    
    df.mydata<-stat.infill(mydata())
    
    make_model <- function(model_type, formula,subset=NULL,...) {
      do.call(model_type, args = list(formula = formula,data = quote(df.mydata), ...))
    }
    df<-sapply(df.mydata,as.numeric)
    
    #PCA
    if(input$help_page_un=='pca_un'){
      
      input$goButtonpca
      if(input$goButtonpca > 0){
        isolate({
          
          pcaout<-prcomp(df,scale=TRUE)
          pr.var<-pcaout$sdev ^2
          pve<-pr.var/sum(pr.var)
          
          print(list(pcaout,Proportion_variance_explained=pve),row.names = FALSE)      
        })     
      }
      else{
        return(cat('Select from Side Panel also \n'))
      }   
    }
    
    #Kmeans
    if(input$help_page_un=='kmeans_un'){
      
      input$goButtonkmeans
      if(input$goButtonkmeans > 0){
        isolate({
          
          print(list("Cluster centres"=clusters_imp()$centers,"Total Sum of Squares"=clusters_imp()$totss,
                     "Within-Cluster Sum of Squares"=clusters_imp()$withinss,
                     "Between-Cluster Sum of Squares"=clusters_imp()$betweenss)) 
        })     
      }
      else{
        return(cat('Select from Side Panel also \n'))
      }   
    }
    
    #clustering
    if(input$help_page_un=='clust_un'){
      
      input$goButtonclust
      if(input$goButtonclust > 0){ 
        isolate({
          
          if(input$clust_method=='complete'){
            clust_mod<-hclust(dist(scale(df)), method ="complete")
          }
          if(input$clust_method=='ward'){
            clust_mod<-hclust(dist(scale(df)), method ="ward.D2")
          }
          if(input$clust_method=='single'){
            clust_mod<-hclust(dist(scale(df)), method ="single")
          }
          if(input$clust_method=='average'){
            clust_mod<-hclust(dist(scale(df)), method ="average")
          }
          if(input$clust_method=='median'){
            clust_mod<-hclust(dist(scale(df)), method ="median")
          }
          if(input$clust_method=='centroid'){
            clust_mod<-hclust(dist(scale(df)), method ="centroid")
          }
        print(clust_mod)     
        })     
      } 
      else{
        return(cat('Select from Side Panel also \n'))
      }   
    }
  }
  else{
    df<-sapply(mydata(),as.numeric)
    
  #PCA
  if(input$help_page_un=='pca_un'){
    
    input$goButtonpca
    if(input$goButtonpca > 0){
      isolate({

        pcaout<-prcomp(na.omit(df),scale=TRUE)
        pr.var<-pcaout$sdev ^2
        pve<-pr.var/sum(pr.var)
        
       print(list(pcaout,Proportion_variance_explained=pve),row.names = FALSE)      
      })     
    }
    else{
      return(cat('Select from Side Panel also \n'))
    }   
  }
  
  #Kmeans
  if(input$help_page_un=='kmeans_un'){
    
    input$goButtonkmeans
    if(input$goButtonkmeans > 0){
      isolate({
        
         print(list("Cluster centres"=clusters()$centers,"Total Sum of Squares"=clusters()$totss,
                    "Within-Cluster Sum of Squares"=clusters()$withinss,
                    "Between-Cluster Sum of Squares"=clusters()$betweenss))  
      })     
    }
    else{
      return(cat('Select from Side Panel also \n'))
    }   
  }
  
  #clustering
  if(input$help_page_un=='clust_un'){
    
    input$goButtonclust
    if(input$goButtonclust > 0){ 
      isolate({
        
        if(input$clust_method=='complete'){
          clust_mod<-hclust(dist(scale(df)), method ="complete")
        }
        if(input$clust_method=='ward'){
          clust_mod<-hclust(dist(scale(df)), method ="ward.D2")
        }
        if(input$clust_method=='single'){
          clust_mod<-hclust(dist(scale(df)), method ="single")
        }
        if(input$clust_method=='average'){
          clust_mod<-hclust(dist(scale(df)), method ="average")
        }
        if(input$clust_method=='median'){
          clust_mod<-hclust(dist(scale(df)), method ="median")
        }
        if(input$clust_method=='centroid'){
          clust_mod<-hclust(dist(scale(df)), method ="centroid")
        }
        print(clust_mod)
  
      })     
    } 
    else{
      return(cat('Select from Side Panel also \n'))
    }   
  }
  
  }
})
######################################
#Plotting pca,clustering
output$un.Plot <- renderPlot({ 
  
  if(input$imput_un==TRUE){
    
    df.mydata<-stat.infill(mydata())
    
    make_model <- function(model_type, formula,subset=NULL,...) {
      do.call(model_type, args = list(formula = formula,data = quote(df.mydata), ...))
    }
    
    #PCA
    if(input$help_page_un=='pca_un' ){ 
      
      input$goButtonpca
      if(input$goButtonpca > 0){
        isolate({
          
          df<-sapply(df.mydata,as.numeric)
          pcaout<-prcomp(df,scale=TRUE)
          pr.var<-pcaout$sdev ^2
          pve<-pr.var/sum(pr.var)
          
          if(input$plot_pca_cum=='pcaplot'){
            plot(pve , xlab="Principal Component", ylab="Proportion of
                 Variance Explained",type='b')
          }
          else if(input$plot_pca_cum=='cumpcaplot'){
            plot(cumsum (pve), xlab="Principal Component", ylab ="Cumulative Proportion of Variance Explained",
                 type='b')
          }
          })
    }
    else{
      return(cat('Select from Side Panel also \n'))
    }   
    }
    
    #Kmeans
    if(input$help_page_un=='kmeans_un' ){ 
      
      input$goButtonkmeans
      if(input$goButtonkmeans > 0){
        isolate({
          
          plot(datakmeans_imp(),
               col = clusters_imp()$cluster,
               pch = 20, cex = 2)
          points(clusters_imp()$centers, pch = 4, cex = 2, lwd = 4)
        })
      }
      else{
        return(cat('Select from Side Panel also \n'))
      }   
    }
    
    #Hierarchical Clustering
    if(input$help_page_un=='clust_un' ){ 
      
      input$goButtonclust
      if(input$goButtonclust > 0){
        isolate({
          
          if(input$clust_method=='complete'){
            clust_mod<-hclust(dist(scale(df)), method ="complete")
          }
          if(input$clust_method=='ward'){
            clust_mod<-hclust(dist(scale(df)), method ="ward.D2")
          }
          if(input$clust_method=='single'){
            clust_mod<-hclust(dist(scale(df)), method ="single")
          }
          if(input$clust_method=='average'){
            clust_mod<-hclust(dist(scale(df)), method ="average")
          }
          if(input$clust_method=='median'){
            clust_mod<-hclust(dist(scale(df)), method ="median")
          }
          if(input$clust_method=='centroid'){
            clust_mod<-hclust(dist(scale(df)), method ="centroid")
          }
          
          plot(clust_mod, main ="Hierarchical Clustering with Scaled Features")
        })
      }
      else{
        return(cat('Select from Side Panel also \n'))
      }   
    }  
  }
  else{
    df<-sapply(mydata(),as.numeric)
  #PCA
  if(input$help_page_un=='pca_un' ){ 
    
    input$goButtonpca
    if(input$goButtonpca > 0){
      isolate({
 
    pcaout<-prcomp(na.omit(df),scale=TRUE)
    pr.var<-pcaout$sdev ^2
    pve<-pr.var/sum(pr.var)
    
    if(input$plot_pca_cum=='pcaplot'){
      plot(pve , xlab="Principal Component", ylab="Proportion of
Variance Explained",type='b')
    }
  else if(input$plot_pca_cum=='cumpcaplot'){
     plot(cumsum (pve), xlab="Principal Component", ylab ="Cumulative Proportion of Variance Explained",
         type='b')
  }
})
}
  else{
    return(cat('Select from Side Panel also \n'))
  }   
}

#Kmeans
if(input$help_page_un=='kmeans_un' ){ 
  
  input$goButtonkmeans
  if(input$goButtonkmeans > 0){
    isolate({
      
      plot(datakmeans(),
           col = clusters()$cluster,
           pch = 20, cex = 2)
      points(clusters()$centers, pch = 4, cex = 2, lwd = 4)
      
    })
  }
  else{
    return(cat('Select from Side Panel also \n'))
  }   
}

#Hierarchical Clustering
if(input$help_page_un=='clust_un' ){ 
  
  input$goButtonclust
  if(input$goButtonclust > 0){
    isolate({
      
      if(input$clust_method=='complete'){
        clust_mod<-hclust(dist(scale(df)), method ="complete")
      }
      if(input$clust_method=='ward'){
        clust_mod<-hclust(dist(scale(df)), method ="ward.D2")
      }
      if(input$clust_method=='single'){
        clust_mod<-hclust(dist(scale(df)), method ="single")
      }
      if(input$clust_method=='average'){
        clust_mod<-hclust(dist(scale(df)), method ="average")
      }
      if(input$clust_method=='median'){
        clust_mod<-hclust(dist(scale(df)), method ="median")
      }
      if(input$clust_method=='centroid'){
        clust_mod<-hclust(dist(scale(df)), method ="centroid")
      }
      plot(clust_mod, main ="Hierarchical Clustering with Scaled Features")
    })
  }
  else{
    return(cat('Select from Side Panel also \n'))
  }   
}
}  
})
#############################################
cloud<-function(data){
  tm.cloud <-tm_map(data, removePunctuation)
  tm.cloud <-tm_map(tm.cloud,stripWhitespace)
  tm.cloud <- tm_map(tm.cloud, removePunctuation)
  tm.cloud <- tm_map(tm.cloud, function(x)removeWords(x,stopwords("SMART")))
  tm.cloud<-tm_map(tm.cloud,stemDocument, language = "english")
  tdm <- TermDocumentMatrix(tm.cloud)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  ##### with colors #####
  if(require(RColorBrewer)){
    pal <- brewer.pal(6,"Dark2")
    pal <- pal[-(1)]
    wordcloud(d$word,d$freq,scale=c(5,.5),min.freq=3,max.words=100, random.order=T, rot.per=.15, colors=pal)
  }
}

#function to produce DTM after all cleaning and preprocessing
dtm.corp<-function(corpus){
  
  corp.tm<-tm_map(corpus, content_transformer(tolower))
  corp.tm<-tm_map(corpus, removePunctuation)
  corp.tm<-tm_map(corp.tm,stripWhitespace)
  corp.tm<-tm_map(corp.tm,removeWords, stopwords("en"))
  corp.tm<-tm_map(corp.tm,stemDocument, language = "english")
  dtm  <- TermDocumentMatrix(corp.tm)
  dtm  <- removeSparseTerms(dtm, 0.95)
  
  return(dtm)
}
##############################################
output$text_mn <- renderPrint({ 

 #svm modelling
if(input$help_page_text=="svm_text"){
  
  input$goButtontext_corp_svm
  if(input$goButtontext_corp_svm > 0){
    isolate({
      
      if(input$radio_text_svm == "All"){  
        textColumns<-c()
        for(i in 1:ncol(mydata())){
          textColumns <- cbind(textColumns,mydata()[,i])
        }
      } 
      else{
        textColumns <- cbind(mydata()[input$text_x_svm],mydata()[input$text_x_svm]) 
      }
      
  #CREATE A TERM-DOCUMENT MATRIX THAT REPRESENTS WORD FREQUENCIES IN EACH DOCUMENT
  text_matrix_svm<-create_matrix(textColumns,language="english", stemWords=TRUE, removePunctuation=TRUE, weighting= weightTf,removeNumbers=TRUE) #
  
  #creat train and test vectors
  train_text_svm <- 1:(input$textnum_svm)
  test_text_svm<- (length(train_text_svm)+1):nrow(mydata())
  
  #creates a container for training, classifying, and analyzing documents.
  contain_svm<-create_container(text_matrix_svm,mydata()[,input$text_y_svm],trainSize=train_text_svm,testSize=test_text_svm,virgin=FALSE) 
   
 #modelling
 model_svm<-train_model(contain_svm,"SVM")
 
 #get the predictions
 results_svm<-classify_model(contain_svm, model_svm)
 
 
 labels_out_svm <- data.frame(
   CORRECT_LABEL = mydata()[,input$text_y_svm][test_text_svm],
   results_svm, 
   stringsAsFactors = F)  

 
 #print analytics on request
 if(input$pr_analytics_svm==TRUE){
   
   #container for analytics
   contain_svm2<-create_container(text_matrix_svm,as.numeric(factor(mydata()[,input$text_y_svm])),trainSize=train_text_svm,testSize=test_text_svm,virgin=FALSE) #
   
   model_svm2<-train_model(contain_svm2,"SVM")
   results_svm2<-classify_model(contain_svm2, model_svm2)
   analytics<-create_analytics(contain_svm2,cbind(results_svm2))
   
   print(list(summary(analytics), analytics@algorithm_summary,
               #RAW SUMMARY OF ALL DATA AND SCORING
               cbind(analytics@document_summary,Target_Label=mydata()[,input$text_y_svm][test_text_svm]),
               #SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
              cbind(analytics@label_summary,Target_LABEL= unique(mydata()[,input$text_y_svm])),
              # Confusion Matrices -- look for possible problems
              #table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$SVM_LABEL),
              # CHECK OVERALL ACCURACY OF ALGORITHMS
              recall_accuracy(analytics@document_summary$MANUAL_CODE, analytics@document_summary$SVM_LABEL)),row.names = FALSE) 
 }
 else{
   print(list(labels_out_svm,Proportions_Table=prop.table(table(labels_out_svm[,1] == labels_out_svm[,2])),
              Cross_Validated_Algorithm=cross_validate(contain_svm,2,algorithm="SVM")),row.names = FALSE)
 }
 
 })
  }
 else{
   return(cat('Select from Side Panel also \n'))
 }  
}
#################################################
#maxent modelling
if(input$help_page_text=="maxent_text"){
  
  input$goButtontext_corp_maxent
  if(input$goButtontext_corp_maxent > 0){
    isolate({
      
      if(input$radio_text_maxent == "All"){  
        textColumns<-c()
        for(i in 1:ncol(mydata())){
          textColumns <- cbind(textColumns,mydata()[,i])
        }
      } 
      else{
        textColumns <- cbind(mydata()[input$text_x_maxent],mydata()[input$text_x_maxent]) 
      }
      
  #CREATE A TERM-DOCUMENT MATRIX THAT REPRESENTS WORD FREQUENCIES IN EACH DOCUMENT
  text_matrix_maxent<-create_matrix(textColumns,
                            language="english", stemWords=TRUE, removePunctuation=TRUE, weighting= weightTf,removeNumbers=TRUE)
      
  #creat train and test vectors
  train_text_maxent <- 1:input$textnum_maxent
  test_text_maxent=(length(train_text_maxent)+1):nrow(mydata())
      
  #creates a container for training, classifying, and analyzing documents.
  contain_maxent<-create_container(text_matrix_maxent,mydata()[,input$text_y_maxent],trainSize=train_text_maxent,testSize=test_text_maxent,virgin=FALSE) 
      
  #modelling
  model_maxent<-train_model(contain_maxent,"MAXENT")
  #get the predictions
  results_maxent<-classify_model(contain_maxent, model_maxent)
  
  labels_out_maxent <- data.frame(
    CORRECT_LABEL = mydata()[,input$text_y_maxent][test_text_maxent],
    results_maxent,
    stringsAsFactors = F)
  
  #print analytics on request
  if(input$pr_analytics_maxent==TRUE){
    #container for analytics
    contain_maxent2<-create_container(text_matrix_maxent,as.numeric(factor(mydata()[,input$text_y_maxent])),trainSize=train_text_maxent,testSize=test_text_maxent,virgin=FALSE)
    
    model_maxent2<-train_model(contain_maxent2,"MAXENT")
    results_maxent2<-classify_model(contain_maxent2, model_maxent2)
    analytics<-create_analytics(contain_maxent2,cbind(results_maxent2))
    
    print(list(summary(analytics), analytics@algorithm_summary,
                #RAW SUMMARY OF ALL DATA AND SCORING
                cbind(analytics@document_summary,Target_Label=mydata()[,input$text_y_maxent][test_text_maxent]),
                #SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
                cbind(analytics@label_summary,Target_LABEL= unique(mydata()[,input$text_y_maxent])),
                # Confusion Matrices -- look for possible problems
               # table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$MAXENT_LABEL),
                # CHECK OVERALL ACCURACY OF ALGORITHMS
                recall_accuracy(analytics@document_summary$MANUAL_CODE, analytics@document_summary$MAXENT_LABEL)),row.names = FALSE) 
  }
  else{    
   
    capture.output(print(list(labels_out_maxent,Proportions_Table=prop.table(table(labels_out_maxent[,1] == labels_out_maxent[,2])),
                Cross_Validated_Algorithm=cross_validate(contain_maxent,2,algorithm="MAXENT"))))  
   
  }
    })
  }
  else{
    return(cat('Select from Side Panel also \n'))
  }
}

###############################################
#random forest modelling
if(input$help_page_text=="rf_text"){
  
  input$goButtontext_corp_rf
  if(input$goButtontext_corp_rf > 0){
    isolate({
      
      if(input$radio_text_rf == "All"){  
        textColumns<-c()
        for(i in 1:ncol(mydata())){
          textColumns <- cbind(textColumns,mydata()[,i])
        }
      } 
      else{
        textColumns <- cbind(mydata()[input$text_x_rf],mydata()[input$text_x_rf]) 
      }
      
      #CREATE A TERM-DOCUMENT MATRIX THAT REPRESENTS WORD FREQUENCIES IN EACH DOCUMENT
      text_matrix_rf<-create_matrix(textColumns,
                              language="english", stemWords=TRUE, removePunctuation=TRUE, weighting= weightTf,removeNumbers=TRUE)
      
      #creat train and test vectors
      train_text_rf <- 1:input$textnum_rf
      test_text_rf=(length(train_text_rf)+1):nrow(mydata())
      
      #creates a container for training, classifying, and analyzing documents.
      contain_rf<-create_container(text_matrix_rf,mydata()[,input$text_y_rf],trainSize=train_text_rf,testSize=test_text_rf,virgin=FALSE) 
      
      #modelling
      model_rf<-train_model(contain_rf,"RF",na.action=na.omit)
      #get the predictions
      results_rf<-classify_model(contain_rf, model_rf)
      
      labels_out_rf <- data.frame(
        CORRECT_LABEL = mydata()[,input$text_y_rf][test_text_rf],
        results_rf,
        stringsAsFactors = F)
      
      #print analytics on request
      if(input$pr_analytics_rf==TRUE){
        #container for analytics
        contain_rf2<-create_container(text_matrix_rf,as.numeric(factor(mydata()[,input$text_y_rf])),trainSize=train_text_rf,testSize=test_text_rf,virgin=FALSE)
        
        model_rf2<-train_model(contain_rf2,"RF")
        results_rf2<-classify_model(contain_rf2, model_rf2)
        
        analytics<-create_analytics(contain_rf2,cbind(results_rf2))
        
        print(list(summary(analytics), analytics@algorithm_summary,
                    #RAW SUMMARY OF ALL DATA AND SCORING
                    cbind(analytics@document_summary,Target_Label=mydata()[,input$text_y_rf][test_text_rf]), 
                    #SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
                    cbind(analytics@label_summary,Target_LABEL= unique(mydata()[,input$text_y_rf])),
                    # Confusion Matrices -- look for possible problems
                    #table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$RF_LABEL),
                    # CHECK OVERALL ACCURACY OF ALGORITHMS
                   recall_accuracy(analytics@document_summary$MANUAL_CODE, analytics@document_summary$RF_LABEL)),row.names =NULL)
      }
else{
  print(list(labels_out_rf,Proportions_Table=prop.table(table(labels_out_rf[,1] == labels_out_rf[,2])),
             Cross_Validated_Algorithm=cross_validate(contain_rf,2,algorithm="RF")),row.names = FALSE)
}
    })
  }
  else{
    return(cat('Select from Side Panel also \n'))
  }
}

#######################################################
#glmnet modelling
if(input$help_page_text=="glmnet_text"){
  
  input$goButtontext_corp_glmnet
  if(input$goButtontext_corp_glmnet > 0){
    isolate({
      
      if(input$radio_text_glmnet == "All"){  
        textColumns<-c()
        for(i in 1:ncol(mydata())){
          textColumns <- cbind(textColumns,mydata()[,i])
        }
      } 
      else{
        textColumns <- cbind(mydata()[input$text_x_glmnet],mydata()[input$text_x_glmnet]) 
      }
      
      #CREATE A TERM-DOCUMENT MATRIX THAT REPRESENTS WORD FREQUENCIES IN EACH DOCUMENT
      text_matrix_glmnet<-create_matrix(textColumns,
                            language="english", stemWords=TRUE, removePunctuation=TRUE, weighting= weightTf,removeNumbers=TRUE)
      
      #creat train and test vectors
      train_text_glmnet <- 1:input$textnum_glmnet
      test_text_glmnet=(length(train_text_glmnet)+1):nrow(mydata())
      
      #creates a container for training, classifying, and analyzing documents.
      contain_glmnet<-create_container(text_matrix_glmnet,mydata()[,input$text_y_glmnet],trainSize=train_text_glmnet,testSize=test_text_glmnet,virgin=FALSE) 
      
      #modelling
      model_glmnet<-train_model(contain_glmnet,"GLMNET")
      #get the predictions
      results_glmnet<-classify_model(contain_glmnet, model_glmnet)
      
      labels_out_glmnet <- data.frame(
        CORRECT_LABEL = mydata()[,input$text_y_glmnet][test_text_glmnet],
        results_glmnet,
        stringsAsFactors = F)
      
      #print analytics on request
      if(input$pr_analytics_glmnet==TRUE){
        #container for analytics
        contain_glmnet2<-create_container(text_matrix_glmnet,as.numeric(factor(mydata()[,input$text_y_glmnet])),trainSize=train_text_glmnet,testSize=test_text_glmnet,virgin=FALSE)
        
        model_glmnet2<-train_model(contain_glmnet2,"GLMNET")
        results_glmnet2<-classify_model(contain_glmnet2, model_glmnet2)
        analytics<-create_analytics(contain_glmnet2,cbind(results_glmnet2))
        
        print(list(summary(analytics), analytics@algorithm_summary,
                    #RAW SUMMARY OF ALL DATA AND SCORING
                    cbind(analytics@document_summary,Target_Label=mydata()[,input$text_y_glmnet][test_text_glmnet]),
                    #SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
                    cbind(analytics@label_summary,Target_LABEL= unique(mydata()[,input$text_y_glmnet])),
                    # Confusion Matrices -- look for possible problems
                   # table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$GLMNET_LABEL),
                    # CHECK OVERALL ACCURACY OF ALGORITHMS
                    recall_accuracy(analytics@document_summary$MANUAL_CODE, analytics@document_summary$GLMNET_LABEL)),row.names = FALSE) 
      }
      else{
        print(list(labels_out_glmnet,Proportions_Table=prop.table(table(labels_out_glmnet[,1] == labels_out_glmnet[,2])),
                   Cross_Validated_Algorithm=cross_validate(contain_glmnet,2,algorithm="GLMNET")),row.names = FALSE)
      }
    })
  }
  else{
    return(cat('Select from Side Panel also \n'))
  }
}

###################################################
#Bagging modelling
if(input$help_page_text=="bagging_text"){
  
  input$goButtontext_corp_bagg
  if(input$goButtontext_corp_bagg > 0){
    isolate({
      
      if(input$radio_text_bagg == "All"){  
        textColumns<-c()
        for(i in 1:ncol(mydata())){
          textColumns <- cbind(textColumns,mydata()[,i])
        }
      } 
      else{
        textColumns <- cbind(mydata()[input$text_x_bagg],mydata()[input$text_x_bagg]) 
      }
      
      #CREATE A TERM-DOCUMENT MATRIX THAT REPRESENTS WORD FREQUENCIES IN EACH DOCUMENT
      text_matrix_bagg<-create_matrix(textColumns,
                          language="english", stemWords=TRUE, removePunctuation=TRUE, weighting= weightTf,removeNumbers=TRUE)
      
      #creat train and test vectors
      train_text_bagg <- 1:input$textnum_bagg
      test_text_bagg=(length(train_text_bagg)+1):nrow(mydata())
      
      #creates a container for training, classifying, and analyzing documents.
      contain_bagg<-create_container(text_matrix_bagg,mydata()[,input$text_y_bagg],trainSize= train_text_bagg,testSize=test_text_bagg,virgin=FALSE) 
      
      #modelling
      model_bagg<-train_model(contain_bagg,"BAGGING")
      #get the predictions
      results_bagg<-classify_model(contain_bagg, model_bagg)
      
      labels_out_bagg <- data.frame(
        CORRECT_LABEL = mydata()[,input$text_y_bagg][test_text_bagg],
        results_bagg,
        stringsAsFactors = F)
      
      #print analytics on request
      if(input$pr_analytics_bagg==TRUE){
        #container for analytics
        contain_bagg2<-create_container(text_matrix_bagg,as.numeric(factor(mydata()[,input$text_y_bagg])),trainSize=train_text_bagg,testSize=test_text_bagg,virgin=FALSE)
        
        model_bagg2<-train_model(contain_bagg2,"BAGGING")
        results_bagg2<-classify_model(contain_bagg2, model_bagg2)
        analytics<-create_analytics(contain_bagg2,cbind(results_bagg2))
        
        print(list(summary(analytics), analytics@algorithm_summary,
                   #RAW SUMMARY OF ALL DATA AND SCORING
                   cbind(analytics@document_summary,Target_Label=mydata()[,input$text_y_bagg][test_text_bagg]),
                   #SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
                   cbind(analytics@label_summary,Target_LABEL= unique(mydata()[,input$text_y_bagg])),
                   # Confusion Matrices -- look for possible problems
                  # table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$BAGGING_LABEL),
                   # CHECK OVERALL ACCURACY OF ALGORITHMS
                   recall_accuracy(analytics@document_summary$MANUAL_CODE, analytics@document_summary$BAGGING_LABEL)),row.names = FALSE) 
      }
      else{
        print(list(labels_out_bagg,Proportions_Table=prop.table(table(labels_out_bagg[,1] == labels_out_bagg[,2])),
                   Cross_Validated_Algorithm=cross_validate(contain_bagg,2,algorithm="BAGGING")),row.names = FALSE)
      }
    })
  }
  else{
    return(cat('Select from Side Panel also \n'))
  }
}

##################################################
#Boosting modelling
if(input$help_page_text=="boosting_text"){
  
  input$goButtontext_corp_boost
  if(input$goButtontext_corp_boost > 0){
    isolate({
      
      if(input$radio_text_boost == "All"){  
        textColumns<-c()
        for(i in 1:ncol(mydata())){
          textColumns <- cbind(textColumns,mydata()[,i])
        }
      } 
      else{
        textColumns <- cbind(mydata()[input$text_x_boost],mydata()[input$text_x_boost]) 
      }
      
      #CREATE A TERM-DOCUMENT MATRIX THAT REPRESENTS WORD FREQUENCIES IN EACH DOCUMENT
      text_matrix_boost<-create_matrix(textColumns,
                              language="english", stemWords=TRUE, removePunctuation=TRUE, weighting= weightTf,removeNumbers=TRUE)
      
      #creat train and test vectors
      train_text_boost <- 1:input$textnum_boost
      test_text_boost=(length(train_text_boost)+1):nrow(mydata())
      
      #creates a container for training, classifying, and analyzing documents.
      contain_boost<-create_container(text_matrix_boost,mydata()[,input$text_y_boost],trainSize=train_text_boost,testSize=test_text_boost,virgin=FALSE) 
      
      #modelling
      model_boost<-train_model(contain_boost,"BOOSTING")
      #get the predictions
      results_boost<-classify_model(contain_boost, model_boost)
      
      labels_out_boost <- data.frame(
        CORRECT_LABEL = mydata()[,input$text_y_boost][test_text_boost],
        results_boost,
        stringsAsFactors = F)
      
      #print analytics on request
      if(input$pr_analytics_boost==TRUE){
        #container for analytics
        contain_boost2<-create_container(text_matrix_boost,as.numeric(factor(mydata()[,input$text_y_boost])),trainSize=train_text_boost,testSize=test_text_boost,virgin=FALSE)
        
        model_boost2<-train_model(contain_boost2,"BOOSTING")
        results_boost2<-classify_model(contain_boost2, model_boost2)
        analytics<-create_analytics(contain_boost2,cbind(results_boost2))
        
        print(list(summary(analytics), analytics@algorithm_summary,
                   #RAW SUMMARY OF ALL DATA AND SCORING
                   cbind(analytics@document_summary,Target_Label=mydata()[,input$text_y_boost][test_text_boost]),
                   #SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
                   cbind(analytics@label_summary,Target_LABEL= unique(mydata()[,input$text_y_boost])), 
                   # Confusion Matrices -- look for possible problems
                  # table(true = analytics@document_summary$MANUAL_CODE, predict = analytics@document_summary$BOOSTING_LABEL),
                   # CHECK OVERALL ACCURACY OF ALGORITHMS
                   recall_accuracy(analytics@document_summary$MANUAL_CODE, analytics@document_summary$BOOSTING_LABEL)),row.names = FALSE) 
      }
      else{
        print(list(labels_out_boost,Proportions_Table=prop.table(table(labels_out_boost[,1] == labels_out_boost[,2])),
                   Cross_Validated_Algorithm=cross_validate(contain_boost,2,algorithm="BOOSTING")),row.names = FALSE) 
      }
    })
  }
  else{
    return(cat('Select from Side Panel also \n'))
  }
}
})
######################################
#Plotting Text Mining
output$text.Plot <- renderPlot({
 
  input$goButtontext
  if(input$goButtontext > 0){
    isolate({
  
  #create corpus
  t.corp<-Corpus(DataframeSource(as.matrix(mydata())))
  corp.mat<-dtm.corp(t.corp)
  # remove sparse terms 
  corp.mat <- removeSparseTerms(corp.mat, sparse=0.95)
  m2 <- as.matrix(corp.mat)
  
  #Wordcloud
  if(input$text_plot_method=="wordcloud"){
  print(cloud(t.corp))
  }
  
  #Word frequency
  if(input$text_plot_method=="topwords"){
  ##get the top 20 words
  v <- apply(corp.mat,1,sum)
  v <- sort(v, decreasing = TRUE)
  v1 <- sort(v[1:20])
  barplot(v1, horiz=TRUE, cex.names = 0.7, las = 1, col=grey.colors(10), main="Frequency of Terms")
  }
  
  #hierarchical clustering 
  if(input$text_plot_method=="text_hclust"){ 
    tdm.dense = melt(m2, value.name = "count")
    # cluster terms
    distMatrix <- dist(scale(m2))
    fit <- hclust(distMatrix, method="ward.D2") # play around with 'ward' , 'complete' etc. 
    plot(fit)
  }
  
  #k-means clustering
  if(input$text_plot_method=="text_kmeans"){
    #we check the popular words in every cluster and also the cluster centers
    #transpose the matrix to cluster documents (tweets)
    m3 <- t(m2)  
    #number of clusters
    rge <- apply(m3, 2, max) - apply(m3, 2, min)
    cluster.dat <- sweep(m3, 2, rge, FUN = "/")
    n <- nrow(cluster.dat)
    wss <- rep(0, 10)
    wss[1] <- (n - 1) * sum(apply(cluster.dat, 2, var))
    for (i in 2:10)
      wss[i] <- sum(kmeans(cluster.dat,
                           centers = i)$withinss)
    k <- 10
    kmeansResult <- kmeans(m3, k)
    
    #cluster centers
    round(kmeansResult$centers, digits=3)
    
    #check the top three words in every cluster
    for (i in 1:k) {
      cat(paste("cluster ", i, ": ", sep=""))
      s <- sort(kmeansResult$centers[i,], decreasing=T)
      cat(names(s)[1:3], "\n")
    }
    #Partitioning (clustering) of the data into k clusters "around medoids", a more robust version of K-means.
    #For large datasets use clara(),
    pamx <- clara(m3, 3)
    #silhouette plot. With the silhouette, a large si (almost 1) suggests that
    #the corresponding observations are very well clustered, a small si (around 0) means that the
    #observation lies between two clusters, and observations with a negative si are probably placed in the wrong cluster
    si <- silhouette(pamx)
    
   par(mfrow = c(2, 2))
   print(list( plot(1:10, wss, type = "b", xlab = "Number of groups",
                    ylab = "Within groups sum of squares"),
               #Creates a bivariate plot visualizing a partition (clustering) of the data.
               #All observation are represented by points in the plot, using principal components.
               #Around each cluster an ellipse is drawn.
              clusplot(pamx,kmeansResult$clusters, shade=F, labels=5, lines=1, color=T, lty=4, main='Principal Components plot showing K-means clusters'),
              plot(si)))
  }
  
  #Neural networks
  if(input$text_plot_method=='network'){
    # change it to a Boolean matrix
    m2[m2>=1] <- 1
    #transform into a term-term adjacency matrix
    termMatrix <- m2 %*% t(m2)
    #inspect terms
    termMatrix[1:10,1:10]
    
    #build a graph from the above matrix
    g <- graph.adjacency(termMatrix, weighted=T, mode="undirected")
    #remove loops
    g <- simplify(g)
    #set labels and degrees of vertices
    V(g)$label <- V(g)$name
    V(g)$degree <- degree(g)
    layout1 <- layout.fruchterman.reingold(g)
    plot(g, layout=layout1)
  } 
    })
  }
  
})


output$help_text <- renderText({ 
  
  if(input$general_page=='gen_help'){
  }

  if(input$general_page=='plot_help'){
  }
  
  if(input$general_page=='mod_help'){
  }
  
  if(input$general_page=='text_mod_help'){
    
  }
})

  
})  
  
  
