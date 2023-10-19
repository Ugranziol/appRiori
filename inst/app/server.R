library(hypr, warn.conflicts = FALSE)
library(sortable, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(pracma, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(utils, warn.conflicts = FALSE)

##############  Functions for loading default datasets

load_default_data <- function(id) {
  id <- strsplit(id, ":", TRUE)[[1]]
  e <- new.env()
  vars <- data(list = id[2], package = id[1], envir = e)
  e[[id[length(id)]]]
}


default_data_labels <- function() {
  default_datasets <- utils::data()$results
  default_datasets <- default_datasets[order(default_datasets[,"Title"]),]
  cluster <- gsub("^([^(]+)( \\((.*)\\))?$","\\3",default_datasets[,"Item"])
  item <- gsub("^([^(]+)( \\((.*)\\))?$","\\1",default_datasets[,"Item"])
  datasets_select_labels <- ifelse(cluster=="", sprintf("%s:%s", default_datasets[,"Package"], item), sprintf("%s:%s:%s", default_datasets[,"Package"], cluster, item))
  names(datasets_select_labels) <- sprintf("%s (%s:%s)", default_datasets[,"Title"], default_datasets[,"Package"], item)
  datasets_select_labels[vapply(datasets_select_labels, function(id) {
    df <- load_default_data(id)
    is.data.frame(df) && any(apply(df, 2, function(x) is.character(x) || is.factor(x)))
  }, logical(1))]
}

hypr_call <- function(h) {
  as.expression(as.call(c(list(as.name("hypr")), formula(h), list(levels = levels(h)))))
}

##############  Function aimed to find the greatest common divisor of a vector or n X 1 matrix
gcd_vector <- function(x) Reduce(gcd, x)



server = function(input, output,session) {

                             ######################################################
                             ############ Code for the data management ############
                             ######################################################


############## Reactive element that takes as input the uploaded data.frame
updateSelectInput(session, "default_data", choices = default_data_labels())
 mydf=reactive({
   if(input$data_type == "upload") {
     req(input$file1)
     mydf_0=read.table(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote,
                       dec=input$deci)
   } else if(input$data_type == "preinstalled") {
     req(input$default_data)
     mydf_0 <- as.data.frame(load_default_data(input$default_data))
   }
   return(mydf_0)
  })

 ncomparisons <- reactiveValues()

 mydfname = reactive({
   if(input$data_type == "upload") {
     req(input$file1)
     gsub("\\.csv$", "", basename(input$file1$name))
   } else if(input$data_type == "preinstalled") {
     req(input$default_data)
     gsub("^.*:", "", input$default_data)
   }
 })



############## After the data frame is uploaded, this chunk of code updates the check box that stores the data.frame's variable
  observe({

    x=colnames(mydf())


    updateCheckboxGroupInput(session, "show_vars",
                             choices = x,
                             selected = NULL
    )
  })


############## Once the data are uploaded, they are showed in a nice format
  output$contents <- DT::renderDataTable(DT::datatable(
    data=mydf()[,input$show_vars,drop=FALSE],
    options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#375a7f', 'color': '#fff'});",
        "}"), autoWidth = TRUE
    )
  ))

############## Generate a summary of the dataset
  output$structure <- renderPrint({
    str(mydf()[,input$show_vars,drop=FALSE])
  })

############## Chunk of code that allows to select only "character" or "factor" variables
  observe({

    x=mydf() %>% select_if(~is.factor(.)|is.character(.))

    #
    updateSelectInput(session, "in1",
                             choices = colnames(x),
                             selected = NULL)

    updateSelectInput(session, "v1",
                      choices = colnames(x),
                      selected = NULL)

    updateSelectInput(session, "v2",
                      choices = colnames(x),
                      selected = NULL)

    updateSelectInput(session, "v3",
                      choices = colnames(x),
                      selected = NULL)

  })

                              #########################################################
                              ############ Single variable contrast coding ############
                              #########################################################


  ############# When the variable are correctly uploaded, it is given the user to select the number of contrast to set, out of n-1 contrasts
  observeEvent(input$in1,{
    y=mydf()
    ncomparisons$hm1 <- length(levels(factor(y[,input$in1])))-1
    updateSelectInput(session, "hm1",
                       choices = 1:ncomparisons$hm1,
                      selected = ncomparisons$hm1
                       )


  })

  ############## Chuck of code that is required when the user select the option "Customized" for the drag-n-drop option
  ############## Step 1: creating the drag-n-drop menus, with n-1 blocks to drag. For each Block, a different input is defined.



  observeEvent(input$in1, {
    a=mydf()
    fattore=factor(a[,input$in1])
    output$inputGroup = renderUI({
      ifelse(length(levels(fattore))==1,assign("num2",1),assign("num2",as.numeric(input$hm1)))
      input_list <- lapply(1:num2, function(i) {
        inputName <- paste("input", i, sep = "")
        bucket_list(
          header = paste("contrast #",i),
          group_name = "bucket_list_group",
          orientation = "horizontal",
          add_rank_list(
            text = "Drag from here",
            labels =
              levels(fattore)
            ,
            input_id = inputName
          ),
          add_rank_list(
            text = "to here",
            labels = NULL,
            input_id = paste(inputName,1,sep = "")
          ),
          add_rank_list(
            text = "or here",
            labels = NULL,
            input_id = paste(inputName,2,sep = "")
          )

        )
      })
      do.call(tagList, input_list)
    })
  })

  ############## For each block, the input name is stored and manipulated to fit an hypr() model. Basically, This code create a list of formulas.

  reattivo=reactive({
    a=mydf()
    fattore=factor(a[,input$in1])
    ifelse(length(levels(fattore))==1,assign("num2",1),assign("num2",as.numeric(input$hm1)))
    eta=paste(lapply(1:num2, function(i) {
      inputName1 <- paste("input", i, "1",sep = "")
      if(length(input[[inputName1]])==1){
        input[[inputName1]]
      }else{
        paste(input[[inputName1]],collapse="+")
      }
    }))

    for (e in 1:length(eta)) {
      if(str_detect(eta[[e]],"\\+")==FALSE){
        eta[[e]]
      }else{
        eta[[e]]=paste("(",eta[[e]],")","/",length(str_split(eta[[e]], "\\+")[[1]]))
      }
    }

    beta=paste(lapply(1:num2, function(i) {
      inputName2 <- paste("input", i, "2",sep = "")
      if(length(input[[inputName2]])==1){
        input[[inputName2]]
      }else{
        paste(input[[inputName2]],collapse="+")
      }

    }))

    for (e in 1:length(beta)) {
      if(str_detect(beta[[e]],"\\+")==FALSE){
        beta[[e]]
      }else{
        beta[[e]]=paste("(",beta[[e]],")","/",length(str_split(beta[[e]], "\\+")[[1]]))
      }
    }

    list(paste(eta,"~",beta))

  })



  ############ The following code produces the basis of the new contrasts of matrix, based on the option selected. It takes as input all the previous line of code
  faktor=reactive({
    a=mydf()
    fattore=factor(a[,input$in1])
    if(input$cont=="Treatment"){
      contrasts(fattore)
    }else if(input$cont=="Simple"){
      nLevels=length(levels(fattore))
      dummy <- contr.treatment(nLevels)
      dimnames(dummy) <- NULL
      coding <- matrix(rep(1/nLevels, prod(dim(dummy))), ncol=nLevels-1)
      s_cod <- round((dummy - coding),2)
      contrasts(fattore)=s_cod
      contrasts(fattore)
    }else if(input$cont=="Sum"){
      contrasts(fattore)=contr.sum(length(levels(fattore)))
      contrasts(fattore)
    }else if(input$cont=="Scaled"){
      contrasts(fattore)=contr.sum(length(levels(fattore)))/length(levels(fattore))
      contrasts(fattore)
    }else if(input$cont=="Sliding difference"){
      contrasts(fattore)=MASS::contr.sdif(length(levels(fattore)))
      contrasts(fattore)
    }else if(input$cont=="Helmert"){
      contrasts(fattore)=contr.helmert(length(levels(fattore)))
      contrasts(fattore)
    }else if(input$cont=="Reverse Helmert"){
      tmp0=contr.helmert(length(levels(fattore)))
      tmp0.1=apply(tmp0,2,rev)
      tmp0.2=tmp0.1[,ncol(tmp0.1):1]
      contrasts(fattore)=tmp0.2
      contrasts(fattore)
    }else if(input$cont=="Polynomial"){
      contrasts(fattore)=contr.poly(length(levels(fattore)))
      contrasts(fattore)
    }else {
      reat=unlist(reattivo())
      res=list()

      for (a in 1:length(reat)) {
        res[[a]]=formula(reat[a])
      }

      res
    }

  })


  ############## This code displays the levels of the selected variable
  output$lev=renderPrint({
    y=mydf()

    cbind(levels(factor(y[,input$in1])))

  })

  ############## This code displays the simplified contrast matrix generated by R, that always corresponds to a dummy coding matrix
  output$original=renderPrint({
    z=mydf()

    contrasts(factor(z[,input$in1]))

  })


  ############## This code allows to display the new contrast matrix
  output$new=renderPrint({
    z_new=mydf()
    tryCatch({
      if(is.list(faktor())){
        h <- hypr(faktor(),levels = levels(factor(z_new[,input$in1])))
        if(input$cont == "Customized") {
          h <- filler_contrasts(h, ncomparisons$hm1)
        } else {
          names(h) <- paste0("C", seq_along(names(h)))
        }
        cm = cmat(h, as_fractions = FALSE)
        cm = matrix(cm, nrow = nrow(cm), ncol = ncol(cm), dimnames = dimnames(cm))
        print(round(cm,2))
      }else{
        cm=faktor()
        colnames(cm)=paste("C", 1:ncol(cm),sep = "")
        print(round(cm,2))
      }},error=function(e){
        cat("Waiting..")
    })


  })

  ############## This code allows to display the hypothesis matrix related to the new contrast matrix
  output$hypmat=renderPrint({
    z_hypmat=mydf()
    tryCatch({
      h <- hypr(faktor(),levels = levels(factor(z_hypmat[,input$in1])))
      hm=round(hmat(h, as_fractions=FALSE),2)
      rownames(hm)=paste("C", 1:nrow(hm),sep = "")
      print(t(hm))
    },error=function(e){
      cat("Waiting..")
    })
  })

  ############## This code generates and displays the correlation matrix of the new contrast matrix

  output$cormat=renderPrint({
    z_cormat=mydf()
    tryCatch({
      if(is.list(faktor())){
        h <- hypr(faktor(),levels = levels(factor(z_cormat[,input$in1])))
        cm=cor(cmat(h))
        colnames(cm)=paste("C", 1:ncol(cm),sep = "")
        rownames(cm)=paste("C", 1:nrow(cm),sep = "")
        print(round(cm,2))
      }else{
        cm=cor(faktor())
        colnames(cm)=paste("C", 1:ncol(cm),sep = "")
        rownames(cm)=paste("C", 1:nrow(cm),sep = "")
        print(round(cm,2))
      }},error=function(e){
        cat("Waiting..")
    })


  })


  ############# This code generates warning message(s) when not-linear-independent contrasts are set

  dataerrors <- reactive({
    z_error=mydf()
    tryCatch({
      if(is.list(faktor())){
        h <- hypr(faktor(),levels = levels(factor(z_error[,input$in1])))
        hc=cmat(h)
        cat("Contrasts are linearly independent")
      }
    }, error=function(e){
      cat("Waiting..")
    }, warning = function(w) {
      if(grepl("Your hypotheses are not linearly independent", w, fixed = TRUE)){return(w$message)}
    })
  })

  output$contrasts_warnings <- renderPrint({
    dataerrors()
  })

  ############## The following lines generates the "ready-to-use" code corresponding to the solution planned by the user.
  faktor2=reactive({
    a=mydf()
    fattore=factor(a[,input$in1])
    if(input$cont=="Treatment"){
      paste0("contr.treatment(",length(levels(fattore)),")")
    }else if(input$cont=="Simple"){
      paste("matrix(",(paste("c(",paste(faktor(), collapse = ','),")")),",",nrow(faktor()),",",ncol(faktor()),")")
    }else if(input$cont=="Sum"){
      paste0("contr.sum(",length(levels(fattore)),")")
    }else if(input$cont=="Scaled"){
      paste0("contr.sum(",length(levels(fattore)),")","/",length(levels(fattore)))
    }else if(input$cont=="Sliding difference"){
      paste0("MASS::contr.sdif(",length(levels(fattore)),")")
    }else if(input$cont=="Helmert"){
      paste0("contr.helmert(",length(levels(fattore)),")")
    }else if(input$cont=="Reverse Helmert"){
      paste("matrix(",(paste("c(",paste(faktor(), collapse = ','),")")),",",nrow(faktor()),",",ncol(faktor()),")")
    }else if(input$cont=="Polynomial"){
      paste0("contr.poly(",length(levels(fattore)),")")
    }else{
      h <- hypr(faktor(),levels = levels(factor(a[,input$in1]))) %>% filler_contrasts(ncomparisons$hm1)
      paste("matrix(",(paste("c(",paste(cmat(h), collapse = ','),")")),",",nrow(cmat(h)),",",ncol(cmat(h)),")")
    }

  })

  ############## The following lines prints the code corresponding to the solution planned by the user.
  output$res=renderPrint({
    tryCatch({
    a=mydf()
    fname=mydfname()
    h = hypr(faktor(),levels = levels(factor(a[,input$in1])))
    if(is.null(input$radio_output)){
      cat("Please, select the kind of output you prefer!")
    }else if(input$radio_output=="br"){
      cat(paste0(fname,"$",input$in1,"=","factor(",fname,"$",input$in1,")"))
      cat(sep = "\n")
    if(input$cont=="Customized"){
      cat(paste0("contrasts(",fname,"$",input$in1,",","how.many=",ncomparisons$hm1,")","=",faktor2()))
    }else{
      cat(paste0("contrasts(",fname,"$",input$in1,")","=",faktor2()))
    }
      }else{
        cat(paste0(fname,"$",input$in1,"=","factor(",fname,"$",input$in1,")"))
        cat(sep = "\n")
        cat(paste0("h <- ",as.character(hypr_call(h))))
        cat(sep = "\n")
        if(input$cont=="Customized"){
          cat(paste0("contrasts(",fname,"$",input$in1,", how.many =",ncomparisons$hm1,")",
                     "=","cmat(h)"))
        } else {
          cat(paste0("contrasts(",fname,"$",input$in1,")",
                     "=","cmat(h)"))
        }
      }

  },error=function(e){
    cat("It seems that something is missing. Complete the previous step!")
  })

  })



                             ######################################################
                             ############ Interactions contrast coding ############
                             ######################################################



  ############## As for the single variable, with this code the user can select how many contrast to set, out of (n1 X n2 [X n3])-1 contrasts
  observeEvent(c(input$v1,input$v2,input$v3),{
    ba=mydf()
    fattore1=factor(ba[,input$v1])
    fattore2=factor(ba[,input$v2])
    fattore3=factor(ba[,input$v3])
    if(input$radio=="Two way"){
      lev_interaction=factor(levels(interaction(fattore1,fattore2,sep="_")))
    }else{
      lev_interaction=factor(levels(interaction(fattore1,fattore2,fattore3,sep="_")))
    }
    ncomparisons$hm2 <- length(levels(lev_interaction))-1
    updateSelectInput(session, "hm2",
                      choices = 1:ncomparisons$hm2,
                      selected = ncomparisons$hm2
    )

  })

  ############## Reactive element that allows to generate the drag-n-drop menu for the "Fully customized 1" option.
  toObserve= reactive({
    list(input$v1 ,input$v2 ,input$v3)
  })

  ############## Chuck of code that is required when the user select the option "Fully customized 1" for the drag-n-drop option
  ############## Step 1: creating the drag-n-drop menus, with (n1 X n2) -1 blocks to drag. For each Block, a different input is defined.

  observeEvent(toObserve(), {
    a=mydf()
    ifelse(input$radio== 'Three way',
           assign("toint",interaction(factor(a[,input$v1]),factor(a[,input$v2]),factor(a[,input$v3]))),
           assign("toint",interaction(factor(a[,input$v1]),factor(a[,input$v2]))))

    output$inputGroup2 = renderUI({
      ifelse(length(levels(toint))==1,assign("num2",1),assign("num2",as.numeric(input$hm2)))
      input_list <- lapply(1:num2, function(i) {
        inputName <- paste("input", i, sep = "")
        bucket_list(
          header = paste("contrast #",i),
          group_name = "bucket_list_group",
          orientation = "horizontal",
          add_rank_list(
            text = "Drag from here",
            labels =
              levels(toint)
            ,
            input_id = inputName
          ),
          add_rank_list(
            text = "to here",
            labels = NULL,
            input_id = paste(inputName,1,sep = "")
          ),
          add_rank_list(
            text = "or here",
            labels = NULL,
            input_id = paste(inputName,2,sep = "")
          )

        )
      })
      do.call(tagList, input_list)
    })
  })

  ############## For each block, the input name is stored and manipulated to fit an hypr() model. Basically, This code create a list of formulas.
  reattivo_int=reactive({
    a=mydf()
    ifelse(input$radio== 'Three way',
           assign("toint",interaction(factor(a[,input$v1]),factor(a[,input$v2]),factor(a[,input$v3]))),
           assign("toint",interaction(factor(a[,input$v1]),factor(a[,input$v2]))))

    ifelse(length(levels(toint))==1,assign("num2",1),assign("num2",as.numeric(input$hm2)))
    eta=paste(lapply(1:num2, function(i) {
      inputName1 <- paste("input", i, "1",sep = "")
      if(length(input[[inputName1]])==1){
        input[[inputName1]]
      }else{
        paste(input[[inputName1]],collapse="+")
      }
    }))

    for (e in 1:length(eta)) {
      if(str_detect(eta[[e]],"\\+")==FALSE){
        eta[[e]]
      }else{
        eta[[e]]=paste("(",eta[[e]],")","/",length(str_split(eta[[e]], "\\+")[[1]]))
      }
    }

    beta=paste(lapply(1:num2, function(i) {
      inputName2 <- paste("input", i, "2",sep = "")
      if(length(input[[inputName2]])==1){
        input[[inputName2]]
      }else{
        paste(input[[inputName2]],collapse="+")
      }

    }))

    for (e in 1:length(beta)) {
      if(str_detect(beta[[e]],"\\+")==FALSE){
        beta[[e]]
      }else{
        beta[[e]]=paste("(",beta[[e]],")","/",length(str_split(beta[[e]], "\\+")[[1]]))
      }
    }

    list(paste(eta,"~",beta))

  })

  ############ The following code produces the basis of the new contrasts of matrix, based on the option selected. It takes as input all the previous line of code
  facktor_int=reactive({
    y=mydf()

    levelnames <- levels(if(input$radio== 'Three way') interaction(factor(y[,input$v1]),factor(y[,input$v2]),factor(y[,input$v3])) else interaction(factor(y[,input$v1]),factor(y[,input$v2])))

      reat=unlist(reattivo_int())

      res = lapply(reat, formula)

      h=hypr(res, levels = levelnames)
      cm=cmat(h,as_fractions = FALSE)
      cm2=matrix(cm,nrow = nrow(cm),ncol = ncol(cm))
      rownames(cm2)=rownames(cm)
      cm2



  })



  ############## Reactive element that creates a table where each column contains the level of each of the two or three variables
  tab_lv=reactive({
    ba=mydf()
    fattore1=factor(ba[,input$v1])
    fattore2=factor(ba[,input$v2])
    fattore3=factor(ba[,input$v3])
    n=max(length(levels(fattore1)), length(levels(fattore2)),length(levels(fattore3)))
    lev1=levels(fattore1);length(lev1)=n
    lev2=levels(fattore2);length(lev2)=n
    lev3=levels(fattore3);length(lev3)=n

    if(input$radio == "Three way"){
      mydata=data.frame(cbind(V1=lev1,V2=lev2,V3=lev3))
      mydata[is.na(mydata)]=""
    }else{
      mydata=data.frame(cbind(V1=lev1,V2=lev2))
      mydata[is.na(mydata)]=""
    }
    mydata
  })

  ############## This code displays the levels of the selected variables
  output$lev_int=renderPrint({
    tab_lv()
  })


  ############## Reactive element that creates the default contrast matrix generated by R in case of interactions. Note that each variable will be coded accordingly to a dummy coding.
  cont_mat=reactive({
    y=mydf()
    size1=length(levels(factor(y[,input$v1])))
    size2=length(levels(factor(y[,input$v2])))
    size3=length(levels(factor(y[,input$v3])))

    if(input$radio=="Three way"){
      simdat4=appRiori:::mixedDesign(B=c(size1,size2,size3),W=NULL,n=5,long = T)
      names(simdat4)[1:3]=c(as.character(input$v1),as.character(input$v2),as.character(input$v3))

      levels(simdat4[,1])=levels(factor(y[,input$v1]))
      levels(simdat4[,2])=levels(factor(y[,input$v2]))
      levels(simdat4[,3])=levels(factor(y[,input$v3]))

      contrasts(simdat4[,1])=contr.treatment(size1)
      contrasts(simdat4[,2])=contr.treatment(size2)
      contrasts(simdat4[,3])=contr.treatment(size3)

      Xctr=simdat4%>%
        group_by_(input$v1,input$v2,input$v3)%>%
        summarise()%>%
        model.matrix(formula(paste("~","1+",input$v1,"*",input$v2,"*",input$v3)),.)%>%
        as.data.frame()%>%
        as.matrix()

      rownames(Xctr)=sort(levels(interaction(simdat4[,1],simdat4[,2],simdat4[,3],sep="_",lex.order = F)))
      colnames(Xctr)[2:length(colnames(Xctr))]=paste("C",1:(ncol(Xctr)-1),sep = "")

      # if(input$onlyI==TRUE){
      #   int_values=(size1-1)+(size2-1)+(size3-1)+2
      #   Xctr=cbind(Xctr[,int_values:ncol(Xctr)])
      #   rownames(Xctr)=sort(levels(interaction(simdat4[,1],simdat4[,2],simdat4[,3],sep="_",lex.order = F)))
      #   colnames(Xctr)=paste("C",1:(ncol(Xctr)),sep = "")
      # }


    }else{
      simdat4=appRiori:::mixedDesign(B=c(size1,size2),W=NULL,n=5,long = T)
      names(simdat4)[1:2]=c(as.character(input$v1),as.character(input$v2))

      levels(simdat4[,1])=levels(factor(y[,input$v1]))
      levels(simdat4[,2])=levels(factor(y[,input$v2]))

      contrasts(simdat4[,1])=contr.treatment(size1)
      contrasts(simdat4[,2])=contr.treatment(size2)

      Xctr=simdat4%>%
        group_by_(input$v1,input$v2)%>%
        summarise()%>%
        model.matrix(formula(paste("~","1+",input$v1,"*",input$v2)),.) %>%
        as.data.frame()%>%
        as.matrix()

      rownames(Xctr)=sort(levels(interaction(simdat4[,1],simdat4[,2],sep="_",lex.order = F)))
      colnames(Xctr)[2:length(colnames(Xctr))]=paste("C",1:(ncol(Xctr)-1),sep = "")

      # if(input$onlyI==TRUE){
      #   int_values=(size1-1)+(size2-1)+2
      #   Xctr=cbind(Xctr[,int_values:ncol(Xctr)])
      #   rownames(Xctr)=sort(levels(interaction(simdat4[,1],simdat4[,2],sep="_",lex.order = F)))
      #   colnames(Xctr)=paste("C",1:(ncol(Xctr)),sep = "")
      # }
    }
    Xctr


  })

  ############# When the variable are correctly uploaded, it is given the user to select the number of contrast to set, out of n-1 contrasts
  observeEvent(input$v1,{
    y=mydf()
    ncomparisons$ihm1 <- length(levels(factor(y[,input$v1])))-1
    updateSelectInput(session, "ihm1",
                      choices = 1:ncomparisons$ihm1,
                      selected = ncomparisons$ihm1
    )


  })

  ############## Chuck of code that is required when the user select the option "Customized" for the drag-n-drop option
  ############## Step 1: creating the drag-n-drop menus, with n-1 blocks to drag. For each Block, a different input is defined.



  observeEvent(input$v1, {
    a=mydf()
    fattore=factor(a[,input$v1])
    output$inputGroup.i1 = renderUI({
      ifelse(length(levels(fattore))==1,assign("numi1",1),assign("numi1",as.numeric(input$ihm1)))
      input_listi1 <- lapply(1:numi1, function(i) {
        inputNamei1 <- paste("input", i, sep = "")
        bucket_list(
          header = paste("contrast #",i),
          group_name = "bucket_list_group",
          orientation = "horizontal",
          add_rank_list(
            text = "Drag from here",
            labels =
              levels(fattore)
            ,
            input_id = paste(inputNamei1,"_1",sep = "")
          ),
          add_rank_list(
            text = "to here",
            labels = NULL,
            input_id = paste(inputNamei1,"a",sep = "")
          ),
          add_rank_list(
            text = "or here",
            labels = NULL,
            input_id = paste(inputNamei1,"b",sep = "")
          )

        )
      })
      do.call(tagList, input_listi1)
    })
  })

  ############## For each block, the input name is stored and manipulated to fit an hypr() model. Basically, This code create a list of formulas.

  reattivoi1=reactive({
    a=mydf()
    fattore=factor(a[,input$v1])
    ifelse(length(levels(fattore))==1,assign("numi1",1),assign("numi1",as.numeric(input$ihm1)))
    eta=paste(lapply(1:numi1, function(i) {
      inputNamei11 <- paste("input", i, "a",sep = "")
      if(length(input[[inputNamei11]])==1){
        input[[inputNamei11]]
      }else{
        paste(input[[inputNamei11]],collapse="+")
      }
    }))

    for (e in 1:length(eta)) {
      if(str_detect(eta[[e]],"\\+")==FALSE){
        eta[[e]]
      }else{
        eta[[e]]=paste("(",eta[[e]],")","/",length(str_split(eta[[e]], "\\+")[[1]]))
      }
    }

    beta=paste(lapply(1:numi1, function(i) {
      inputNamei12 <- paste("input", i, "b",sep = "")
      if(length(input[[inputNamei12]])==1){
        input[[inputNamei12]]
      }else{
        paste(input[[inputNamei12]],collapse="+")
      }

    }))

    for (e in 1:length(beta)) {
      if(str_detect(beta[[e]],"\\+")==FALSE){
        beta[[e]]
      }else{
        beta[[e]]=paste("(",beta[[e]],")","/",length(str_split(beta[[e]], "\\+")[[1]]))
      }
    }

    list(paste(eta,"~",beta))

  })


  ############## The following code creates the contrast matrix for each variable selected to interact. It works with the default contrasts function of R.
  faktorS1=reactive({
    a=mydf()
    fattore=factor(a[,input$v1])
    if(input$cont1=="Treatment"){
      contrasts(fattore)
    }else if(input$cont1=="Simple"){
      nLevels1=length(levels(fattore))
      dummy1 <- contr.treatment(nLevels1)
      dimnames(dummy1) <- NULL
      coding1 <- matrix(rep(1/nLevels1, prod(dim(dummy1))), ncol=nLevels1-1)
      s_cod1 <- round((dummy1 - coding1),2)
      contrasts(fattore)=s_cod1
      contrasts(fattore)
    }else if(input$cont1=="Sum"){
      contrasts(fattore)=contr.sum(length(levels(fattore)))
      contrasts(fattore)
    }else if(input$cont1=="Scaled"){
      contrasts(fattore)=contr.sum(length(levels(fattore)))/length(levels(fattore))
      contrasts(fattore)
    }else if(input$cont1=="Sliding difference"){
      contrasts(fattore)=MASS::contr.sdif(length(levels(fattore)))
      contrasts(fattore)
    }else if(input$cont1=="Helmert"){
      contrasts(fattore)=contr.helmert(length(levels(fattore)))
      contrasts(fattore)
    }else if(input$cont1=="Reverse Helmert"){
      tmp0=contr.helmert(length(levels(fattore)))
      tmp0.1=apply(tmp0,2,rev)
      tmp0.2=tmp0.1[,ncol(tmp0.1):1]
      contrasts(fattore)=tmp0.2
      contrasts(fattore)
    }else if(input$cont1=="Polynomial"){
      contrasts(fattore)=contr.poly(length(levels(fattore)))
      contrasts(fattore)
    }else{
      reati1=unlist(reattivoi1())
      resi1=list()

      for (a in 1:length(reati1)) {
        resi1[[a]]=formula(reati1[a])
      }

      resi1
    }

  })


  ############# When the variable are correctly uploaded, it is given the user to select the number of contrast to set, out of n-1 contrasts
  observeEvent(input$v2,{
    y=mydf()
    ncomparisons$ihm2 <- length(levels(factor(y[,input$v2])))-1
    updateSelectInput(session, "ihm2",
                      choices = 1:ncomparisons$ihm2,
                      selected = ncomparisons$ihm2
    )

  })

  ############## Chuck of code that is required when the user select the option "Customized" for the drag-n-drop option
  ############## Step 1: creating the drag-n-drop menus, with n-1 blocks to drag. For each Block, a different input is defined.




  observeEvent(input$v2, {
    a=mydf()
    fattore=factor(a[,input$v2])
    output$inputGroup.i2 = renderUI({
      ifelse(length(levels(fattore))==1,assign("num2",1),assign("num2",as.numeric(input$ihm2)))
      input_listi2 <- lapply(1:num2, function(i) {
        inputNamev <- paste("input", i, sep = "")
        bucket_list(
          header = paste("contrast #",i),
          group_name = "bucket_list_group",
          orientation = "horizontal",
          add_rank_list(
            text = "Drag from here",
            labels =
              levels(fattore)
            ,
            input_id = paste(inputNamev, "_2", sep ="")
          ),
          add_rank_list(
            text = "to here",
            labels = NULL,
            input_id = paste(inputNamev,"c",sep = "")
          ),
          add_rank_list(
            text = "or here",
            labels = NULL,
            input_id = paste(inputNamev,"d",sep = "")
          )

        )
      })
      do.call(tagList, input_listi2)
    })
  })

  ############## For each block, the input name is stored and manipulated to fit an hypr() model. Basically, This code create a list of formulas.

  reattivoi2=reactive({
    a=mydf()
    fattore=factor(a[,input$v2])
    ifelse(length(levels(fattore))==1,assign("num2",1),assign("num2",as.numeric(input$ihm2)))
    eta=paste(lapply(1:num2, function(i) {
      inputNamev1 <- paste("input", i, "c",sep = "")
      if(length(input[[inputNamev1]])==1){
        input[[inputNamev1]]
      }else{
        paste(input[[inputNamev1]],collapse="+")
      }
    }))

    for (e in 1:length(eta)) {
      if(str_detect(eta[[e]],"\\+")==FALSE){
        eta[[e]]
      }else{
        eta[[e]]=paste("(",eta[[e]],")","/",length(str_split(eta[[e]], "\\+")[[1]]))
      }
    }

    beta=paste(lapply(1:num2, function(i) {
      inputNamev2 <- paste("input", i, "d",sep = "")
      if(length(input[[inputNamev2]])==1){
        input[[inputNamev2]]
      }else{
        paste(input[[inputNamev2]],collapse="+")
      }

    }))

    for (e in 1:length(beta)) {
      if(str_detect(beta[[e]],"\\+")==FALSE){
        beta[[e]]
      }else{
        beta[[e]]=paste("(",beta[[e]],")","/",length(str_split(beta[[e]], "\\+")[[1]]))
      }
    }

    list(paste(eta,"~",beta))

  })

  ################################################
  faktorS2=reactive({
    a=mydf()
    fattore=factor(a[,input$v2])
    if(input$cont2=="Treatment"){
      contrasts(fattore)
    }else if(input$cont2=="Simple"){
      nLevels2=length(levels(fattore))
      dummy2 <- contr.treatment(nLevels2)
      dimnames(dummy2) <- NULL
      coding2 <- matrix(rep(1/nLevels2, prod(dim(dummy2))), ncol=nLevels2-1)
      s_cod2 <- round((dummy2 - coding2),2)
      contrasts(fattore)=s_cod2
      contrasts(fattore)
    }else if(input$cont2=="Sum"){
      contrasts(fattore)=contr.sum(length(levels(fattore)))
      contrasts(fattore)
    }else if(input$cont2=="Scaled"){
      contrasts(fattore)=contr.sum(length(levels(fattore)))/length(levels(fattore))
      contrasts(fattore)
    }else if(input$cont2=="Sliding difference"){
      contrasts(fattore)=MASS::contr.sdif(length(levels(fattore)))
      contrasts(fattore)
    }else if(input$cont2=="Helmert"){
      contrasts(fattore)=contr.helmert(length(levels(fattore)))
      contrasts(fattore)
    }else if(input$cont2=="Reverse Helmert"){
      tmp0=contr.helmert(length(levels(fattore)))
      tmp0.1=apply(tmp0,2,rev)
      tmp0.2=tmp0.1[,ncol(tmp0.1):1]
      contrasts(fattore)=tmp0.2
      contrasts(fattore)
    }else if(input$cont2=="Polynomial"){
      contrasts(fattore)=contr.poly(length(levels(fattore)))
      contrasts(fattore)
    }else{
      reati2=unlist(reattivoi2())
      resi2=list()

      for (a in 1:length(reati2)) {
        resi2[[a]]=formula(reati2[a])
      }

      resi2
    }

  })
###################################################

  ############# When the variable are correctly uploaded, it is given the user to select the number of contrast to set, out of n-1 contrasts
  observeEvent(input$v3,{
    y=mydf()
    ncomparisons$ihm3 <- length(levels(factor(y[,input$v3])))-1
    updateSelectInput(session, "ihm3",
                      choices = 1:ncomparisons$ihm3,
                      selected = ncomparisons$ihm3
    )

  })

  ############## Chuck of code that is required when the user select the option "Customized" for the drag-n-drop option
  ############## Step 1: creating the drag-n-drop menus, with n-1 blocks to drag. For each Block, a different input is defined.




  observeEvent(input$v3, {
    a=mydf()
    fattore=factor(a[,input$v3])
    output$inputGroup.i3 = renderUI({
      ifelse(length(levels(fattore))==1,assign("num2",1),assign("num2",as.numeric(input$ihm3)))
      input_listi3 <- lapply(1:num2, function(i) {
        inputNamev <- paste("input", i, sep = "")
        bucket_list(
          header = paste("contrast #",i),
          group_name = "bucket_list_group",
          orientation = "horizontal",
          add_rank_list(
            text = "Drag from here",
            labels =
              levels(fattore)
            ,
            input_id = paste(inputNamev,"_3",sep="")
          ),
          add_rank_list(
            text = "to here",
            labels = NULL,
            input_id = paste(inputNamev,"e",sep = "")
          ),
          add_rank_list(
            text = "or here",
            labels = NULL,
            input_id = paste(inputNamev,"f",sep = "")
          )

        )
      })
      do.call(tagList, input_listi3)
    })
  })

  ############## For each block, the input name is stored and manipulated to fit an hypr() model. Basically, This code create a list of formulas.

  reattivoi3=reactive({
    a=mydf()
    fattore=factor(a[,input$v3])
    ifelse(length(levels(fattore))==1,assign("num2",1),assign("num2",as.numeric(input$ihm3)))
    eta=paste(lapply(1:num2, function(i) {
      inputNamev1 <- paste("input", i, "e",sep = "")
      if(length(input[[inputNamev1]])==1){
        input[[inputNamev1]]
      }else{
        paste(input[[inputNamev1]],collapse="+")
      }
    }))

    for (e in 1:length(eta)) {
      if(str_detect(eta[[e]],"\\+")==FALSE){
        eta[[e]]
      }else{
        eta[[e]]=paste("(",eta[[e]],")","/",length(str_split(eta[[e]], "\\+")[[1]]))
      }
    }

    beta=paste(lapply(1:num2, function(i) {
      inputNamev2 <- paste("input", i, "f",sep = "")
      if(length(input[[inputNamev2]])==1){
        input[[inputNamev2]]
      }else{
        paste(input[[inputNamev2]],collapse="+")
      }

    }))

    for (e in 1:length(beta)) {
      if(str_detect(beta[[e]],"\\+")==FALSE){
        beta[[e]]
      }else{
        beta[[e]]=paste("(",beta[[e]],")","/",length(str_split(beta[[e]], "\\+")[[1]]))
      }
    }

    list(paste(eta,"~",beta))

  })

  ################################################
  faktorS3=reactive({
    a=mydf()
    fattore=factor(a[,input$v3])
    if(input$cont3=="Treatment"){
      contrasts(fattore)
    }else if(input$cont3=="Simple"){
      nLevels3=length(levels(fattore))
      dummy3 <- contr.treatment(nLevels3)
      dimnames(dummy3) <- NULL
      coding3 <- matrix(rep(1/nLevels3, prod(dim(dummy3))), ncol=nLevels3-1)
      s_cod3 <- round((dummy3 - coding3),2)
      contrasts(fattore)=s_cod3
      contrasts(fattore)
    }else if(input$cont3=="Sum"){
      contrasts(fattore)=contr.sum(length(levels(fattore)))
      contrasts(fattore)
    }else if(input$cont3=="Scaled"){
      contrasts(fattore)=contr.sum(length(levels(fattore)))/length(levels(fattore))
      contrasts(fattore)
    }else if(input$cont3=="Sliding difference"){
      contrasts(fattore)=MASS::contr.sdif(length(levels(fattore)))
      contrasts(fattore)
    }else if(input$cont3=="Helmert"){
      contrasts(fattore)=contr.helmert(length(levels(fattore)))
      contrasts(fattore)
    }else if(input$cont3=="Reverse Helmert"){
      tmp0=contr.helmert(length(levels(fattore)))
      tmp0.1=apply(tmp0,2,rev)
      tmp0.2=tmp0.1[,ncol(tmp0.1):1]
      contrasts(fattore)=tmp0.2
      contrasts(fattore)
    }else if(input$cont3=="Polynomial"){
      contrasts(fattore)=contr.poly(length(levels(fattore)))
      contrasts(fattore)
    }else{
      reati3=unlist(reattivoi3())
      resi3=list()

      for (a in 1:length(reati3)) {
        resi3[[a]]=formula(reati3[a])
      }

      resi3
    }

  })


  ############## Once the contrast matrix for each variable has been created, the following reactive element defines the new contrast matrix.
  ############## Note that such a reactive element is valid only for the default contrasts function of R for the "Suggested for you" options in case of atwo-way interaction
  ############## For the "Fully customized" options, the input is the reactive element called "factor_int()" coded above.
  cont_mat_int=reactive({
    y=mydf()
    size1=length(levels(factor(y[,input$v1])))
    size2=length(levels(factor(y[,input$v2])))
    size3=length(levels(factor(y[,input$v3])))

    var1=factor(y[,input$v1])
    var2=factor(y[,input$v2])

    if(input$radio=="Three way"){
      simdat4=appRiori:::mixedDesign(B=c(size1,size2,size3),W=NULL,n=5,long = T)
      names(simdat4)[1:3]=c(as.character(input$v1),as.character(input$v2),as.character(input$v3))

      levels(simdat4[,1])=levels(factor(y[,input$v1]))
      levels(simdat4[,2])=levels(factor(y[,input$v2]))
      levels(simdat4[,3])=levels(factor(y[,input$v3]))

      if(input$cont1=="Customized"){
        h1=hypr(faktorS1(),levels=levels(factor(y[,input$v1])))
        contrasts(simdat4[,1],how.many=ncomparisons$ihm1)=cmat(h1)
      }else{
        contrasts(simdat4[,1])=faktorS1()
      }

      if(input$cont2=="Customized"){
        h2=hypr(faktorS2(),levels=levels(factor(y[,input$v2])))
        contrasts(simdat4[,2],how.many=ncomparisons$ihm2)=cmat(h2)
      }else{
        contrasts(simdat4[,2])=faktorS2()
      }


      if(input$cont3=="Customized"){
        h3=hypr(faktorS3(),levels=levels(factor(y[,input$v3])))
        contrasts(simdat4[,3],how.many=ncomparisons$ihm3)=cmat(h3)
      }else{
        contrasts(simdat4[,3])=faktorS3()
      }


      Xctr=simdat4%>%
        group_by_(input$v1,input$v2,input$v3)%>%
        summarise()%>%
        model.matrix(formula(paste("~","1+",input$v1,"*",input$v2,"*",input$v3)),.)%>%
        as.data.frame()%>%
        as.matrix()

      rownames(Xctr)=sort(levels(interaction(simdat4[,1],simdat4[,2],simdat4[,3],sep="_",lex.order = F)))
      #colnames(Xctr)[2:length(colnames(Xctr))]=paste("C",1:(ncol(Xctr)-1),sep = "")

      # if(input$onlyI==TRUE){
      #   int_values=(size1-1)+(size2-1)+(size3-1)+2
      #   Xctr=cbind(Xctr[,int_values:ncol(Xctr)])
      #   rownames(Xctr)=sort(levels(interaction(simdat4[,1],simdat4[,2],simdat4[,3],sep="_",lex.order = F)))
      #   colnames(Xctr)=paste("C",1:(ncol(Xctr)),sep = "")
      # }

      Xctr
    }else{
      simdat4=appRiori:::mixedDesign(B=c(size1,size2),W=NULL,n=5,long = T)
      names(simdat4)[1:2]=c(as.character(input$v1),as.character(input$v2))

      levels(simdat4[,1])=levels(factor(y[,input$v1]))
      levels(simdat4[,2])=levels(factor(y[,input$v2]))

      if(input$cont1=="Customized"){
        h1=hypr(faktorS1(),levels=levels(factor(y[,input$v1])))
        contrasts(simdat4[,1],how.many=ncomparisons$ihm1)=cmat(h1)
      }else{
        contrasts(simdat4[,1])=faktorS1()
      }

      if(input$cont2=="Customized"){
        h2=hypr(faktorS2(),levels=levels(factor(y[,input$v2])))
        contrasts(simdat4[,2],how.many=ncomparisons$ihm2)=cmat(h2)
      }else{
        contrasts(simdat4[,2])=faktorS2()
      }

      Xctr=simdat4%>%
        group_by_(input$v1,input$v2)%>%
        summarise()%>%
        model.matrix(formula(paste("~","1+",input$v1,"*",input$v2)),.)%>%
        as.data.frame()%>%
        as.matrix()

      rownames(Xctr)=sort(levels(interaction(simdat4[,1],simdat4[,2],sep="_",lex.order = F)))
      #colnames(Xctr)[2:length(colnames(Xctr))]=paste("C",1:(ncol(Xctr)-1),sep = "")

      # if(input$onlyI==TRUE){
      #   int_values=(size1-1)+(size2-1)+2
      #   Xctr=cbind(Xctr[,int_values:ncol(Xctr)])
      #   rownames(Xctr)=sort(levels(interaction(simdat4[,1],simdat4[,2],sep="_",lex.order = F)))
      #   colnames(Xctr)=paste("C",1:(ncol(Xctr)),sep = "")
      # }
      Xctr
    }



  })


  ############## This code allows to display the default simplified contrast matrix.
  output$original_int=renderPrint({
    tryCatch({
      cont_mat()
    },error=function(e){
      cat("Select different variables!")
    })

  })

  ############## This code allows to display the new contrast matrix.
  output$new_int=renderPrint({
    tryCatch({
      if(input$fc2==TRUE){
        mat=facktor_int()
        colnames(mat)=paste("C",1:(ncol(mat)),sep = "")
        h <- hypr(mat) %>% filler_contrasts(ncomparisons$hm2)
        mat <- cmat(h, as_fractions = FALSE)
        mat <- matrix(mat, nrow = nrow(mat), ncol = ncol(mat), dimnames = dimnames(mat))
        round(mat,2)
      }else{
        cmi=cont_mat_int()
        round(cmi,2)
      }},error=function(e){
        cat("Waiting..")
    })

  })

  ############## This code allows to display the hypothesis matrix related to the new contrast matrix
  output$hypmat_int=renderPrint({
    tryCatch({
      if(input$fc2==TRUE){
        h <- hypr(facktor_int())
        hm=round(hmat(h,as_fractions=FALSE),2)
        rownames(hm)=paste("C",1:(nrow(hm)),sep = "")
        print(t(hm))
      }else{
        h <- hypr(cont_mat_int())
        print(round(t(hmat(h,as_fractions=FALSE)),2))
      }},error=function(e){
        cat("Waiting..")
    })


  })

  ############## This code allows to display the new correlation matrix.
  output$cormat_int=renderPrint({
    tryCatch({
      if(input$fc2==TRUE){
        cm=cor(facktor_int())
        colnames(cm)=paste("C", 1:ncol(cm),sep = "")
        rownames(cm)=paste("C", 1:nrow(cm),sep = "")
        print(round(cm,2))
      }else{
        cm=cor(cont_mat_int())
        cm=cm[-1,-1]
        colnames(cm)=paste("C", 1:ncol(cm),sep = "")
        rownames(cm)=paste("C", 1:nrow(cm),sep = "")
        print(round(cm,2))
      }},error=function(e){
        cat("Waiting..")
    })

  })


  ############# This code generates warning message(s) when not-linear-independent contrasts are set

  dataerrors2 <- reactive({
    tryCatch({
      if(input$fc2==TRUE){
        mat=facktor_int()
        cat("Contrasts are linearly independent")
      }}, error=function(e){
      cat("Waiting..")
    }, warning = function(w) {
      if(grepl("Your hypotheses are not linearly independent", w, fixed = TRUE)){return(w$message)}
    })
  })

  output$contrasts_warnings2 <- renderPrint({
    dataerrors2()
  })
  ############## The following lines generates the "ready-to-use" code corresponding to the solution planned by the user, for each variable of the interaction.
  faktorV1=reactive({
    a=mydf()
    fattore=factor(a[,input$v1])
    if(input$cont1=="Treatment"){
      paste0("contr.treatment(",length(levels(fattore)),")")
    }else if(input$cont1=="Simple"){
      paste("matrix(",(paste("c(",paste(faktorS1(), collapse = ','),")")),",",nrow(faktorS1()),",",ncol(faktorS1()),")")
    }else if(input$cont1=="Sum"){
      paste0("contr.sum(",length(levels(fattore)),")")
    }else if(input$cont1=="Scaled"){
      paste0("contr.sum(",length(levels(fattore)),")","/",length(levels(fattore)))
    }else if(input$cont1=="Sliding difference"){
      paste0("MASS::contr.sdif(",length(levels(fattore)),")")
    }else if(input$cont1=="Helmert"){
      paste0("contr.helmert(",length(levels(fattore)),")")
    }else if(input$cont1=="Reverse Helmert"){
      paste("matrix(",(paste("c(",paste(faktorS1(), collapse = ','),")")),",",nrow(faktorS1()),",",ncol(faktorS1()),")")
    }else if(input$cont1=="Polynomial"){
      paste0("contr.poly(",length(levels(fattore)),")")
    }else{
      h <- hypr(faktorS1(),levels = levels(factor(a[,input$v1])))
      paste("matrix(",(paste("c(",paste(cmat(h), collapse = ','),")")),",",nrow(cmat(h)),",",ncol(cmat(h)),")")
    }

  })

  faktorV2=reactive({
    a=mydf()
    fattore=factor(a[,input$v2])
    if(input$cont2=="Treatment"){
      paste0("contr.treatment(",length(levels(fattore)),")")
    }else if(input$cont2=="Simple"){
      paste("matrix(",(paste("c(",paste(faktorS2(), collapse = ','),")")),",",nrow(faktorS2()),",",ncol(faktorS2()),")")
    }else if(input$cont2=="Sum"){
      paste0("contr.sum(",length(levels(fattore)),")")
    }else if(input$cont2=="Scaled"){
      paste0("contr.sum(",length(levels(fattore)),")","/",length(levels(fattore)))
    }else if(input$cont2=="Sliding difference"){
      paste0("MASS::contr.sdif(",length(levels(fattore)),")")
    }else if(input$cont2=="Helmert"){
      paste0("contr.helmert(",length(levels(fattore)),")")
    }else if(input$cont2=="Reverse Helmert"){
      paste("matrix(",(paste("c(",paste(faktorS2(), collapse = ','),")")),",",nrow(faktorS2()),",",ncol(faktorS2()),")")
    }else if(input$cont2=="Polynomial"){
      paste0("contr.poly(",length(levels(fattore)),")")
    }else{
      h <- hypr(faktorS2(),levels = levels(factor(a[,input$v2])))
      paste("matrix(",(paste("c(",paste(cmat(h), collapse = ','),")")),",",nrow(cmat(h)),",",ncol(cmat(h)),")")
    }

  })

  faktorV3=reactive({
    a=mydf()
    fattore=factor(a[,input$v3])
    if(input$cont3=="Treatment"){
      paste0("contr.treatment(",length(levels(fattore)),")")
    }else if(input$cont3=="Simple"){
      paste("matrix(",(paste("c(",paste(faktorS3(), collapse = ','),")")),",",nrow(faktorS3()),",",ncol(faktorS3()),")")
    }else if(input$cont3=="Sum"){
      paste0("contr.sum(",length(levels(fattore)),")")
    }else if(input$cont3=="Scaled"){
      paste0("contr.sum(",length(levels(fattore)),")","/",length(levels(fattore)))
    }else if(input$cont3=="Sliding difference"){
      paste0("MASS::contr.sdif(",length(levels(fattore)),")")
    }else if(input$cont3=="Helmert"){
      paste0("contr.helmert(",length(levels(fattore)),")")
    }else if(input$cont3=="Reverse Helmert"){
      paste("matrix(",(paste("c(",paste(faktorS3(), collapse = ','),")")),",",nrow(faktorS3()),",",ncol(faktorS3()),")")
    }else if(input$cont3=="Polynomial"){
      paste0("contr.poly(",length(levels(fattore)),")")
    }else{
      h <- hypr(faktorS3(),levels = levels(factor(a[,input$v3])))
      paste("matrix(",(paste("c(",paste(cmat(h), collapse = ','),")")),",",nrow(cmat(h)),",",ncol(cmat(h)),")")
    }

  })


  faktorV1_hypr=reactive({
    a=mydf()
    fattore=factor(a[,input$v1])
    mat=eval(parse(text=faktorV1()))
    rownames(mat)=levels(fattore)
    hypr(mat)
  })

  faktorV2_hypr=reactive({
    a=mydf()
    fattore=factor(a[,input$v2])
    mat=eval(parse(text=faktorV2()))
    rownames(mat)=levels(fattore)
    hypr(mat)
  })

  faktorV3_hypr=reactive({
    a=mydf()
    fattore=factor(a[,input$v3])
    mat=eval(parse(text=faktorV3()))
    rownames(mat)=levels(fattore)
    hypr(mat)
  })

  ##############.. or when a "Fully customized" option has been selected..
   faktor2_int=reactive({
      h <- hypr(facktor_int())
      paste("matrix(",(paste("c(",paste(cmat(h), collapse = ','),")")),",",nrow(cmat(h)),",",ncol(cmat(h)),")")
  })

   ##############.. or when a "Only interaction" option has been selected.
   # faktor3_int=reactive({
   #   tmp=cont_mat_int()
   #   paste("matrix(",(paste("c(",paste(tmp, collapse = ','),")")),",",nrow(tmp),",",ncol(tmp),")")
   # })
   #
   #

 ############## The following lines prints the "ready-to-use" code corresponding to the solution planned by the user.

   output$res_int=renderPrint({
     tryCatch({
    fname=mydfname()
    a=mydf()
    if(is.null(input$radio_output2)){
      cat("Please, select the kind of output you prefer!")
    }else{
    if(input$radio== 'Two way'){
      cat(paste0(fname,"$",input$v1,"=","factor(",fname,"$",input$v1,")"))
      cat(sep = "\n")
      cat(paste0(fname,"$",input$v2,"=","factor(",fname,"$",input$v2,")"))
      cat(sep = "\n")
      if(input$fc2==TRUE){
        if(input$radio_output2=="br"){
        cat(paste0(fname,"$","Planned_interaction","=","interaction(",fname,"$",input$v1,",",
                   fname,"$",input$v2,", sep = '_'",")"))
        cat(sep = "\n")
        cat(paste0("contrasts(",fname,"$","Planned_interaction",",","how.many=",ncomparisons$hm2,")","=",faktor2_int()))
        }else{
        h <- hypr(facktor_int())
        cat(paste0("h <- ", hypr_call(h)))
        cat(sep = "\n")
        cat(paste0("contrasts(",fname,"$","Planned_interaction,","how.many=",ncomparisons$hm2,")",
                   "=","cmat(h)"))}
      }else{
        if(input$radio_output2=="br"){
        if(input$cont1=="Customized"){
          h1 <- hypr(faktorS1(),levels = levels(factor(a[,input$v1])))
          cat(paste0("contrasts(",fname,"$",input$v1,",","how.many=",ncomparisons$ihm1,")","=",faktorV1()))
        }else{
          cat(paste0("contrasts(",fname,"$",input$v1,")","=",faktorV1()))
        }
        cat(sep = "\n")
        if(input$cont2=="Customized"){
          h2 <- hypr(faktorS2(),levels = levels(factor(a[,input$v2])))
          cat(paste0("contrasts(",fname,"$",input$v2,",","how.many=",ncomparisons$ihm2,")","=",faktorV2()))
        }else{
          cat(paste0("contrasts(",fname,"$",input$v2,")","=",faktorV2()))
        }
        }else{
        cat(paste0("h_",input$v1, " <- ", hypr_call(faktorV1_hypr())))
        cat(sep = "\n")
        if(input$cont1=="Customized"){
          cat(paste0("contrasts(",fname,"$",input$v1,", how.many = ",ncomparisons$ihm1,")","=","cmat(h_",input$v1,")"))
        }else{
          cat(paste0("contrasts(",fname,"$",input$v1,")","=","cmat(h_",input$v1,")"))
        }
        cat(sep = "\n")
        cat(paste0("h_",input$v2, " <- ", hypr_call(faktorV2_hypr())))
        cat(sep = "\n")
        if(input$cont2=="Customized"){
          cat(paste0("contrasts(",fname,"$",input$v2,", how.many = ",ncomparisons$ihm2,")","=","cmat(h_",input$v2,")"))
        }else{
          cat(paste0("contrasts(",fname,"$",input$v2,")","=","cmat(h_",input$v2,")"))
        }
        cat(sep = "\n")
       }}

    }else{
      cat(paste0(fname,"$",input$v1,"=","factor(",fname,"$",input$v1,")"))
      cat(sep = "\n")
      cat(paste0(fname,"$",input$v2,"=","factor(",fname,"$",input$v2,")"))
      cat(sep = "\n")
      cat(paste0(fname,"$",input$v3,"=","factor(",fname,"$",input$v3,")"))
      cat(sep = "\n")
      if(input$fc2==TRUE){
        if(input$radio_output2=="br"){
      cat(paste0(fname,"$","Planned_interaction","=","interaction(",fname,"$",input$v1,",",
                 fname,"$",input$v2,",",
                 fname,"$",input$v3,",",
                 ", sep = '_'",")"))
      cat(sep = "\n")
      cat(paste0("contrasts(",fname,"$","Planned_interaction, how.many =",ncomparisons$hm2,")",
                 "=",faktor2_int()))
      }else{
      cat(paste0("h <- ", hypr_call(hypr(facktor_int()))))
      cat(sep = "\n")
      cat(paste0("contrasts(",fname,"$","Planned_interaction, how.many =",ncomparisons$hm2,")",
                 "=","cmat(h)"))}
      }else{
        if(input$radio_output2=="br"){
        if(input$cont1=="Customized"){
          h1 <- hypr(faktorS1(),levels = levels(factor(a[,input$v1])))
          cat(paste0("contrasts(",fname,"$",input$v1,",","how.many=",ncomparisons$ihm1,")","=",faktorV1()))
        }else{
          cat(paste0("contrasts(",fname,"$",input$v1,")","=",faktorV1()))
        }
        cat(sep = "\n")
        if(input$cont2=="Customized"){
          h2 <- hypr(faktorS2(),levels = levels(factor(a[,input$v2])))
          cat(paste0("contrasts(",fname,"$",input$v2,",","how.many=",ncomparisons$ihm2,")","=",faktorV2()))
        }else{
          cat(paste0("contrasts(",fname,"$",input$v2,")","=",faktorV2()))
        }
        cat(sep = "\n")
        if(input$cont3=="Customized"){
          h3 <- hypr(faktorS3(),levels = levels(factor(a[,input$v3])))
          cat(paste0("contrasts(",fname,"$",input$v3,",","how.many=",ncomparisons$ihm3,")","=",faktorV3()))
        }else{
          cat(paste0("contrasts(",fname,"$",input$v3,")","=",faktorV3()))
        }
        }else{
        cat(paste0("h_",input$v1, " <- ", hypr_call(faktorV1_hypr())))
        cat(sep = "\n")
        if(input$cont1=="Customized") {
          cat(paste0("contrasts(",fname,"$",input$v1,", how.many = ",ncomparisons$ihm1,")","=","cmat(h_",input$v1,")"))
        } else {
          cat(paste0("contrasts(",fname,"$",input$v1,")","=","cmat(h_",input$v1,")"))
        }
        cat(sep = "\n")
        cat(paste0("h_",input$v2, " <- ", hypr_call(faktorV2_hypr())))
        cat(sep = "\n")
        if(input$cont2=="Customized") {
          cat(paste0("contrasts(",fname,"$",input$v2,", how.many = ",ncomparisons$ihm2,")","=","cmat(h_",input$v2,")"))
        } else {
          cat(paste0("contrasts(",fname,"$",input$v2,")","=","cmat(h_",input$v2,")"))
        }
        cat(sep = "\n")
        cat(paste0("h_",input$v3, " <- ", hypr_call(faktorV3_hypr())))
        cat(sep = "\n")
        if(input$cont2=="Customized") {
          cat(paste0("contrasts(",fname,"$",input$v3,", how.many = ",ncomparisons$ihm3,")","=","cmat(h_",input$v3,")"))
        } else {
          cat(paste0("contrasts(",fname,"$",input$v3,")","=","cmat(h_",input$v3,")"))
        }
       }}
    }
     }},error=function(e){
       cat("It seems that something is missing. Complete the previous step!")
     })


  })

   output$selection=renderPrint({
     a=mydf()
     fattore=factor(a[,input$in1])
     if(input$cont=="Treatment"){
       cat(paste0("You selected ",input$cont," contrasts."))
       cat(sep = "\n")
       cat(paste0("Your final comparisons will be: ",length(levels(fattore))-1))
       cat(sep = "\n")
       cat(paste0("With Treatment contrasts, you can compare your n-1 levels with a reference level."))
       cat(sep = "\n")
       cat(paste0("Remember: in this case, the reference level is: ",levels(fattore)[1]))
     }else if(input$cont=="Simple"){
       cat(paste0("You selected ",input$cont," contrasts."))
       cat(sep = "\n")
       cat(paste0("Your final comparisons will be: ",length(levels(fattore))-1))
       cat(sep = "\n")
       cat(paste0("With Simple contrasts, you can compare your n-1 levels with a reference level."))
       cat(sep = "\n")
       cat(paste0("These contrasts are centered!"))
       cat(sep = "\n")
       cat(paste0("Remember: in this case, the reference level is: ",levels(fattore)[1]))
     }else if(input$cont=="Sum"){
       cat(paste0("You selected ",input$cont," contrasts."))
       cat(sep = "\n")
       cat(paste0("Your final comparisons will be: ",length(levels(fattore))-1))
       cat(sep = "\n")
       cat(paste0("With Sum contrasts, you can compare your n-1 levels with the grand mean of the observations."))
       cat(sep = "\n")
       cat(paste0("Remember: in this case, the level selected to represent the grand mean is: ",levels(fattore)[length(levels(fattore))]))
     }else if(input$cont=="Scaled"){
       cat(paste0("You selected ",input$cont," contrasts."))
       cat(sep = "\n")
       cat(paste0("Your final comparisons will be: ",length(levels(fattore))-1))
       cat(sep = "\n")
       cat(paste0("With Scaled Sum contrasts, you can compare your n-1 levels with the grand mean of the observations."))
       cat(sep = "\n")
       cat(paste0("These contrasts are scaled!"))
       cat(sep = "\n")
       cat(paste0("Remember: in this case, the level selected to represent the grand mean is: ",levels(fattore)[length(levels(fattore))]))
     }else if(input$cont=="Sliding difference"){
       cat(paste0("You selected ",input$cont," contrasts."))
       cat(sep = "\n")
       cat(paste0("Your final comparisons will be: ",length(levels(fattore))-1))
       cat(sep = "\n")
       cat(paste0("With Repeated contrasts, you can compare each level with its subsequent neighbor."))
     }else if(input$cont=="Helmert"){
       cat(paste0("You selected ",input$cont," contrasts."))
       cat(sep = "\n")
       cat(paste0("Your final comparisons will be: ",length(levels(fattore))-1))
       cat(sep = "\n")
       cat(paste0("With Helmert contrasts, you can compare each level with the average of all its preceding levels."))
       cat(sep = "\n")
       cat(paste0("These contrasts are orthogonal!"))
     }else if(input$cont=="Reverse Helmert"){
       cat(paste0("You selected ",input$cont," contrasts."))
       cat(sep = "\n")
       cat(paste0("Your final comparisons will be: ",length(levels(fattore))-1))
       cat(sep = "\n")
       cat(paste0("With Reverse Helmert contrasts, you can compare each level with the average of all its subsequent levels."))
       cat(sep = "\n")
       cat(paste0("These contrasts are orthogonal!"))
     }else if(input$cont=="Polynomial"){
       cat(paste0("You selected ",input$cont," contrasts."))
       cat(sep = "\n")
       cat(paste0("Your final comparisons will be: ",length(levels(fattore))-1))
       cat(sep = "\n")
       cat(paste0("With Polynomial contrasts, you can shape the trend of your levels."))
       cat(sep = "\n")
       cat(paste0("These contrasts are orthogonal!"))
     }else{
       cat(paste0("You selected ",input$cont," contrasts."))
       cat(sep = "\n")
       cat(paste0("Your final comparisons will be: ",as.numeric(input$hm1)))
       cat(sep = "\n")
       cat(paste0("With Customized contrasts, you can compare the levels you want in the way you want."))
       cat(sep = "\n")
       cat(paste0("These contrasts are orthogonal!"))
     }
   })

   output$selection2=renderPrint({
     tryCatch({
       if(input$fc2==TRUE){
         mat=facktor_int()
         cat(paste0("You selected fully customazized interaction contrasts."))
         cat(sep = "\n")
         cat(sep = "\n")
         cat(paste0("With fully customazized contrasts, you considering the factorial design as one-way combination variable, also known as linearization of the design. In other words, the levels of all variables are combined to define a unique variable with a number of levels equal to the product of the starting variables' levels."))
         cat(sep = "\n")
         cat(paste0("Your final comparisons will be: ",ncol(mat)))
       }else{
         mat2=cont_mat_int()
         cat(paste0("For your first variable, you selected ",input$cont1," contrasts."))
         cat(sep = "\n")
         cat(paste0("For your second variable, you selected ",input$cont2," contrasts."))
         if(input$radio=="Three way"){
           cat(sep = "\n")
           cat(paste0("For your third variable, you selected ",input$cont3," contrasts."))
         }
         cat(sep = "\n")
         if(input$radio=="Two way"){
         cat(paste0("Remember that, from column C1 to column C",ncol(faktorS1()), " you are examining the comparisons related to the first variable"))
         cat(sep = "\n")
         cat(paste0("Remember that, from column C",ncol(faktorS1())+1, " to column C",ncol(faktorS1())+ncol(faktorS2()), " you are examining the comparison related to the second variable"))
         cat(sep = "\n")
         cat(paste0("Remember that, the last ",ncol(mat2)-(ncol(faktorS1())+ncol(faktorS2()))-1, " columns refer to the interaction comparisons"))
         }else{
           cat(paste0("Remember that, from column C1 to column C",ncol(faktorS1()), " you are examining the comparisons related to the first variable"))
           cat(sep = "\n")
           cat(paste0("Remember that, from column C",ncol(faktorS1())+1, " to column C",ncol(faktorS1())+ncol(faktorS2()), " you are examining the comparison related to the second variable"))
           cat(sep = "\n")
           cat(paste0("Remember that, from column C",ncol(faktorS1())+ncol(faktorS2())+1, " to column C",ncol(faktorS1())+ncol(faktorS2())+ncol(faktorS3()), " you are examining the comparison related to the third variable"))
           cat(sep = "\n")
           cat(paste0("Remember that, the last ",ncol(mat2)-(ncol(faktorS1())+ncol(faktorS2())+ncol(faktorS3()))-1, " columns refer to the interaction comparisons"))
         }
         cat(sep = "\n")
         cat(paste0("Interpret the model coefficients accordingly!"))
       }},error=function(e){
         # cat("Waiting..")
       })
   })


}
