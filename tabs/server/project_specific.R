

assigned_projects <- reactive({
  username <- input$userName
  user_projects <- tbl(pool, "tblUserInformation") %>% collect() %>%
    dplyr::filter(UserName == username)
  
   pjs <- stringi::stri_remove_empty(c(ifelse(grepl("YES",user_projects$Project1[1])==T, "IBBTV",''),
            ifelse(grepl("YES",user_projects$Project2[1])==T, "IBXW",''),
            ifelse(grepl("YES",user_projects$Project3[1])==T, "IBSV",''),
            ifelse(grepl("YES",user_projects$Project4[1])==T, "Nematode",'')
           ))
   pjs
})

output$select_project_specific_bttn <- renderUI({
  div(br(), br(),
  radioGroupButtons(inputId = "project_selected",label = "", size = "lg",width = "75%",#status = "primary",
    choices = assigned_projects(), individual = TRUE, justified = TRUE, 
    checkIcon = list(yes = icon("ok", lib = "glyphicon"))
  ),br())
})


## Vector Inventory Data

vector_inventory <- reactive({
  tb <- paste0("IBBTV", "_tblVectorInventory")
  
  # rearrange columns by length
  sql <- paste("SELECT
  Cassette1ForwardCode,Cassette1ReverseCode,Cassette2ForwardCode,Cassette2ReverseCode,Cassette3ForwardCode,Cassette3ReverseCode,Cassette4ForwardCode,Cassette4ReverseCode,Cassette5ForwardCode,
  Cassette5ReverseCode,Cassette6ForwardCode,Cassette6ReverseCode,Cassette7ForwardCode,Cassette7ReverseCode,Cassette8ForwardCode,Cassette8ReverseCode,Cassette9ForwardCode,Cassette9ReverseCode,
  Cassette10ForwardCode,Cassette10ReverseCode,VectorID,VectorPrefix,VectorCode,VectorSuffix,BacterialSelection,PlantSelection,Synonym1,Synonym2,Synonym3,Synonym4,Synonym5,Backbone,Cassette1Promoter,
  Cassette1Gene,Cassette1Terminator,Cassette1Feature1,Cassette1Feature1Desc,Cassette1Feature2,Cassette1Feature2Desc,Cassette1Feature3,Cassette1Feature3Desc,Cassette2Promoter,Cassette2Gene,
  Cassette2Terminator,Cassette2Feature1,Cassette2Feature1Desc,Cassette2Feature2,Cassette2Feature2Desc,Cassette2Feature3,Cassette2Feature3Desc,Cassette3Promoter,Cassette3Gene,Cassette3Terminator,
  Cassette3Feature1,Cassette3Feature1Desc,Cassette3Feature2,Cassette3Feature2Desc,Cassette3Feature3,Cassette3Feature3Desc,Cassette4Promoter,Cassette4Gene,Cassette4Terminator,Cassette4Feature1,
  Cassette4Feature1Desc,Cassette4Feature2,Cassette4Feature2Desc,Cassette4Feature3,Cassette4Feature3Desc,Cassette5Promoter,Cassette5Gene,Cassette5Terminator,Cassette5Feature1,Cassette5Feature1Desc,
  Cassette5Feature2,Cassette5Feature2Desc,Cassette5Feature3,Cassette5Feature3Desc,ClonedBy,ClonedDate,LabBookNumber,PageNumber,Cassette1ForwardName,Cassette1ForwardSequence,Cassette1ReverseName,
  Cassette1ReverseSequence,Cassette2ForwardName,Cassette2ForwardSequence,Cassette2ReverseName,Cassette2ReverseSequence,Cassette3ForwardName,Cassette3ForwardSequence,Cassette3ReverseName,
  Cassette3ReverseSequence,Cassette4ForwardName,Cassette4ForwardSequence,Cassette4ReverseName,Cassette4ReverseSequence,Cassette5ForwardName,Cassette5ForwardSequence,Cassette5ReverseName,
  Cassette5ReverseSequence,SequencingCompleted,DateOfSequencing,SeqPrimersLabBookNumber,SeqPrimersPageNumber,CheckedBy,CheckedDate,VerifiedBy,VerifiedDate,TranformedIntoAgro,Strain,ConfirmedByPCR,
  ConfirmedByPCRDate,DNAStorageLocation,DNAStorageBox,DNAStoredBy,DNAStorageDate,EColiGlycerolStorageLocation,EColiGlycerolStorageBox,EColiGlycerolStoredBy,EColiGlycerolStorageDate,AgroGlycerolStorageLocation,
  AgroGlycerolStorageBox,AgroGlycerolStoredBy,AgroGlycerolStorageDate,Cassette6Promoter,Cassette6Gene,Cassette6Terminator,Cassette6Feature1,Cassette6Feature1Desc,Cassette6Feature2,Cassette6Feature2Desc,
  Cassette6Feature3,Cassette6Feature3Desc,Cassette7Promoter,Cassette7Gene,Cassette7Terminator,Cassette7Feature1,Cassette7Feature1Desc,Cassette7Feature2,Cassette7Feature2Desc,Cassette7Feature3,
  Cassette7Feature3Desc,Cassette8Promoter,Cassette8Gene,Cassette8Terminator,Cassette8Feature1,Cassette8Feature1Desc,Cassette8Feature2,Cassette8Feature2Desc,Cassette8Feature3,Cassette8Feature3Desc,
  Cassette9Promoter,Cassette9Gene,Cassette9Terminator,Cassette9Feature1,Cassette9Feature1Desc,Cassette9Feature2,Cassette9Feature2Desc,Cassette9Feature3,Cassette9Feature3Desc,Cassette10Promoter,
  Cassette10Gene,Cassette10Terminator,Cassette10Feature1,Cassette10Feature1Desc,Cassette10Feature2,Cassette10Feature2Desc,Cassette10Feature3,Cassette10Feature3Desc,Cassette6ForwardName,Cassette6ForwardSequence,
  Cassette6ReverseName,Cassette6ReverseSequence,Cassette7ForwardName,Cassette7ForwardSequence,Cassette7ReverseName,Cassette7ReverseSequence,Cassette8ForwardName,Cassette8ForwardSequence,Cassette8ReverseName,
  Cassette8ReverseSequence,Cassette9ForwardName,Cassette9ForwardSequence,Cassette9ReverseName,Cassette9ReverseSequence,Cassette10ForwardName,Cassette10ForwardSequence,Cassette10ReverseName,Cassette10ReverseSequence,
  SequencingPrimers,ContigExpressSequencingAlignment,SequencingFiles,VNTI_Map_Location
  FROM ",tb)
  
  dt <- dbGetQuery(pool,sql)
  
  dt %>%
    dplyr::select(VectorID,VectorPrefix,VectorCode,VectorSuffix,BacterialSelection,PlantSelection,Synonym1,Synonym2,Synonym3,Synonym4,Synonym5,
                  Backbone,Cassette1Promoter,Cassette1Gene,Cassette1Terminator,Cassette1Feature1,Cassette1Feature1Desc,Cassette1Feature2,Cassette1Feature2Desc,
                  Cassette1Feature3,Cassette1Feature3Desc,Cassette2Promoter,Cassette2Gene,Cassette2Terminator,Cassette2Feature1,Cassette2Feature1Desc,
                  Cassette2Feature2,Cassette2Feature2Desc,Cassette2Feature3,Cassette2Feature3Desc,Cassette3Promoter,Cassette3Gene,Cassette3Terminator,
                  Cassette3Feature1,Cassette3Feature1Desc,Cassette3Feature2,Cassette3Feature2Desc,Cassette3Feature3,Cassette3Feature3Desc,Cassette4Promoter,
                  Cassette4Gene,Cassette4Terminator,Cassette4Feature1,Cassette4Feature1Desc,Cassette4Feature2,Cassette4Feature2Desc,Cassette4Feature3,
                  Cassette4Feature3Desc,Cassette5Promoter,Cassette5Gene,Cassette5Terminator,Cassette5Feature1,Cassette5Feature1Desc,Cassette5Feature2,
                  Cassette5Feature2Desc,Cassette5Feature3,Cassette5Feature3Desc,VNTI_Map_Location,ClonedBy,ClonedDate,LabBookNumber,PageNumber,
                  Cassette1ForwardName,Cassette1ForwardCode,Cassette1ForwardSequence,Cassette1ReverseName,Cassette1ReverseCode,Cassette1ReverseSequence,
                  Cassette2ForwardName,Cassette2ForwardCode,Cassette2ForwardSequence,Cassette2ReverseName,Cassette2ReverseCode,Cassette2ReverseSequence,
                  Cassette3ForwardName,Cassette3ForwardCode,Cassette3ForwardSequence,Cassette3ReverseName,Cassette3ReverseCode,Cassette3ReverseSequence,
                  Cassette4ForwardName,Cassette4ForwardCode,Cassette4ForwardSequence,Cassette4ReverseName,Cassette4ReverseCode,Cassette4ReverseSequence,
                  Cassette5ForwardName,Cassette5ForwardCode,Cassette5ForwardSequence,Cassette5ReverseName,Cassette5ReverseCode,Cassette5ReverseSequence,
                  SequencingPrimers,SequencingCompleted,DateOfSequencing,SeqPrimersLabBookNumber,SeqPrimersPageNumber,ContigExpressSequencingAlignment,
                  SequencingFiles,CheckedBy,CheckedDate,VerifiedBy,VerifiedDate,TranformedIntoAgro,Strain,ConfirmedByPCR,ConfirmedByPCRDate,
                  DNAStorageLocation,DNAStorageBox,DNAStoredBy,DNAStorageDate,EColiGlycerolStorageLocation,EColiGlycerolStorageBox,EColiGlycerolStoredBy,
                  EColiGlycerolStorageDate,AgroGlycerolStorageLocation,AgroGlycerolStorageBox,AgroGlycerolStoredBy,AgroGlycerolStorageDate,Cassette6Promoter,
                  Cassette6Gene,Cassette6Terminator,Cassette6Feature1,Cassette6Feature1Desc,Cassette6Feature2,Cassette6Feature2Desc,Cassette6Feature3,
                  Cassette6Feature3Desc,Cassette7Promoter,Cassette7Gene,Cassette7Terminator,Cassette7Feature1,Cassette7Feature1Desc,Cassette7Feature2,
                  Cassette7Feature2Desc,Cassette7Feature3,Cassette7Feature3Desc,Cassette8Promoter,Cassette8Gene,Cassette8Terminator,Cassette8Feature1,
                  Cassette8Feature1Desc,Cassette8Feature2,Cassette8Feature2Desc,Cassette8Feature3,Cassette8Feature3Desc,Cassette9Promoter,Cassette9Gene,
                  Cassette9Terminator,Cassette9Feature1,Cassette9Feature1Desc,Cassette9Feature2,Cassette9Feature2Desc,Cassette9Feature3,Cassette9Feature3Desc,
                  Cassette10Promoter,Cassette10Gene,Cassette10Terminator,Cassette10Feature1,Cassette10Feature1Desc,Cassette10Feature2,Cassette10Feature2Desc,
                  Cassette10Feature3,Cassette10Feature3Desc,Cassette6ForwardName,Cassette6ForwardCode,Cassette6ForwardSequence,Cassette6ReverseName,
                  Cassette6ReverseCode,Cassette6ReverseSequence,Cassette7ForwardName,Cassette7ForwardCode,Cassette7ForwardSequence,Cassette7ReverseName,
                  Cassette7ReverseCode,Cassette7ReverseSequence,Cassette8ForwardName,Cassette8ForwardCode,Cassette8ForwardSequence,Cassette8ReverseName,
                  Cassette8ReverseCode,Cassette8ReverseSequence,Cassette9ForwardName,Cassette9ForwardCode,Cassette9ForwardSequence,Cassette9ReverseName,
                  Cassette9ReverseCode,Cassette9ReverseSequence,Cassette10ForwardName,Cassette10ForwardCode,Cassette10ForwardSequence,Cassette10ReverseName,
                  Cassette10ReverseCode,Cassette10ReverseSequence)
})

vector_inventory_values <- reactiveValues()

## 1. ------------DATA FORM---------------------------------------
source("tabs/server/project_specific/data_form.R", local=T)

## 2. ------------ADMINISTRATION -------------------------------------
source("tabs/server/project_specific/administration.R", local=T)

## 3. ------------REPORTS --------------------------------------------
source("tabs/server/project_specific/reports.R", local=T)

## 4. ------------USER DETAILS --------------------------------------
source("tabs/server/project_specific/user_details.R", local=T)
