# ajouter les réponses executer de mon coté au mail pour vérif
############


correction_generator <- function(path){

path <- file.path(paste0("data/groupe_",numero_gp),"correction")


# base exercises
correction_eval.env <- new.env()

source(file.path(path,"correction.R"), local=correction_eval.env,chdir = TRUE)


path_to_submit_answer <- list.files(file.path(path,"etu_file"),
                                    pattern = "\\.R$",
                                    full.names = TRUE)



# Extract plain text
correction_text <- extract_text_from_exercise(file.path(path,"correction.R"),file.path(path,"Exercie.R"))
answer_text <- extract_text_from_exercise(path_to_submit_answer,
                                          file.path(path,"Exercie.R"))


# Eval line by line student code
error_list <- character()
exercice_eval.env <- new.env()
ll <- parse(file = path_to_submit_answer,  encoding="UTF-8")
for (i in seq_along(ll)) {
  tryCatch(eval(ll[[i]], envir=exercice_eval.env), 
           error = function(e) {error_list[paste0("Question_",i)] <<- as.character(e)}
           )
}

# Total number of Question to handle bad ordering question or missing question
nb_of_question <- max(as.numeric(na.omit(str_extract(ls(envir = correction_eval.env),"(?<=Question_)\\d+(?=(_c)?)") ) ))



# Use total number of Question to add missing and reorder
error_return_with_Q <- character(nb_of_question)
names(error_return_with_Q) <- paste0("Question_",1:nb_of_question) 
error_return_with_Q[names(error_list)] <- error_list


# Use total number of Question to add missing and reorder
extract_exercice <- env_extract(exercice_eval.env,nb_of_question)
extract_correction <- env_extract(correction_eval.env,nb_of_question)




# Comparisson
compar_with_cor <- lapply(seq_along(extract_correction),
  function(x) reponse_comparison(extract_correction[[x]],extract_exercice[[x]])) %>% unlist()


browser()

Correction_df <- data.frame(Numero_question = 1:nb_of_question,
                            Reponse_etu = answer_text$answer_text,
                            Correction = correction_text$answer_text,
                            Is_TRUE = compar_with_cor,
                            Reponse_num_etu = paste0(extract_exercice,collapse = "\n"),
                            Correction_num = paste0(extract_correction,collapse = "\n"),
                            Associate_commentary = answer_text$comm_answer,
                            error = error_return_with_Q)

}

# write.xlsx(
#   Correction_df,
#   "retour/Correction_file.xlsx",
#   sheetName = "Sheet1",
#   col.names = TRUE,
#   row.names = TRUE,
#   append = FALSE,
#   showNA = TRUE,
#   password = NULL
# )
# wb <- loadWorkbook("retour/Correction_file.xlsx")
# sheets <- getSheets(wb)
# # autosize column widths
# autoSizeColumn(sheets[[1]], colIndex=1:ncol(Correction_df))
# saveWorkbook(wb,"retour/Correction_file.xlsx")
