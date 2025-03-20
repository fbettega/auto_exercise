library(tidyverse)

source("source.R", encoding = "utf-8")




# pour les plots juste mes commenté 
# ajouter une quali







Subject_generator <- function(num_grp,
                              question_stat_desc = 0,
                              question_stat_desc_group = 3,
                              question_test = 10){
 
  # set.seed(seed_fun)
  # dir.create(paste0("data/groupe_",num_grp,"/correction"), showWarnings = FALSE,recursive = TRUE)
  # dir.create(paste0("data/groupe_",num_grp,"/exercice"), showWarnings = FALSE,recursive = TRUE)
  # cat(code_fun,file=paste0("data/groupe_",num_grp,"/exercice/votre_code",".txt"),sep="\n")
  # # TO DO
  # # find more question for qualitative var
  # # Test paired test need add some simulation
  # # improve test against another modality for more distance beetween modalities
  # # ajout question de R genre select/filter
  # # two sided test
  # # plot question
  # # rajouter test chisq impossible ?$
  # # Varying alpha



  df_final <- read.csv("Simulated_data/data_set_cc.csv")
  nb_of_questions <- question_stat_desc + question_stat_desc_group + question_test


  quantitative_var <- c("age", "taille", "poids", "IAH","Epworth" ,"Depression" )
  qualitative_var <- c("sexe", "depression","refluxGastro","hypercholesterolemie","bmi","asthme" ,"bcpo" ,
                       "hypertensionArterielle","hypercholesterolemie","glaucome","arteriopathie")




  decile <- sample(1:9,1) * 10


  possible_quantile <- paste0("le quantile ", decile, " %")
  names(possible_quantile) <- paste0("Quantile_", decile)
  
  IC_alpha <- sample(c(99,98,95),2)
  possible_IC <-  paste0( "l'intervalle de confiance à ", IC_alpha, " % de la moyenne ")  
  names(possible_IC) <- paste0("IC_", IC_alpha)
  
  possible_descriptive_function_quant_uni <- list(
    mean = "la moyenne",
    median = "la médiane",
    min = "le minimum",
    max = "le maximum",
    sd = "l'écart-type",
    var = "la variance",
    # Mode = 'le mode',
    IQR = "l'écart interquartile"#,
    #amplitude = "l'amplitude"
  ) %>% append(possible_quantile) %>% append(possible_IC)

  possible_descriptive_function_quant_bi <- list(
    # cor = "le coefficient de corrélation entre",
    #cov = "la covariance entre"
  ) 

  possible_descriptive_function_quali <- list(table = "présenter les effectifs par classe")



  data_set_description <- paste0(c("# Présentation des données",
                                   "Le fichier OSA.csv décrit 103 patients venus consulter pour des troubles du sommeil.",
                                   "Les variables disponibles sont les suivantes : \n",
                                   "* age : l'âge des patients  ",
                                   "* sexe : le sexe des patients 1 Homme 2 Femme",
                                   "* taille : la taille en cm",
                                   "* poids : le poids en kg",
                                   "* hypercholesterolemie : la présence d'hypercholestérolémie (1)",
                                   "* IAH : l'IAH par heure (nombres d'apnées et/ou hypopnées par heure)",
                                   "* refluxGastro : reflux gastro-œsophagien (1)",
                                   "* depression : l'existence d'antécédents de dépression (1)  ",
                                   "* bmi : L'IMC en catégorie  (Normal < 25, overweight ]25 ; 30], obese moderate ]30 ; 35], obese severe ]35 ; 40], obese very severe > 40)",
                                   "* asthme : l'existence d'antécédents d'asthme (1)  ",
                                   "* bcpo : l'existence d'antécédents de Bronchopneumopathie chronique obstructive (1)  ",
                                   "* hypertensionArterielle : l'existence d'antécédents d'hypertension arterielle (1)  ",
                                   "* hypercholesterolemie : l'existence d'antécédents d'hypercholesterolemie (1)  ",
                                   "* glaucome : l'existence d'antécédents de glaucome (1)  ",
                                   "* arteriopathie : l'existence d'antécédents d'arteriopathie (1)  ",
                                   "* Depression : L'échelle de dépression de pichot (0-13) 0 : non déprimé ,13 : très déprimé  ",
                                   "* Epworth : L'échelle de somnolence d'Epworth (0-24) 0 : non somnolent ,24 : très somnolent ",
                                   "\nLes variables quantitatives sont toutes potentiellement « variables d'intérêts » ; les ",
                                   "variables catégorielles vont servir à définir les sous populations.",
                                   "\nUne réponse ne sera considérée comme correcte que si vous répondez avec la/les commande(s) R adaptées ainsi que la/les valeur(s) numérique(s) arrondie(s) à 2 chiffres significatifs",
                                   "\nAttention, pour chaque test il est obligatoire de présenter les données (Vérifier la présence d'outliers), définir les hypothèses, les conditions d'application et de conclure. Précisez nous lorsque vous faites un Student si la statistique suit une loi Normale ou une loi de Student. Quand cela n'est pas précisé les tests statistiques sont à réaliser au seuil $\\alpha = 5$ %.",
                                   "\nDans le cas où une donnée semble fausse, il est fortement recommandé de retirer la ligne complète.",
                                   "\nLe sujet est volontairement très long afin de vous laisser le droit à l'erreur et de choisir vos questions. La notation tient compte de cette longueur, **pas de panique**.",
                                   "\n\\\n\\\n\\\n\\\n\\\n\\\n\\\n"),
                                 collapse =   "  \n" 
                                 )
  
  

  
  ################################################################################
  ###############            Question                                       ######
  ################################################################################
  cat(paste0("---
title: \"Contrôle continu statistiques numero : ", paste0(num_grp),"\"
author: \"Nom :    \\nPrénom:\"   
output: pdf_document
---"),
    "\n\n", data_set_description,
    file = (con <- file(paste0("data/" ,"CC_",num_grp,".Rmd"), "w",
      encoding = "UTF-8"
    )), sep = "\n"
  )
  close(con)


 #write.csv(df_final, paste0("data/groupe_", num_grp, "/exercice/jeux_de_donne.csv"))


  test_a_faire <- c("tt", "other_var","norma","chisq","chisq_ajustment")
  number_of_ttest <- 0
  for (i in 1:nb_of_questions) {
    # always same form of answer
    answer_fields <- ""
    if(!exists("already_select_fun_univariate")) already_select_fun_univariate <- c()
    all_descriptive_function <- c(
      possible_descriptive_function_quant_uni,
      possible_descriptive_function_quant_bi,
      possible_descriptive_function_quali
    )
    
    
    possible_descriptive_function <- all_descriptive_function[all_descriptive_function %notin% already_select_fun_univariate]
    
    if (i <= question_stat_desc) {
      # Univariate question
      ################################################################################
      ###############            Question                                       ######
      ################################################################################

      
      fun <- sample(possible_descriptive_function, 1)
      
      if (fun %in% possible_descriptive_function_quant_bi) {
        var <- sample(df_final %>% select(all_of(quantitative_var)) %>% colnames(), 2)
        statement <- paste0(
          "\n\n# Question ", i, " : \n","Calculer ", fun,
          " entre les colonnes ", paste0(var, collapse = " et "), "\n"
        )
      } else if (fun %in% possible_descriptive_function_quali) {
        var <- sample(df_final %>% select(all_of(qualitative_var)) %>% colnames(), 1)
        statement <- paste0("\n\n# Question ", i, " :  \n Pour la colonne ", var, " ", fun, "\n")
      } else {
        var <- sample(df_final %>% select(all_of(quantitative_var)) %>% colnames(), 1)
        statement <- paste0(
          "\n\n# Question ", i," : \n","Calculer ", fun, " de la colonne ", var, "\n"
        )
      }



      Question <- paste0(
        statement,
        answer_fields
      )

      cat(Question, "\n\\\n\\\n\\\n\\\n\\\n\\\n\\\n",
        file = (con <- file(paste0("data/" ,"CC_",num_grp,".Rmd"), "a", encoding = "UTF-8")),
        sep = "\n", append = TRUE
      )
      close(con)
      already_select_fun_univariate <- c(already_select_fun_univariate, fun)
    } else if (i <= question_stat_desc + question_stat_desc_group) {

      ################################################################################
      ###############            Question                                       ######
      ################################################################################

      if(!exists("already_select_fun_group")) already_select_fun_group <- c()
      
      
      
      possible_descriptive_function_group <- possible_descriptive_function[possible_descriptive_function %notin% c(already_select_fun_group,already_select_fun_univariate)]


      
      fun <- sample(possible_descriptive_function_group, 1)
      var_quanti <- sample(df_final %>% select(all_of(quantitative_var)) %>% colnames(), 1)
      var_quali_name <- sample(df_final %>%
        select(all_of(qualitative_var)) %>%
        colnames(), 1)

      var_quali <- df_final %>%
        select(all_of(var_quali_name)) %>%
        unlist() %>%
        as.character() %>%
        sample(1)

      
      fun <- sample(possible_descriptive_function_group, 1)
      
      if (fun %in% possible_descriptive_function_quant_bi) {
        var <- sample(df_final %>% select(all_of(quantitative_var)) %>% colnames(), 2)
        statement <- paste0(
          "\n\n# Question ", i, " : \n","Calculer ", fun,
          " entre les colonnes ", paste0(var, collapse = " et "), "\n", 
          " uniquement pour les lignes où la colonne ",
          var_quali_name, " prend la valeur ", var_quali, "\n"
        )
        
      } else if (fun %in% possible_descriptive_function_quali) {
        var <- sample(df_final %>% select(all_of(qualitative_var)) %>% colnames(), 1)
        statement <- paste0("\n\n# Question ", i, " :  \n Pour la colonne ", var, " ", fun, "\n",
                            " uniquement pour les lignes où la colonne ",
                            var_quali_name, " prend la valeur ", var_quali, "\n")
      } else {
        var <- sample(df_final %>% select(all_of(quantitative_var)) %>% colnames(), 1)
        statement <- paste0(
          "\n\n# Question ", i," : \n","Calculer ", fun, " de la colonne ", var_quanti,
          "\n", " uniquement pour les lignes où la colonne ",
          var_quali_name, " prend la valeur ", var_quali, "\n"
        )
      }
      
      
      
      

      Question <- paste0(statement, "\n\n", answer_fields)




      cat(Question, "\n\\vspace{5cm} ", 
          file = (con <- file(paste0("data/" ,"CC_",num_grp,".Rmd"),
                              "a", encoding = "UTF-8")),
          sep = "\n", append = TRUE)
      close(con)
      already_select_fun_group <- c(already_select_fun_group, fun)


      # Test question
    } else {
      
      
      if(!exists("already_select_var_ttest"))  already_select_var_ttest <- c()
      
      print(length(already_select_var_ttest))
      
      if (length(already_select_var_ttest) == length(quantitative_var) -1) already_select_var_ttest <- c()


      print(length(already_select_var_ttest))


      # Bug avec l'IAH
 
  
        var_quali_name_test <- sample(df_final %>%
          select(all_of(qualitative_var)) %>%
          colnames(), 1)
        var_quali_moda_test <- df_final %>%
          select(-all_of(already_select_var_ttest)) %>%
          select(all_of(var_quali_name_test)) %>%
          unlist() %>%
          as.character() %>%
          sample(1)
        

        


        test_en_cours <- sample(test_a_faire,1) 
        test_a_faire <- test_a_faire[test_a_faire !=test_en_cours]
        
        if (length(test_a_faire) == 0 ) {
          test_a_faire <- c("tt", "other_var","norma","chisq","chisq_ajustment")
          }
        
        if ("tt" == test_en_cours) {
      
          
          
          
          var_quanti_test <- sample(df_final %>%
                                      select(-all_of(already_select_var_ttest)) %>%
                                      select(any_of(quantitative_var)) %>% colnames(), 1)
          
          already_select_var_ttest <- c(already_select_var_ttest, var_quanti_test)
          
          
          
          subset_quanti <- df_final[df_final[, var_quali_name_test] == var_quali_moda_test, var_quanti_test]
          statement <- paste0(
            "\n\n# Question ", i," : \n Tester si la colonne ",
            var_quanti_test, "\n",
            " uniquement pour les individus ayant la modalité ",
            var_quali_moda_test, " dans la colonne ", var_quali_name_test, "\n",
            " est différente de ",
            signif(
              mean(subset_quanti) +
                rnorm(1,
                  mean = mean(subset_quanti) * 0.05,
                  sd = 0.2
                ),
              4
            ),
            "\n"
          )
          
          
          
          ###############################################################################
          ### comparison between two vars
        } else if("other_var" == test_en_cours) {
          var_quanti_test <- sample(df_final %>%
                                      select(-all_of(already_select_var_ttest)) %>%
                                      select(any_of(quantitative_var)) %>% colnames(), 1)
          already_select_var_ttest <- c(already_select_var_ttest, var_quanti_test)
          
          
          # force l'ajout d'un kruskal 
          if (number_of_ttest == 0) {
            
            var_quanti_test <- sample(df_final %>%
                                        select(-all_of(already_select_var_ttest)) %>%
                                        select(any_of(quantitative_var)) %>% colnames(), 1)
            
            already_select_var_ttest <- c(already_select_var_ttest, var_quanti_test)
            
            subset_quanti <- df_final[df_final[, var_quali_name_test] == var_quali_moda_test, var_quanti_test]
            statement <- paste0(
              "\n\n# Question ", i," : \n Tester si la colonne ",
              var_quanti_test, "\n",
              " est différente selon les modalités de la variable bmi",
               "\n"
            )
            
            
            
          } else {
          var_quali_moda_test_2 <- df_final %>%
            filter(!!rlang::sym(var_quali_name_test) != var_quali_moda_test) %>%
            select(all_of(var_quali_name_test)) %>%
            unlist() %>%
            as.character() %>%
            sample(1)
          
          

          statement <- paste0(
            "\n\n# Question ", i," : \n Tester si la colonne ",
            var_quanti_test, "\n",
            "pour les individus ayant la modalité ",
            var_quali_moda_test, " dans la colonne ", var_quali_name_test, "\n",
            "est différente ( de la colonne ", var_quanti_test, ")", "\n",
            "pour les individus avec la modalité ",
            var_quali_moda_test_2, " dans la colonne ", var_quali_name_test,
            "\n"
          )
          }
          number_of_ttest <- number_of_ttest + 1
        } else if("norma" == test_en_cours){
          var_quanti_test <- sample(df_final %>%
                                      select(-all_of(already_select_var_ttest)) %>%
                                      select(any_of(quantitative_var)) %>% colnames(), 1)
          already_select_var_ttest <- c(already_select_var_ttest, var_quanti_test)
          
          
          var_quanti_test <- sample(df_final %>%
                                      select(-all_of(already_select_var_ttest)) %>%
                                      select(any_of(quantitative_var)) %>% colnames(), 1)
          
          subset_quanti <- df_final[df_final[, var_quali_name_test] == var_quali_moda_test, var_quanti_test]
          statement <- paste0(
            "\n\n# Question ", i," : \n Tester si la colonne ",
            var_quanti_test, "\n",
            " uniquement pour les individus ayant la modalité ",
            var_quali_moda_test, " dans la colonne ", var_quali_name_test, "\n",
            " est distribuée selon une loi normale",
            "\n"
          )
        }

        else if("chisq" == test_en_cours){
        if(!exists("already_select_var_chisqtest"))  already_select_var_chisqtest <- c()
          var_chisq_test_poss <- df_final %>%
            select(all_of(qualitative_var)) %>%
            select(-all_of(already_select_var_chisqtest)) %>%
            colnames()

          var_chisq <- sample(var_chisq_test_poss,2)
  
          already_select_var_chisqtest <- c(already_select_var_chisqtest,var_chisq)
        
        statement <- paste0(
          "\n\n# Question ", i, " : \n  Tester si les variables ",
          paste0(var_chisq, collapse = " et "), "\n",
          " sont indépendantes.",
          "\n"
        )
        
        } else if("chisq_ajustment" == test_en_cours){
          
          if(!exists("already_select_var_chisqtest"))  already_select_var_chisqtest <- c()
          var_chisq_test_poss <- df_final %>%
            select(all_of(qualitative_var)) %>%
            select(-all_of(already_select_var_chisqtest)) %>%
            colnames()
          
          var_chisq <- sample(var_chisq_test_poss,1)

          
          true_prop <- table(df_final[,var_chisq])/nrow(df_final)
          var_prop <- runif(length(true_prop),-0.1,0.1)
          
          prop <- softmax(true_prop-var_prop)
          already_select_var_chisqtest <- c(already_select_var_chisqtest,var_chisq)
          statement <- paste0(
            "\n\n# Question ", i, " : \n Tester si la variable ",
            var_chisq, " est distribuée \n",
            "selon les probabilités suivantes : ", paste0(paste0(names(true_prop),
                                                               " : ",
                                                               round(prop,3)
                                                               ),
                                                        collapse = " ; "),
            "\n"
          )
          
        }
      
      Question <- paste0(
        statement, "\n\n",
        answer_fields
      )

      cat(Question, "\n\\vspace{5cm} ", file = (con <- file(paste0("data/" ,"CC_",num_grp,".Rmd"), "a", encoding = "UTF-8")), sep = "\n", append = TRUE)
      close(con)
    }
  }
  cat(paste0("Question bonus : "),
      "\n\n",
      "\n\n# Question bonus 1 : \n Présenter les effectifs par classe pour l'IMC catégorisé.",
      "\n\\vspace{5cm} ",
      "\n\n# Question bonus 2 : \n **Calculer** le mode pour une variable qualitative de votre choix.",
      file = (con <- file(paste0("data/" ,"CC_",num_grp,".Rmd"), "a",
                          encoding = "UTF-8"
      )), sep = "\n"
  )
  close(con)
  
  
  
  
  
  # del_val <-  do.call(file.remove,list(list.files(paste0("data/groupe_",num_grp,"/exercice/",collapse = ""), full.names = TRUE)))
  # unlink(paste0("data/groupe_",num_grp,"/exercice",collapse = ""),recursive = TRUE)
}







for (i in 1:5){
  Subject_generator(i)
}


filte_to_knit <- list.files("data/",pattern = ".Rmd$",full.names = TRUE)


lapply(filte_to_knit,function(x) {
  rmarkdown::render(x, "pdf_document")
  
  
})

