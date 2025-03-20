amplitude <- function(x) {
  max(x) - min(x)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk))) 
  }
  val <- exp(par - Lk)
  return(val)
}


"%notin%" <- Negate("%in%")

my_signif <- function(x, digits) floor(x) + signif(x %% 1, digits)


quantile_genrator <- function(q) {
  function(x) {
    quantile(x, q)
  }
}

q_params <- seq(10, 100, 10)

q_function <- structure(lapply(q_params / 100, quantile_genrator),
  names = paste0("Quantile_", q_params)
)
attach(q_function)

################################################################################
# custom distrib with just varying n
# define distribution function
# continous


rbeta_f <- function(x) rbeta(x, shape1 = runif(1, 0.1, 10), shape2 = runif(1, 0.1, 10))
rnorm_f <- function(x) {
  rnorm(x,
    mean = rnorm(1, runif(1, min = -100, 100),
      sd = runif(1, 0.1, 20)
    ),
    sd = rnorm(1, runif(1, min = -5, 5),
      sd = runif(1, 0.1, 2)
    )^2
  )
}
runif_f <- function(x) {
  numbers <- c(
    rnorm(1, runif(1, min = -1000, 1000),
      sd = 20
    ),
    rnorm(1, runif(1, min = -1000, 1000),
      sd = 20
    )
  )

  runif(x, min = min(numbers), max = max(numbers))
}
rexp_f <- function(x) {
  rexp(x, rate = rnorm(1, runif(1, min = -5, 5),
    sd = runif(1, 0.1, 2)
  )^2)
}
rweibull_f <- function(x) rweibull(x, shape = runif(1, 0, 10), scale = runif(1, 0, 10))
rgamma_f <- function(x) rgamma(x, shape = rexp(1, 0.2), scale = rexp(1, 0.2))
# discrete
rbinom_f <- function(x) rbinom(x, round(runif(1, 1, 5), 0), runif(1))
rpois_f <- function(x) rpois(x, round(runif(1, 1, 3), 0))
rbern_f <- function(x) Rlab::rbern(x, runif(1, 0.2, 0.8))
rnbinom_f <- function(x) rnbinom(x, round(runif(1, 20, 40), 0), runif(1, 0.2, 0.8))
rhyper_f <- function(x) {
  m <- round(runif(1, 1, 10), 0)
  n <- round(runif(1, 1, 10), 0)

  rhyper(x, m, n, k = round(runif(1, 1, m + n), 0))
}

################################################################################
# function that extract student text and remove guideline
extract_text_from_exercise <- function(path, cor_baseline_path, nb_Q) {
  conn <- file(cor_baseline_path, open = "r")
  base_text <- readLines(conn, encoding = "UTF-8") %>%
    .[. != ""] %>%
    .[str_which(., "^\\s*#+.*")]
  close(conn)




  conn <- file(path, open = "r")
  text <- readLines(conn, encoding = "UTF-8") %>%
    .[. != ""] %>%
    .[str_which(., "Question_1(_c)? "):length(.)] %>%
    .[. %notin% base_text]
  close(conn)






  reponse_Question_text <- text[str_which(text, "^\\s*Question_\\d+(_c)?")]
  question_name <- na.omit(str_match(reponse_Question_text, "^\\s*Question_\\d+"))
  reponse_Question_text <- str_remove_all(reponse_Question_text, "^\\s*Question_\\d+(_c)?\\s*<-\\s*")
  names(reponse_Question_text) <- question_name


  if (length(reponse_Question_text) < nb_Q) {
    seq_question <- 1:nb_Q
    missing_name_index <- seq_question[seq_question %notin% as.numeric(
      na.omit(
        str_extract(
          names(reponse_Question_text),
          "(?<=Question_)\\d+(?=(_c)?)"
        )
      )
    )]
    seq_question_a_complete <- paste0("Question_", missing_name_index)
    reponse_Question_text[seq_question_a_complete] <- NA
  }

  reponse_Question_text <- reponse_Question_text[order(
    as.numeric(str_match(names(reponse_Question_text), "(?<=Question_)\\d+"))
  )]




  index_question_test <- c(str_which(text, "^\\s*Question_\\d+(_c)?"), length(text))



  list_of_comm_by_Q <- lapply(seq_along(index_question_test[-1]), function(x) {
    paste0(na.omit(str_extract(
      text[index_question_test[x]:
      index_question_test[x + 1]],
      "^\\s*#+.*"
    )), collapse = "\n")
  }) %>% unlist()

  names(list_of_comm_by_Q) <- question_name


  if (length(list_of_comm_by_Q) != nb_Q) {
    seq_question <- 1:nb_Q
    missing_name_index <- seq_question[seq_question %notin% as.numeric(
      na.omit(
        str_extract(
          names(list_of_comm_by_Q),
          "(?<=Question_)\\d+(?=(_c)?)"
        )
      )
    )]
    seq_question_a_complete <- paste0("Question_", missing_name_index)
    list_of_comm_by_Q[seq_question_a_complete] <- NA
  }
  list_of_comm_by_Q <- list_of_comm_by_Q[order(
    as.numeric(str_match(names(list_of_comm_by_Q), "(?<=Question_)\\d+"))
  )]






  # order to dodge order change by student
  return(list(
    answer_text = reponse_Question_text[order(
      as.numeric(str_match(names(reponse_Question_text), "(?<=Question_)\\d+"))
    )],
    comm_answer = list_of_comm_by_Q[order(
      as.numeric(str_match(names(list_of_comm_by_Q), "(?<=Question_)\\d+"))
    )]
  ))
}



###############################################################################
# Get variables from env and add missing Q and reorder
env_extract <- function(env, nb_Q) {
  question_in_env <- mget(ls(env, pattern = "^\\s*Question_\\d+(_c)?\\s*"), envir = env)





  if (length(question_in_env) != nb_Q) {
    seq_question <- 1:nb_Q
    missing_name_index <- seq_question[seq_question %notin% as.numeric(
      na.omit(
        str_extract(
          names(question_in_env),
          "(?<=Question_)\\d+(?=(_c)?)"
        )
      )
    )]
    seq_question_a_complete <- paste0("Question_", missing_name_index)
    question_in_env[seq_question_a_complete] <- NA
  }

  question_in_env <- question_in_env[order(
    as.numeric(str_match(names(question_in_env), "(?<=Question_)\\d+"))
  )]

  return(question_in_env)
}

################################################################################
# compare answer
reponse_comparison <- function(test_cor, test_eval) {
  if (is.vector(test_cor)) {
    res <- (test_cor == test_eval & !is.na(test_eval))
  } else if (is.table(test_cor)) {
    res <- (all(test_cor == test_eval) & all(!is.na(test_eval)))
  } else if (is.list(test_cor) & ("p.value" %in% names(test_cor)) & all(!is.na(test_eval))) {
    res <- all(c(
      test_cor$p.value == test_eval$p.value,
      test_cor$method == test_eval$method,
      test_cor$alternative == test_eval$alternative
    ))
  } else {
    res <- FALSE
  }
  if (length(res) != 1) {
    res <- FALSE
  }
  return(res)
}


##### Mail #####################################################################
Mail_etus <- function(recipients_vec,
                      credential_path = "credential.txt",
                      subject_fun, # string wioth sbuject
                      mail_body_fun, # string with mail body
                      attach_files_paths, # vect of attch files paths
                      bbc_str = NULL,
                      files_name = NULL) {
  # browser()
  credentail <- readLines(credential_path)
  sender <- credentail[1]
  mdp <- credentail[2]
  recipients <- stringi::stri_remove_empty(recipients_vec)



  send.mail(
    from = "francois.bettega@univ-grenoble-alpes.fr",
    to = recipients,
    bcc = c(bbc_str),
    subject = subject_fun,
    body = mail_body_fun,
    smtp = list(
      host.name = "smtps.univ-grenoble-alpes.fr",
      port = 587,
      user.name = "bettegaf",
      passwd = mdp,
      tls = TRUE
    ),
    authenticate = TRUE,
    send = TRUE,
    attach.files = attach_files_paths,
    file.names = files_name # this is an optional parameter
    # file.descriptions = c("Code R fournis", "Rapport fournis") #this is an optional parameter
  )


  # send.mail(from = sender,
  #           to = recipients,
  #           bcc = c(bbc_str),
  #           subject = subject_fun,
  #           body = mail_body_fun,
  #           smtp = list(host.name = "smtp.gmail.com",
  #                       port = 465,
  #                       user.name = sender,
  #                       passwd = mdp,
  #                       ssl=TRUE),
  #           authenticate = TRUE,
  #           send = TRUE,
  #           attach.files = attach_files_paths,
  #           file.names = files_name #this is an optional parameter
  #           # file.descriptions = c("Code R fournis", "Rapport fournis") #this is an optional parameter
  # )
}

############
# check before shapiro that size > 3
###############################################################################################################################################################################################
########################### Data generator######################################################################################################################################################
Subject_generator <- function(seed_fun,
                              code_fun,
                              num_grp,
                              question_stat_desc = 5,
                              question_stat_desc_group = 3,
                              question_test = 2,
                              nb_of_qualitative_var = 3,
                              nb_of_quantitative_var = 3,
                              nb_cor_quanti_var = 2,
                              base_data_size = 100) {
  set.seed(seed_fun)
  dir.create(paste0("data/groupe_", num_grp, "/correction/data"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0("data/groupe_", num_grp, "/correction/data"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0("data/groupe_", num_grp, "/exercice/data"), showWarnings = FALSE, recursive = TRUE)


  cat(code_fun, file = paste0("data/groupe_", num_grp, "/exercice/votre_code", ".txt"), sep = "\n")
  # TO DO
  # find more question for qualitative var
  # Test paired test need add some simulation
  # improve test against another modality for more distance beetween modalities
  # ajout question de R genre select/filter
  # two sided test
  # plot question
  # rajouter test chisq impossible ?$
  # Varying alpha


  nb_of_questions <- question_stat_desc + question_stat_desc_group + question_test
  data_set_size <- base_data_size + round(runif(1, -10, 50), 0)



  # list of avaible func
  Distribution_continous <- c(
    "rbeta_f",
    "rnorm_f",
    "runif_f",
    "rexp_f",
    "rweibull_f",
    "rgamma_f"
  )

  Distribution_discret <- c(
    "rbinom_f",
    "rpois_f",
    "rbern_f",
    "rnbinom_f",
    "rhyper_f"
  )

  # Data_set creation

  choosen_fun <- c(
    sample(Distribution_discret, nb_of_qualitative_var),
    sample(Distribution_continous, nb_of_quantitative_var)
  )


  df_generator <- function() {
    choosen_fun <- c(
      sample(Distribution_discret, nb_of_qualitative_var),
      sample(Distribution_continous, nb_of_quantitative_var)
    )


    column_names <- lapply(
      1:(nb_of_qualitative_var + nb_of_quantitative_var), # create ramdom column names
      function(x) {
        (paste0(
          "colonne_",
          paste0(sample(letters, 2, replace = TRUE),
            collapse = ""
          )
        ))
      }
    ) %>% unlist()




    df <- lapply(choosen_fun, function(x) do.call(x, list(x = data_set_size))) %>%
      as.data.frame() %>%
      rename_all(~ paste0(
        "Var_", c(
          rep("quali_", nb_of_qualitative_var),
          rep("quanti_", nb_of_quantitative_var)
        ),
        c(
          seq(1, nb_of_qualitative_var),
          seq(1, nb_of_quantitative_var)
        )
      ))
  }



  # prevent impossible chisq test
  chisq_checker <- function(df_fun) {
    possible_test <- df_fun %>%
      colnames() %>%
      combn(., 2)

    min_effectif <- apply(possible_test, 2, function(x) {
      suppressWarnings(min(chisq.test(df_fun[, x[1]], df_fun[, x[2]])$expected))
    })
    res <- list(cbn_test = possible_test, effectif_tt = min_effectif)
    return(res)
  }

  # prevent 1 modalities qualit var
  df_checker <- function(df) {
    cond_multiple_moda <- df %>%
      select(contains("quali_")) %>%
      summarise_all(~ (length(unique(.)) > 1 & length(unique(.)) <= 26)) %>%
      unlist() %>%
      all()

    at_least_five_moda <- df %>%
      select(contains("quali_")) %>%
      summarise_all(~ min(table(.)) > 5) %>%
      unlist() %>%
      all()


    if (cond_multiple_moda) {
      cond_chisq_poss <- !all(chisq_checker(df %>% select(contains("quali_")))$effectif_tt < 5)
    } else {
      cond_chisq_poss <- FALSE
    }


    if (cond_multiple_moda & cond_chisq_poss & at_least_five_moda) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  recusive_generation <- function() {
    df <- df_generator()

    if (!df_checker(df)) {
      return(df)
    } else {
      return(recusive_generation())
    }
  }



  df <- recusive_generation()



  # create column with correlation

  base_of_cor_var <- sample(df %>% select(contains("quanti"), contains("cor_")) %>% colnames(), nb_cor_quanti_var)

  df_cor <- df
  for (i in seq_along(base_of_cor_var)) {
    corelated_col <- sample(df %>% colnames(), round(runif(1, 1, ncol(df)), 0), replace = TRUE)
    coef_cor <- runif(length(corelated_col), -5, 5)
    link <- sample(c("+", "*", "-"), length(corelated_col), replace = TRUE) # i don't want use "/" because exact 0 could lead to bug and can also lead to problematic distribution
    noise <- paste0("rnorm(nrow(df),mean = 0.05 * mean(", c(base_of_cor_var[i], corelated_col), ") ,sd = 1)")
    formula <- paste0("(", base_of_cor_var[i], "+", noise[1], ")", paste0(link, "(", coef_cor, "*", corelated_col, "+", noise[-1], ")", collapse = ""))
    cor_col_name <- paste0("cor_", i)


    df_cor <- df_cor %>%
      mutate(!!cor_col_name := eval(parse(text = formula)))
  }



  df_final <- df_cor %>% mutate_at(vars(contains("quali")), ~ factor(., labels = sample(LETTERS, length(unique(.)))))



  LET <- distinct(expand.grid(LETTERS, LETTERS, LETTERS))[sample(1:676, dim(df_final)[2]), ]

  random_colnames <- paste0("Colonne_", apply(LET, 1, paste0, collapse = ""))

  Var_dict <- data.frame(base_colname = colnames(df_final), random_colnames = random_colnames)


  colnames(df_final) <- random_colnames



  quantitative_var <- Var_dict$random_colnames[str_detect(Var_dict$base_colname, "Var_quanti_\\d+|cor_\\d+")]
  qualitative_var <- Var_dict$random_colnames[str_detect(Var_dict$base_colname, "Var_quali_\\d+")]


  decile <- round(runif(3, 1, 10), 0) * 10


  possible_quantile <- paste0("le quantile ", decile, " %")
  names(possible_quantile) <- paste0("Quantile_", decile)


  possible_descriptive_function_quant_uni <- list(
    mean = "la moyenne",
    median = "la mediane",
    min = "le minimum",
    max = "le maximum",
    sd = "l'écart type",
    var = "la variance",
    Mode = "le mode",
    IQR = "l'écart interquartile",
    amplitude = "l'amplitude"
  ) %>% append(possible_quantile)

  possible_descriptive_function_quant_bi <- list(
    cor = "le coefficient de corrélation entre",
    cov = "la covariance entre"
  )

  possible_descriptive_function_quali <- list(table = "présenter les effectifs par classes (ATTENTION : utilisez la méthode présenter dans le polycopié)")


  ################################################################################
  ###############            Question                                       ######
  ################################################################################
  cat("# Ceci un controle continu généré automatiquement",
    "# Si vous avez des problèmes d'affichage des caractères spéciaux,",
    "# ce fichier est encodé en utf-8 \n",
    "\n\n\n",
    file = (con <- file(paste0("data/groupe_", num_grp, "/", "exercice/Exercie.R"), "w",
      encoding = "UTF-8"
    )), sep = "\n"
  )
  close(con)

  write.csv(df_final, paste0("data/groupe_", num_grp, "/exercice/data/jeux_de_donne.csv"))


  ################################################################################
  ###############            Correction                                     ######
  ################################################################################


  write.csv(df_final, paste0("data/groupe_", num_grp, "/correction/data/jeux_de_donne.csv"))
  cat("# Ceci une correction de controle continu généré automatiquement",
    "# Si vous avez des problèmes d'affichage des caractères spéciaux,",
    "# ce fichier est encodé en utf-8 \n", "\n\n\n",
    "df <- read.csv('data/jeux_de_donne.csv') \n",
    file = (con <- file(paste0("data/groupe_", num_grp, "/", "correction/correction.R"), "w", encoding = "UTF-8")),
    sep = "\n", append = TRUE
  )
  close(con)



  for (i in 1:nb_of_questions) {
    # always same form of answer
    answer_fields <- paste0("Question_", i, " <- ", paste0(rep("\n", 3), collapse = ""))

    if (i <= question_stat_desc) {
      # Univariate question
      ################################################################################
      ###############            Question                                       ######
      ################################################################################
      already_select_fun_univariate <- c()
      all_descriptive_function <- c(
        possible_descriptive_function_quant_uni,
        possible_descriptive_function_quant_bi,
        possible_descriptive_function_quali
      )


      possible_descriptive_function <- all_descriptive_function[all_descriptive_function %notin% already_select_fun_univariate]

      fun <- sample(possible_descriptive_function, 1)

      if (fun %in% possible_descriptive_function_quant_bi) {
        var <- sample(df_final %>% select(all_of(quantitative_var)) %>% colnames(), 2)
        statement <- paste0(
          "# Question ", i, " : Calculer ", fun,
          " entre les colonnes ", paste0(var, collapse = " et "), "\n"
        )

        correction <- paste0("Question_", i, "_c <- ", names(fun), "(df$", var[1], ",df$", var[2], ")", collapse = "")
      } else if (fun %in% possible_descriptive_function_quali) {
        var <- sample(df_final %>% select(all_of(qualitative_var)) %>% colnames(), 1)
        statement <- paste0("# Question ", i, " : Pour la colonne ", var, " ", fun, "\n")

        correction <- paste0("Question_", i, "_c <- ", names(fun), "(df$", var, ")", collapse = "")
      } else {
        var <- sample(df_final %>% select(all_of(quantitative_var)) %>% colnames(), 1)
        statement <- paste0(
          "# Question ", i,
          " : Calculer ", fun, " de la colonne ", var, "\n"
        )
        correction <- paste0("Question_", i, "_c <- ", names(fun), "(df$", var, ")", collapse = "")
      }



      Question <- paste0(
        statement, "# repondez en complétant le code ci-dessous\n\n",
        answer_fields
      )

      cat(Question, "\n\n\n",
        file = (con <- file(paste0("data/groupe_", num_grp, "/", "exercice/Exercie.R"), "a", encoding = "UTF-8")),
        sep = "\n", append = TRUE
      )
      close(con)
      already_select_fun_univariate <- c(already_select_fun_univariate, fun)
      ################################################################################
      ###############            Correction                                     ######
      ################################################################################
      cat(correction, "\n\n\n",
        file = (con <- file(paste0("data/groupe_", num_grp, "/", "correction/correction.R"), "a", encoding = "UTF-8")),
        sep = "\n", append = TRUE
      )
      close(con)





      # Using a grouping var
    } else if (i <= question_stat_desc + question_stat_desc_group) {

      ################################################################################
      ###############            Question                                       ######
      ################################################################################

      already_select_fun_group <- c()
      possible_descriptive_function_group <- possible_descriptive_function_quant_uni[possible_descriptive_function_quant_uni %notin% already_select_fun_group]

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

      statement <- paste0(
        "# Question ", i, " : Calculer ", fun, " de la colonne ", var_quanti,
        "\n", "# Uniquement pour les lignes où la colonne ",
        var_quali_name, " prend la valeur ", var_quali, "\n"
      )
      Question <- paste0(statement, "# repondez en complétant le code ci-dessous\n\n", answer_fields)

      correction <- paste0("Question_", i, "_c <- ", names(fun), "(df$", var_quanti, "[df$", var_quali_name, "==", '"', var_quali, '"', "]", ")", collapse = "")





      cat(Question, "\n\n\n", file = (con <- file(paste0("data/groupe_", num_grp, "/", "exercice/Exercie.R"), "a", encoding = "UTF-8")), sep = "\n", append = TRUE)
      close(con)
      already_select_fun_group <- c(already_select_fun_group, fun)


      ################################################################################
      ###############            Correction                                     ######
      ################################################################################
      cat(correction, "\n\n\n",
        file = (con <- file(paste0("data/groupe_", num_grp, "/", "correction/correction.R"), "a", encoding = "UTF-8")),
        sep = "\n", append = TRUE
      )
      close(con)



      # Test question
    } else {
      chisq_check_res <- chisq_checker(df_final %>%
        select(all_of(qualitative_var)))

      where_chisq_poss <- chisq_check_res$cbn_test[, chisq_check_res$effectif_tt >= 5]

      already_select_var_ttest <- c()

      if (length(where_chisq_poss) > 0) { # randmoly select test if chisq is possible otherwise take mean comparison
        test_name <- sample(c("mean", "chi2"), 1, prob = c(0.8, 0.2))
      } else {
        test_name <- "mean"
      }


      if (test_name == "mean") {
        var_quanti_test <- sample(df_final %>%
          select(-all_of(already_select_var_ttest)) %>%
          select(all_of(quantitative_var)) %>% colnames(), 1)
        var_quali_name_test <- sample(df_final %>%
          select(all_of(qualitative_var)) %>%
          colnames(), 1)
        var_quali_moda_test <- df_final %>%
          select(-all_of(already_select_var_ttest)) %>%
          select(all_of(var_quali_name_test)) %>%
          unlist() %>%
          as.character() %>%
          sample(1)

        # add check here 
        
        
        already_select_var_ttest <- c(already_select_var_ttest, var_quanti_test)
        if ("tt" == sample(c("tt", "other_var"), 1)) {
          subset_quanti <- df_final[df_final[, var_quali_name_test] == var_quali_moda_test, var_quanti_test]
          statement <- paste0(
            "# Question ", i, " : Tester si la moyenne de la colonne ",
            var_quanti_test, "\n",
            "# uniquement pour les individus ayant la modalité ",
            var_quali_moda_test, " dans la colonne ", var_quali_name_test, "\n",
            "# est différente de ",
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
          ################################################################################
          ###############                     Correction                   ###############
          ################################################################################
          if ( # case where paramatric test is possible
            length(subset_quanti) >= 30 |
              shapiro.test(subset_quanti)$p.value >= 0.05) {
            correction <- paste0("Question_", i, "_c <- ", "t.test(df[df[,", '"',
              var_quali_name_test, '"', "]==", '"', var_quali_moda_test, '"', ",",
              '"', var_quanti_test, '"', "],", "mu = ", signif(
                mean(subset_quanti) +
                  rnorm(1,
                    mean = mean(subset_quanti) * 0.05,
                    sd = 0.2
                  ),
                4
              ), ")",
              collapse = ""
            )
          } else {
            correction <- paste0("Question_", i, "_c <- ", "wilcox.test(df[df[,", '"',
              var_quali_name_test, '"', "]==", '"', var_quali_moda_test, '"', ",",
              '"', var_quanti_test, '"', "],", "mu = ", signif(
                mean(subset_quanti) +
                  rnorm(1,
                    mean = mean(subset_quanti) * 0.05,
                    sd = 0.2
                  ),
                4
              ), ")",
              collapse = ""
            )
          }
          ###############################################################################
          ### comparison between two vars
        } else {
          var_quali_moda_test_2 <- df_final %>%
            filter(!!rlang::sym(var_quali_name_test) != var_quali_moda_test) %>%
            select(all_of(var_quali_name_test)) %>%
            unlist() %>%
            as.character() %>%
            sample(1)

          statement <- paste0(
            "# Question ", i, " : Tester si la moyenne de la colonne ",
            var_quanti_test, "\n",
            "# pour les individus ayant la modalité ",
            var_quali_moda_test, " dans la colonne ", var_quali_name_test, "\n",
            "# est différente de la moyenne ( de la colonne", var_quanti_test, ")", "\n",
            "# pour les individus avec la modalité ",
            var_quali_moda_test_2, " dans la colonne ", var_quali_name_test,
            "\n"
          )


          ################################################################################
          ###############                     Correction                   ###############
          ################################################################################
          sub_quati_two_sample_1 <- df_final[df_final[, var_quali_name_test] == var_quali_moda_test, var_quanti_test]
          sub_quati_two_sample_2 <- df_final[df_final[, var_quali_name_test] == var_quali_moda_test_2, var_quanti_test]

          if ( # case where paramatric test is possible
            (length(sub_quati_two_sample_1) >= 30 | shapiro.test(sub_quati_two_sample_1)$p.value >= 0.05) &
            (length(sub_quati_two_sample_2) >= 30 | shapiro.test(sub_quati_two_sample_2)$p.value >= 0.05) &
            (var.test(sub_quati_two_sample_1,sub_quati_two_sample_2)$p.value >= 0.05)
          ) {
            correction <- paste0("Question_", i, "_c <- ", "t.test(df[df[,", '"',
              var_quali_name_test, '"', "]==", '"', var_quali_moda_test, '"', ",",
              '"', var_quanti_test, '"', "]",
              ",df[df[,", '"',
              var_quali_name_test, '"', "]==", '"', var_quali_moda_test_2, '"', ",",
              '"', var_quanti_test, '"', "]",
              ")",
              collapse = ""
            )
          } else {
            correction <- paste0("Question_", i, "_c <- ", "wilcox.test(df[df[,", '"',
              var_quali_name_test, '"', "]==", '"', var_quali_moda_test, '"', ",",
              '"', var_quanti_test, '"', "]",
              ",df[df[,", '"',
              var_quali_name_test, '"', "]==", '"', var_quali_moda_test_2, '"', ",",
              '"', var_quanti_test, '"', "]",
              ")",
              collapse = ""
            )
          }
        }
      } else {
        if (is.vector(where_chisq_poss)) {
          var_chisq <- where_chisq_poss
          where_chisq_poss <- c() # remove choosen col for next chisq
        } else {
          choose_a_col_chisq <- sample(1:ncol(where_chisq_poss), 1)
          var_chisq <- where_chisq_poss[, choose_a_col_chisq] # if two possible chisq randomly choose one
          where_chisq_poss <- where_chisq_poss[, -choose_a_col_chisq] # remove choosen col for next chisq
        }

        statement <- paste0(
          "# Question ", i, " : Tester si les variables ",
          paste0(var_chisq, collapse = " et "), "\n",
          "# sont indépendante.",
          "\n"
        )
        ################################################################################
        ###############                     Correction                   ###############
        ################################################################################
        correction <- paste0("Question_", i, "_c <- ", "chisq.test(table(df$", var_chisq[1],
          ",", "df$", var_chisq[2], "))",
          collapse = ""
        )
      }

      Question <- paste0(
        statement, "# repondez en complétant le code ci-dessous\n",
        "#  Pour la que la réponse soit valide \n",
        "# vous devez repondre en assignant tous le test (cf exemple ci-dessous\n",
        '# Question_X <-  t.test(x,y, alternative = c("two.sided"))\n',
        "# Pour obtenir la totalité des points il est aussi nécessaire d'inscrire SOUS la réponse: \n",
        "#     D'ajouter un commentaire avec les hypothèse du test, \n",
        "#     Ainsi qu'une interprétation de de la p-value au seuil alpha (5% si rien n'est précisé). \n",
        "# NB pensez bien a mettre # avant votre texte afin que ce ne soit pas considéré comme du code R\n\n",
        answer_fields
      )

      cat(Question, "\n\n\n", file = (con <- file(paste0("data/groupe_", num_grp, "/", "exercice/Exercie.R"), "a", encoding = "UTF-8")), sep = "\n", append = TRUE)
      close(con)


      ###############################################################################
      ###############            Correction                                     ######
      ################################################################################
      cat(correction, "\n\n\n",
        file = (con <- file(paste0("data/groupe_", num_grp, "/", "correction/correction.R"), "a", encoding = "UTF-8")),
        sep = "\n", append = TRUE
      )
      close(con)
    }
  }
  # cat(correction,"\n\n\n",
  #     file=(con <- file(paste0("data/groupe_",num_grp,"/correction/correction.R"), "a", encoding="UTF-8")),
  #     sep="\n",append=TRUE); close(con)
  file.copy(paste0("data/groupe_", num_grp, "/exercice/Exercie.R"), paste0("data/groupe_", num_grp, "/correction/Exercie.R"), overwrite = TRUE)



  myProject <- function(proj, ...) {

    # require(ProjectTemplate)
    # create.project(proj, ...)

    x <- c(
      "Version: 1.0", "", "RestoreWorkspace: Default", "SaveWorkspace: Default",
      "AlwaysSaveHistory: Default", "", "EnableCodeIndexing: Yes",
      "UseSpacesForTab: Yes", "NumSpacesForTab: 4", "Encoding: UTF-8",
      "", "RnwWeave: knitr", "LaTeX: pdfLaTeX"
    )

    cat(paste(x, collapse = "\n"), file = file.path(proj, paste0(basename(proj), ".Rproj")))

    # message(paste(basename(proj), "has been created"))
  }

  myProject(paste0("data/groupe_", num_grp, "/exercice"), template = "minimal", merge.strategy = "allow.non.conflict")

  cat('setHook("rstudio.sessionInit", function(newSession) {
    if (newSession)
      rstudioapi::navigateToFile("Exercie.R", line = -1L, column = -1L)
  }, action = "append")', file = paste0("data/groupe_", num_grp, "/exercice/.Rprofile"))


  files <- list.files(paste0("data/groupe_", num_grp, "/exercice/", collapse = ""), full.names = TRUE)
  zip::zipr(
    paste0("data/groupe_", num_grp, "/groupe_", num_grp, ".zip"),
    file.path(files)
  )

  unlink(paste0("data/groupe_", num_grp, "/exercice", collapse = ""), recursive = TRUE)
}

#################################################################################################################################################################################################
#################################################################################################################################################################################################
#################################################################################################################################################################################################


# ajouter les réponses executer de mon coté au mail pour vérif
############


correction_generator <- function(path) {
  browser()

  # base exercises
  correction_eval.env <- new.env()

  source(file.path(path, "correction.R"),
    local = correction_eval.env,
    chdir = TRUE
  )

  path_to_submit_answer <- list.files(file.path(path, "etu_file"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  # Total number of Question to handle bad ordering question or missing question
  nb_of_question <- max(as.numeric(na.omit(str_extract(ls(envir = correction_eval.env), "(?<=Question_)\\d+(?=(_c)?)"))))


  # Extract plain text
  correction_text <- extract_text_from_exercise(file.path(path, "correction.R"), file.path(path, "Exercie.R"), nb_of_question)

  answer_text <- extract_text_from_exercise(
    path_to_submit_answer,
    file.path(path, "Exercie.R"), nb_of_question
  )


  # Eval line by line student code

  error_list <- character()
  exercice_eval.env <- new.env()
  conn <- file(path_to_submit_answer, open = "r")

  text_answer_modif <- readLines(conn, encoding = "UTF-8") %>%
    str_replace("data/jeux_de_donne.csv", file.path(path, "data/jeux_de_donne.csv")) %>%
    c(., "\n #end of subject")
  close(conn)
  # ll <- parse(file = path_to_submit_answer,  encoding="UTF-8")

  ll <- parse(text = text_answer_modif, encoding = "UTF-8")
  for (i in seq_along(ll)) {
    tryCatch(eval(ll[[i]], envir = exercice_eval.env),
      error = function(e) {
        error_list[paste0("line : ", i)] <<- as.character(e)
      }
    )
  }






  # Use total number of Question to add missing and reorder



  # Use total number of Question to add missing and reorder
  extract_exercice <- env_extract(exercice_eval.env, nb_of_question)
  extract_correction <- env_extract(correction_eval.env, nb_of_question)



  # Comparisson
  compar_with_cor <- lapply(
    seq_along(extract_correction),
    function(x) reponse_comparison(extract_correction[[x]], extract_exercice[[x]])
  ) %>% unlist()



  extract_exercice_text <- lapply(
    extract_exercice,
    function(x) ifelse(is.list(x), paste0(x, collapse = "\n"), paste0(x))
  ) %>% unlist()

  extract_correction_text <- lapply(
    extract_correction,
    function(x) ifelse(is.list(x), paste0(x, collapse = "\n"), paste0(x))
  ) %>% unlist()
  # Warning if column definition change remember change in def file
  Correction_df <- data.frame(
    Numero_question = 1:nb_of_question,
    Reponse_etu = answer_text$answer_text,
    Correction = correction_text$answer_text,
    Is_TRUE = compar_with_cor, # 11
    Reponse_num_etu = extract_exercice_text, # NA  only
    Correction_num = extract_correction_text, # one element
    Associate_commentary = answer_text$comm_answer # empty
    # error = error_return_with_Q
  ) # empty

  return(list(
    df = Correction_df,
    error = error_list
  ))
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
