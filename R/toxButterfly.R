#' Create symmetrical PRO-CTCAE bar plots over time between two groups/arms.
#'
#' 	  Data format should be in 'long' format, where each PRO-CTCAE item is a
#' 	  variable/column. PRO-CTCAE item must have format type of yn_2_fmt,
#' 	  frq_5_fmt, int_5_fmt, or sev_5_int.
#'
#' @param dsn A data.frame object with PRO-CTCAE data
#' @param id_var A character string.Name of ID variable differentiating each
#'   PRO-CTCAE survey/participant entered as a quoted string.
#' @param cycle_var A character string. Name of variable differentiating one
#'   longitudinal/repeated. PRO-CTCAE survey from another, within an individual
#'   ID.
#' @param proctcae_var A character string. Name of the PRO-CTCAE Variable to be
#'   graphed by the function. Can be a composite, scale or indicator PRO-CTCAE
#'   variable and it does not need to be labeled.
#' @param arm_var A character string. Name of arm variable differentiating
#'   treatment groups. Must be character or factor class.
#' @param cycle_var A character string. Name of cycle variable associated with
#'   time.
#' @param arm_labels A vector with two elements of character type. Labels of the
#'   arm/group variable if different from the current labels. The first will be
#'   left side of the butterfly and the second the right side of the plot.
#' @param cycle_order Logical. T for "top-bottom" displaying the first time at
#'   the top versus F for "bottom-top" the first level will be at the bottom.
#'   Defaults to \code{TRUE}.
#' @param missing Logical. Determines how the missing data will be displayed in
#' the graph. T would exclude the missing data from the bar chart and would
#' display the N along the side of the plot. F would include the missing data
#' into the bar plot as the first level. Defaults to \code{TRUE}.
#' @param plot_limit A number. Limit the number of cycles to be plotted up to
#'   and including a given cycle number. All available cycle time points are
#'   plotted if no cycle number is provided. Defaults to \code{NA}.
#' @param colors A number. Specify the coloring scheme of symptom grades within
#'   frequency bars. Options include: 1 = Blue and red color shading, 2 =
#'   qualitative color shades (color blind friendly), 3 = black and white.
#'   Defaults to 1.
#' @param return_data Logical. Return the data used to construct plots as element
#'   of the returned object as well as all of the elements used to create all
#'   plots created by the function. Defaults to \code{FALSE}.
#' @return A plot object. The returned object is a symmetrical butterfly plot made
#'   up by two stacked sideways barplots a legend and labels. When
#'   \code{return_data} is set to \code{TRUE} the function returns a list of
#'   objects. The first object is the butterfly figure the next objects are the
#'   plots or parts of the plots that make up the butterfly and the last items
#'   in the list are data elements used to create the parts of the plot.
#' @importFrom magrittr %>%
#' @importFrom ggplot2 %+%
#' @importFrom ggplot2 ggplot_add
#' @examples
#' # --- Simulate example PRO-CTCAE data
#' \donttest{
#' acute_butterfly = toxButterfly(dsn = ProAE::tox_acute,
#'  id_var = "id",
#'  proctcae_var = "PROCTCAE_78A_SCL",
#'  cycle_var = "time",
#'  arm_var = "arm",
#'  colors = 1,
#'  plot_data = TRUE,
#'  missing = FALSE)
#' acute_butterfly
#' }
#' @export

toxButterfly <-   function(dsn,
                           proctcae_var,
                           id_var,
                           cycle_var,
                           arm_var,
                           arm_labels = c("Arm A","Arm B"),
                           cycle_order = T,
                           missing = T,
                           plot_data=F,
                           plot_limit = NA,
                           colors = 1){

  ## -------------------------------------------------------------
  ## --- Taking care of the Binding Issues
  ## -------------------------------------------------------------

  a = NULL
  b = NULL
  Cycle = NULL
  n = NULL
  percent = NULL
  proqol = NULL
  x = NULL
  y = NULL


  # ----------------------------------------------------------------
  # -- Checks for parameters
  # ----------------------------------------------------------------


  ## -- Check for presence of required parameters.

  if(exists("dsn")){
    if(!is.data.frame(dsn)){
      stop("param dsn must be provided as a data.frame object")
    }
  } else {stop("param dsn not provided")}

  if(exists("id_var")){
    if(!is.character(id_var)){
      stop("param id_var must be provided as a character string")
    } else if (!(id_var %in% colnames(dsn))){
      stop(paste0("param id_var (", id_var, ") not found as a variable in dsn
                  (", deparse(substitute(dsn)), ")"))
    }
  } else {stop("param id_var not provided")}

  if(exists("cycle_var")){
    if(!is.character(cycle_var)){
      stop("param cycle_var must be provided as a character string")
    } else if (!(cycle_var %in% colnames(dsn))){
      stop(paste0("param cycle_var (", cycle_var, ") not found as a variable in dsn
                  (", deparse(substitute(dsn)), ")"))
    }
  } else {stop("param cycle_var not provided")}

  if(exists("arm_var")){
    if(!is.character(arm_var)){
      stop("param arm_var must be provided as a character string")
    } else if (!(arm_var %in% colnames(dsn))){
      stop(paste0("param arm_var (", arm_var, ") not found as a variable in dsn
                  (", deparse(substitute(dsn)), ")"))
    }
  } else {stop("param cycle_var not provided")}
  if(exists("proctcae_var")){
    if(!is.character(proctcae_var)){
      stop("param proctcae_var must be provided as a character string")
    } else if (!(proctcae_var %in% colnames(dsn))){
      stop(paste0("param proctcae_var (", proctcae_var, ") not found as a variable in dsn
                  (", deparse(substitute(dsn)), ")"))
    }
  } else {stop("param proctcae_var not provided")}

  ## -- Check to make sure optional parameters are given in the proper types
  if(exists("arm_labels")){
    if(!is.character(arm_labels) & length(arm_labels)!=2){
      stop("param arm_labels must be provided as character strings within a vector that has length of 2.")
    }
  }

  if(exists("cycle_order")){
    if(!is.logical(cycle_order)){
      stop("param cycle_order must be logical. T/F")
    }
  }

  if(exists("missing")){
    if(!is.logical(missing)){
      stop("param missing must be logical. T/F")
    }
  }

  if(exists("plot_data")){
    if(!is.logical(plot_data)){
      stop("param plot_data must be logical. T/F")
    }
  }

  if(exists("colors")){
    if(!is.numeric(colors) & (colors==1 | colors==2 | colors==3)){
      stop("param colors must be numeric and 1,2 or 3.
           Please see documentation for more details")
    }
  }



  ## -- Check for any duplicate individuals within cycles

  if(any(duplicated(dsn[,c(id_var, cycle_var)]))){
    stop(paste0("Duplicate observations were found within id_var and
                cycle_var combinations (", id_var, " and ", cycle_var, ")"))
  }

  ## -------------------------------------------------------------
  ## --- Reference data sets
  ## -------------------------------------------------------------


  proctcae_vars[, ] = lapply(proctcae_vars[, ], as.character)

  ## -- Check to make sure the PRO-CTCAE variable is in the list
  if(((proctcae_var %in% proctcae_vars$name==T)==F & endsWith(proctcae_var,"COMP")==F)){
    stop("proctcae_var is not in list")
  }

  ## -- Check to make sure the user is only supplying PROCTCAE variables that formats
  ## -- that the function can handle

  ### Assign the format type to the PROCTCAE variable

  if(endsWith(proctcae_var,"COMP")){
    fmt = "comp"
    label = paste(label(dsn[[proctcae_var]]),"Composite","")
  } else{
    fmt = proctcae_vars[proctcae_vars$name==proctcae_var,]$fmt_name

    label = proctcae_vars[proctcae_vars$name==proctcae_var,]$short_label
  }

  if(fmt %in% c("sev_6_fmt","sev_7_fmt","yn_3_fmt","yn_4_fmt","frq_7_fmt")){
    stop("The functionality of toxButterfly() is limited to only PRO-CTCAE
    variables with format typesyn_2_fmt, frq_5_fmt, int_5_fmt, and sev_5_fmt.")
  }


  ## -- Check to see if the missing option is truly useful

  if(length(is.na(dsn[[proctcae_var]])==T)==0 & missing==F){
    warning("There are no surveys that have missing values and you specified to
            display the N on the sides of the plots.The function is not set up to handle
             this request and instead the N will not display along the sides. ")
    missing = T
  }

  # ----------------------------------------------------------------
  # -- Defaults
  # ----------------------------------------------------------------

  if(is.na(arm_labels[1])==T & is.na(arm_labels[2])==T){
    dsn = dsn %>% dplyr::arrange(dsn$arm_var)
    arm_labels = c(as.character(levels(dsn[[arm_var]])))
  }

  if(is.na(cycle_order)){
    cycle_order = T
  }

  if(is.na(missing)){
    missing = T
  }

  if(is.na(plot_data)){
    plot_data = F
  }

  if(is.na(colors)){
    colors = 1
  }


  ############ Extract the levels of the arm variable to filter on it.
  armlevels= sort(unique(dsn[[arm_var]]))

  if(length(unique(armlevels))!=2){
    stop("The functionality of toxButterfly() is limited to only two arm stratification.")
  }



  # ----------------------------------------------------------------
  # -- Take Arm A or first Arm
  # ----------------------------------------------------------------

  ############ Just exatract Arm 1 patients
  a = dsn[dsn[[arm_var]]==armlevels[1],]


  #### Reorder the variable so the butterfly can be symmetric
  if(fmt %in% c("frq_5_fmt","int_5_fmt","sev_5_fmt")){
    a[[proctcae_var]] = ordered(as.character(a[[proctcae_var]]),levels=c(4,3,2,1,0,NA))
  } else if (fmt=="yn_2_fmt"){
    a[[proctcae_var]] = ordered(as.character(a[[proctcae_var]]),levels=c(1,0,NA))
  } else if (fmt=="comp") {
    a[[proctcae_var]] = ordered(as.character(a[[proctcae_var]]),levels=c(3,2,1,0,NA))
  } else {
    a[[proctcae_var]] = as.factor(a[[proctcae_var]])
    a[[proctcae_var]] = ordered(as.character(a[[proctcae_var]]),levels=rev(levels(a[[proctcae_var]])))
  }

  ## Calculate the number patients in the first arm. Used for calculations later.
  a_pt = as.numeric(length(unique(a[[id_var]])))

  ## Create a dataset that looks at the counts for each of the by variables
  a = data.frame(table(a[[cycle_var]],a[[arm_var]],a[[proctcae_var]],useNA="always"))

  ## Remove all of the levels that have no observations and the missing levels depending of the
  ## missingness option
  if(missing==F){
    a = a[a$Freq>0 & is.na(a$Var3)==F,]
  } else if (missing==T){
    a = a[a$Freq>0,]
  }

  ## Relabel the variables to make it easier later on
  names(a)[names(a) == "Var1"] <- "Cycle"
  names(a)[names(a) == "Var2"] <- "Arm"
  names(a)[names(a) == "Var3"] <- "proqol"
  names(a)[names(a) == "Freq"] <- "n"

  ## Calculate the percentages and counts for the graph but it differs based on the missingness type.
  if(missing==F){
    a <- a %>% dplyr::arrange(a$Cycle,a$Arm,a$proqol) %>%
      dplyr::group_by(a$Cycle,a$Arm) %>%
      dplyr::mutate(
        t = sum(a$n),
        percent=((a$n/a$t)*100))
  } else if (missing==T){
    a <- a %>% dplyr::arrange(a$Cycle,a$Arm,a$proqol) %>%
      dplyr::group_by(a$Cycle,a$Arm) %>%
      dplyr::mutate(
        t2 = sum(a$n),
        t=a_pt
      ) %>% dplyr::ungroup() %>%
      dplyr::mutate(
        n = dplyr::case_when(
          is.na(a$proqol)==T ~ as.numeric(a$t-a$t2)+a$n,
          TRUE ~ as.numeric(a$n)
        )
      ) %>% dplyr::group_by(a$Cycle,a$Arm) %>%
      dplyr::mutate(
        percent=((a$n/a$t)*100)
      )
  }

  # -----------------------------------------------------------------------------------
  # -- Doing the exact same thing as above except just for the Arm B or the second Arm
  # -----------------------------------------------------------------------------------

  ########### Just extract Arm 2 patients
  b = dsn[dsn[[arm_var]]==armlevels[2],]

  if(fmt %in% c("frq_5_fmt","int_5_fmt","sev_5_fmt")){
    b[[proctcae_var]] = ordered(as.character(b[[proctcae_var]]),levels=c(NA,0,1,2,3,4))
  } else if (fmt=="yn_2_fmt"){
    b[[proctcae_var]] = ordered(as.character(b[[proctcae_var]]),levels=c(NA,0,1))
  } else if (fmt=="comp") {
    b[[proctcae_var]] = ordered(as.character(b[[proctcae_var]]),levels=c(NA,0,1,2,3))
  } else {
    b[[proctcae_var]] = as.factor(b[[proctcae_var]])
    b[[proctcae_var]] = ordered(as.character(b[[proctcae_var]]),levels=rev(levels(b[[proctcae_var]])))
  }

  b_pt = as.numeric(length(unique(b[[id_var]])))


  b = data.frame(table(b[[cycle_var]],b[[arm_var]],b[[proctcae_var]],useNA="always"))


  if(missing==F){
    b = b[b$Freq>0 & is.na(b$Var3)==F,]
  } else if (missing==T){
    b = b[b$Freq>0,]
  }

  names(b)[names(b) == "Var1"] <- "Cycle"
  names(b)[names(b) == "Var2"] <- "Arm"
  names(b)[names(b) == "Var3"] <- "proqol"
  names(b)[names(b) == "Freq"] <- "n"

  if(missing==F){
    b <- b %>% dplyr::arrange(b$Cycle,b$Arm,b$proqol) %>%
      dplyr::group_by(b$Cycle,b$Arm) %>%
      dplyr::mutate(
        t = sum(b$n),
        percent=((b$n/b$t)*100))
  } else if (missing==T){
    b <- b %>% dplyr::arrange(b$Cycle,b$Arm,b$proqol) %>%
      dplyr::group_by(b$Cycle,b$Arm) %>%
      dplyr::mutate(
        t2 = sum(b$n),
        t=b_pt
      ) %>% dplyr::ungroup() %>%
      dplyr::mutate(
        n = dplyr::case_when(
          is.na(b$proqol)==T ~ as.numeric(b$t-b$t2)+b$n,
          TRUE ~ as.numeric(b$n)
        )
      ) %>% dplyr::group_by(b$Cycle,b$Arm) %>%
      dplyr::mutate(
        percent=((b$n/b$t)*100)
      )
  }



  ## These are the counts by cycle to show missingness
  a_n <- dsn[dsn[[arm_var]]==armlevels[1] & is.na(dsn[[proctcae_var]])==F,]
  a_n <- data.frame(table(a_n[[cycle_var]],useNA="always"))
  a_n <- a_n[a_n$Freq>0,]
  names(a_n)[names(a_n) == "Var1"] <- "Cycle"
  names(a_n)[names(a_n) == "Freq"] <- "n"

  b_n <- dsn[dsn[[arm_var]]==armlevels[2] & is.na(dsn[[proctcae_var]])==F,]
  b_n <- data.frame(table(b_n[[cycle_var]],useNA="always"))
  b_n <- b_n[b_n$Freq>0,]
  names(b_n)[names(b_n) == "Var1"] <- "Cycle"
  names(b_n)[names(b_n) == "Freq"] <- "n"

  # ----------------------------------------------------------------
  # -- Calculate the N's by timepoint for each arm will be the outer most part of the graph
  # ----------------------------------------------------------------
  ##### Looking at the levels of the cycle variable to tell the floating N where to go in the graph
  a_cyclelevels = as.numeric(length(unique(a_n$Cycle)))
  a_frac = 1/(a_cyclelevels-1)
  b_cyclelevels = as.numeric(length(unique(b_n$Cycle)))
  b_frac = 1/(b_cyclelevels-1)
  #### This tells the free text for the N where to go so it is aligned with the bars
  a_n$y1 = seq(0,1,a_frac)
  a_n$y2 = 1-a_n$y1
  a_n$x = 1

  b_n$y1 = seq(0,1,b_frac)
  b_n$y2 = 1-b_n$y1
  b_n$x = 1

  #### If both arms only ever have one N value the outer part of the graph would be all
  #### a single value and we will not print the n.
  if (length(unique(a_n$n))==1 & length(unique(b_n$n))==1){
    display_n <- F
  } else {
    display_n <- T
  }

  #### Great the labels for each of the Arms
  arm_label_a = arm_labels[1]
  arm_label_b = arm_labels[2]



  # ----------------------------------------------------------------
  # -- Order proctcae_var correctly
  # ----------------------------------------------------------------
  ## To make sure the butterfly is symmetric
  if (fmt=="sev_5_fmt" & missing==F){
    a <-  a %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          a$proqol == 0 ~ "None",
          a$proqol == 1 ~ "Mild",
          a$proqol == 2 ~ "Moderate",
          a$proqol == 3 ~ "Severe",
          a$proqol == 4 ~ "Very Severe",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(a$proqol),
                         levels=c("None","Mild","Moderate","Severe","Very Severe"))
      )
    b <-  b %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          b$proqol == 0 ~ "None",
          b$proqol == 1 ~ "Mild",
          b$proqol == 2 ~ "Moderate",
          b$proqol == 3 ~ "Severe",
          b$proqol == 4 ~ "Very Severe",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(b$proqol),
                         levels=c("None","Mild","Moderate","Severe","Very Severe"))
      )
    level_labels = c("None","Mild","Moderate","Severe","Very Severe")
  } else if (fmt=="sev_5_fmt" & missing==T){
    a <-  a %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          is.na(a$proqol) == T ~ "Missing",
          a$proqol == 0 ~ "None",
          a$proqol == 1 ~ "Mild",
          a$proqol == 2 ~ "Moderate",
          a$proqol == 3 ~ "Severe",
          a$proqol == 4 ~ "Very Severe",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(a$proqol),
                         levels=c("Missing","None","Mild","Moderate","Severe","Very Severe"))
      )
    b <-  b %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          is.na(b$proqol) == T ~ "Missing",
          b$proqol == 0 ~ "None",
          b$proqol == 1 ~ "Mild",
          b$proqol == 2 ~ "Moderate",
          b$proqol == 3 ~ "Severe",
          b$proqol == 4 ~ "Very Severe",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(b$proqol),
                         levels=c("Missing","None","Mild","Moderate","Severe","Very Severe"))
      )
    level_labels = c("Missing","None","Mild","Moderate","Severe","Very Severe")
  } else if (fmt=="frq_5_fmt" & missing==F){
    a <- a %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          a$proqol == 0 ~ "Never",
          a$proqol == 1 ~ "Rarely",
          a$proqol == 2 ~ "Occasionally",
          a$proqol == 3 ~ "Frequently",
          a$proqol == 4 ~ "Almost Constantly",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(a$proqol),
                         levels=c("Never","Rarely","Occasionally","Frequently","Almost Constantly"))
      )
    b <- b %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          b$proqol == 0 ~ "Never",
          b$proqol == 1 ~ "Rarely",
          b$proqol == 2 ~ "Occasionally",
          b$proqol == 3 ~ "Frequently",
          b$proqol == 4 ~ "Almost Constantly",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(b$proqol),
                         levels=c("Never","Rarely","Occasionally","Frequently","Almost Constantly"))
      )
    level_labels = c("Never","Rarely","Occasionally","Frequently","Almost Constantly")
  } else if (fmt=="frq_5_fmt" & missing==T){
    a <- a %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          is.na(a$proqol)==T ~ "Missing",
          a$proqol == 0 ~ "Never",
          a$proqol == 1 ~ "Rarely",
          a$proqol == 2 ~ "Occasionally",
          a$proqol == 3 ~ "Frequently",
          a$proqol == 4 ~ "Almost Constantly",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(a$proqol),
                         levels=c("Missing","Never","Rarely","Occasionally","Frequently","Almost Constantly"))
      )
    b <- b %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          is.na(b$proqol)==T ~ "Missing",
          b$proqol == 0 ~ "Never",
          b$proqol == 1 ~ "Rarely",
          b$proqol == 2 ~ "Occasionally",
          b$proqol == 3 ~ "Frequently",
          b$proqol == 4 ~ "Almost Constantly",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(b$proqol),
                         levels=c("Missing","Never","Rarely","Occasionally","Frequently","Almost Constantly"))
      )
    level_labels = c("Missing","Never","Rarely","Occasionally","Frequently","Almost Constantly")
  } else if (fmt=="int_5_fmt" & missing==F){
    a <- a %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          a$proqol == 0 ~ "Not at all",
          a$proqol == 1 ~ "A little bit",
          a$proqol == 2 ~ "Somewhat",
          a$proqol == 3 ~ "Quite a bit",
          a$proqol == 4 ~ "Very much",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(a$proqol),
                         levels=c("Not at all","A little bit","Somewhat","Quite a bit","Very much"))
      )
    b <- b %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          b$proqol == 0 ~ "Not at all",
          b$proqol == 1 ~ "A little bit",
          b$proqol == 2 ~ "Somewhat",
          b$proqol == 3 ~ "Quite a bit",
          b$proqol == 4 ~ "Very much",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(b$proqol),
                         levels=c("Not at all","A little bit","Somewhat","Quite a bit","Very much"))
      )
    level_labels = c("Not at all","A little bit","Somewhat","Quite a bit","Very much")
  } else if (fmt=="int_5_fmt" & missing==T){
    a <- a %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          is.na(a$proqol)==T ~ "Missing",
          a$proqol == 0 ~ "Not at all",
          a$proqol == 1 ~ "A little bit",
          a$proqol == 2 ~ "Somewhat",
          a$proqol == 3 ~ "Quite a bit",
          a$proqol == 4 ~ "Very much",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(a$proqol),
                         levels=c("Missing","Not at all","A little bit","Somewhat","Quite a bit","Very much"))
      )
    b <- b %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          is.na(b$proqol)==T ~ "Missing",
          b$proqol == 0 ~ "Not at all",
          b$proqol == 1 ~ "A little bit",
          b$proqol == 2 ~ "Somewhat",
          b$proqol == 3 ~ "Quite a bit",
          b$proqol == 4 ~ "Very much",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(b$proqol),
                         levels=c("Missing","Not at all","A little bit","Somewhat","Quite a bit","Very much"))
      )
    level_labels = c("Missing","Not at all","A little bit","Somewhat","Quite a bit","Very much")
  } else if (fmt=="yn_2_fmt" & missing==F){
    a <- a %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          a$proqol == 0 ~ "No",
          a$proqol == 1 ~ "Yes",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(a$proqol),levels=c("No","Yes"))
      )
    b <- b %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          b$proqol == 0 ~ "No",
          b$proqol == 1 ~ "Yes",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(b$proqol),levels=c("No","Yes"))
      )
    level_labels = c("No","Yes")
  } else if (fmt=="yn_2_fmt" & missing==T){
    a <- a %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          is.na(a$proqol)==T ~ "Missing",
          a$proqol == 0 ~ "No",
          a$proqol == 1 ~ "Yes",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(a$proqol),levels=c("Missing","No","Yes"))
      )
    b <- b %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          is.na(b$proqol)==T ~ "Missing",
          b$proqol == 0 ~ "No",
          b$proqol == 1 ~ "Yes",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(b$proqol),levels=c("Missing","No","Yes"))
      )
    level_labels = c("Missing","No","Yes")
  } else if (fmt=="comp" & missing ==F){
    a <- a %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          a$proqol == 0 ~ "0",
          a$proqol == 1 ~ "1",
          a$proqol == 2 ~ "2",
          a$proqol == 3 ~ "3",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(a$proqol),levels=c("0","1","2","3"))
      )
    b <- b %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          b$proqol == 0 ~ "0",
          b$proqol == 1 ~ "1",
          b$proqol == 2 ~ "2",
          b$proqol == 3 ~ "3",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(b$proqol),levels=c("0","1","2","3"))
      )
    level_labels = c("0","1","2","3")
  } else if (fmt=="comp" & missing==T){
    a <- a %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          is.na(a$proqol)==T ~ "Missing",
          a$proqol == 0 ~ "0",
          a$proqol == 1 ~ "1",
          a$proqol == 2 ~ "2",
          a$proqol == 3 ~ "3",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(a$proqol),levels=c("Missing","0","1","2","3"))
      )
    b <- b %>%
      dplyr::mutate(
        proqol = dplyr::case_when(
          is.na(b$proqol)==T ~ "Missing",
          b$proqol == 0 ~ "0",
          b$proqol == 1 ~ "1",
          b$proqol == 2 ~ "2",
          b$proqol == 3 ~ "3",
          TRUE ~ ""
        ),
        proqol = ordered(as.character(b$proqol),levels=c("Missing","0","1","2","3"))
      )
    level_labels = c("Missing","0","1","2","3")
  } else {
    level_labels = levels(b$proqol)
  }


  if (cycle_order == T){
    a$Cycle = ordered(a$Cycle,levels=rev(levels(a$Cycle)))
    b$Cycle = ordered(b$Cycle,levels=rev(levels(b$Cycle)))
    dsn[[cycle_var]] = as.factor(dsn[[cycle_var]])
    dsn[[cycle_var]] = ordered(dsn[[cycle_var]],
                               levels=rev(levels(dsn[[cycle_var]])))
    a_n$Cycle = ordered(a_n$Cycle,levels=rev(levels(a_n$Cycle)))
    b_n$Cycle = ordered(b_n$Cycle,levels=rev(levels(b_n$Cycle)))
    a_n$y = a_n$y2
    b_n$y = b_n$y2
  } else {
    a_n$y = a_n$y1
    b_n$y = b_n$y1
  }
  # ----------------------------------------------------------------
  # -- Set up a missingness variable to see the graphs with a pattern
  # ----------------------------------------------------------------
  a <- a %>%
    dplyr::mutate(
      missing = dplyr::case_when(
        a$proqol=="Missing" ~ 1,
        TRUE ~ 0
      )
    )
  b <- b %>%
    dplyr::mutate(
      missing = dplyr::case_when(
        b$proqol=="Missing" ~ "Yes",
        TRUE ~ "No"
      )
    )
  # ----------------------------------------------------------------
  # -- Set the colors for the graphs based on the input
  # ----------------------------------------------------------------


  if((colors==1 | is.na(colors)==T) & missing == F &
     fmt %in% c("sev_5_fmt","int_5_fmt","frq_5_fmt")){
    color <- c("white","#90B0D9","#4D7EBF","#13478C","#142233")
  } else if ((colors==2) & missing == F &
             fmt %in% c("sev_5_fmt","int_5_fmt","frq_5_fmt")) {
    color <- c("white","#0571B0","#92C5DE","#F4A582","#CA0020")
  } else if ((colors==3) & missing == F &
             fmt %in% c("sev_5_fmt","int_5_fmt","frq_5_fmt")) {
    color <- c("white","#E6E6E6","#BDBDBD","#636363","#000000")
  } else if ((colors==1 | is.na(colors)==T) &
             missing == T & fmt %in% c("sev_5_fmt","int_5_fmt","frq_5_fmt")){
    #color <- c("#FFFFFF","#CCCCE7","#9999D0","#6565B9","#3232A2","#00008B")
    #color <- c("white","#90B0D9","#4D7EBF","#13478C","#16355B","#142233")
    color <- c("#8D8D8D","white","#90B0D9","#4D7EBF","#13478C","#142233")
    #color <- c("white","white","#90B0D9","#4D7EBF","#13478C","#142233")
  } else if (colors==2 & fmt %in% c("sev_5_fmt","int_5_fmt","frq_5_fmt")){
    color <- c("#999999",  "#56B4E9", "#009E73", "#F0E442",  "#D55E00", "#CC79A7")
  } else if ((colors==1 | is.na(colors)==T) & missing == F & fmt=="comp"){
    color <- c("white","#E5BFC6","#D9576E","#99293D")
  } else if (colors==2 & missing==F & fmt=="comp"){
    color <- c("white","#A6DBA0","#C2A5CF","#7B3294")
  } else if (colors==3 & missing==F & fmt=="comp"){
    color <- c("white","#BDBDBD" ,"#636363","#000000")
  } else if (colors==3 & missing == F & fmt=="yn_2_fmt"){
    color <- c("#D3D3D3","#8B8B8B")
  } else if (colors==3 & missing == T & fmt=="yn_2_fmt"){
    color <- c("#FFFFFF","#D3D3D3","#8B8B8B")
  } else if (colors==2 & missing == F & fmt=="yn_2_fmt"){
    color <- c("#92C5DE","#CA0020")
  } else if (colors==2 & missing == T & fmt=="yn_2_fmt"){
    color <- c("white","#92C5DE","#CA0020")
  } else if (colors==1 & missing == F & fmt=="yn_2_fmt"){
    color <- c("#90B0D9","#13478C")
  } else if (colors==1 & missing == T & fmt=="yn_2_fmt"){
    color <- c("white","#90B0D9","#13478C")
  } else if (colors==3 & missing == T &
             fmt %in% c("sev_5_fmt","int_5_fmt","frq_5_fmt")){
    color <- c("white","#E6E6E6","#BDBDBD","#636363","#363636","#000000")
  } else if (colors==3 & missing==T & fmt=="comp"){
    color <- c("white","#BDBDBD" ,"#636363","#363636","#000000")
  } else if ((colors==1 | is.na(colors)==T) & missing == T & fmt=="comp"){
    color <- c("#8D8D8D","white","#E5BFC6","#D9576E","#730F21")
  } else if (colors==2 & missing==T & fmt=="comp"){
    color <- c("white","#0571B0","#92C5DE","#F4A582","#CA0020")
  }



  # ----------------------------------------------------------------
  # -- Create the bar plot for Arm B or the second arm
  # -- this will make the right wing of the butterfly
  # ----------------------------------------------------------------

  if (fmt %in% c("sev_5_fmt","int_5_fmt","frq_5_fmt") & missing == F){
    bar_plotb <- ggplot2::ggplot(ggplot2::aes(x=Cycle,y=percent,fill=proqol),
                                 data=b) +
      ggplot2::geom_bar(stat = 'identity', color = "black") +
      ggplot2::coord_flip() +
      ggplot2::ggtitle(arm_label_b) + ggplot2::ylab("Percent") +
      ggplot2::scale_fill_manual(values=color,
                                 drop=F,
                                 name= label,
                                 labels = level_labels) +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y  = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.title = ggplot2::element_text(label),
                     legend.position = "none")
  } else if (fmt %in% c("sev_5_fmt","int_5_fmt","frq_5_fmt") & missing == T){
    bar_plotb <- ggplot2::ggplot(ggplot2::aes(x=Cycle,y=percent,fill=proqol),
                                 data=b) +
      ggplot2::geom_bar(stat = 'identity',color = "black") +
      ggplot2::coord_flip() +
      ggplot2::ggtitle(arm_label_b) + ggplot2::ylab("Percent") +
      ggplot2::scale_fill_manual(values=color,
                                 drop=F,
                                 name= label,
                                 labels = level_labels) +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y  = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.title = ggplot2::element_text(label),
                     legend.position = "none")

  } else if (fmt=="yn_2_fmt" & missing == F) {
    bar_plotb <- ggplot2::ggplot(ggplot2::aes(x=Cycle,y=percent,fill=proqol),
                                 data=b) +
      ggplot2::geom_bar(stat = 'identity', color = "black") +
      ggplot2::coord_flip() +
      ggplot2::ggtitle(arm_label_b) + ggplot2::ylab("Percent") +
      ggplot2::scale_fill_manual(values=color,
                                 drop=F,
                                 name= label,
                                 labels = level_labels) +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y  = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.title = ggplot2::element_text(label),
                     legend.position = "none")
  } else if (fmt=="yn_2_fmt" & missing == T) {
    bar_plotb <- ggplot2::ggplot(ggplot2::aes(x=Cycle,y=percent,fill=proqol),
                                 data=b) +
      ggplot2::geom_bar(stat = 'identity', color = "black") +
      ggplot2::coord_flip() +
      ggplot2::ggtitle(arm_label_b) + ggplot2::ylab("Percent") +
      ggplot2::scale_fill_manual(values=color,
                                 drop=F,
                                 name= label,
                                 labels = level_labels) +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y  = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.title = ggplot2::element_text(label),
                     legend.position = "none")
  } else if (fmt=="comp" & missing == F) {
    bar_plotb <- ggplot2::ggplot(ggplot2::aes(x=Cycle,y=percent,fill=proqol),
                                 data=b) +
      ggplot2::geom_bar(stat = 'identity', color = "black") +
      ggplot2::coord_flip() +
      ggplot2::ggtitle(arm_label_b) + ggplot2::ylab("Percent") +
      ggplot2::scale_fill_manual(values=color,
                                 drop=F,
                                 name= label,
                                 labels = level_labels) +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y  = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.title = ggplot2::element_text(label),
                     legend.position = "none")
  } else if (fmt=="comp" & missing == T) {
    bar_plotb <- ggplot2::ggplot(ggplot2::aes(x=Cycle,y=percent,fill=proqol),
                                 data=b) +
      ggplot2::geom_bar(stat = 'identity',color = "black") +
      ggplot2::coord_flip() +
      ggplot2::ggtitle(arm_label_b) + ggplot2::ylab("Percent") +
      ggplot2::scale_fill_manual(values=color,
                                 drop=F,
                                 name= label,
                                 labels = level_labels) +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y  = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.title = ggplot2::element_text(label),
                     legend.position = "none")
  }


  # ----------------------------------------------------------------
  # -- Create the bar plot for Arm A or the first arm
  # -- this will make the left wing of the butterfly
  # ----------------------------------------------------------------

  if (fmt %in% c("sev_5_fmt","int_5_fmt","frq_5_fmt") & missing == F){
    bar_plota <- ggplot2::ggplot(ggplot2::aes(x=Cycle,y=percent,fill=proqol),data=a) +
      ggplot2::geom_bar(stat = 'identity', color = "black") +
      ggplot2::coord_flip() + ggplot2::ggtitle(arm_label_a) + ggplot2::ylab("Percent") +
      ggplot2::scale_fill_manual(values=color,
                                 drop=F,
                                 name= label,
                                 labels = level_labels) +
      ggplot2::scale_y_reverse() +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.title = ggplot2::element_text(label),
                     legend.position="none")
  } else if (fmt %in% c("sev_5_fmt","int_5_fmt","frq_5_fmt") & missing == T){
    bar_plota <- ggplot2::ggplot(ggplot2::aes(x=Cycle,y=percent,fill=proqol),data=a) +
      ggplot2::geom_bar(stat = 'identity',color = "black") +
      ggplot2::coord_flip() + ggplot2::ggtitle(arm_label_a) + ggplot2::ylab("Percent") +
      ggplot2::scale_fill_manual(values=color,
                                 drop=F,
                                 name= label,
                                 labels = level_labels) +
      ggplot2::scale_y_reverse() +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.title = ggplot2::element_text(label),
                     legend.position="none")
  } else if (fmt=="yn_2_fmt" & missing == F ) {
    bar_plota <- ggplot2::ggplot(ggplot2::aes(x=Cycle,y=percent,fill=proqol),data=a) +
      ggplot2::geom_bar(stat = 'identity', color = "black") +
      ggplot2::coord_flip() + ggplot2::ggtitle(arm_label_a) + ggplot2::ylab("Percent") +
      ggplot2::scale_fill_manual(values=color,
                                 drop=F,
                                 name= label,
                                 labels = level_labels) +
      ggplot2::scale_y_reverse() +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.title = ggplot2::element_text(label),
                     legend.position="none")
  } else if (fmt=="yn_2_fmt" & missing == T) {
    bar_plota <- ggplot2::ggplot(ggplot2::aes(x=Cycle,y=percent,fill=proqol),data=a) +
      ggplot2::geom_bar(stat = 'identity',color = "black") +
      ggplot2::coord_flip() + ggplot2::ggtitle(arm_label_a) + ggplot2::ylab("Percent") +
      ggplot2::scale_fill_manual(values=color,
                                 drop=F,
                                 name= label,
                                 labels = level_labels) +
      ggplot2::scale_y_reverse() +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.title = ggplot2::element_text(label),
                     legend.position="none")
  } else if (fmt=="comp" & missing == F) {
    bar_plota <- ggplot2::ggplot(ggplot2::aes(x=Cycle,y=percent,fill=proqol),data=a) +
      ggplot2::geom_bar(stat = 'identity', color = "black") +
      ggplot2::coord_flip() + ggplot2::ggtitle(arm_label_a) + ggplot2::ylab("Percent") +
      ggplot2::scale_fill_manual(values=color,
                                 drop=F,
                                 name= label,
                                 labels = level_labels) +
      ggplot2::scale_y_reverse() +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.title = ggplot2::element_text(label),
                     legend.position="none")
  } else if (fmt=="comp" & missing == T) {
    bar_plota <- ggplot2::ggplot(ggplot2::aes(x=Cycle,y=percent,fill=proqol),data=a) +
      ggplot2::geom_bar(stat = 'identity', color = "black") +
      ggplot2::coord_flip() + ggplot2::ggtitle(arm_label_a) + ggplot2::ylab("Percent") +
      ggplot2::scale_fill_manual(values=color,
                                 drop=F,
                                 name= label,
                                 labels = level_labels) +
      ggplot2::scale_y_reverse() +
      ggplot2::xlab(ggplot2::element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.title = ggplot2::element_text(label),
                     legend.position="none")
  }


  #### Cycle labels
  ### make sure that the cycle variable is a character if it is numeric or
  ### a factor it might break things.
  # ----------------------------------------------------------------
  # -- Create the labels for the cycles
  # -- this will make the middle of the butterfly or the spine/thorax
  # ----------------------------------------------------------------


  lab <- ggplot2::ggplot(data=a_n,ggplot2::aes(y=Cycle)) + ggplot2::xlim(0,0) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size=12,face="bold",hjust=0.5)) +
    ggplot2::ylab("") #+ ggtitle("Timepoint")

  # ----------------------------------------------------------------
  # -- Create the list of the N for each arm for each timepoint
  # -- these will be outside the wings
  # ----------------------------------------------------------------

  n_a <- ggplot2::ggplot(data=a_n,ggplot2::aes(y=y,x=x)) +
    ggplot2::geom_text(ggplot2::aes(label=n,family=n)) +
    ggplot2::ggtitle("N") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust=0.5),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank()) +
    ggplot2::theme(title = ggplot2::element_text(face = "italic"))

  n_b <- ggplot2::ggplot(data=b_n,ggplot2::aes(y=y,x=x)) +
    ggplot2::geom_text(ggplot2::aes(label=n,family=n)) +
    ggplot2::ggtitle("N") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust=0.5),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank()) +
    ggplot2::theme(title = ggplot2::element_text(face = "italic"))


  # ----------------------------------------------------------------
  # -- Create the butterfly plot
  # -- based on all the parts
  # ----------------------------------------------------------------

  if(missing==F & display_n == T){
    butterfly <- n_a +
      bar_plota +
      lab +
      bar_plotb +
      n_b +
      patchwork::plot_layout(ncol=5,widths=c(0.05,0.435,0.03,0.435,0.05),guides='collect') &
      ggplot2::theme(legend.position = "bottom")

  }else if (missing==T) {
    butterfly <- bar_plota +
      lab +
      bar_plotb +
      patchwork::plot_layout(ncol=3,widths=c(0.44,0.03,0.44),guides='collect') &
      ggplot2::theme(legend.position = "bottom")

  } else if (missing ==F & display_n == F){
    butterfly <- bar_plota +
      lab +
      bar_plotb +
      patchwork::plot_layout(ncol=3,widths=c(0.44,0.03,0.44),guides='collect') &
      ggplot2::theme(legend.position = "bottom")
  }

  # ----------------------------------------------------------------
  # -- Create the returns
  # ----------------------------------------------------------------

  if(plot_data==T) {
    return(butterfly)
  } else if(plot_data==F & missing==F & display_n==T){
    plots <- list(butterfly,n_a,bar_plota,lab,bar_plotb,n_b,a,b,a_n,b_n)
    return(plots)
  } else if(plot_data==F & missing==F & display_n==F){
    plots <- list(butterfly,bar_plota,lab,bar_plotb,a,b,a_n,b_n)
    return(plots)
  } else if(plot_data==F & missing==T & display_n==T){
    plots <- list(butterfly,bar_plota,lab,bar_plotb,a,b,a_n,b_n)
    return(plots)
  }
}
