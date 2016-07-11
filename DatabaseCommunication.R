library(RPostgreSQL)
library(pracma)
connectDatabase <- function(dbname, host, user, port, password) {
  # loads the PostgreSQL driver
  driver <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  connection <- dbConnect(driver, dbname = dbname,
                          host = host, port = port,
                          user = user, password = password)
  return(connection)
}

#connect to a postgresql database


#create current database schema
#should only be run once
createDatabase <- function(con) {
  
  create_project <- "CREATE TABLE project(
  project_code text,
  pk SERIAL PRIMARY KEY
  );"
  dbGetQuery(con, create_project)
  
  create_timepoint <- "CREATE TABLE timepoint(
    pk SERIAL PRIMARY KEY,
    visit integer,
    days integer
  );"
  dbGetQuery(con, create_timepoint)
  
  create_cell_type <- "CREATE TABLE sample_type(
  sample_type text,
  pk SERIAL PRIMARY KEY
  );"
  dbGetQuery(con, create_cell_type)
  
  
  create_lab <- "CREATE TABLE lab(
  name text,
  pk SERIAL PRIMARY KEY
  );"
  dbGetQuery(con, create_lab)  
  
  create_study <- "CREATE TABLE study(
  study_name text,
  extraction_method_protocol text,
  population_spec text,
  treatment_category_spec text,
  sample_cell_condition text,
  lotnumber integer,
  lab_pk integer REFERENCES lab(pk),
  project_pk integer REFERENCES project(pk),
  sample_type_pk integer REFERENCES sample_type(pk),
  pk SERIAL PRIMARY KEY
  );"
  dbGetQuery(con, create_study)
  
  dbGetQuery(con, "CREATE INDEX study_index ON study(project_pk)")
  
  create_unit <- "CREATE TABLE variable_unit(
  pk SERIAL PRIMARY KEY,
  unit text
  );"
  dbGetQuery(con, create_unit)
  
  create_variable_name <- "CREATE TABLE variable_name(
    pk SERIAL PRIMARY KEY,
    name_abbrev text,
    name_full text
  );"
  dbGetQuery(con, create_variable_name)
  
  create_variable <- "CREATE TABLE variable(
  pk SERIAL PRIMARY KEY,
  protocol_number text,
  upper_limit text,
  lower_limit text,
  no_sample_available text,
  variable_unit_pk integer REFERENCES variable_unit(pk),
  variable_name_pk integer REFERENCES variable_name(pk)
  );"
  dbGetQuery(con, create_variable)

  create_igroup <- "CREATE TABLE igroup(
    pk SERIAL PRIMARY KEY,
    igroup text
  );"
  dbGetQuery(con, create_igroup)
  
  create_subject <- "CREATE TABLE subject(
  pk SERIAL PRIMARY KEY,
  subject_num integer,
  igroup_pk integer REFERENCES igroup(pk)
  );"
  dbGetQuery(con, create_subject)
  
  dbGetQuery(con, "CREATE INDEX subject_index ON subject(igroup_pk)")
  
  create_measurement <- "CREATE TABLE measurement(
  pk SERIAL PRIMARY KEY,
  value text,
  timepoint_pk integer REFERENCES timepoint(pk),
  variable_pk integer REFERENCES variable(pk),
  subject_pk integer REFERENCES subject(pk),
  study_pk integer REFERENCES study(pk)
  );"
  dbGetQuery(con, create_measurement)
  
  dbGetQuery(con, "CREATE INDEX measurement_index ON measurement(timepoint_pk, variable_pk, subject_pk, study_pk)")
  
  
  create_int_to_string_func <- "CREATE OR REPLACE FUNCTION convert_to_integer(v_input text)
RETURNS NUMERIC AS $$
  DECLARE v_numeric_value NUMERIC DEFAULT NULL;
  BEGIN
  BEGIN
  v_numeric_value := v_input::NUMERIC;
  EXCEPTION WHEN OTHERS THEN
  RETURN NULL;
  END;
  RETURN v_numeric_value;
  END;
  $$ LANGUAGE plpgsql;"
  dbGetQuery(con, create_int_to_string_func)
}

con <- connectDatabase("postgres", "localhost", "postgres", 5432, "Passw0rd")
if(!(dbExistsTable(con, "project"))) createDatabase(con)
dbDisconnect(con)

#helper function to check if row exists if not insert
#could be written as a single query but all solutions I know of 
#lead to a data race <- maybe not acceptable
checkInsert <- function(con, table_name, check_column, check_value, column_list, insert_list) {
  check_query <- sprintf("SELECT pk FROM %s WHERE %s=\'%s\'", table_name, check_column, check_value)
  checked_pk <- dbGetQuery(con, check_query)
  if(length(checked_pk) == 0) {
    add_query <- sprintf("INSERT INTO %s(%s) VALUES (\'%s\') RETURNING pk", 
                         table_name, paste(column_list, collapse=", "), paste(insert_list, collapse="\', \'"))
    checked_pk <- strtoi(dbGetQuery(con, add_query))
  }
  return(checked_pk)
}

#parse a study to db given its general info table and actual data table
addStudy <- function(con, general_info_root, study_data_root, study_name, total_studies, progress) {
  #arbitrary length because R is super slow appending in for-loop
  #may need to error check if first lines ever longer than 10
    general_info_list <- list()
    general_info_file <- file(general_info_root)
    general_info_lines <- readLines(general_info_file)
    #again 10 is hard coded
    index <- 0
    for (q in 1:100) {
      #split by any number of commas to be able to account for dynamic row number
      line <- as.list(strsplit(general_info_lines[[q]], ",+")[[1]])
      index <- q
      if (length(line) == 0) next;
      if (length(line) == 1) next;
      if (strcmp(line[[1]], "Visit number")) {
        index <- index + 1
        break
      }
      general_info_list[[line[[1]]]] <- line[[2]]
    }
    line <- as.list(strsplit(general_info_lines[[index]], ",+")[[1]])
    visit_to_days <- list()
    break_flag <- FALSE
    while (!(length(line) == 0)) {
      visit_to_days[[toString(line[[1]])]] <- line[[2]]
      index <- index + 1
      tryCatch({
      line <- as.list(strsplit(general_info_lines[[index]], ",+")[[1]])
      }, error = function(e) {
        break_flag <<- TRUE
      })
      if (break_flag) break
    }
    for(name in names(visit_to_days)) {
      if (is.na(as.numeric(name))) next
      time_unit <- sub("[^[:alpha:]]+", "", visit_to_days[[name]])
      if (strcmp(time_unit, "days")) {
        visit_to_days[[name]] <- strtoi( sub("[^[:digit:]]+", "", visit_to_days[[name]]))
      } else if (strcmp(time_unit, "weeks")) {
        visit_to_days[[name]] <- strtoi( sub("[^[:digit:]]+", "", visit_to_days[[name]])) * 7
      } else {
        next
      }
    }
    close(general_info_file)
    project_code <- general_info_list[["Study code"]][[1]]
    sample_type <- general_info_list[["Sample/Cell type"]][[1]]
    lab_name <- general_info_list[["Lab name"]][[1]]
    extraction_protocol_number <- general_info_list[["Protocol number extraction method"]][[1]]
    population_specs <- general_info_list[["Specifics on subject population"]][[1]]
    treatment_group_specs <-  general_info_list[["Specifics on treatment groups"]][[1]]
    sample_condition <- general_info_list[["Special sample/cell condition"]][[1]]
    lotnumber <- strtoi(general_info_list[["Lotnumber"]][[1]])
    
    project_pk <- strtoi(checkInsert(con, "project", "project_code", project_code, c("project_code"), c(project_code)))
    sample_type_pk <- strtoi(checkInsert(con, "sample_type", "sample_type", sample_type, c("sample_type"), c(sample_type)))
    lab_pk <- strtoi(checkInsert(con, "lab", "name", lab_name, c("name"), c(lab_name)))
    
    add_study <- sprintf("INSERT INTO study(study_name, extraction_method_protocol, population_spec, treatment_category_spec,
                         sample_cell_condition, lotnumber, project_pk, sample_type_pk, lab_pk) 
                         VALUES (\'%s\', \'%s\', \'%s\', \'%s\', \'%s\', %i, %i, %i, %i) RETURNING pk",
                         study_name, extraction_protocol_number, population_specs,
                         treatment_group_specs, sample_condition, strtoi(lotnumber), strtoi(project_pk), strtoi(sample_type_pk), strtoi(lab_pk))
    study_pk <- strtoi(dbGetQuery(con, add_study))
  
    #arbitrary 10 again
    study_file_con <- file(study_data_root)
    variable_attr_list <- list()
    study_file_lines <- readLines(study_file_con)
    lines_to_skip <- -2
    previous_line <- NULL
    name_list <- list()
    #read the first box w/variable attributes
    #hardcoded again
    for (q in 1:15) {
      lines_to_skip <- lines_to_skip + 1
      line <- as.list(strsplit(study_file_lines[[q]], ",+")[[1]])
      if (length(line) == 0) next;
      if (length(line) == 1) next;
      if (!(is.null(previous_line)) && strcmp(previous_line[[1]], "Subject#") == TRUE) break;
      if (strcmp(line[[1]], "Subject#") == TRUE) {
        abbrev_names = line[which(!line %in% list("Group", "Subject#", "Remarks", "Timepoint (Visit)", "Barcode", "X"))]
        for (t in 1:length(abbrev_names)) {
          print(abbrev_names)
          name_list[[abbrev_names[[t]]]] <- previous_line[[match(abbrev_names[[t]], line)]]
        }
      }
      variable_attr_list[[line[[1]]]] <- tail(line, -1)
      previous_line_string <- study_file_lines[[q]]
      if (strcmp(substring(previous_line_string, nchar(previous_line_string), nchar(previous_line_string)), ",") == TRUE) {
        previous_line_string <- paste(previous_line_string, ",")
      }
      previous_line <- as.list(strsplit(previous_line_string, ",")[[1]])
    }
    close(study_file_con)
    study_file <- read.csv(study_data_root, sep = ',', header = T, skip=lines_to_skip, check.names=FALSE)
    #collect primary keys of variables
    current_variables_pks <- vector(mode="integer", length=1000)
  
    #loop through all the variables
    measurement_names <- colnames(study_file)[which(!colnames(study_file)
                                                    %in% list("Group", "Subject#", "Remarks", "Timepoint (Visit)", "Barcode", "X"))]
    for (p in 1:length(measurement_names)) {
      variable_name <- name_list[[measurement_names[[p]]]]
      if (is.null(variable_name)) variable_name <- "NA"
      variable_units <- variable_attr_list[["Units"]][[p]]
      if (is.null(variable_units)) variable_units <- "NA"
      variable_name <- str_replace_all(variable_name, "[[:punct:]]", "")
      
      variable_name_pk <- checkInsert(con, "variable_name", "name_abbrev", measurement_names[[p]],
                                     c("name_abbrev", "name_full"), c(measurement_names[[p]], variable_name))
      variable_unit_pk <- checkInsert(con, "variable_unit", "unit", variable_units,
                                     c("unit"), variable_units)
  
      variable_protocol <- variable_attr_list[["Protocol number analysis"]][[p]]
      if (is.null(variable_protocol)) variable_protocol <- "NA"
      variable_lower_limit <- variable_attr_list[["Lower limit"]][[p]]
      if (is.null(variable_lower_limit)) variable_lower_limit <- "NA"
      variable_upper_limit <- variable_attr_list[["Upper Limit"]][[p]]
      if (is.null(variable_upper_limit)) variable_upper_limit <- "NA"
      variable_no_sample <- variable_attr_list[["No sample available"]][[p]]
      if (is.null(variable_no_sample)) variable_no_sample <- "NA"
      add_variable <- sprintf("INSERT INTO variable(protocol_number, upper_limit, lower_limit, no_sample_available,
                             variable_name_pk, variable_unit_pk)
                             VALUES (\'%s\', \'%s\', \'%s\', \'%s\', %i, %i) RETURNING pk",
                             variable_protocol, variable_upper_limit, variable_lower_limit, variable_no_sample,
                            strtoi(variable_name_pk), strtoi(variable_unit_pk))
      variable_pk <- strtoi(dbGetQuery(con, add_variable))
      current_variables_pks[[p]] <- variable_pk
    }
    subject_pk_cache <- list()
    
    get_subjects <- sprintf("SELECT subject.pk as subject_pk, subject.subject_num FROM project JOIN study ON study.project_pk = project.pk
                                      JOIN measurement ON measurement.study_pk = study.pk JOIN subject ON subject.pk = measurement.subject_pk
                                      WHERE project.pk=%i GROUP BY subject.pk, subject.subject_num", project_pk)
    subject_data <- dbGetQuery(con, get_subjects)
    
    get_groups <- sprintf("SELECT igroup.pk as igroup_pk, igroup.igroup FROM project JOIN study ON study.project_pk = project.pk
                          JOIN measurement ON measurement.study_pk = study.pk JOIN subject ON subject.pk = measurement.subject_pk JOIN igroup
                          ON igroup.pk = subject.igroup_pk WHERE project.pk=%i GROUP BY igroup.pk, igroup.igroup", project_pk)
    group_data <- dbGetQuery(con, get_groups) 
    
    get_timepoints <- sprintf("SELECT timepoint.pk as timepoint_pk, timepoint.visit FROM project JOIN study ON study.project_pk = project.pk
                              JOIN measurement ON measurement.study_pk = study.pk JOIN timepoint ON timepoint.pk = measurement.timepoint_pk
                              WHERE project.pk=%i GROUP BY timepoint.pk, timepoint.visit", project_pk)
    timepoint_data <- dbGetQuery(con, get_timepoints) 
    
    subjects <- list()
    groups <- list()
    timepoints <- list()
    if (!(nrow(subject_data) == 0)) {
      subjects <- subject_data[,"subject_pk"]
      names(subjects) <-  as.character(subject_data[,"subject_num"])
      groups <- group_data[,"igroup_pk"]
      names(groups) <- as.character(group_data[,"igroup"])
      timepoints <- timepoint_data[,"timepoint_pk"]
      names(timepoints) <- as.character(timepoint_data[,"visit"])
    }
    for (i in 1:nrow(study_file)) {
      if (strcmp(toString(study_file[i, "Subject#"]),   "NA") == TRUE) {
       break 
      }
      current_group <- as.character(study_file[i, "Group"])
      current_subject <- study_file[i, "Subject#"]
      current_timepoint <- study_file[i, "Timepoint (Visit)"]
      subject_pk <- NULL
      if (!(current_subject %in% names(subjects))) {
        if (!(current_group %in% names(groups))) {
          add_group <- sprintf("INSERT INTO igroup(igroup)
                               VALUES (\'%s\') RETURNING pk", current_group)
          group_pk <- strtoi(dbGetQuery(con, add_group))
          groups[[current_group]] <- group_pk
        }
        group_pk <- groups[[current_group]]
        add_subject <- sprintf("INSERT INTO subject(subject_num, igroup_pk)
                               VALUES (%i, %i) RETURNING pk", current_subject, group_pk)
        subject_pk <- strtoi(dbGetQuery(con, add_subject))
        subjects[[toString(current_subject)]] <- subject_pk
      }
    
      if (!(current_timepoint %in% names(timepoints))) {
        days <- -1
        if (toString(current_timepoint) %in% names(visit_to_days)) {
          days <- visit_to_days[[toString(current_timepoint)]]
        }
        add_timepoint <- sprintf("INSERT INTO timepoint(visit, days)
                               VALUES (%s, %s) RETURNING pk", current_timepoint, days)
        timepoint_pk <- strtoi(dbGetQuery(con, add_timepoint))
        timepoints[[toString(current_timepoint)]] <- timepoint_pk
      }
      
      subject_pk <- subjects[[toString(current_subject)]]
      timepoint_pk <- timepoints[[toString(current_timepoint)]]
      insert_all_measurements <- ""
      for(j in 1:length(measurement_names)){
        add_measurement <- sprintf("INSERT INTO measurement(value, timepoint_pk, subject_pk, variable_pk, study_pk)
                                   VALUES (\'%s\', %i, %i, %i, %i) RETURNING pk", study_file[i, measurement_names[j]],
                                   timepoint_pk, subject_pk, current_variables_pks[j], study_pk)
        insert_all_measurements <- paste(add_measurement, ";", insert_all_measurements, sep='')
      }
      dbGetQuery(con, insert_all_measurements)
      subject_pk_cache[[current_subject]] <- subject_pk
      progress$inc(1/(nrow(study_file) * total_studies))
    }
}

#retrieve data from n studies with extra column "new names" for conversion to wide format
getStudyDataFrame <- function(con, study_pks) {
  # get_variable_names <- sprintf("SELECT name_abbrev
  #                              FROM measurement
  #                              JOIN subject ON subject.pk = measurement.subject_pk
  #                              JOIN study ON study.pk = measurement.study_pk
  #                              JOIN variable ON variable.pk = measurement.variable_pk JOIN variable_name ON variable_name.pk = variable.variable_name_pk
  #                              WHERE study.pk=%i
  #                              ORDER BY subject_num, timepoint", study_pk)
  # 
  # variable_list <- unique(dbGetQuery(con, get_variable_names)[,"name_abbrev"])
  # initial_split_string <- "SELECT x.subject_num as \"Subject#\", x.category as \"Group\", x.timepoint as \"Timepoint (Visit)\""
  # for (l in 1:length(variable_list)) {
  #   initial_split_string <- paste(initial_split_string, sprintf("split_part(x.value_list, ',', %i) AS \"%s\"", l, paste(variable_list[[l]], toString(study_pk))), sep=", ")
  # }
  base_query <- "SELECT DISTINCT ON (subject_num, visit, name_abbrev) subject_num as \"Subject#\", igroup as \"Group\", value,
                  name_abbrev || '_' || visit as new_name FROM study JOIN measurement ON measurement.study_pk = study.pk 
                  JOIN subject ON subject.pk = measurement.subject_pk JOIN igroup ON igroup.pk = subject.igroup_pk
                  JOIN timepoint ON timepoint.pk = measurement.timepoint_pk JOIN variable ON variable.pk = measurement.variable_pk 
                  JOIN variable_name ON variable_name.pk = variable.variable_name_pk WHERE"
  for (i in 1:length(study_pks)-1) {
    base_query <- paste(base_query, sprintf("study.pk=%i OR", strtoi(study_pks[i])))
  }
  base_query <- paste(base_query, sprintf("study.pk=%i ORDER BY subject_num, visit, name_abbrev", strtoi(study_pks[length(study_pks)])))
  return(dbGetQuery(con, base_query))
}

getVariableAcross <- function(con, search_category, search_query) {
  info_table <- NULL
  get_info <- NULL
  if (strcmp(search_category, "Group")) {
    get_info <- sprintf("SELECT igroup as \"Group\", project_code as \"Project\", count(DISTINCT subject_num) as \"Number of Subjects\" FROM project JOIN study ON study.project_pk = project.pk JOIN
                        measurement ON measurement.study_pk = study.pk JOIN subject ON subject.pk = measurement.subject_pk JOIN
                        igroup ON igroup.pk = subject.igroup_pk WHERE igroup LIKE lower(\'%%%s%%\') GROUP BY igroup, project_code", search_query)
  } else {
    get_info <- sprintf("SELECT name_full as \"Name\", project_code as \"Project\", unit as \"Units\", upper_limit as \"Upper Limit\", 
                                lower_limit as \"Lower Limit\", protocol_number as \"Protocol\", 
                                 array_agg('(' || days || ',' || count || ')') as \"(Day, Samples)\" FROM 
                                (SELECT name_full, project_code, unit, upper_limit,
                                lower_limit, protocol_number, days, count(value) 
                                filter (WHERE value != \'nq\' AND value != \'NQ\' AND value != \'Nq\'
                                AND value != \'nQ\' AND value != \'NS\' AND value != \'ns\' AND value != \'Ns\' AND value != \'nS\'
                                AND value != \'BLD\' AND value != \'bld\' AND value != \'Bld\' AND value != \'\' AND value != \'n.q.\') as count
                                FROM project JOIN study ON study.project_pk = project.pk JOIN measurement ON measurement.study_pk = study.pk
                                JOIN timepoint ON timepoint.pk = measurement.timepoint_pk 
                                JOIN variable ON variable.pk = measurement.variable_pk JOIN variable_name ON variable_name.pk = variable.variable_name_pk
                                JOIN variable_unit ON variable_unit.pk = variable.variable_unit_pk WHERE lower(name_full) LIKE lower(\'%%%s%%\') GROUP BY name_full, project_code, unit, upper_limit, lower_limit, protocol_number, days ORDER BY days) t
                               GROUP BY name_full, project_code, unit, upper_limit, lower_limit, protocol_number ORDER BY project_code", search_query)
  }
  info_table <- dbGetQuery(con, get_info)
  return(info_table)
}
