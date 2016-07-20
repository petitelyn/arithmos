#file where all the complex database communication takes place

library(RPostgreSQL)
library(pracma)

#create database connection
#database values hard coded here
connectDatabase <- function() {
  driver <- dbDriver("PostgreSQL")
  connection <- dbConnect(driver, dbname = "arithmos",
                          host = "localhost", port = "5432",
                          user = "postgres", password = "Passw0rd")
  return(connection)
}

#Populates the postgreSQL database with tables
#should only be run if database is empty
#more here to document the structure of the database
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
  project_pk integer REFERENCES project(pk) ON DELETE CASCADE,
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
  timepoint_pk integer REFERENCES timepoint(pk) ON DELETE CASCADE,
  variable_pk integer REFERENCES variable(pk) ON DELETE CASCADE,
  subject_pk integer REFERENCES subject(pk) ON DELETE CASCADE,
  study_pk integer REFERENCES study(pk) ON DELETE CASCADE
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

#helper function to check if a row has been inserted, insert it if it hasn't, and return primary key
checkInsert <- function(con, table_name, check_column, check_value, column_list, insert_list) {
  #could possibly be written as a single query but difficult to avoid data race
  check_query <- sprintf("SELECT pk FROM %s WHERE %s=\'%s\'", table_name, check_column, check_value)
  checked_pk <- dbGetQuery(con, check_query)
  if(length(checked_pk) == 0) {
    add_query <- sprintf("INSERT INTO %s(%s) VALUES (\'%s\') RETURNING pk", 
                         table_name, paste(column_list, collapse=", "), paste(insert_list, collapse="\', \'"))
    checked_pk <- strtoi(dbGetQuery(con, add_query))
  }
  return(checked_pk)
}

#Parses a study into the database
#see readme for study format requirements
addStudy <- function(con, general_info_root, study_data_root, study_name, total_studies, progress, error_output) {
    #this function was put together under time constraints as a quick fix to uploading studies of a specific format
    #as the project expands to include more lenient file formatting, a totally new function should be built
    #this function has become a bit of a monster and it would be better to start from scratch
  
    #this is either -1 for an error or the primary key of the project a study is a part of 
    return_code <- 0
    #not indenting because the two try/catches really just wrap the whole function 
    tryCatch({
    general_info_list <- list()
    general_info_file <- file(general_info_root)
    general_info_lines <- readLines(general_info_file)
    index <- 1
    #split by any number of commas to be able to account for dynamic row number
    line <- as.list(strsplit(general_info_lines[[index]], ",+")[[1]])
    #parse the general info file until 'Visit number' row
    while (!(strcmp(line[[1]], "Visit number"))) {
      if (length(line) > 1) {
        general_info_list[[line[[1]]]] <- line[[2]]
      }
      index <- index + 1
      line <- as.list(strsplit(general_info_lines[[index]], ",+")[[1]])
    }
    #must increment index to next line
    #kind of repeated code, maybe accomplishable with a do-while but I am afraid of R
    index <- index + 1
    line <- as.list(strsplit(general_info_lines[[index]], ",+")[[1]])
    #this is a dictionary for the visit# to the time (in days, weeks, months, etc)
    visit_to_days <- list()
    #break off when the lines end
    break_flag <- FALSE
    #build collect the visit to time length dictionary
    while (!(length(line) == 0)) {
      #hardcoded to the specific position in the given format
      visit_to_days[[toString(line[[1]])]] <- line[[2]]
      index <- index + 1
      tryCatch({
      line <- as.list(strsplit(general_info_lines[[index]], ",+")[[1]])
      }, error = function(e) {
        break_flag <<- TRUE
      })
      if (break_flag) break
    }
    #convert all timepoints defined as days, weeks, months, or years to just a numeric number of days
    for(name in names(visit_to_days)) {
      #skip timepoints where the number of visits is not a number
      if (is.na(as.numeric(name))) next
      time_unit <- sub("[^[:alpha:]]+", "", visit_to_days[[name]])
      time_length <- strtoi( sub("[^[:digit:]]+", "", visit_to_days[[name]]))
      if (strcmp(time_unit, "days")) {
        visit_to_days[[name]] <- time_length
      } else if (strcmp(time_unit, "weeks")) {
        visit_to_days[[name]] <-  time_length * 7
      } else if (strcmp(time_unit, "months")) { 
        visit_to_days[[name]] <- time_length * 30 
      }  else if (strcmp(time_unit, "years")) { 
        visit_to_days[[name]] <- time_length * 365
      } else {
        #assume the default is days
        #leaving this here for another default
        visit_to_days[[name]] <- time_length
      }
    }
    close(general_info_file)
    #Extract all of the study specific information
    project_code <- general_info_list[["Study code"]][[1]]
    sample_type <- general_info_list[["Sample/Cell type"]][[1]]
    lab_name <- general_info_list[["Lab name"]][[1]]
    extraction_protocol_number <- general_info_list[["Protocol number extraction method"]][[1]]
    population_specs <- general_info_list[["Specifics on subject population"]][[1]]
    treatment_group_specs <-  general_info_list[["Specifics on treatment groups"]][[1]]
    sample_condition <- general_info_list[["Special sample/cell condition"]][[1]]
    lotnumber <- strtoi(general_info_list[["Lotnumber"]][[1]])
    #check to see if the project has been uploaded or not
    project_pk <- strtoi(checkInsert(con, "project", "project_code", project_code, c("project_code"), c(project_code)))
    #check for sample type and lab
    sample_type_pk <- strtoi(checkInsert(con, "sample_type", "sample_type", sample_type, c("sample_type"), c(sample_type)))
    lab_pk <- strtoi(checkInsert(con, "lab", "name", lab_name, c("name"), c(lab_name)))
    #get all of the studies, check if a study with the same file name has been uploaded
    all_studies <- dbGetQuery(con, sprintf("SELECT study_name FROM project JOIN study ON study.project_pk = project.pk WHERE project.pk=%i", project_pk))
    for (name in all_studies[["study_name"]]) {
      if (strcmp(study_name, name)) {
        print("Study with same name already uploaded.")
        return(-1)
      }
    }
    #insert the new study 
    add_study <- sprintf("INSERT INTO study(study_name, extraction_method_protocol, population_spec, treatment_category_spec,
                         sample_cell_condition, lotnumber, project_pk, sample_type_pk, lab_pk) 
                         VALUES (\'%s\', \'%s\', \'%s\', \'%s\', \'%s\', %i, %i, %i, %i) RETURNING pk",
                         study_name, extraction_protocol_number, population_specs,
                         treatment_group_specs, sample_condition, strtoi(lotnumber), strtoi(project_pk), strtoi(sample_type_pk), strtoi(lab_pk))
    study_pk <- strtoi(dbGetQuery(con, add_study))},
    error = function(e) {
      print(e)
      return_code <<- -1
    })
    #can't return directly from try/catch
    if(return_code == -1) return(return_code)
    
    #second try and catch now that a study has been added
    #if an error occurs after this point, the study is deleted
    tryCatch({
    #load the study data
    study_file_con <- file(study_data_root)
    #build list of variable attributes (units, etc)
    variable_attr_list <- list()
    study_file_lines <- readLines(study_file_con)
    lines_to_skip <- -2
    previous_line <- NULL
    name_list <- list()
    #read the first box w/variable attributes
    #the rows before Subject# column is defined
    #15 is a hard coded number, but based on the specified format it should not go beyond 15
    for (q in 1:15) {
      #record how many lines to skip when start reading actual file
      lines_to_skip <- lines_to_skip + 1
      line <- as.list(strsplit(study_file_lines[[q]], ",+")[[1]])
      #skipe empty or null lines
      if (length(line) == 0) next;
      if (length(line) == 1) next;
      #break when go past Subject# line
      if (!(is.null(previous_line)) && strcmp(previous_line[[1]], "Subject#") == TRUE) break;
      if (strcmp(line[[1]], "Subject#") == TRUE) {
        #if hit Subject# line, define all the variable names
        abbrev_names = line[which(!line %in% list("Group", "Subject#", "Remarks", "Timepoint (Visit)", "Barcode", "X"))]
        for (t in 1:length(abbrev_names)) {
          name_list[[abbrev_names[[t]]]] <- previous_line[[match(abbrev_names[[t]], line)]]
        }
      }
      #build variable attribute list
      #hardcoded for the specified format
      variable_attr_list[[line[[1]]]] <- tail(line, -1)
      previous_line_string <- study_file_lines[[q]]
      #add a comma to the previous line to be able to split along commas on empty lines
      if (strcmp(substring(previous_line_string, nchar(previous_line_string), nchar(previous_line_string)), ",") == TRUE) {
        previous_line_string <- paste(previous_line_string, ",")
      }
      #split along commas
      previous_line <- as.list(strsplit(previous_line_string, ",")[[1]])
    }
    close(study_file_con)
    #read the study file at a new connection on the skpped number of lines. This should start on the Subject# line
    study_file <- read.csv(study_data_root, sep = ',', header = T, skip=lines_to_skip, check.names=FALSE)
    #collect primary keys of variables
    current_variables_pks <- vector(mode="integer", length=1000)
  
    #get all the variables as list
    measurement_names <- colnames(study_file)[which(!colnames(study_file)
                                                    %in% list("Group", "Subject#", "Remarks", "Timepoint (Visit)", "Barcode", "X"))]
    #loop through and add all variables with their given attributes
    for (p in 1:length(measurement_names)) {
      #get variable attributes, if null set to defaults
      variable_name <- name_list[[measurement_names[[p]]]]
      if (is.null(variable_name) || nchar(variable_name) == 0 || strcmp(variable_name, " ")) variable_name <- measurement_names[[p]]
      variable_units <- variable_attr_list[["Units"]][[p]]
      if (is.null(variable_units)) variable_units <- "NA"
      variable_protocol <- variable_attr_list[["Protocol number analysis"]][[p]]
      if (is.null(variable_protocol)) variable_protocol <- "NA"
      variable_lower_limit <- variable_attr_list[["Lower limit"]][[p]]
      if (is.null(variable_lower_limit)) variable_lower_limit <- "NA"
      variable_upper_limit <- variable_attr_list[["Upper Limit"]][[p]]
      if (is.null(variable_upper_limit)) variable_upper_limit <- "NA"
      variable_no_sample <- variable_attr_list[["No sample available"]][[p]]
      if (is.null(variable_no_sample)) variable_no_sample <- "NA"
      #add the variables, starting in name and units tables
      variable_name <- str_replace_all(variable_name, "[[:punct:]]", "")
      
      variable_name_pk <- checkInsert(con, "variable_name", "name_abbrev", measurement_names[[p]],
                                     c("name_abbrev", "name_full"), c(measurement_names[[p]], variable_name))
      variable_unit_pk <- checkInsert(con, "variable_unit", "unit", variable_units,
                                     c("unit"), variable_units)
  
      add_variable <- sprintf("INSERT INTO variable(protocol_number, upper_limit, lower_limit, no_sample_available,
                             variable_name_pk, variable_unit_pk)
                             VALUES (\'%s\', \'%s\', \'%s\', \'%s\', %i, %i) RETURNING pk",
                             variable_protocol, variable_upper_limit, variable_lower_limit, variable_no_sample,
                            strtoi(variable_name_pk), strtoi(variable_unit_pk))
      variable_pk <- strtoi(dbGetQuery(con, add_variable))
      current_variables_pks[[p]] <- variable_pk
    }
    #get a cache of the primary keys of the current subjects, groups, and timepoints
    #this was added to speed up function by avoiding querying every time a new subject/group/timepoint is found
    #these can probably be done as a single query but it was giving me issues
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
    #if it is a new project with no subjects/groups/timepoints initialize cache lists
    if (!(nrow(subject_data) == 0)) {
      subjects <- subject_data[,"subject_pk"]
      names(subjects) <-  as.character(subject_data[,"subject_num"])
      groups <- group_data[,"igroup_pk"]
      names(groups) <- as.character(group_data[,"igroup"])
      timepoints <- timepoint_data[,"timepoint_pk"]
      names(timepoints) <- as.character(timepoint_data[,"visit"])
    }
    #loop through actual study rows
    for (i in 1:nrow(study_file)) {
      #break if subjects row finds NA
      if (strcmp(toString(study_file[i, "Subject#"]),   "NA") == TRUE) {
       break 
      }
      #get the current subject/group/timepoint
      current_group <- as.character(study_file[i, "Group"])
      current_subject <- study_file[i, "Subject#"]
      current_timepoint <- study_file[i, "Timepoint (Visit)"]
      subject_pk <- NULL
      #if subject isnt in cache, insert into database and add pk to cache
      if (!(current_subject %in% names(subjects))) {
        #if group isnt in cache, insert into database and add pk
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
      #if timepoint not in cache, add timepoint and get pk
      if (!(current_timepoint %in% names(timepoints))) {
        days <- -1
        #get the day number as recorded earlier
        if (toString(current_timepoint) %in% names(visit_to_days)) {
          days <- visit_to_days[[toString(current_timepoint)]]
        }
        add_timepoint <- sprintf("INSERT INTO timepoint(visit, days)
                               VALUES (%s, %s) RETURNING pk", current_timepoint, days)
        timepoint_pk <- strtoi(dbGetQuery(con, add_timepoint))
        timepoints[[toString(current_timepoint)]] <- timepoint_pk
      }
      #now get all pks from cache
      #pks serve as foreign keys for the measurements
      subject_pk <- subjects[[toString(current_subject)]]
      timepoint_pk <- timepoints[[toString(current_timepoint)]]
      insert_all_measurements <- ""
      #build one gigantic query to add all measurements in a row at once
      #added to speed up function
      for(j in 1:length(measurement_names)){
        add_measurement <- sprintf("INSERT INTO measurement(value, timepoint_pk, subject_pk, variable_pk, study_pk)
                                   VALUES (\'%s\', %i, %i, %i, %i) RETURNING pk", study_file[i, measurement_names[j]],
                                   timepoint_pk, subject_pk, current_variables_pks[j], study_pk)
        insert_all_measurements <- paste(add_measurement, ";", insert_all_measurements, sep='')
      }
      #insert all the measurements
      dbGetQuery(con, insert_all_measurements)
      #increment progress
      progress$inc(1/(nrow(study_file) * total_studies))
    }}, error = function(e) {
      #if there is an error, delete all measurements related to study
      #note that if there is a weird variable/subject/group uploaded this will not delete that 
      #the study pk that cascades only ends up deleting the measurements
      #see the database structure
      print(e)
      dbGetQuery(con, sprintf("DELETE FROM study WHERE study.pk=%i", study_pk))
      return_code <<- -1
    })
    return(project_pk)
}

#Return combined list of all measurements from all studies with primary keys in study_pks
#returned format is one measurement per row (neither long nor wide)
getStudyDataWideFormat <- function(con, study_pks) {
  
  base_query <- "SELECT DISTINCT ON (subject_num, visit, name_abbrev) subject_num as \"Subject#\", igroup as \"Group\", value,
                  name_abbrev || '_' || visit as new_name FROM study JOIN measurement ON measurement.study_pk = study.pk 
                  JOIN subject ON subject.pk = measurement.subject_pk JOIN igroup ON igroup.pk = subject.igroup_pk
                  JOIN timepoint ON timepoint.pk = measurement.timepoint_pk JOIN variable ON variable.pk = measurement.variable_pk 
                  JOIN variable_name ON variable_name.pk = variable.variable_name_pk WHERE"
  for (i in 1:length(study_pks)-1) {
    base_query <- paste(base_query, sprintf("study.pk=%i OR", strtoi(study_pks[i])))
  }
  base_query <- paste(base_query, sprintf("study.pk=%i ORDER BY subject_num, visit, name_abbrev", strtoi(study_pks[length(study_pks)])))
  one_line_format <- dbGetQuery(con, base_query)
  wide_format <- spread(one_line_format, "new_name", "value")
  return(wide_format)
}

#Returns table of intervention groups across all projects given the text search_query
getGroupAcross <- function(con, search_query) {
  
    get_info <- sprintf("SELECT igroup as \"Group\", project_code as \"Project\", count(DISTINCT subject_num) as \"Number of Subjects\" 
                        FROM project JOIN study ON study.project_pk = project.pk JOIN
                        measurement ON measurement.study_pk = study.pk JOIN subject ON subject.pk = measurement.subject_pk JOIN
                        igroup ON igroup.pk = subject.igroup_pk
                        WHERE lower(igroup) LIKE lower(\'%%%s%%\') GROUP BY igroup, project_code ORDER BY igroup, project_code", search_query)
    info_table <- dbGetQuery(con, get_info)
    return(info_table)
}


#Returns table of sample types across all projects given the text search_query
getSampleTypeAcross <- function(con, search_query) {
  
  get_info <- sprintf("SELECT study_name as \"Study Name\", project_code as \"Project\", sample_type as \"Sample Type\" FROM project 
                      JOIN study ON study.project_pk = project.pk JOIN
                      sample_type ON sample_type.pk = study.sample_type_pk 
                      WHERE lower(sample_type) LIKE lower(\'%%%s%%\') GROUP BY study_name, project_code, sample_type ORDER BY sample_type, project_code", search_query)
  info_table <- dbGetQuery(con, get_info)
  return(info_table)
}


#Returns table of variables across all projects given the text search_query 
getVariableAcross <- function(con, search_query) {
  
  #ugly query because necessity for two aggregate functions
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
                                JOIN variable_unit ON variable_unit.pk = variable.variable_unit_pk 
                                WHERE lower(name_full) LIKE lower(\'%%%s%%\') GROUP BY name_full, project_code, 
                                unit, upper_limit, lower_limit, protocol_number, days ORDER BY days) t
                               GROUP BY name_full, project_code, unit, upper_limit, lower_limit, protocol_number ORDER BY name_full, project_code", search_query)
  info_table <- dbGetQuery(con, get_info)
  return(info_table)
}
