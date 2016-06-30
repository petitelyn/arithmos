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
  
  
  create_feature_name <- "CREATE TABLE feature_name(
  pk SERIAL PRIMARY KEY,
  name_abbrev text,
  name_full text
  );"
  dbGetQuery(con, create_feature_name)
  
  create_feature_unit <- "CREATE TABLE feature_unit(
  pk SERIAL PRIMARY KEY,
  unit text
  );"
  dbGetQuery(con, create_feature_unit)
  
  create_feature <- "CREATE TABLE feature(
  pk SERIAL PRIMARY KEY,
  protocol_number text,
  analysis_remark text,
  upper_limit text,
  lower_limit text,
  no_sample_available text,
  feature_name_pk integer REFERENCES feature_name(pk),
  feature_unit_pk integer REFERENCES feature_unit(pk)
  );"
  dbGetQuery(con, create_feature)
  
  create_project_features <- "CREATE TABLE project_features(
  project_pk integer REFERENCES project(pk),
  feature_pk integer REFERENCES feature(pk),
  PRIMARY KEY(project_pk, feature_pk)
  );"
  dbGetQuery(con, create_project_features)
  
  create_subject <- "CREATE TABLE subject(
  pk SERIAL PRIMARY KEY,
  subject_num integer,
  subject_name text,
  category integer 
  );"
  dbGetQuery(con, create_subject)
  
  create_study_subjects <- "CREATE TABLE study_subjects(
  study_pk integer REFERENCES study(pk),
  subject_pk integer REFERENCES subject(pk),
  PRIMARY KEY (study_pk, subject_pk)
  );"
  dbGetQuery(con, create_study_subjects)
  
  create_category_index <- "CREATE INDEX category ON subject(category)"
  dbGetQuery(con, create_category_index)
  
  create_measurement <- "CREATE TABLE measurement(
  pk SERIAL PRIMARY KEY,
  value text,
  timepoint integer,
  feature_pk integer REFERENCES feature(pk),
  subject_pk integer REFERENCES subject(pk),
  study_pk integer REFERENCES study(pk)
  );"
  dbGetQuery(con, create_measurement)
  
  create_timepoint_index <- "CREATE INDEX timepoint ON measurement(timepoint)"
  dbGetQuery(con, create_timepoint_index)
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
addStudy <- function(con, general_info_root, study_data_root, study_name) {
  #arbitrary length because R is super slow appending in for-loop
  #may need to error check if first lines ever longer than 10
  withProgress(message="Uploading data", {
  general_info_list <- list(length=10)
  general_info_lines <- readLines(file(general_info_root))
  #again 10 is hard coded
  for (q in 1:10) {
    #split by any number of commas to be able to account for dynamic row number
    line <- as.list(strsplit(general_info_lines[[q]], ",+")[[1]])
    if (length(line) == 0) next;
    if (length(line) == 1) next;
    if (strcmp(line[[1]], "Remarks: ") == TRUE) break;
    general_info_list[[line[[1]]]] <- line[[2]]
  }
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
  feature_attr_list <- list(length=10)
  study_file_lines <- readLines(file(study_data_root))
  lines_to_skip <- -2
  previous_line <- NULL
  name_list <- list()
  #read the first box w/feature attributes
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
        name_list[[abbrev_names[[t]]]] <- previous_line[[match(abbrev_names[[t]], line)]]
      }
    }
    feature_attr_list[[line[[1]]]] <- tail(line, -1)
    previous_line_string <- study_file_lines[[q]]
    if (strcmp(substring(previous_line_string, nchar(previous_line_string), nchar(previous_line_string)), ",") == TRUE) {
      previous_line_string <- paste(previous_line_string, ",")
    }
    previous_line <- as.list(strsplit(previous_line_string, ",")[[1]])
  }
  study_file <- read.csv(study_data_root, sep = ',', header = T, skip=lines_to_skip, check.names=FALSE)
  #collect primary keys of features
  current_features_pks <- vector(mode="integer", length=1000)

  #loop through all the features
  measurement_names <- colnames(study_file)[which(!colnames(study_file)
                                                  %in% list("Group", "Subject#", "Remarks", "Timepoint (Visit)", "Barcode", "X"))]
  for (p in 1:length(measurement_names)) {
    feature_name <- name_list[[measurement_names[[p]]]]
    if (is.null(feature_name)) feature_name <- "NA"
    feature_units <- feature_attr_list[["Units"]][[p]]
    if (is.null(feature_units)) feature_units <- "NA"
    feature_name <- str_replace_all(feature_name, "[[:punct:]]", "")
    
    feature_name_pk <- checkInsert(con, "feature_name", "name_abbrev", measurement_names[[p]],
                                   c("name_abbrev", "name_full"), c(measurement_names[[p]], feature_name))
    feature_unit_pk <- checkInsert(con, "feature_unit", "unit", feature_units,
                                   c("unit"), feature_units)

    feature_protocol <- feature_attr_list[["Protocol number analysis"]][[p]]
    if (is.null(feature_protocol)) feature_protocol <- "NA"
    feature_lower_limit <- feature_attr_list[["Lower limit"]][[p]]
    if (is.null(feature_lower_limit)) feature_lower_limit <- "NA"
    feature_upper_limit <- feature_attr_list[["Upper Limit"]][[p]]
    if (is.null(feature_upper_limit)) feature_upper_limit <- "NA"
    feature_no_sample <- feature_attr_list[["No sample available"]][[p]]
    if (is.null(feature_no_sample)) feature_no_sample <- "NA"
    add_feature <- sprintf("INSERT INTO feature(protocol_number, upper_limit, lower_limit, no_sample_available,
                           feature_name_pk, feature_unit_pk)
                           VALUES (\'%s\', \'%s\', \'%s\', \'%s\', %i, %i) RETURNING pk",
                           feature_protocol, feature_upper_limit, feature_lower_limit, feature_no_sample,
                          strtoi(feature_name_pk), strtoi(feature_unit_pk))
    feature_pk <- strtoi(dbGetQuery(con, add_feature))
    add_project_features <- sprintf("INSERT INTO project_features(project_pk, feature_pk) VALUES (%i, %i)", project_pk, feature_pk)

    dbGetQuery(con, add_project_features)
    current_features_pks[[p]] <- feature_pk
  }
  subject_pk_cache <- rep(-1, 10000)
  study_subject_pk_cache <- rep(-1, 10000)
  for (i in 1:nrow(study_file)) {
    incProgress(1/nrow(study_file))
    if (strcmp(toString(study_file[i, "Subject#"]),   "NA") == TRUE) {
     break 
    }
    current_category <- study_file[i, "Group"]
    current_subject <- study_file[i, "Subject#"]
    current_timepoint <- study_file[i, "Timepoint (Visit)"]
    subject_pk <- subject_pk_cache[[current_subject]]
    if (subject_pk == -1) {
      subject_pk <- strtoi(dbGetQuery(con, sprintf("SELECT subject.pk FROM project JOIN study ON study.project_pk = project.pk
                                                  JOIN study_subjects ON study_subjects.study_pk = study.pk JOIN subject ON
                                                  subject.pk = study_subjects.subject_pk
                                                  WHERE project.pk=%i AND subject.subject_num=%i GROUP BY subject.pk", project_pk, current_subject)))
    }
    if (length(subject_pk) == 0) {
      add_subject <- sprintf("INSERT INTO subject(subject_num, category)
                             VALUES (%i, %i) RETURNING pk", current_subject, current_category)
      subject_pk <- strtoi(dbGetQuery(con, add_subject))
    }
    study_subjects_pk <- dbGetQuery(con, sprintf("SELECT study_pk, subject_pk FROM study_subjects WHERE study_pk=%i AND subject_pk=%i", study_pk, subject_pk))
    if (length(study_subjects_pk) == 0) {
      dbGetQuery(con, sprintf("INSERT INTO study_subjects(study_pk, subject_pk) VALUES (%i, %i)", study_pk, subject_pk))
    }
    for(j in 1:length(measurement_names)){
      add_measurement <- sprintf("INSERT INTO measurement(value, timepoint, subject_pk, feature_pk, study_pk)
                                 VALUES (\'%s\', %i, %i, %i, %i) RETURNING pk", study_file[i, measurement_names[j]],
                                 current_timepoint, subject_pk, current_features_pks[j], study_pk)
      dbGetQuery(con, add_measurement)
    }
    subject_pk_cache[[current_subject]] <- subject_pk
  }
  })
}

#retrieve data from n studies with extra column "new names" for conversion to wide format
getStudyDataFrame <- function(con, study_pks) {
  # get_feature_names <- sprintf("SELECT name_abbrev
  #                              FROM measurement
  #                              JOIN subject ON subject.pk = measurement.subject_pk
  #                              JOIN study ON study.pk = measurement.study_pk
  #                              JOIN feature ON feature.pk = measurement.feature_pk JOIN feature_name ON feature_name.pk = feature.feature_name_pk
  #                              WHERE study.pk=%i
  #                              ORDER BY subject_num, timepoint", study_pk)
  # 
  # feature_list <- unique(dbGetQuery(con, get_feature_names)[,"name_abbrev"])
  # initial_split_string <- "SELECT x.subject_num as \"Subject#\", x.category as \"Group\", x.timepoint as \"Timepoint (Visit)\""
  # for (l in 1:length(feature_list)) {
  #   initial_split_string <- paste(initial_split_string, sprintf("split_part(x.value_list, ',', %i) AS \"%s\"", l, paste(feature_list[[l]], toString(study_pk))), sep=", ")
  # }
  base_query <- "SELECT DISTINCT ON (subject_num, timepoint, name_abbrev) subject_num as \"Subject#\", category as \"Group\", value,
                 name_abbrev || '_' || timepoint as new_name
                            FROM measurement
  JOIN subject ON subject.pk = measurement.subject_pk
  JOIN study ON study.pk = measurement.study_pk
  JOIN feature ON feature.pk = measurement.feature_pk
  JOIN feature_name ON feature_name.pk = feature.feature_name_pk
  WHERE"
  for (i in 1:length(study_pks)-1) {
    base_query <- paste(base_query, sprintf("study.pk=%i OR", strtoi(study_pks[i])))
  }
  base_query <- paste(base_query, sprintf("study.pk=%i ORDER BY subject_num, timepoint, name_abbrev", strtoi(study_pks[length(study_pks)])))
  return(dbGetQuery(con, base_query))
}

#retrieve data frame of a single study in format of data spreadsheet
getProjectDataFrame <- function(con, project_pk) {
  get_study_pks <- sprintf("SELECT study.pk FROM project JOIN study ON study.project_pk = project.pk 
                               WHERE project.pk = %i", project_pk)
  study_pks <- dbGetQuery(con, get_study_pks)[['pk']]
  frame <- getStudyDataFrame(con, study_pks[[1]])
  for (i in 2:length(study_pks)) {
    frame_to_merge <- getStudyDataFrame(con, study_pks[[i]])
    frame <- merge(frame, frame_to_merge, all=TRUE)
  }
  frame
}