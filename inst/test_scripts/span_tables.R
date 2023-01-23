library(formods)
library(readxl)
library(dplyr)
library(tidyr)

# First we need to create some data. 
# This will read in a test dataset:
DS = readxl::read_excel(
 path  = system.file(package="formods", 
                     "test_data", 
                     "TEST_DATA.xlsx"),
 sheet = "DATA")

# This will filter the dataset down and modify the formatting
DS = dplyr::filter(DS, EVID == 0)                          |>
  dplyr::filter(ID <= 30)                                  |>
  dplyr::select(ID, TIME, DV, CMT)                         |>
  dplyr::mutate(CMT  = ifelse(.data[["CMT"]] == "C_ng_ml", 
                             "Test Article", 
                             .data[["CMT"]]))              |>
  dplyr::select(ID, TIME, DV, CMT)                         |>
  dplyr::mutate(CMT = ifelse(.data[["CMT"]] == "BM_ng_ml", 
                            "Biomarker", 
                            .data[["CMT"]]))               |>
  dplyr::rename(Analyte = "CMT")                           |>
  dplyr::mutate(DV = ifelse(.data[["DV"]] == 0, "BQL", .data[["DV"]]))

# This represents the large table we want to split up into smaller tables 
wide_df = tidyr::pivot_wider(DS,
  values_from = "DV",
  names_from  = "ID")               |>
  dplyr::arrange(Analyte, TIME)     |>
  dplyr::mutate(TIME = as.character(.data[["TIME"]]))    

# The first two columns represent the rows
# that are common across the tables:
row_common = wide_df[,1:2]

# The remaining columns represent the body of the table:
table_body = wide_df[,3:ncol(wide_df)]

# Next we create matrices that contain the header 
# information for each component above:
row_common_head = matrix(
  data  = c("Time", "Analyte",
            "(hr)", "(ng/ml)"),
  ncol  = 2,
  byrow = TRUE)

table_body_head = matrix(
  data  = c(rep("Subject ID", times=ncol(table_body)), 
            names(table_body)),
  ncol  = ncol(table_body),
  byrow = TRUE)


# This builds all of the tables:
span_res = span_table(table_body      = table_body,
                      row_common      = row_common,
                      table_body_head = table_body_head,
                      row_common_head = row_common_head,
                      notes_detect    = c("BQL"))

# This will show you the first one:
span_res$tables[["Table 1"]]$ft

# This will build a single table with the rows and 
# column ranges specified
bs_res =   build_span(table_body      = table_body,
                      row_common      = row_common,
                      table_body_head = table_body_head,
                      row_common_head = row_common_head,
                      col_sel         = c(1:10),
                      row_sel         = c(1:10),
                      notes_detect    = c("BQL"))

# This will show you the result:
bs_res$ft
