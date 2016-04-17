# Create the education and poverty data set for SOC 345

#' Extract and merge data
#'
#' This function creates and assigns a custom class to handle numbers with commas.
#' @keywords poverty education data
#' @export
#' create_educ_poverty_data_function()


create_educ_poverty_data <- function()
{

    loadNamespace('googlesheets')
    loadNamespace('dplyr')
    poverty13 <- dplyr::select (poverty.states, FIPStxt, Area_Name, PCTPOVALL_2013)
    poverty13$FIPS.Code <- as.integer(poverty13$FIPStxt)

    lessthanhighschool13 <- select(education.states, Area.name, FIPS.Code,                                Percent.of.adults.with.less.than.a.high.school.diploma..2009.2013)

    education_and_poverty<-merge(poverty13, lessthanhighschool13,
                                 by.x='FIPS.Code', by.y='FIPS.Code')

    # First let's create the region data set
    gs_region<-googlesheets::gs_url('https://docs.google.com/spreadsheets/d/1h_jY4A44WoSLkrqhwZZ9oJh51N2GybwVvGgEaY3n2gc/pubhtml')
    region_data<-googlesheets::gs_read(gs_region)

    education_and_poverty <- merge(education_and_poverty, region_data,
                                   by.x='FIPS.Code', by.y='FIPS.Code')
    education_and_poverty

}
