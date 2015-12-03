gettopvars <- function()
{
  vars <- c(
    "?",
    "proprietary_name",
    "establishment_type",
    "pma_number",
    "k_number"
  )
  return(vars)
}

getproductvars <- function()
{
  vars <- c(
    "owner_operator_number",
        "created_date",
        "exempt",
        "product_code"
  )
  return(vars)
}

getopenfdavars <- function()
{
  vars <- c(
    "device_name",
    "medical_specialty_description",
    "device_class",
    "regulation_number"
  )
  return(vars)
}

getregistrationvars <- function()
{
  vars <- c(
      "status_code",
      "iso_country_code",
      "city",
      "registration_number",
      "zip_code",
      "owner_operator",
        "owner_operator_number",
        "official_correspondent",
        "firm_name",
        "contact_address",
      "name",
      "address_line_1",
      "fei_number",
      "initial_importer_flag",
      "address_line_2",
      "postal_code",
      "state_code",
      "us_agent",
      "reg_expiry_date_year"
  )
  return(vars)
}

getchoicevars <- function()
{
  return( c("Any Variable", getregistrationvars(), 
            getopenfdavars(), getproductvars(), gettopvars() ))
}