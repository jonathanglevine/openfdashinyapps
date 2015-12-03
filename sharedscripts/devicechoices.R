geteventvars <- function()
{ 
  vars <- c(
    'adverse_event_flag',
    'date_of_event',
    'date_received',
    'date_report',
    'number_devices_in_event',
    'number_patients_in_event',
    'product_problem_flag',
    'report_number'
  )
  return(vars) 
}

getsourcevars <- function()
{ 
  vars <- c(
    'health_professional',
    'initial_report_to_fda',
    'report_source_code',
    'reporter_occupation_code',
    'reprocessed_and_reused_flag'
  )
  return(vars) 
}




getothervars <- function()
{
  vars <- which( !( gettopvars() %in% c( getsourcevars(), geteventvars(), 
                                         getuser_dmvars(), getuser_fivars(), 
                                         getsuspectvars(), getkeyvars() ) ) )
  n <- gettopvars()
  return( n[vars] )
}

getdevicevars <- function()
{ 
  vars <- c(
    'expiration_date_of_device',
    'device_age_text',
    'manufacturer_d_address_1',
    'baseline_510_k__flag',
    'manufacturer_d_address_2',
    'device_event_key',
    'device_sequence_number',
    'manufacturer_d_state',
    'manufacturer_d_zip_code',
    'manufacturer_d_city',
    'lot_number',
    'manufacturer_d_postal_code',
    'manufacturer_d_zip_code_ext',
    'model_number',
    'date_received',
    'device_report_product_code',
    'device_operator',
    'device_availability',
    'baseline_510_k__number',
    'other_id_number',
    'generic_name',
    'manufacturer_d_name',
    'manufacturer_d_country',
    'brand_name',
    'device_age_text',
    'device_evaluated_by_manufacturer',
    'catalog_number',
    'baseline_510_k__exempt_flag',
    'implant_flag',
    'date_removed_flag'
  )
  return(paste0( 'device.', vars) ) 
}

getdeviceindexvars <- function()
{ 
  vars <- c(
    'device_event_key',
    'device_sequence_number',
    'date_received'
  )
  return(paste0( 'device.', vars) ) 
}

getdeviceidentificationvars <- function()
{ 
  vars <- c(
    'device_report_product_code',
    'generic_name',
    'brand_name'
  )
  return(paste0( 'device.', vars) ) 
}

getdevicemodelvars <- function()
{ 
  vars <- c(
    'model_number',
    'catalog_number',
    'lot_number',
    'other_id_number'
  )
  return(paste0( 'device.', vars) ) 
}

getdevicagevars <- function()
{ 
  vars <- c(
    'device_age_text',
    'expiration_date_of_device'
  )
  return(paste0( 'device.', vars) ) 
}

getdevicevaluationvars <- function()
{ 
  vars <- c(
    'device_availability',
    'date_returned_to_manufacturer',
    'device_evaluated_by_manufacturer'
  )
  return(paste0( 'device.', vars) ) 
}
gettopvars <- function()
{ 
  vars <- c(
    'adverse_event_flag',
    'date_manufacturer_received',
    'date_of_event',
    'date_received',
    'date_report',
    'device_date_of_manufacturer',
    'distributor_address_1',
    'distributor_address_2',
    'distributor_city',
    'distributor_name',
    'distributor_state',
    'distributor_zip_code',
    'distributor_zip_code_ext',
    'event_key',
    'event_location',
    'event_type',
    'health_professional',
    'initial_report_to_fda',
    'manufacturer_address_1',
    'manufacturer_address_2',
    'manufacturer_city',
    'manufacturer_contact_address_1',
    'manufacturer_contact_address_2',
    'manufacturer_contact_area_code',
    'manufacturer_contact_city',
    'manufacturer_contact_country',
    'manufacturer_contact_exchange',
    'manufacturer_contact_extension',
    'manufacturer_contact_f_name',
    'manufacturer_contact_l_name',
    'manufacturer_contact_pcity',
    'manufacturer_contact_pcountry',
    'manufacturer_contact_phone_number',
    'manufacturer_contact_plocal',
    'manufacturer_contact_postal_code',
    'manufacturer_contact_state',
    'manufacturer_contact_t_name',
    'manufacturer_contact_zip_code',
    'manufacturer_country',
    'manufacturer_g1_address_1',
    'manufacturer_g1_address_2',
    'manufacturer_g1_city',
    'manufacturer_g1_country',
    'manufacturer_g1_name',
    'manufacturer_g1_postal_code',
    'manufacturer_g1_state',
    'manufacturer_g1_zip_code',
    'manufacturer_g1_zip_code_ext',
    'manufacturer_link_flag',
    'manufacturer_name',
    'manufacturer_postal_code',
    'manufacturer_state',
    'manufacturer_zip_code',
    'manufacturer_zip_code_ext',
    'mdr_report_key',
    'mdr_text',
    'number_devices_in_event',
    'number_patients_in_event',
    'previous_use_code',
    'product_problem_flag',
    'remedial_action',
    'removal_correction_number',
    'report_number',
    'report_source_code',
    'report_to_fda',
    'report_to_manufacturer',
    'reporter_occupation_code',
    'reprocessed_and_reused_flag',
    'single_use_flag',
    'source_type',
    'type_of_report',
    'manufacturer_contact_zip_ext'
  )
  return(vars) 
}



getuser_fivars <- function()
{ 
  vars <- c(
    'date_manufacturer_received',
    'source_type',
    'date_facility_aware',
    'report_date',
    'date_report_to_fda',
    'date_report_to_manufacturer',
    'event_location',
    'report_to_fda',
    'report_to_manufacturer',
    'type_of_report',
    'distributor_address_1',
    'distributor_address_2',
    'distributor_city',
    'distributor_name',
    'distributor_state',
    'distributor_zip_code',
    'distributor_zip_code_ext'
  )
  return(vars) 
}

getuser_dmvars <- function()
{ 
  vars <- c(
    'date_manufacturer_received',
    'source_type',
    'event_type',
    'device_date_of_manufacture',
    'previous_use_code',
    'remedial_action',
    'removal_correction_number',
    'single_use_flag',
    'manufacturer_contact_t_name',
    'manufacturer_contact_f_name',
    'manufacturer_contact_l_name',
    'manufacturer_contact_street_1',
    'manufacturer_contact_street_2',
    'manufacturer_contact_city',
    'manufacturer_contact_state',
    'manufacturer_contact_zip_code',
    'manufacturer_contact_zip_ext',
    'manufacturer_contact_postal',
    'manufacturer_contact_country',
    'manufacturer_contact_pcountry',
    'manufacturer_contact_area_code',
    'manufacturer_contact_exchange',
    'manufacturer_contact_extension',
    'manufacturer_contact_pcity',
    'manufacturer_contact_phone_number',
    'manufacturer_contact_plocal',
    'manufacturer_g1_name',
    'manufacturer_g1_street_1',
    'manufacturer_g1_street_2',
    'manufacturer_g1_city',
    'manufacturer_g1_state',
    'manufacturer_g1_zip_code',
    'manufacturer_g1_zip_ext',
    'manufacturer_g1_postal_code',
    'manufacturer_g1_country'
  )
  
  return(vars) 
}


getsuspectvars <- function()
{ 
  vars <- c(
    'manufacturer_address_1',
    'manufacturer_address_2',
    'manufacturer_city',
    'manufacturer_country',
    'manufacturer_name',
    'manufacturer_state',
    'manufacturer_zip_code',
    'manufacturer__postal_code',
    'manufacturer_zip_code_ext'
  )
  return(vars) 
}

getkeyvars <- function()
{ 
  vars <- c(
    'event_key',
    'manufacturer_link_flag',
    'mdr_report_key',
    'mdr_text'
  )
  return(vars) 
}
getopenfdavars <- function()
{ 
  vars <- c(
    'device.openfda.device_name',
    'device.openfda.medical_specialty_description',
    'device.openfda.device_class',
    'device.openfda.regulation_number'
  )
  return(vars) 
}

getmdrtextvars <- function()
{ 
  vars <- c(
    'mdr_text.patient_sequence_number',
    'mdr_text.text_type_code',
    'mdr_text.text',
    'mdr_text.mdr_text_key',
    'mdr_text.date_received'
  )
  return(vars) 
}

getchoicevars <- function()
{
  return( c('Any Variable'  ,gettopvars(), getdevicevars(), getopenfdavars(), getmdrtextvars() ) )
}
getdatechoices <- function(){
  s <- c( 'date_received' )
  return(s)
}