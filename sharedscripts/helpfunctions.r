

loadhelp <- function(tabname){
  
  l <- vector('character', 11)
  names(l) <- c('selectvars', 'dataoptions', 'graphoptions', 'axisoptions',
                'loaddata', 'overview', 'overviewside', 'overviewsidedl', 'LRT', 'overview_501', 'overview_devclass')
#*1**********************************
  l['loaddata'] <- "<h3>Loading Data</h3>This program can read data in text, .xpt, 
  and saved R dataframe formats. <br>
  Select the file type, and the click on the 'Browse'
  button the file you want to upload<br>
  Please be aware that if you are running this sofware
  on a remote server the data may be available to unauthorized individuals."
#2*********************************** 
  l['overviewside'] <- "<h4>Reporting Ratios (PRR and RoR)</h4>
The proportional reporting ratio (PRR) is a simple way to get a measure of how common an adverse
event for a particular drug is compared to how common the event is in the overall database.  <br>
A PRR > 1 for a drug-event combination indicates that a greater proportion of the reports for
the drug are for the event than the proportion of events in the rest of the database.  
For example, a PRR of 2 for a drug event combination indicates that the proportion of reports for
the drug-event combination is twice the proportion of the event in the overall database<br>
<br>
PRR = (m/n)/( (M-m)/(N-n) )<br>
 Where <br>
      m = #reports with drug and event<br>
      n = #reports with drug<br>
      M = #reports with event in database<br>
      N = #reports in database<br>
<br><br>
A similar measure is the reporting odds ratio (ROR).<br>
<br><br>
ROR = (m/d)/(M/D)
<br><br>
 Where <br>
      m = #reports with drug and event<br>
      d = n-m<br>
      M = #reports with event in database<br>
      D = N-M<br>
<br><br>

 Often PRR analyses are stratified by various attributes of
 the report such as patient age, gender, report date in an effort to improve precision.
Other approaches, such as Bayesian shrinkage estimates of the PRR (e.g. MGPS) are also used.
<br><br>

<h4>References</h4>
<br>
Guidance for Industry. Good Pharmacovigilance Practices and Pharmacoepidemiologic Assessment. 
Food and Drug Administration, US Department of Health and Human Services. March 2005. 
<a href='http://www.fda.gov/downloads/regulatoryinformation/guidances/ucm126834.pdf'>
http://www.fda.gov/downloads/regulatoryinformation/guidances/ucm126834.pdf</a> .  Accessed Dec 2014.
<br><br>
Bate A, Evans, S. Quantitative signal detection using spontaneous ADR reporting.  
<i>Pharmacoepidemiol and Drug Saf</i>  2009 Jun;18(6):427-36. doi: 10.1002/pds.1742.
<br><br>
Szarfman A, Tonning JM, Doraiswamy PM.
 Pharmacovigilance in the 21st century: new systematic tools for an old problem.  
<i>Pharmacotherapy</i> 2004 Sep;24(9):1099-104.
<br><br>
Dumouchel W. Bayesian data mining in large frequency tables,
with an application to the FDA Spontaneous Reporting System,
<i>American Statistician</i> 1999; 53(3):177-190.

"

#2a*********************************** 
l['overview'] <- "<h3>Data Reference</h3> <br>
<h4>Background</h4>
From the <a href='https://open.fda.gov/drug/event/reference'>OpenFDA Website</a><br>
<br>
<p>The openFDA drug adverse event API returns data from the <a href='https://open.fda.gov/data/faers/'>FDA Adverse Event Reporting System (FAERS)</a>, a database that contains information on adverse event and medication error reports submitted to FDA. Currently, this data covers publically releasable records submitted to the FDA from 2004-2013. The data is updated quarterly.</p>

  <p>An adverse event is submitted to the FDA to report any undesirable experience associated with the use of a medical product in a patient. For drugs, this includes serious drug side effects, product use errors, product quality problems, and therapeutic failures for prescription or over-the-counter medicines and medicines administered to hospital patients or at outpatient infusion centers.</p>

<p>Reporting of adverse events by healthcare professionals and consumers is voluntary in the United States. FDA receives some adverse event reports directly from healthcare professionals (such as physicians, pharmacists, nurses and others) and consumers (such as patients, family members, lawyers and others). Healthcare professionals and consumers may also report adverse events to the products’ manufacturers. If a manufacturer receives an adverse event report, it is normally required to send the report to FDA.</p>

FAERS data does have limitations. There is no certainty that the reported event (adverse event or medication error) was actually due to the product. FDA does not require that a causal relationship between a product and event be proven, and reports do not always contain enough detail to properly evaluate an event.<br /><br />Further, FDA does not receive reports for every adverse event or medication error that occurs with a product. Many factors can influence whether or not an event will be reported, such as the time a product has been marketed and publicity about an event.<br /><br />Submission of a safety report does not constitute an admission that medical personnel, user facility, importer, distributor, manufacturer or product caused or contributed to the event. The information in these reports has not been scientifically or otherwise verified as to a cause and effect relationship and cannot be used to estimate the incidence of these events.


<p>In 2012, FDA changed from the Adverse Event Reporting System (AERS) to the FDA Adverse Event Reporting System (FAERS). There was a minor shift in terms as part of this transition. If you are using data from before December 2012, you should be aware of this shift.</p>

<h3 id='responsible-use-of-the-data'>Responsible use of the data</h3>

<p>Adverse event reports submitted to FDA do not undergo extensive validation or verification. Therefore, <strong>a causal relationship cannot be established between product and reactions listed in a report.</strong> While a suspected relationship <em>may</em> exist, it is not medically validated and should not be the sole source of information for clinical decision making or other assumptions about the safety or efficacy of a product.</p>

<p>Additionally, it is important to remember that adverse event reports represent a small percentage of total usage numbers of a product. Common products may have a higher number of adverse events due to the higher total number of people using the product. In recent years the FDA has undertaken efforts to increase collection of adverse events. Increases in the total number of adverse events is likely caused by improved reporting.</p>

<h3 id='how-adverse-events-are-organized'>How adverse events are organized</h3>

<p>Adverse events are collected through a series of <em>safety reports.</em> Each is identified by a 8-digit string (for instance, <code>6176304-1</code>). The first 7 digits (before the hyphen) identify the individual report, and the last digit (after the hyphen) is a checksum. Rather than updating individual records in FAERS, subsequent updates are submitted in seperate reports.</p>


"

#2b*********************************** 
  l['selectvars'] <- "<h3>Selecting Variables</h3> <br>
<h4>Draw Plot</h4>
Once you have selected the variables
and options to use, click this button to draw the graph.<br>
  <h4>Select Time Variable</h4>
The time variable is used to specify the baseline value to use in the graph.
<br>
 <h4>Which Baseline?</h4>
By default the graph uses the smallest value of the time variable to indicate which vale is the baseline.
If you select 'Selected Value of Time Variable' you specify a value the time variable to indicate the baseline value.
<br>
  <h4>Select Treatment Variable</h4>
The treatment variable is used to indicate treatment or other grouping.
<br>
  <h4>Select Lab Variable</h4>
The numeric values to graph.
<br>
  <h4>Select ID Variable</h4>
  The variable that represent the subject identifier, or other experimental unit identifier.
"  
  #2c*********************************** 
  l['overviewsidedl'] <- "<h4>Reporting Ratios (PRR)</h4>
  The proportional reporting ratio (PRR) is a simple way to get a measure of how common an adverse
  event for a particular drug is compared to how common the event is in the overall database.  <br>
  A PRR > 1 for a drug-event combination indicates that a greater proportion of the reports for
  the drug are for the event than the proportion of events in the rest of the database.  
  For example, a PRR of 2 for a drug event combination indicates that the proportion of reports for
  the drug-event combination is twice the proportion of the event in the overall database<br>
  <br>
  PRR = (A/AB)/( (AC-A)/(N-AB) )<br>
  Where <br>
  A = #reports with drug and event<br>
  AB = #reports with drug<br>
  AC = #reports with event in database<br>
  N = #reports in database<br>
  <br><br>
  
  
  Often PRR analyses are stratified by various attributes of
  the report such as patient age, gender, report date in an effort to improve precision.
  Other approaches, such as Bayesian shrinkage estimates of the PRR (e.g. MGPS) are also used.
  <br><br>
  
  <h4>References</h4>
  <br>
  Guidance for Industry. Good Pharmacovigilance Practices and Pharmacoepidemiologic Assessment. 
  Food and Drug Administration, US Department of Health and Human Services. March 2005. 
  <a href='http://www.fda.gov/downloads/regulatoryinformation/guidances/ucm126834.pdf'>
  http://www.fda.gov/downloads/regulatoryinformation/guidances/ucm126834.pdf</a> .  Accessed Dec 2014.
  <br><br>
  Bate A, Evans, S. Quantitative signal detection using spontaneous ADR reporting.  
  <i>Pharmacoepidemiol and Drug Saf</i>  2009 Jun;18(6):427-36. doi: 10.1002/pds.1742.
  <br><br>
  Szarfman A, Tonning JM, Doraiswamy PM.
  Pharmacovigilance in the 21st century: new systematic tools for an old problem.  
  <i>Pharmacotherapy</i> 2004 Sep;24(9):1099-104.
  <br><br>
  Dumouchel W. Bayesian data mining in large frequency tables,
  with an application to the FDA Spontaneous Reporting System,
  <i>American Statistician</i> 1999; 53(3):177-190.
  
  "
  
  #2a*********************************** 
  l['overview'] <- "<h3>Data Reference</h3> <br>
  <h4>Background</h4>
  From the <a href='https://open.fda.gov/drug/event/reference'>OpenFDA Website</a><br>
  <br>
  <p>The openFDA drug adverse event API returns data from the <a href='https://open.fda.gov/data/faers/'>FDA Adverse Event Reporting System (FAERS)</a>, a database that contains information on adverse event and medication error reports submitted to FDA. Currently, this data covers publically releasable records submitted to the FDA from 2004-2013. The data is updated quarterly.</p>
  
  <p>An adverse event is submitted to the FDA to report any undesirable experience associated with the use of a medical product in a patient. For drugs, this includes serious drug side effects, product use errors, product quality problems, and therapeutic failures for prescription or over-the-counter medicines and medicines administered to hospital patients or at outpatient infusion centers.</p>
  
  <p>Reporting of adverse events by healthcare professionals and consumers is voluntary in the United States. FDA receives some adverse event reports directly from healthcare professionals (such as physicians, pharmacists, nurses and others) and consumers (such as patients, family members, lawyers and others). Healthcare professionals and consumers may also report adverse events to the products’ manufacturers. If a manufacturer receives an adverse event report, it is normally required to send the report to FDA.</p>
  
  FAERS data does have limitations. There is no certainty that the reported event (adverse event or medication error) was actually due to the product. FDA does not require that a causal relationship between a product and event be proven, and reports do not always contain enough detail to properly evaluate an event.<br /><br />Further, FDA does not receive reports for every adverse event or medication error that occurs with a product. Many factors can influence whether or not an event will be reported, such as the time a product has been marketed and publicity about an event.<br /><br />Submission of a safety report does not constitute an admission that medical personnel, user facility, importer, distributor, manufacturer or product caused or contributed to the event. The information in these reports has not been scientifically or otherwise verified as to a cause and effect relationship and cannot be used to estimate the incidence of these events.
  
  
  <p>In 2012, FDA changed from the Adverse Event Reporting System (AERS) to the FDA Adverse Event Reporting System (FAERS). There was a minor shift in terms as part of this transition. If you are using data from before December 2012, you should be aware of this shift.</p>
  
  <h3 id='responsible-use-of-the-data'>Responsible use of the data</h3>
  
  <p>Adverse event reports submitted to FDA do not undergo extensive validation or verification. Therefore, <strong>a causal relationship cannot be established between product and reactions listed in a report.</strong> While a suspected relationship <em>may</em> exist, it is not medically validated and should not be the sole source of information for clinical decision making or other assumptions about the safety or efficacy of a product.</p>
  
  <p>Additionally, it is important to remember that adverse event reports represent a small percentage of total usage numbers of a product. Common products may have a higher number of adverse events due to the higher total number of people using the product. In recent years the FDA has undertaken efforts to increase collection of adverse events. Increases in the total number of adverse events is likely caused by improved reporting.</p>
  
  <h3 id='how-adverse-events-are-organized'>How adverse events are organized</h3>
  
  <p>Adverse events are collected through a series of <em>safety reports.</em> Each is identified by a 8-digit string (for instance, <code>6176304-1</code>). The first 7 digits (before the hyphen) identify the individual report, and the last digit (after the hyphen) is a checksum. Rather than updating individual records in FAERS, subsequent updates are submitted in seperate reports.</p>
  
  
  "
#3***********************************  
l['dataoptions'] <- "<h3>Data Options</h3>
<h4>Select Units</h4>
Specify the units used in the axis labels by either indicating the column that 
contains the units or entering your own units.
<h4>Select Lab Name</h4>
Specify the name of the lab test by either using the name of the lab variable, 
specifying the column that 
contains the lab name, or entering your own lab name.
<h4>Select Subset Variable</h4>
Select variable to be used to subset the data.  Usually this is a column containing
the names of the lab tests 
when multiple lab tests are in a single file.
<h4>Select Subset Value</h4>
Select value of subset variable to be used to indicate that value should be 
included in data set.
"
#4***********************************  
l['graphoptions'] <- "<h3>Graph Options</h3>
<h4>Study Name</h4>

<h4>Log Axes</h4>
<h4>Show Density Estimates</h4>
<h4>Hide Min</h4>
<h4>Hide Max</h4>
<h4>Horizontal Reference lines</h4>

<h4>Vertical Reference lines</h4>"

#5***********************************  
l['axisoptions'] <- "<h3>Axis Options</h3>

<h4>x-axis limits:</h4>
Automatic
User Specified
<h4>y-axis limits:</h4>
Automatic
User Specified
<h4>x-axis ticks:</h4>
Automatic
User Specified
<h4>y-axis ticks:</h4>
Automatic
User Specified"

#5***********************************  
l['about'] <- "<h3>About</h3>

This software was developed by FDA's Office Of Health Informatics
(OHI) as part of the openFDA initiative. 
<br>
<h4>Development Team</h4>
<b>Taha Kass-hout, MD, MS</b><br>
Creator of openFDA initiative.<br>
<b>Jonathan 'Jay' Levine, PhD</b><br>
Software design and implementation using R and Shiny, analytical methods.<br>
<b>Roselie Bright, ScD</b>, <i>FDA OC/OCS/Office of Health Informatics</i><br>
Software testing and evaluation, openFDA implementation.<br>
<b>Zhiheng Xu, PhD</b>, <i>FDA CDRH</i><br>
Analytical methods, openFDA implementation.<br><br>
<a href='mailto:open@fda.hhs.gov'>Email the openFDA team</a>"

#6***********************************  
l['LRT'] <- '<h3>Likelihood Ratio Test (LRT) Methodology</h3>

The RR is defined as the ratio of reporting rate for a particular AE for a specified drug/drug class relative to the reporting rate for all other AEs for the fixed drug/drug group.

RR >1 implies that the observed reporting rate for the particular AE is higher than the reporting rate for other AEs for the (fixed) drug/drug group. 

An AE with RR>1 can be a potential signal for the drug/drug group of interest.

RR = (a/(a+b))/(c/(c+d))
( See Table 2 in <a href="lrtmethod.pdf"  target="_blank"> Likelihood Ratio Test (LRT) Methodology document </a> for letter definitions. )
LogLR (LLR) represents the logarithm of likelihood ratio test statistic by AE expressed in terms of SOC, PT, etc. 

The larger the logLR value is, the stronger is the association between the particular AE and (fixed) drug.

logLR = a x &#91;log(a) – log(a +b)&#93; +c x &#91;log(c)-log (c + d)&#93; - (a + c) x &#91;log(a + c)-log(a + b +c + d)&#93;
Is calculated using LogLR. AE represents the significance of the observed association between the AE and a fixed drug/drug group.  P-values less than 0.05 are indicative of those AEs being signals for the (fixed) drug. Users can use different threshold for the p-values for signal detection (such as 0.025, 0.01, etc). 
'

#7*********************************** 
l['overview_501'] <- '<h3>Data Reference</h3> <br>
<h4>About device 510(k)</h4>

The premarket notification dataset contains details about specific products and the original sponsors of premarket notification applications. It also contains administrative and tracking information about the applications and receipt and decision dates.

A 510(k) is a premarket submission made to FDA to demonstrate that the device to be marketed is at least as safe and effective, that is, substantially equivalent, to a legally marketed device (21 CFR 807.92(a)(3)) that is not subject to PMA. Submitters must compare their device to one or more similar legally marketed devices and make and support their substantial equivalency claims. A legally marketed device, as described in 21 CFR 807.92(a)(3), is a device that was legally marketed prior to May 28, 1976 (preamendments device), for which a PMA is not required, or a device which has been reclassified from Class III to Class II or I, or a device which has been found substantially equivalent through the 510(k) process. The legally marketed device(s) to which equivalence is drawn is commonly known as the “predicate.”

For additional information, see  <a href="http://www.fda.gov/MedicalDevices/DeviceRegulationandGuidance/HowtoMarketYourDevice/PremarketSubmissions/PremarketNotification510k/default.htm">here</a>.


'
#8*********************************** 
l['overview_devclass'] <- '<h3>Data Reference</h3> <br>
<h2 id="about-device-classification">About device classification<a href="#about-device-classification" class="header-link"><i class="fa fa-link"></i></a></h2>

<p>The U.S. Food and Drug Administration (FDA) regulates medical devices in the United States. Medical devices range from simple tongue depressors and bedpans to complex programmable pacemakers with microchip technology and laser surgical devices. In addition, medical devices include in vitro diagnostic products, such as general purpose lab equipment, reagents, and test kits, which may include monoclonal antibody technology. Certain electronic radiation emitting products with medical application and claims meet the definition of medical device. Examples include diagnostic ultrasound products, x-ray machines, and medical lasers.</p>

<p>The Product Classification dataset contains medical device names, their associated product codes, their medical specialty areas (panels) and their classification. The name and product code identify the generic category of a device for FDA. The product code assigned to a device is based upon the medical device product classification designated under 21 CFR Parts 862-892. </p>
<p>The Food and Drug Administration (FDA) has established classifications for approximately 1,700 different generic types of devices and grouped them into 16 medical specialties referred to as panels. Each of these generic types of devices is assigned to one of three regulatory classes based on the level of control necessary to assure the safety and effectiveness of the device.</p>
<p>For additional information, see <a href="http://www.fda.gov/MedicalDevices/DeviceRegulationandGuidance/Overview/ClassifyYourDevice/default.htm">here</a>.</p>
'
return(l[tabname])
}

#Help strings
gettt <- function(){
  l <- vector('character' )
  #*1**********************************
  l <- append( l, c('cocloud' =  "Word cloud for concomitant medications") )
  
  #2*********************************** 
  l <- append( l, c('eventtable' =  "Table of counts for selected drug") )
  #3*********************************** 
  l <- append( l, c('drugvar1' =  "<b>Select Drug Variable</b>" ) )
  l <- append( l, c('drugvar2' =  "Select the openFDA drug variable to search") )
  l <- append( l, c('v1head' =  "<b>Select Drug Variable</b>" ) )
  l <- append( l, c('v1text' =  "Select the openFDA drug variable to search") )
  l <- append( l, c('drugname1' =  '<b>Select Drug Name</b>') )
  l <- append( l, c('drugname2' =  'Enter the name of a drug to analyze' ) )
  l <- append( l, c('eventname1' =  '<b>Select Event Name</b>' ) )
  l <- append( l, c('eventname2' =  'Enter the name of an event to analyze' ) )
  l <- append( l, c('gcount1' =  '<b>Record Count</b>' ) )
  l <- append( l, c('gcount2' =  'Number of records that match search criteria in openFDA' ) )
  l <- append( l, c('gquery1' =  '<b>openFDA Query</b>' ) )
  l <- append( l, c('gquery2' =  'Click the query to see the results of the query in JSON format' ) )
  l <- append( l, c('freqtab1' =  'Frequency Table' ) )
  l <- append( l, c('freqtab2' =  'Counts' ) )
  l <- append( l, c('word1' =   '<b>Word Cloud</b>'  ) )
  l <- append( l, c('word2' =  'Size of words are proportional to the frequency of the word.' ) )
  l <- append( l, c('cloudprrhead' =   'a<b>Word Cloud</b>'  ) )
  l <- append( l, c('cloudprrtext' =  'Size of words are proportional to the PRR of the word.' ) )
  l <- append( l, c('wordPRR' =  'Size of words are proportional to the PRR of the word.' ) )
  l <- append( l, c('wordLRT' =  'Size of words are proportional to the LLR of the word.' ) )
  l <- append( l, c('textplot1' =   '<b>Text Plot</b>'  ) )
  l <- append( l, c('textplot2' =  'Plot of number of events and PRRs for terms.  Selecting a region of terms displays a table of the selected terms' ) )
  l <- append( l, c('dot1' =   'Dot Chart'  ) )
  l <- append( l, c('dot2' =  'Categories are on the y-axis, and frequency is on the x-axis' ) )
  l <- append( l, c('pie1' =   'Pie Chart'  ) )
  l <- append( l, c('pie2' =  'Counts represented as pieces of a pie.' ) )
  
  l <- append( l, c('limit1' =  'Maximum Number of Terms' ) )
  l <- append( l, c('limit2' =  'Maximum number of terms to evaluate.  Most frequent terms are returned first.' ) )
  l <- append( l, c('cplimit1' =  'Maximum Number of Change Points' ) )
  l <- append( l, c('cplimit2' =  'Maximum number of change points to calculate' ) )
  
  l <- append( l, c('drugprr' =  'Drug name is linked to PRR results for drug-event combinations.' ) )
  l <- append( l, c('eventprr' =  'Drug name is linked to PRR results for drug-event combinations.' ) )
  l <- append( l, c('ind?' =  'Indication is linked to medline dictionary definition for indication term.' ) )
  l <- append( l, c('event?' =  '"M" is linked to medline dictionary definition for event term.' ) )
  l <- append( l, c('drug?' =  '"L" is linked to SPL labels for drug in openFDA.' ) )
  l <- append( l, c('dash?' =  '"D" is linked to a dashboard display for the drug.' ) )
  l <- append( l, c('count' =  'Frequency is linked to report that meet the search criteria.' ) )
  
  l <- append( l, c('codrug1' =   'Concomitant Medications'   )  )
  l <- append( l, c('drug1' =   'Drug Name' ) )
  l <- append( l, c('codrug1a' =  paste('Frequency table for drugs found in selected reports.', l['drugprr'] ) ) )
   l <- append( l, c('codrug2' =  paste(l['codrug1a'], l['drug?']) ) )
   l <- append( l, c('codrug3' =  paste( l['codrug2'], l['dash?'] ) ) )
   
   l <- append( l, c('event1' =   'Reported Events'  ) )
   l <- append( l, c('event2' =  'Frequency table of events found in selected reports.  Event term is linked to PRR results for the event. "M" is linked to medline dictionary definition for event term' ) )
   
   l <- append( l, c('indication1' =   'Reported Indication for Drug'  ) )
   l <- append( l, c('indication2' =  'Frequency table of reported indication for which the drug was administered.  Indication is linked to medline dictionary definition for event term' ) )
   
   l <- append( l, c('ts1' =   'Time Series'  ) )
   l <- append( l, c('ts2' =  'Monthly and cumulative counts for drug-event combination.' ) )
   l <- append( l, c('prr1' =  "Proportional Reporting Ratio"   ) )
   l <- append( l, c('prr2' =  "The proportional reporting ratio (PRR) is a simple way to get a measure of how common an adverse event for a particular drug is compared to how common the event is in the overall database.  <br>" ) )
   l <- append( l, c('prr3' =  "A PRR > 1 for a drug-event combination indicates that a greater proportion of the reports for the drug are for the event than the proportion of events in the rest of the database." ) )
   l <- append( l, c('prr4' =  "For example, a PRR of 2 for a drug event combination indicates that the proportion of reports for the drug-event combination is twice the proportion of the event in the overall database." ) )
   l <- append( l, c('prr5' =  paste( l['prr2'], l['prr3'],l['prr4'], l['event?'] ) ) )
   l <- append( l, c('prr_E' =  paste( l['prr2'], l['prr3'], l['prr4'], l['drug?'], l['dash?'] ) ) )
   
   l <- append( l, c('numsims1' =   'Number of Simulations'  ) )
   l <- append( l, c('numsims2' =  'The number of simulations to perform.  Must be between 1,000 and 100,000' ) )
   
#   browser()
                             return( l)
}

ttstrings <- gettt()
tt <- function( tabname, l=ttstrings ){
  out <- l[tabname]
  return( as.character(out) )
}