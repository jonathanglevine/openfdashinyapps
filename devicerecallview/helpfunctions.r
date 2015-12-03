loadhelp <- function(tabname){
  
  l <- vector('list', 8)
  names(l) <- c('selectvars', 'dataoptions', 'graphoptions', 'axisoptions',
                'loaddata', 'overview', 'overviewside')
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
the drug-event combination is twice the proportion of the event in overall database<br>
<br>
PRR = (m/n)/( (M-m)/(N-n) )<br>
 Where <br>
      m = #reports with drug and event<br>
      n = #reports with drug<br>
      M = #reports with event in database<br>
      N = #reports in database<br>
<br><br>
A similar measure is the reporting odds ratio (ROR).<br
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
<p>The openFDA drug adverse event API returns data from the <a href='/data/faers/'>FDA Adverse Event Reporting System (FAERS)</a>, a database that contains information on adverse event and medication error reports submitted to FDA. Currently, this data covers publically releasable records submitted to the FDA from 2004-2013. The data is updated quarterly.</p>

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



return(l[tabname])
}