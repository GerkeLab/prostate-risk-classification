/**********************************************************************************************************************/
  /*  charlson.comorbidity.macro.sas                                                                                    */
  /*  Last updated: 12/5/2018                                                                                           */
  /**********************************************************************************************************************/
  /*  If you encounter any problems with this macro, please email the SAS log file to seer-medicare@imsweb.com.         */
  /**********************************************************************************************************************/
  /*  This SAS macro scans ICD-9 diagnosis and procedure codes from hospital and physician claims for sixteen comorbid  */
  /*  conditions and creates variables to indicate each condition found and the earliest date in which it was found.    */
  /*  It also calculates a comorbidity index (Charlson score) for a patient with respect to cancer.                     */
  /*  Note, this version does not look for HCPCS codes.                                                                 */
  /*                                                                                                                    */
  /*  The macro is designed to look for claims reported in the year prior to diagnosis of cancer, however, the macro    */
  /*  can accommodate any time period by inputting different start and end dates for the window of time for which       */
  /*  claims are to be scanned for ICD-9 definitions of comorbid conditions. NCI recommends calculating Charlson        */
  /*  comorbidity scores using claims for the 12 calendar months prior to the month of diagnosis.                       */
  /*                                                                                                                    */
  /*  The macro may be run using only hospital claims (MEDPAR) or a combination of hospital and physician claims (NCH   */
                                                                                                                    /*  and OUTPAT). If physician claims are used, it has a switch in the call statement that may be used so that it can  */
  /*  remove codes that are considered to have unreliable diagnosis coding ("ruleouts"). Note, if the RULEOUT Algorithm */
  /*  is used, then the input dataset should include claims for at least 30 days before and after the "window".         */
  /*                                                                                                                    */
  /*  When evaluating codes for reliability, the macro employs the following algorithm:                                 */
  /*    KEEPS: all diagnosis codes on MEDPAR claims.                                                                    */
  /*    KEEPS: all diagnosis codes on the Outpatient or Physician/Supplier claims that are also found on MEDPAR claims. */
  /*    KEEPS: all diagnosis codes on the Outpatient or Physician/supplier claims that appear more than once over a     */
  /*     time span exceeding 30 days. (Billing cycles may cause multiple bills to be submitted for the same procedure   */
                                         /*     within that time frame.)                                                                                       */
  /*                                                                                                                    */
  /*  In order to use this macro:                                                                                       */
  /*  1. Include this file in your SAS program                                                                          */
  /*      %include '/directory path/charlson.comorbidity.macro.sas';                                                    */
  /*  2. Create a clean file of claim records to send to the macro.  You may include information from the claims        */
  /*      files MEDPAR, Outpatient SAF and/or Physicial/Supplier (NCH).  All claim records of interest should be        */
  /*      "set together" into a single SAS dataset.                                                                     */
  /*  3. After setting up your dataset, call the macro COMORB:                                                          */
  /*     %macro COMORB(INFILE,PATID,STARTDATE,ENDDATE,CLAIMDATE,CLAIMTYPE,DAYS,DXVARLIST,SXVARLIST,RULEOUT,OUTFILE);    */
  /*     For example:                                                                                                   */
  /*     %COMORB(Claims,patient_id,start_date,end_date,claim_date,filetype,los,dgn_cd1-dgn_cd25,surg1-surg25,1,Comorb); */
  /*     would send the dataset 'Claims', with the person identifier 'patient_id' to the macro.  The number of days     */
  /*     for a hospital stay is found in the variable 'los'.  The dataset includes diagnosis codes 'dgn_cd1-dgn_cd25'   */
  /*     and surgery codes 'surg1-surg25'.  Diagnosis and surgery codes are in ICD-9 format.  The file source of each   */
  /*     claim record is found in the variable 'filetype' (M=MEDPAR, O=Outpatient, N=NCH).  The date of the claim       */
  /*     from the claim record is designated as 'claim_date'. The '1' indicates that diagnosis codes will be            */
  /*     processed via the RULEOUT algorithm.                                                                           */
  /*                                                                                                                    */
  /*  The macro returns a SAS dataset (default name=Comorbidities) which contains 1 record for each person that had     */
  /*  at least one valid claim record within the specified time window.  The variables included in this data set are    */
  /*  the Patient ID,Charlson index, and the comorbid condition indicator flags for the time period of interest.        */
  /*  The data set is sorted by the person identifier.                                                                  */
  /*                                                                                                                    */
  /**********************************************************************************************************************/
  /*  INFILE:    Dataset name: a SAS dataset of Medicare claims that contains the following variables:                  */
  /*  PATID:     Variable name: Unique ID for each patient.                                                             */
  /*  STARTDATE: Variable name: Date the comorbidity window starts, in SAS date format.                                 */
  /*  ENDDATE:   Variable name: Date the comorbidity window ends, in SAS date format.                                   */
  /*  CLAIMDATE: Variable name: Date of the claim found on the claim file, in SAS date format. This can be created by   */
  /*             using the MDY() function (e.g. CLAIMDATE = MDY(FROM_DTM,FROM_DTD,FROM_DTY) or MDY(ADM_M,ADM_D,ADM_Y)). */
  /*  CLAIMTYPE: Variable name: the source of the claim record ('M'=MEDPAR, 'O'=OUTPAT, 'N'=NCH). Note, do not use DME. */
  /*             For MEDPAR claims, all ICD-9 diagnosis codes are accepted (i.e. none are considered "ruleout" codes).  */
  /*  DAYS:      Variable name: contains the length of stay (in days) for hospital visits from MEDPAR.                  */
  /*  DXVARLIST: List of variable names: the diagnosis codes in ICD-9 (e.g. DGN_CD1-DGN_CD25). If there are multiple    */
  /*             variables, some of which cannot be included in a range, please list them using spaces to separate each */
  /*             single element or range (e.g. DGN_CD1-DGN_CD25 ADMDXCDE).                                              */
  /*  SXVARLIST: List of variable names: the surgery or procedure codes in ICD-9 (e.g. SRGCDE1-SRGCDE25).  If there are */
  /*             multiple variables, some of which cannot be included in a range, please list them using spaces to      */
  /*             separate each single element or range (e.g. SRGCDE1-SRGCDE25 PRCDR_CD1).                               */
  /*  RULEOUT:   Flag: Set this to 1 (or R), if the "ruleout algorithm" should be invoked (further details below),      */
  /*             otherwise set this to 0 (or leave it blank).                                                           */
  /*  OUTFILE:   Dataset name: a SAS dataset with the comorbidities and Charlson score determined by the macro.         */
  /*             The number of patients in this output file should equal the number patients in the input file.         */
  /*             If an output dataset name is not specified, the default name is Comorbidities.                         */
  /**********************************************************************************************************************/
  /*  RULEOUT Algorithm:                                                                                                */
  /*    The ruleout algorithm previously was in a separate macro but now is included within the COMORB macro.           */
  /*    It requires a more stringent criteria for which claims to include when identifying comorbidities.               */
  /*    Any "stand-alone" outpatient (OUTPAT or NCH) claims that are not confirmed by other claims are considered to be */
  /*    "ruleout" diagnoses and are excluded from consideration.  A claim is confirmed if the ICD code on that claim    */
  /*    also occurs in MEDPAR or occurs more than 30 days later in OUTPAT or NCH.  All other claims are excluded.       */
  /**********************************************************************************************************************/
  
  
  *** Begin macro COMORB;
%macro COMORB(INFILE,PATID,STARTDATE,ENDDATE,CLAIMDATE,CLAIMTYPE,DAYS,DXVARLIST,SXVARLIST,RULEOUT,OUTFILE);

data claims;
set &INFILE(keep=&PATID &STARTDATE &ENDDATE &CLAIMDATE &CLAIMTYPE &DAYS &DXVARLIST &SXVARLIST);

/***********************************************************************************************************************/
  /*  Example code for creating the "window" of the 1 year before the month of diagnosis                                 */
  /*  Create SAS date variables to indicate start and end of window of time in which diagnosis codes will be checked     */
  /***********************************************************************************************************************/
  /* if not missing(yrdx1) and not missing(modx1) then do;                                                               */
  /*   &STARTDATE = mdy(input(modx1,2.),1,(input(yrdx1,4.)-1)); *** One year before the month of diagnosis;              */
    /*   &ENDDATE   = mdy(input(modx1,2.),1,input(yrdx1,4.))-1; *** Last day of the month before diagnosis;                */
      /*   end;                                                                                                              */
      /* format &STARTDATE &ENDDATE mmddyy10.;                                                                               */
      /***********************************************************************************************************************/
      
      *** Select claim records in appropriate window;  
    *** Keep 30 days extra on both sides of the window for the RULEOUT Algorithm;
    *** (for confirming diagnoses in the Outpatient and Physician/Supplier files);
    %IF &RULEOUT = 1 OR &RULEOUT = R %THEN %DO;
    if (&STARTDATE - 30) <= &CLAIMDATE <= (&ENDDATE + 30);
    %END;
    %ELSE %DO;
    if &STARTDATE <= &CLAIMDATE <= &ENDDATE;
    %END;
    inwindow = (&STARTDATE <= &CLAIMDATE <= &ENDDATE);
    run;
    
    *** Separate incoming data into two datasets: surgical procedures and diagnoses;
    
    *** Dataset SXCLAIMS: outputs each surgical procedure code to a separate observation;
    *** Only MEDPAR & OUTPAT have procedure codes;
    data sxcodes(keep=&PATID &CLAIMDATE sxcode);
    set claims(keep=&PATID &CLAIMDATE &CLAIMTYPE &SXVARLIST inwindow);
    where &CLAIMTYPE in:('M','O') and inwindow;
    array surg (*) $ &SXVARLIST;
    do i=1 to dim(surg);
    if not missing(surg(i)) then do;
    sxcode = upcase(surg(i)); *** Convert any lowercase letters to uppercase;
    output sxcodes;
    end;
    end;
    run;
    
    proc sort nodups data=sxcodes;
    by &PATID sxcode &CLAIMDATE;
    run;
    
    *** One record per procedure code, with the first and last date it appeared in the window;
    data sxcodes(keep=&PATID sxcode first_claim_date last_claim_date daysdiff);
    set sxcodes;
    by &PATID sxcode &CLAIMDATE;
    retain first_claim_date last_claim_date;
    if first.sxcode then first_claim_date = &CLAIMDATE; *** Assign earliest claim date;
    if last.sxcode then do; 
    last_claim_date = &CLAIMDATE; *Assign latest claim date;
    daysdiff = last_claim_date - first_claim_date; 
    output;
    end;
    format first_claim_date last_claim_date mmddyy10.;
    run;
    
    *** Dataset DXCLAIMS: outputs each diagnosis code to a separate observation;
    data dxcodes(keep=&PATID &CLAIMDATE &CLAIMTYPE &DAYS dxcode inwindow &STARTDATE &ENDDATE);
    set claims(keep=&PATID &CLAIMDATE &CLAIMTYPE &DAYS &DXVARLIST inwindow &STARTDATE &ENDDATE);
    array diag(*) $ &DXVARLIST;
    do i=1 to dim(diag);
    if not missing(diag(i)) then do;
    dxcode = upcase(diag(i)); *** Convert any lowercase letters to uppercase;
    output dxcodes;
    end;
    end;
    run;
    
    *** Sort non-missing claims by ID, diagnosis code, and date of claim;
    proc sort nodups data=dxcodes;
    by &PATID dxcode &CLAIMDATE;
    run;
    
    data dxcodes;
    set dxcodes;
    by &PATID dxcode &CLAIMDATE;
    *** one record per ICD is kept, with first date, first date in window, last date in window, last date;
    retain first_date first_claim_date last_claim_date last_date confirmed dayscheck;
    if first.dxcode then do; 
    first_date = &CLAIMDATE; *** Assign earliest claim date;
    first_claim_date = .; 
    last_claim_date = .; 
    last_date = .; 
    confirmed = 0; 
    dayscheck = 0;
    end;
    
    if &CLAIMTYPE=:'M' then confirmed = 1;
    if dxcode=:'410' then do; *** for Acute MI only;
    if (&CLAIMTYPE=:'M' and &DAYS>2) then dayscheck = 1; *** the DAYS variable can be character or numeric;
    else if not (&CLAIMTYPE=:'M') then dayscheck = 1; 
    end;
    
    if inwindow and first_claim_date=. then first_claim_date = &CLAIMDATE;
    if inwindow then last_claim_date = &CLAIMDATE;
    
    if last.dxcode then do; 
    last_date = &CLAIMDATE; *** Assign latest claim date;
    if first_date>. then daysdiff = last_date - first_date; 
    output;
    end;
    
    drop &CLAIMDATE inwindow;
    format first_date first_claim_date last_claim_date last_date mmddyy10.;
    run;
    
    *** Sort data by patient identifier and restrict claims to comorbidity window;
    proc sort data=dxcodes;
    where (&STARTDATE <= first_claim_date <= &ENDDATE);
    by &PATID;
    run;
    
    *** Process claims for ruling out certain ICD codes if the RULEOUT indicator flag is set in macro call;
    %IF &RULEOUT = 1 OR &RULEOUT = R %THEN %DO;
    proc sort data=dxcodes;
    where confirmed or daysdiff>30; *** delete all unconfirmed ICD codes with first & last date less than 30 days apart;
    by &PATID;
    run;
    %END;
    
    *** Some patients can get dropped by the RULEOUT algorithm, so they get added back with this dataset;
    proc sort nodupkey data=claims out=cases(keep=&PATID &STARTDATE &ENDDATE);
    where &STARTDATE <= &CLAIMDATE <= &ENDDATE;
    by &PATID;
    run;
    
    *** set the default output filename;
    %IF %LENGTH(&OUTFILE)=0 %THEN %LET OUTFILE=Comorbidities;
    
    *** Check for comorbidities and create comorbidity indicators and Charlson & NCI Index scores;
    *** Setting the CASES dataset last ensures that start & end dates are output for all patients;
    data &OUTFILE;
    set sxcodes(in=sx) dxcodes(in=dx) cases(in=c keep=&PATID &STARTDATE &ENDDATE);
    by &PATID;
    
    length acute_mi history_mi chf pvd cvd copd dementia paralysis diabetes diabetes_comp 
    renal_disease mild_liver_disease liver_disease ulcers rheum_disease aids Charlson 3.;
    
    length acute_mi_date history_mi_date chf_date pvd_date cvd_date copd_date dementia_date paralysis_date diabetes_date diabetes_comp_date 
    renal_disease_date mild_liver_disease_date liver_disease_date ulcers_date rheum_disease_date aids_date 8.;
    
    retain acute_mi--aids acute_mi_date--aids_date;
    
    *** Initialize comorbidity indicator variables for unique conditions;
    if first.&PATID then call missing(of acute_mi--aids); *** Sets all values to missing;
    if first.&PATID then call missing(of acute_mi_date--aids_date); *** Sets all values to missing;
    
    *** Scan diagnosis codes for comorbidities and set flag variables when found;
    
    if dx then do; *** Begin diagnosis code loop;
    
    *** ACUTE MYOCARDIAL INFARCTION;
    if (dxcode=:'410' and dayscheck) then do; *** los>2;
    acute_mi = 1; 
    if acute_mi_date=. or .<first_claim_date<acute_mi_date then acute_mi_date = first_claim_date; 
    end;
    
    *** HISTORY OF MYOCARDIAL INFARCTION;
    else if dxcode='412  ' then do; 
    history_mi = 1; 
    if history_mi_date=. or .<first_claim_date<history_mi_date then history_mi_date = first_claim_date; 
    end;
    
    *** CONGESTIVE HEART FAILURE;
    else if dxcode in:('39891','4254','4255','4257','4258','4259','428') then do; 
    chf = 1; 
    if chf_date=. or .<first_claim_date<chf_date then chf_date = first_claim_date; 
    end;
    
    *** PERIPHERAL VASCULAR DISEASE;
    else if dxcode in:('0930','440','441','7854','V434') or 
    ('4420'<=:dxcode<=:'4428') or 
    ('4431'<=:dxcode<=:'4439') or 
    ('44770'<=:dxcode<=:'44773') then do; 
    pvd = 1; 
    if pvd_date=. or .<first_claim_date<pvd_date then pvd_date = first_claim_date; 
    end;
    
    *** CEREBROVASCULAR DISEASE;
    else if ('430'<=:dxcode<=:'438') then do; 
    cvd = 1; 
    if cvd_date=. or .<first_claim_date<cvd_date then cvd_date = first_claim_date; 
    end;
    
    *** COPD;
    else if dxcode in:('4168','4169','5064','5191') or
    ('490'<=:dxcode<=:'496') or
    ('500'<=:dxcode<=:'505') then do; 
    copd = 1; 
    if copd_date=. or .<first_claim_date<copd_date then copd_date = first_claim_date; 
    end;
    
    *** DEMENTIA;
    else if dxcode in:('290','2910','2911','2912','29282','2941','3310','3311','3312','33182') then do; 
    dementia = 1; 
    if dementia_date=. or .<first_claim_date<dementia_date then dementia_date = first_claim_date; 
    end;
    
    *** PARALYSIS;
    else if dxcode in:('342','3449') or ('3440'<=:dxcode<=:'3446') then do; 
    paralysis = 1; 
    if paralysis_date=. or .<first_claim_date<paralysis_date then paralysis_date = first_claim_date; 
    end;
    
    *** DIABETES;
    else if dxcode='250  ' or ('2500'<=:dxcode<=:'2503') then do; 
    diabetes = 1; 
    if diabetes_date=. or .<first_claim_date<diabetes_date then diabetes_date = first_claim_date; 
    end;
    
    *** DIABETES WITH COMPLICATIONS;
    else if dxcode=:'3620' or ('2504'<=:dxcode<=:'2509') then do; 
    diabetes_comp = 1; 
    if diabetes_comp_date=. or .<first_claim_date<diabetes_comp_date then diabetes_comp_date = first_claim_date; 
    end;
    
    *** MODERATE-SEVERE RENAL DISEASE;
    else if dxcode in:('40301','40311','40391','40402','40403','40412','40413','40492','40493',
                       '582','583','585','586','588','V420','V451','V56') then do; 
    renal_disease = 1; 
    if renal_disease_date=. or .<first_claim_date<renal_disease_date then renal_disease_date = first_claim_date; 
    end;
    
    *** MILD LIVER DISEASE;
    else if dxcode in:('07032','07033','07054','5712','5714','5715','5716') then do; 
    mild_liver_disease = 1; 
    if mild_liver_disease_date=. or .<first_claim_date<mild_liver_disease_date then mild_liver_disease_date = first_claim_date; 
    end;
    
    *** MODERATE-SEVERE LIVER DISEASE;
    else if dxcode in:('07022','07023','07044','V427') or
    ('4560'<=:dxcode<=:'4562') or
    ('5722'<=:dxcode<=:'5728') then do; 
    liver_disease = 1; 
    if liver_disease_date=. or .<first_claim_date<liver_disease_date then liver_disease_date = first_claim_date; 
    end;
    
    *** PEPTIC ULCER DISEAS;
    else if ('531'<=:dxcode<=:'534') then do; 
    ulcers = 1; 
    if ulcers_date=. or .<first_claim_date<ulcers_date then ulcers_date = first_claim_date; 
    end;
    
    *** RHEUMATOLOGIC DISEASE;
    else if dxcode in:('7100','7101','7104','71481','725  ') or ('7140'<=:dxcode<=:'7142') then do; 
    rheum_disease = 1; 
    if rheum_disease_date=. or .<first_claim_date<rheum_disease_date then rheum_disease_date = first_claim_date; 
    end;
    
    *** AIDS;
    else if dxcode in:('79571','V08') or ('042'<=:dxcode<=:'044') then do; 
    aids = 1; 
    if aids_date=. or .<first_claim_date<aids_date then aids_date = first_claim_date; 
    end;
    
    end; *** End diagnosis code loop;
    
    
    *** Scan surgery or procedure codes for comorbidities and set flag variables when found;
    
    if sx then do; *** Begin surgery code loop;
    
    *** CEREBROVASCULAR DISEASE;
    if sxcode in('0061','0062','0063','0065','3812','3832','3842','3922','3928','3974') then do; 
    cvd = 1; 
    if cvd_date=. or .<first_claim_date<cvd_date then cvd_date = first_claim_date; 
    end;
    
    *** PERIPHERAL VASCULAR DISEASE;
    else if sxcode in('0060','3813','3814','3815','3816','3818','3833','3834','3836','3838',
                      '3843','3844','3846','3848','3868','3925','3929') then do; 
    pvd = 1; 
    if pvd_date=. or .<first_claim_date<pvd_date then pvd_date = first_claim_date; 
    end;
    
    *** MODERATE-SEVERE RENAL DISEASE;
    else if sxcode in('3927','3942','3995','5498','5569') then do; 
    renal_disease = 1; 
    if renal_disease_date=. or .<first_claim_date<renal_disease_date then renal_disease_date = first_claim_date; 
    end;
    
    *** MODERATE-SEVERE LIVER DISEASE;
    else if sxcode in:('391 ','4291','505') then do; 
    liver_disease = 1; 
    if liver_disease_date=. or .<first_claim_date<liver_disease_date then liver_disease_date = first_claim_date; 
    end;
    
    end; *** End surgery code loop;
    
    format acute_mi_date--aids_date mmddyy10.;
    
    *** Define arrays for comorbidity condition flags;
    array comorb (*) acute_mi--aids;
    if last.&PATID then do;
    do i=1 to dim(comorb);
    if comorb(i)=. then comorb(i) = 0;
    end;
    
    *** Calculate the Charlson Comorbidity Score for prior conditions;
    Charlson = 
      1*(acute_mi or history_mi) +
      1*(chf) +
      1*(pvd) +
      1*(cvd) +
      1*(copd) +
      1*(dementia) +
      2*(paralysis) +
      1*(diabetes and not diabetes_comp) +
      2*(diabetes_comp) +
      2*(renal_disease) +
      1*(mild_liver_disease and not liver_disease) +
      3*(liver_disease) +
      1*(ulcers) +
      1*(rheum_disease) +
      6*(aids);
    
    *** Calculate the NCI Comorbidity Index for prior conditions;
    NCIindex = 
      1.14*(acute_mi) +
      1.08*(history_mi) +
      1.91*(chf) +
      1.30*(pvd) +
      1.32*(cvd) +
      1.69*(copd) +
      2.06*(dementia) +
      1.49*(paralysis) +
      1.34*(diabetes or diabetes_comp) +
      1.60*(renal_disease) +
      2.09*(mild_liver_disease or liver_disease) +
      1.08*(ulcers) +
      1.25*(rheum_disease) +
      1.79*(aids);
    
    output;
    end;
    
    keep &PATID &STARTDATE &ENDDATE acute_mi--aids acute_mi_date--aids_date Charlson NCIindex;
    
    label 
    Charlson           = 'Charlson comorbidity score'
    NCIindex           = 'NCI comorbidity index'
    acute_mi           = 'Acute Myocardial Infarction'
    history_mi         = 'History of Myocardial Infarction'
    chf                = 'Congestive Heart Failure'
    pvd                = 'Peripheral Vascular Disease'
    cvd                = 'Cerebrovascular Disease'
    copd               = 'Chronic Obstructive Pulmonary Disease'
    dementia           = 'Dementia'
    paralysis          = 'Hemiplegia or Paraplegia'
    diabetes           = 'Diabetes'
    diabetes_comp      = 'Diabetes with Complications'
    renal_disease      = 'Moderate-Severe Renal Disease'
    mild_liver_disease = 'Mild Liver Disease'
    liver_disease      = 'Moderate-Severe Liver Disease'
    ulcers             = 'Peptic Ulcer Disease'
    rheum_disease      = 'Rheumatologic Disease'
    aids               = 'AIDS'
    acute_mi_date           = 'First indication of Acute Myocardial Infarction'
    history_mi_date         = 'First indication of History of Myocardial Infarction'
    chf_date                = 'First indication of Congestive Heart Failure'
    pvd_date                = 'First indication of Peripheral Vascular Disease'
    cvd_date                = 'First indication of Cerebrovascular Disease'
    copd_date               = 'First indication of Chronic Obstructive Pulmonary Disease'
    dementia_date           = 'First indication of Dementia'
    paralysis_date          = 'First indication of Hemiplegia or Paraplegia'
    diabetes_date           = 'First indication of Diabetes'
    diabetes_comp_date      = 'First indication of Diabetes with Complications'
    renal_disease_date      = 'First indication of Moderate-Severe Renal Disease'
    mild_liver_disease_date = 'First indication of Mild Liver Disease'
    liver_disease_date      = 'First indication of Moderate-Severe Liver Disease'
    ulcers_date             = 'First indication of Peptic Ulcer Disease'
    rheum_disease_date      = 'First indication of Rheumatologic Disease'
    aids_date               = 'First indication of AIDS'
    ;
    run;
    
    %mend; *** End macro COMORB;