BKMVQCR ;PRXM/HC/BWF - BKMV Quality of Care Report; [ 1/12/2005  7:16 PM ] ; 19 Sep 2005  2:10 PM
 ;;2.1;HIV MANAGEMENT SYSTEM;;Feb 07, 2011
 Q
EN(REG) ;EP - Primary
 D ^XBFMK
 ; Check taxonomies - added per bugzilla #1497
 NEW DFLAG
 S DFLAG=1 D EN^BKMVC1
 N BACK,SRCH,EDATE,EXTE,DFN,CNT,DENPOP,COMMIEN,PROVIEN,SEL
EN1 ;
 S BACK=0
 D NOW^%DTC
 S SRCH=$$SELPAT()
 I SRCH=""!(SRCH="^") G XIT
 D ENDATE
 I EDATE=-1 G XIT
 S EDATE=EDATE_".2400" ; Report should include info through midnight on the end date.
 S EXTE=$S(SRCH="A":"ACT",SRCH="R":"RPMS",SRCH="DP":"DP",SRCH="PP":"PP",SRCH="C":"COMM",1:0)
 I EXTE=0 G XIT
 D @EXTE
 I $D(DTOUT)!$D(DUOUT) G XIT
 I $G(BACK) G EN1
 I SRCH'="A" D
 . D EN^DDIOL("")
 . D EN^DDIOL("Since this option searches all patients in RPMS the report may")
 . D EN^DDIOL("take a long time and it is recommended to queue it to a printer.")
 . D EN^DDIOL("")
 . Q
 S %ZIS="MQ" D ^%ZIS I POP G XIT
 I $D(IO("Q")) D
 . S ZTRTN="BCGRND^BKMVQCR"
 . S ZTSAVE("EDATE")="",ZTSAVE("SRCH")="",ZTSAVE("DENPOP")=""
 . S ZTSAVE("COMMIEN")="",ZTSAVE("PROVIEN")="",ZTSAVE("SEL")=""
 . S ZTSAVE("EXTE")="",ZTSAVE("REG")=""
 . S ZTDESC="HMS QUALITY OF CARE AUDIT REPORT"
 . Q
 I $D(IO("Q")) K IO("Q") D ^%ZTLOAD W:$D(ZTSK) !,"REQUEST QUEUED" H 2 D CLEAN G XIT
 D FLTRMSG^BKMIMRP1
BCGRND ; start compiling report
 K ^TMP("BKMVQCR",$J)
 D @(EXTE_"WRK")
 S DFN=0
 F CNT=0:1 S DFN=$O(^TMP("BKMVQCR",$J,"HIVCHK",DFN)) I 'DFN Q
 S ^TMP("BKMVQCR",$J,"HIVTOT1")=CNT
 ; compile totals by Gender and Age
 D GENDER,AGE
 ; run CD4, Viral load, rapid plasma reagin algorithms
 D CD4CHK^BKMVQCR1,VRLLD^BKMVQCR1,RPR^BKMVQCR1
 ; run chlamydia, gonorrhea, tuberculosis, and pneumo algorithms
 D CHLAM^BKMVQCR2,GON^BKMVQCR2,TBT21^BKMVQCR2,PNEUMO^BKMVQCR2
 ; run tetanus, eye, dental, and pap algorithms
 D TETSTAT^BKMVQCR3,EYEEXAM^BKMVQCR3,DENTEXAM^BKMVQCR3,PAP^BKMVQCR3
 ; run ARV and MAC algorithms
 D ARVM03^BKMVQCR4,ARVM02^BKMVQCR4,ARVM05^BKMVQCR4,PCP^BKMVQCR4,MAC^BKMVQCR4
 ; run tobacco, substance abuse algorithms
 D TOBACCO^BKMVQCR5,SUBS01^BKMVQCR5
EN2 ; do mathematical calculations
 D CD4CALC^BKMVQCR6,VRLLDC^BKMVQCR6,RPRCALC^BKMVQCR6
 D CHLAMC^BKMVQCR7,GONCALC^BKMVQCR7,TBCALC^BKMVQCR7
 D PNEUMOC^BKMVQCR8,TETCALC^BKMVQCR8,EYECALC^BKMVQCR8,DENTCALC^BKMVQCR8,PAPCALC^BKMVQCR8
 D ARVCALC^BKMVQCR9,PCP^BKMVQCR9,MAC^BKMVQCR9,TOBCALC^BKMVQCR9,SUBCALC^BKMVQCR9
EN3 ; print the report
 D PRINT^BKMVQCRA
 D CLEAN,^%ZISC,XIT
 Q
SELPAT() ; select the type of patients to search
SELPAT1 ; GOTO return
 K DIR
 S DIR("A")="Select Patients by"
 ; Patient Panel not yet available. Will be more fully defined in iCare software.
 ;S DIR(0)="SO^A:Active from HMS Register;R:All RPMS;DP:HIV Provider;C:Community;PP:Patient Panel"
 ;PRXM/HC/BHS - 12/15/2005 - Deactivated R/DP/C options at this time
 ;S DIR(0)="SO^A:Active from HMS Register;R:All RPMS;DP:HIV Provider;C:Community"
 S DIR(0)="SO^A:Active from HMS Register;R:All RPMS -- Not available in this version;DP:HIV Provider -- Not available in this version;C:Community -- Not available in this version"
 I '$D(^BKM(90451,"D",REG)) D
 .; Patient Panel not yet available. Will be more fully defined in iCare software.
 .;S DIR(0)="SO^R:All RPMS;DP:Designated Provider;C:Community;PP:Patient Panel"
 .S DIR(0)="SO^R:All RPMS -- Not available in this version;DP:Designated Provider -- Not available in this version;C:Community -- Not available in this version"
 D ^DIR I $D(DTOUT)!$D(DUOUT) S Y=""
 I ",R,DP,C,"[(","_Y_",") G SELPAT1
 Q Y
ENDATE ; ending date selection
 S %DT="AE"
 S %DT("A")="Select ending date for report: ",%DT("B")="TODAY"
 D ^%DT
 S EDATE=Y
 Q
ACT ; Active from HMS Register Additional Criteria
 K DIR
 S DIR("A")="Select diagnosis"
 S DIR(0)="SO^A:All;AD:AIDS;H:HIV"
 D ^DIR
 I $D(DTOUT)!$D(DUOUT) Q
 I Y="" S BACK=1 Q
 S SEL=Y
 S DENPOP="Active Register Patients: "_Y(0)
 Q
ACTWRK ; Active from HMS Register Search
 N CHKSTR,REGIEN,REGLOC,STAT,DFN,DXTYP
 N DIAGDT,AIDDT,HIVDT,VSTDT,ENDDT
 S ENDDT=$$FMADD^XLFDT(EDATE,-183)
 I SEL="A" S CHKSTR="AH"
 I SEL="H" S CHKSTR="H"
 I SEL="AD" S CHKSTR="A"
 S REGIEN=0
 F  S REGIEN=$O(^BKM(90451,"D",REG,REGIEN)) Q:REGIEN=""  D
 . S REGLOC=$O(^BKM(90451,"D",REG,REGIEN,0))
 . S STAT=$$GET1^DIQ(90451.01,REGLOC_","_REGIEN_",",.5,"I")
 . I STAT'="A" Q
 . S DFN=$$GET1^DIQ(90451,REGIEN_",",".01","I")
 . I DFN="" Q
 . S DXTYP=$$GET1^DIQ(90451.01,REGLOC_","_REGIEN_",",2.3,"I")
 . I CHKSTR'[DXTYP Q
 . ; find latest date of fields 5 and 5.5
 . S VSTDT=""
 . S HIVDT=$$GET1^DIQ(90451.01,REGLOC_","_REGIEN_",",5.5,"I")
 . I HIVDT'="",HIVDT<VSTDT!(VSTDT="") S VSTDT=HIVDT
 . S AIDDT=$$GET1^DIQ(90451.01,REGLOC_","_REGIEN_",",5,"I")
 . I AIDDT'="",AIDDT<VSTDT!(VSTDT="") S VSTDT=AIDDT
 . I VSTDT="" Q
 . I VSTDT>ENDDT Q
 . S ^TMP("BKMVQCR",$J,"HIVCHK",DFN)=""
 . Q
 Q
RPMS ; All RPMS Additional Criteria (No questions required)
 S DENPOP="All RPMS"
 Q
RPMSWRK ; All RPMS Search
 N DFN,HIVDT
 S HIVDT=$$FMADD^XLFDT(EDATE,-365)
 S DFN=0
 F  S DFN=$O(^AUPNPAT(DFN)) Q:'DFN  D HIVCHK(DFN,EDATE,HIVDT)
 Q
DP ; Designated Provider Additional Criteria
 N PROV,NAME
DP1 ;
 S DIC="^VA(200,",DIC(0)="AEQ",DIC("A")="Select A Provider: " D ^DIC
 I $D(DTOUT)!$D(DUOUT) Q
 I Y=-1 S BACK=1 Q
 S PROV=Y
 S PROVIEN=$P(PROV,U,1)
 S NAME=$P(PROV,U,2)
 I '$D(^VA(200,"AK.PROVIDER",NAME)) D  G DP1
 . W !,NAME," is not an active Provider. Please make another selection.",!
 . Q
 S DENPOP="HIV Provider: "_NAME
 Q
DPWRK ; Designated Provider Search
 N DFN,HIVDT
 S HIVDT=$$FMADD^XLFDT(EDATE,-365)
 ;Currently do not want to search by provider in register, just all RPMS.
 ;Leaving code available in case this changes in the future.
 ;N PATPROV,IENS
 ;I $D(^BKM(90451,"D",REG)) D  Q
 ;. S DFN=0
 ;. ; Search REGISTER PROVIDER in file 90451.01
 ;. F  S DFN=$O(^BKM(90451,"B",DFN)) Q:'DFN  D
 ;.. N DA
 ;.. S DA(1)=$O(^BKM(90451,"B",DFN,"")) Q:DA(1)=""
 ;.. S DA=$O(^BKM(90451,DA(1),1,"B",REG,"")) Q:DA=""
 ;.. S IENS=$$IENS^DILF(.DA)
 ;.. S PATPROV=$$GET1^DIQ(90451.01,IENS,6,"I")
 ;.. I PATPROV'=PROVIEN Q
 ;.. D HIVCHK(DFN,EDATE,HIVDT)
 ;.. Q
 ;. Q
 S DFN=0
 ; Search PRIMARY CARE PROVIDER x-ref in file 9000001 (which is stored by IEN)
 F  S DFN=$O(^AUPNPAT("AK",PROVIEN,DFN)) Q:'DFN  D HIVCHK(DFN,EDATE,HIVDT)
 Q
 ; Not yet implemented since this is going to be defined more fully in the iCare software.
PP ; Patient Panel Additional Criteria
 S DENPOP="Patient Panel"
 Q
PPWRK ; Patient Panel Search
 N DFN,HIVDT
 S HIVDT=$$FMADD^XLFDT(EDATE,-365)
 Q
COMM ; Community Additional Criteria
 N COMM,STOP
 S STOP=0
 S DIC="^AUTTCOM(",DIC(0)="AEQM",DIC("A")="Select A Community: "
 S DIC("W")="D DICW^BKMVQCR"
 F  D  Q:STOP
 . D ^DIC
 . I Y=-1 S STOP=1 Q
 . S COMM=Y
 . S COMMIEN($P(COMM,U))=$P(COMM,U,2)
 . Q
 I $D(DTOUT)!$D(DUOUT) Q
 I '$D(COMMIEN) S BACK=1 Q
 S DENPOP="Selected Communities"
 ; If only one community selected then display the name.
 S COMM=$O(COMMIEN("")) I COMM'="" I $O(COMMIEN(COMM))="" S DENPOP="Community: "_COMMIEN(COMM)
 Q
COMMWRK ; Community Search
 N DFN,HIVDT,COMM
 S HIVDT=$$FMADD^XLFDT(EDATE,-365)
 ;Currently do not want to search by community in register, just all RPMS.
 ;Leaving code available in case this changes in the future.
 ;N REGIEN,PTCOMM
 ;I $D(^BKM(90451,"D",REG)) D  Q
 ;. S REGIEN=0
 ;. F  S REGIEN=$O(^BKM(90451,"D",REG,REGIEN)) Q:REGIEN=""  D
 ;.. S DFN=$$GET1^DIQ(90451,REGIEN_",",.01,"I")
 ;.. S PTCOMM=$$GET1^DIQ(9000001,DFN_",",1117,"I")
 ;.. I PTCOMM="" Q
 ;.. I $D(COMMIEN(PTCOMM)) D HIVCHK(DFN,EDATE,HIVDT)
 ;.. Q
 ;. Q
 S COMM=""
 F  S COMM=$O(COMMIEN(COMM)) Q:COMM=""  D
 . S DFN=0
 . ; Search COMMUNITY x-ref in file 9000001 (which is stored by NAME and not IEN)
 . F  S DFN=$O(^AUPNPAT("AC",COMMIEN(COMM),DFN)) Q:'DFN  D HIVCHK(DFN,EDATE,HIVDT)
 . Q
 Q
 ; HIVDT should be set to EDATE-365 before calling this function.
HIVCHK(DFN,EDATE,HIVDT) ;check patient for HIV status.
 N VSTDATA,CHECK,SIXMONCK,SIXMOS,CNT
 ; search on BGP HIV/AIDS DXS taxonomy
 D ICDTAX^BKMIXX1(DFN,"BGP HIV/AIDS DXS",EDATE,HIVDT,"VSTDATA(VSTDT)")
 S CNT=0,CHECK=0,SIXMONCK=0,SIXMOS=$$FMADD^XLFDT(EDATE,-183)
 F CNT=0:1 S CHECK=$O(VSTDATA(CHECK)) Q:CHECK=""  I CHECK>SIXMOS S SIXMONCK=1
 I CNT>1 I SIXMONCK=1 S ^TMP("BKMVQCR",$J,"HIVCHK",DFN)=""
 Q
GENDER ; gender totals compilation
 N DFN,TOTM,TOTF,TOTU,SEX
 S (DFN,TOTM,TOTF,TOTU)=0
 F  S DFN=$O(^TMP("BKMVQCR",$J,"HIVCHK",DFN)) Q:'DFN  D
 .S SEX=$$GET1^DIQ(2,DFN_",",".02","I")
 .I SEX="F" S TOTF=TOTF+1 Q
 .I SEX="M" S TOTM=TOTM+1 Q
 .S TOTU=TOTU+1
 S ^TMP("BKMVQCR",$J,"FEMALE")=TOTF
 S ^TMP("BKMVQCR",$J,"MALE")=TOTM
 S ^TMP("BKMVQCR",$J,"UNSPEC")=TOTU
 Q
AGE ; age totals compilation
 N DFN,AGE,CNT1,CNT2,CNT3,CNT4
 S DFN=0,(CNT1,CNT2,CNT3,CNT4)=0
 F  S DFN=$O(^TMP("BKMVQCR",$J,"HIVCHK",DFN)) Q:'DFN  D
 . S AGE=$$AGE^BKMIMRP1(DFN)
 . ; AGE could return days, weeks or months for patients under 3 years
 . I AGE'?1.N S CNT1=CNT1+1 Q
 . ; AGE<15 is a single category
 . I AGE<15 S CNT1=CNT1+1 Q
 . I AGE'>44 S CNT2=CNT2+1 Q
 . I AGE'>64 S CNT3=CNT3+1 Q
 . ; AGE>64
 . S CNT4=CNT4+1
 . Q
 S ^TMP("BKMVQCR",$J,"AGE1")=CNT1
 S ^TMP("BKMVQCR",$J,"AGE2")=CNT2
 S ^TMP("BKMVQCR",$J,"AGE3")=CNT3
 S ^TMP("BKMVQCR",$J,"AGE4")=CNT4
 Q
CLEAN ;clean up variables
 K ^TMP("BKMVQCR",$J)
 Q
XIT ;
 D ^XBFMK
 Q
DICW ;EP - This is a specially written FileMan 'WRITE' statement
 N NZ,NAME,COUNTY,STATE,CODE,STCTYCOM
 S NZ=$G(^(0))
 S NAME=$P(NZ,U)
 S COUNTY=$P(NZ,U,2) I COUNTY'="" S COUNTY=$P($G(^AUTTCTY(COUNTY,0)),U)
 S STATE=$P(NZ,U,3) I STATE'="" S STATE=$P($G(^DIC(5,STATE,0)),U)
 S CODE=$P(NZ,U,7)
 S STCTYCOM=$P(NZ,U,8)
 S COUNTY=$J($E(COUNTY,1,15),15)
 S STATE=$J($E(STATE,1,17),17)
 S CODE=$J($E(CODE,1,6),6)
 S STCTYCOM=$J($E(STCTYCOM,1,10),10)
 W ?27,COUNTY," ",STATE," ",CODE," ",STCTYCOM
 Q
