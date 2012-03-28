XUPCH117 ;ISF/RWF - PATCH XU*8*117 ;06/02/99  13:35 [ 04/02/2003   8:29 AM ]
 ;;8.0;KERNEL;**1007**;APR 1, 2003
 ;;8.0;KERNEL;**117**;Apr 10, 1999
 Q
POST ;Update the package file with the data
 N DATA,IX
 F IX=1:1 S DATA=$T(DATA+IX) Q:DATA=""  S DATA=$P(DATA,";;",2) D UPDATE
 D DEL ;Remove old fields.
 D FMDC ;Add package
 Q
UPDATE ;Do the work
 N D,J,L,NIEN,PIEN
 S NIEN=$O(^DIC(9.4,"B",$P(DATA,U),0)),PIEN=$O(^DIC(9.4,"C",$P(DATA,U,2),0))
 I +NIEN,+PIEN,NIEN'=PIEN D
 . D BMES^XPDUTL("For package "_$P(DATA,U)_" with Prefix "_$P(DATA,U,2))
 . D BMES^XPDUTL("NAME ("_NIEN_") and PREFIX ("_PIEN_") don't point to the same record.")
 . Q
 I 'NIEN,'PIEN D NEW
 S:'PIEN PIEN=NIEN S:'NIEN NIEN=PIEN S J=$S(NIEN<PIEN:NIEN,1:PIEN)
 D WORK(J)
 Q
NEW ;
 N FDA,XIEN
 S FDA(9.4,"+1,",.01)=$P(DATA,U),FDA(9.4,"+1,",1)=$P(DATA,U,2)
 D UPDATE^DIE("","FDA","XIEN") S (NIEN,PIEN)=XIEN(1)
 Q
WORK(DA) ;
 N I,K,FDA,IEN
 S K=$P($G(^DIC(9.4,DA,0)),U) I K'=$P(DATA,U) S FDA(9.4,DA_",",.01)=$P(DATA,U)
 F K=3:1 S I=$P(DATA,U,K) Q:I=""  S FDA(9.4014,"?+"_K_","_DA_",",.01)=I
 I $D(FDA) D UPDATE^DIE("","FDA","IEN")
 Q
FMDC ;Setup FMDC version 1.0
 N FDA,IEN,DA
 S DA=$O(^DIC(9.4,"C","FMDC",0)) Q:'DA
 S FDA(9.4,DA_",",13)="1.0",FDA(9.49,"?+2,"_DA_",",.01)="1.0",IEN(2)=1
 D UPDATE^DIE("","FDA","IEN") ;I $D(^TMP("DIERR",$J)) W !,"ERROR"
 Q
DEL ;Delete DD fields
 N IX,DATA,DA,FILE,FLD,XFDA,X,XIEN
 ;DD(9.4 fields to remove
 ;F I=1:1 S X=$P(DATA,";",I) Q:X=""  W !,X,?5,$P($G(^DD(9.4,X,0)),U)
 S FILE=9.4,DATA="4;5;11.1;11.4;11.5;11.6;11.7;12;200.1;200.2;1920;1933",DA=0,XDR=""
 F I=1:1 S X=$P(DATA,";",I) Q:X=""  I '(+$P($G(^DD(9.4,X,0),"^1"),U,2)) S XDR=XDR_X_"///@;"
 F  S DA=$O(^DIC(FILE,DA)) Q:DA'>0  D
 . S DIE="^DIC(9.4,",DR=XDR D ^DIE
 . Q
 D DELLN
 ;DD(9.4,DA(1),22, to remove
 S FILE=9.49,DATA="51;61;62;63"
 D DELLN
 Q
DELLN ;
 N IJ,DIK,DA,FLD
 F IJ=1:1 S FLD=$P(DATA,";",IJ) Q:FLD=""  D
 . K DA,DIK
 . S DO=$P($G(^DD(FILE,FLD,0)),U,2) Q:DO=""
 . I +DO D SUBFLD(FILE,+DO) Q
 . S DA=FLD,DIK="^DD("_FILE_",",DA(1)=FILE D ^DIK
 . Q
 Q
SUBFLD(FL,FD) ;
 N DIU
 S DIU=FD,DIU(0)="DS" D EN^DIU2
 Q
BLD ;BUILD DATA
 D HOME^%ZIS
 N PK,DA,IX
 S PK=0,U="^",IO="SDP.DAT"
 O IO:NEWVERSION
 F  S PK=$O(^RWF("PATCH",PK)) Q:PK'>0  S X=$G(^(PK,0)) D
 . S X=$C(9)_";;"_$P(X_"^^","^",1,2),Y=""
 . F IX=0:0 S IX=$O(^RWF("PATCH",PK,14,IX)) Q:IX'>0  S Y=Y_"^"_$P(^(IX,0),U)
 . S:$L(Y) X=X_Y U IO(0) W !,X
 . U IO W X,!
 . Q
 C IO
 Q
DATA ;
 ;;ACCOUNTS RECEIVABLE^PRCA^PRY^RC
 ;;ADVERSE REACTION TRACKING^GMRA^GMA
 ;;ASISTS^OOPS
 ;;AUTHORIZATION/SUBSCRIPTION^USR
 ;;AUTO REPLENISHMENT/WARD STOCK^PSGW
 ;;AUTOMATED INFO COLLECTION SYS^IBD
 ;;AUTOMATED LAB INSTRUMENTS^LA
 ;;AUTOMATED MED INFO EXCHANGE^DVBA^DVBE^DVBX^DVBY
 ;;CAPACITY MANAGEMENT - RUM^KMPR
 ;;CLINICAL INFO RESOURCE NETWORK^RG^MRF
 ;;CLINICAL MONITORING SYSTEM^QAM
 ;;CMOP^PSX
 ;;CONSULT/REQUEST TRACKING^GMRC^GMRS^GMRT
 ;;CONTROLLED SUBSTANCES^PSD
 ;;CPT/HCPCS CODES^ICPT^DGYA
 ;;DENTAL^DENT
 ;;DIETETICS^FH
 ;;DISCHARGE SUMMARY^GMRD^GMRE
 ;;DRG GROUPER^ICD^IC
 ;;DRUG ACCOUNTABILITY^PSA
 ;;DSS EXTRACTS^ECX
 ;;EEO COMPLAINT TRACKING^EEO
 ;;ENGINEERING^EN
 ;;EQUIPMENT/TURN-IN REQUIEST^PRCN
 ;;EVENT CAPTURE^EC
 ;;FEE BASIS^FB
 ;;FILEMAN DELPHI COMPONENTS^FMDC
 ;;GEN. MED. REC. - GENERATOR^GMRG
 ;;GEN. MED. REC. - I/O^GMRY
 ;;GEN. MED. REC. - VITALS^GMRV
 ;;GENERIC CODE SHEET^GEC
 ;;HEALTH LEVEL SEVEN^HL
 ;;HEALTH SUMMARY^GMTS
 ;;HINQ^DVB^DVBC^DVBU^DVBV^DVBW^DVY
 ;;HOSPITAL BASED HOME CARE^HBH
 ;;ICR - IMMUNOLOGY CASE REGISTRY^IMR
 ;;IFCAP^PRC^PRX
 ;;IMAGING^MAG^ZMAG
 ;;INCIDENT REPORTING^QAN
 ;;INCOME VERIFICATION MATCH^IVM
 ;;INPATIENT MEDICATIONS^PSJ^PSIV^PSG
 ;;INTEGRATED BILLING^IB^PRQ
 ;;INTEGRATED PATIENT FUNDS^PRPF
 ;;INTERIM MANAGEMENT SUPPORT^ECT
 ;;KERNEL^XU^USC^XLF^XPD^XNOA^XQ^ZI^ZOSF^ZOSV^ZT
 ;;LAB SERVICE^LR^LS
 ;;LEXICON UTILITY^LEX^GMPT
 ;;LIBRARY^LBR^LBRS
 ;;LIST MANAGER^VALM
 ;;MAILMAN^XM
 ;;MCCR NATIONAL DATABASE - FIELD^PRQS
 ;;MEDICINE^MC
 ;;MENTAL HEALTH^YS^RUCL
 ;;MINIMAL PATIENT DATASET^VAM
 ;;Missing Patient Register^MPR
 ;;NATIONAL DRUG FILE^PSN
 ;;NATIONAL LABORATORY TEST^NLT
 ;;NETWORK HEALTH EXCHANGE^AFJX
 ;;NURSING SERVICE^NUR
 ;;OCCURRENCE SCREEN^QAO
 ;;ONCOLOGY^ONC
 ;;ORDER ENTRY/RESULTS REPORTING^OR^OCX
 ;;OUTPATIENT PHARMACY^PSO^APSP
 ;;PAID^PRS
 ;;PATIENT DATA EXCHANGE^VAQ
 ;;PATIENT FEEDBACK^A4A8^QAF
 ;;PATIENT REPRESENTATIVE^QAC
 ;;PCE PATIENT CARE ENCOUNTER^PX^EFDP^VSIT
 ;;PHARMACY BENEFITS MANAGEMENT^PSU
 ;;PHARMACY DATA MANAGEMENT^PSS
 ;;PHARMACY PRESCRIPTION PRACTICE^PPP
 ;;POLICE & SECURITY^ES
 ;;PROBLEM LIST^GMPL
 ;;PROGRESS NOTES^GMRP^GMRQ^GMRR
 ;;PROSTHETICS^RMPR^RMPO^RMPS
 ;;QUALITY ASSURANCE INTEGRATION^QAQ
 ;;QUALITY IMPROVEMENT CHECKLIST^QIP
 ;;QUASAR^ACKQ
 ;;RADIOLOGY/NUCLEAR MEDICINE^RA
 ;;RECORD TRACKING^RT
 ;;REGISTRATION^DG^DGJ^DGQE^DPT^VA^VIC
 ;;REMOTE ORDER/ENTRY SYSTEM^RMPF^RMPJ
 ;;RPC BROKER^XWB
 ;;SAGG PROJECT^KMPS^A1B5
 ;;SCHEDULING^SD^SC
 ;;SOCIAL WORK^SOW^SWBH^SWFG
 ;;SPINAL CORD DYSFUNCTION^SPN
 ;;SURGERY^SR
 ;;SURVEY GENERATOR^QAP
 ;;TEXT INTEGRATION UTILITIES^TIU
 ;;TOOLKIT^XT^XD^XIN^XPAR^XQAB^XUC^XUR^ZIN^ZTED
 ;;UTILIZATION MANAGEMENT ROLLUP^IBQ
 ;;VA FILEMAN^DI^DD^DM
 ;;VISUAL IMPAIRMENT SERVICE TEAM^ANRV
 ;;VOLUNTARY TIMEKEEPING^ABSV^ABS2
 ;;WOMEN'S HEALTH^WV
