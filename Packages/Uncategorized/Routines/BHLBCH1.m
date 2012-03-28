BHLBCH1 ; IHS/TUCSON/DCP -HL7 ORU Message Processor (continued) ; 
 ;;1.0;IHS SUPPORT FOR HL7 INTERFACES;;JUL 7, 1997
 ;
 ; This routine is a continuation of BHLBCH.
 ; It is not independently callable.
 ;
START ; ENTRY POINT from BHLBCH
 ;
 D INIT
 I BHLQUIT D EOJ Q
 D CREATE ;create record with FILE^DICN
 I BHLQUIT D EOJ Q
 D EDIT
 I BHLQUIT D EOJ Q
 D @(BHLTYPE)
 I BHLQUIT D EOJ Q
 D PCCLINK
 D EOJ
 Q
 ;
EOJ ; ENTRY POINT from BHLBCH - KILL VARS AND EXIT
 ;
 I BHLQUIT,BHLR D
 .;delete povs
 .S BHLX=0 F  S BHLX=$O(BHLTPOV(BHLX)) Q:BHLX'=+BHLX  S DA=BHLX,DIK="^BCHRPROB(" D ^DIK
 .S DA=BHLR,DIK="^BCHR(" D ^DIK K DA,DIK
 K BHLBCH,BHLDATA,BHLDUZ2,BHLE,BHLFDA,BHLFIELD,BHLFILE,BHLI,BHLID,BHLITEM,BHLJ,BHLMTYP,BHLN,BHLPIECE,BHLPOV,BHLQUIT,BHLR,BHLRES,BHLSEG,BHLSRV,BHLT,BHLTIEN,BHLTPOV,BHLTYPE,BHLVALUE,BHLX
 K C,D0,DA,DD,DI,DIADD,DIC,DIE,DIG,DIH,DIK,DIQUIET,DIU,DIV,DIW,DIX,DIY,DK,DL,DLAYGO,DO,DQ,DR,F,I,X,U
 K C,IEN,SEX,DOB,F,X,Y
 D KILL^AUPNPAT
 K X,Y,I
 Q
INIT ;
 K HLERR,APCDALVR,IEN
 ;check to be sure that all required pieces of data are present
 ;if not, set error and quit
 S (BHLR,BHLQUIT)=0
 S X=$G(BHLBCH("TRANS")) I X="" S HLERR="TRANSACTION INFORMATION MISSING",BHLQUIT=1 Q
 S BHLTYPE=$P(BHLBCH("TRANS"),U) I BHLTYPE="" S HLERR="TRANSACTION TYPE MISSING",BHLQUIT=1 Q
 S BHLID=$P(BHLBCH("TRANS"),U,2) I BHLID="" S HLERR="TRANSACTION UNIQUE ID MISSING",BHLQUIT=1 Q
 S BHLID=$P(BHLBCH("TRANS"),U,3)_BHLID
 D CHK^DIE(90002,.21,"E",BHLID,.BHLRES) I BHLRES="^" S HLERR="UNIQUE ID FAILED INPUT TRANSFORM",BHLQUIT=1 Q
 I $G(BHLBCH("REC"))="" S HLERR="NO RECORD INFORMATION" S BHLQUIT=1 Q  ;must have a minimum of the record node to continue
 I '$O(BHLBCH("POV",0)) S BHLQUIT=1 S HLERR="NO POV PASSED" Q  ;must have at least 1 pov to continue
 ;date,program,chr,prob code,svc code,svc min,act loc are all required to continue
 F X=1:1:4 S Y=$P(BHLBCH("REC"),U,X) I Y="" S HLERR=$P("DATE^PROGRAM^CHR^ACTLOC",U,X)_" REQUIRED ELEMENT MISSING" S BHLQUIT=1 Q
 ;chk pov
 S X=$O(BHLBCH("POV",0)) I 'X S HLERR="POV MISSING",BHLQUIT=1 Q
 S Y=BHLBCH("POV",X) F I=1:1:3 I $P(Y,U,I)="" S HLERR=$P("HLTH PROB CODE^SVC CODE^SVC MINS",U,I)_" REQUIRED ELEMENT MISSING",BHLQUIT=1 Q
 Q
PCCLINK ;
 S BCHEV("TYPE")="A" ;add,edit or delete
 S BCHR=BHLR
 D PROTOCOL^BCHUADD1
 K BCHEV,BCHR
 Q
A ;
 D A^BHLBCH2
 Q
M ;edit - delete original and do add
 D E^BHLBCH2
 Q
FMKILL ;
 K DIE,DIC,DA,DR,DLAYGO,DIADD,DIU,DIY,DIX,DIV,DIW,DD,D0,DO,DI,DK,DIG,DIH,DL,DQ
 Q
EDIT ;edit all passed data, check against input tx
 ;edit record info against input transform
 S BHLT="REC" D CHECK
 Q:BHLQUIT
 S BHLT="POV" S BHLI=0 F  S BHLI=$O(BHLBCH("POV",BHLI)) Q:BHLI=""!(BHLQUIT)  D CHECK
 Q:BHLQUIT
 I $P(BHLBCH("REC"),U,4)="HC",$P(BHLBCH("REC"),U,12)="" S HLERR="IF ACT LOCATION IS HOSP MUST BE CLINIC NAME",BHLQUIT=1 Q
DEM ;
 I $D(BHLBCH("DEMO")) D
 .F I=3:1:7 S X=$P(BHLBCH("DEMO"),U,I) I X["--" S $P(BHLBCH("DEMO"),U,I)=""
 .S BHLT="DEMO" D CHECK
 Q:BHLQUIT
ETESTS ;edit tests and measurements
 S BHLFILE=90002
 I $D(BHLBCH("MSR")) S BHLN=0 F  S BHLN=$O(BHLBCH("MSR",BHLN)) Q:BHLN'=+BHLN!(BHLQUIT)  S BHLMTYP=$P(BHLBCH("MSR",BHLN),U),BHLVALUE=$P(BHLBCH("MSR",BHLN),U,2) D
 .Q:BHLVALUE=""
 .I BHLMTYP="VU"!(BHLMTYP="VC") D
 ..S X=$P(BHLBCH("MSR",BHLN),U,2)
 ..S BHLVALUE=$P($P(BHLVALUE,"~"),"/",2)_"/"_$P($P(BHLVALUE,"~",2),"/",2),$P(BHLBCH("MSR",BHLN),U,2)=BHLVALUE
 .S BHLTIEN=$O(^BCHTMT("B",BHLMTYP,0)) I BHLTIEN="" S BHLQUIT=1,HLERR="MEASUREMENT TYPE NOT FOUND IN TABLE" Q
 .S BHLFIELD=$P(^BCHTMT(BHLTIEN,0),U,3) I BHLFIELD="" Q
 .K Y,BHLRES S DIQUIET=1 D CHK^DIE(BHLFILE,BHLFIELD,"E",BHLVALUE,.BHLRES)
 .I BHLRES="^" S BHLQUIT=1,HLERR=BHLMTYP_" FAILED INPUT TRANSFORM EDIT" Q
 .S BHLFDA(BHLFILE,BHLR_",",BHLFIELD)=BHLRES
 .Q
 Q
CHECK ;
 S BHLFILE=$P($T(@BHLT),";;",2) F BHLJ=1:1 S BHLX=$T(@BHLT+BHLJ),BHLPIECE=$P(BHLX,";;",2) Q:BHLPIECE="QUIT"!(BHLPIECE="")!(BHLQUIT)  D
 .K BHLRES S BHLITEM=$P(BHLX,";;",3),BHLFIELD=$P(BHLX,";;",4),BHLE=$P(BHLX,";;",5)
 .S:BHLT="POV" X=BHLBCH(BHLT,BHLI) S:BHLT'="POV" X=BHLBCH(BHLT) S X=$P(X,U,BHLPIECE)
 .Q:X=""
 .I BHLE]"" D  Q
 ..X BHLE I '$D(X) S HLERR=BHLITEM_" FAILED INPUT TX EDIT",BHLQUIT=1 Q
 ..I BHLFILE=90002 S BHLFDA(BHLFILE,BHLR_",",BHLFIELD)=X
 .K Y,BHLRES S DIQUIET=1 D CHK^DIE(BHLFILE,BHLFIELD,"E",X,.BHLRES)
 .I BHLRES="^" S BHLQUIT=1,HLERR=BHLITEM_" FAILED INPUT TRANSFORM EDIT" Q
 .I BHLFILE=90002 S BHLFDA(BHLFILE,BHLR_",",BHLFIELD)=BHLRES
 .Q
 Q
 ;
CREATE ;create record in CHR RECORD using FILE^DICN
 S BHLR=$O(^BCHR("CUI",BHLID,0)) I BHLR S BHLTYPE="M" Q
 D FMKILL^BHLBCH2
 S DIC="^BCHR(",DIC(0)="L",X=$P($P(BHLBCH("REC"),U),"@"),%DT="T" D ^%DT S X=Y,DLAYGO=90002,DIC("DR")=".16////"_DUZ_";.17////"_DT_";.22////"_DT_";.26////R" K DD,DO D FILE^DICN
 I Y=-1 S HLERR="CREATING CHR RECORD ENTRY FAILED",BHLQUIT=1 Q
 S BHLR=+Y
 Q
REC ;;90002
 ;;1;;DATE;;.01
 ;;2;;PROGRAM;;.02;;
 ;;3;;CHR;;.03;;
 ;;4;;ACT LOC;;.06;;
 ;;5;;REFERRED TO;;.07;;
 ;;6;;REFERRED BY;;.08;;
 ;;7;;EVALUATION;;.09;;
 ;;8;;TRAVEL TIME;;.11;;
 ;;9;;# SERVED;;.12;;
 ;;10;;INSURER;;2102;;
 ;;11;;PURP REFERRAL;;2101;;
 ;;12;;LOC OF ENCOUNTER;;.05;;
 ;;QUIT
POV ;;90002.01
 ;;1;;HLTH PROB CODE;;.01;;S Y=$O(^BCHTPROB("C",X,0)) K:'Y X I Y S X="`"_Y
 ;;2;;SVC CODE;;.04;;S Y=$O(^BCHTSERV("D",X,0)) K:'Y X I Y S X="`"_X
 ;;3;;SVC MINS;;.05;;
 ;;4;;NARRATIVE;;.06;;X $P(^DD(9999999.27,.01,0),U,5,99)
 ;;5;;SUBSTANCE RELATED;;.07;;
 ;;QUIT
DEMO ;;90002
 ;;1;;PATIENT NAME;;1101;;
 ;;2;;DATE OF BIRTH;;1102;;
 ;;3;;SEX;;1103;;
 ;;4;;SSN;;1104;;
 ;;5;;TRIBE;;1105;;
 ;;6;;COMMUNITY OF RESIDENCE;;1106;;S Y=$O(^AUTTCOM("C",X,0)) K:'Y X I Y S X=Y
 ;;7;;CHART NUMBER;;1111;;
 ;;8;;CHART FACILITY;;1109;;
 ;;9;;TEMP RESIDENCE;;1108;;
 ;;QUIT
