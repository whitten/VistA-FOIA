VENPCCKX ; IHS/OIT/GIS - KNOWLEDGE BASE POPULATE THE HOLDING FILE FOR A KB CATEGORY ;
 ;;2.6;PCC+;**1,3**;MAR 23, 2011
 ;
 ;
 ;
HFPOP(KBCIEN,VIEN) ; EP - GIVEN A KNOWLEDGEBASE CATEGORY IEN, POPULATE THE HOLDING FILE
 I '$D(^VEN(7.11,+$G(KBCIEN))) Q  ; MUST BE A VALID KB CATEGORY
 I '$D(^AUPNVSIT(+$G(VIEN),0)) Q  ; MUST BE ASSOCIATED WITH A VALID VISIT
 N TID,IIEN,CODE1,CODE2,NAME,LASTDT,LASTRES,MAND,ORD,STATUS,FCODE,X,Y,Z,%,DIC,DIK,DA,DFN
 S DFN=$P(^AUPNVSIT(VIEN,0),U,5) I '$D(^DPT(+$G(DFN))) Q  ; MUST BE A VALID PATIENT
 S TID=KBCIEN_"_"_VIEN ; TRANSACTION ID FOR HOLDING FILE
 S DA=0,DIK="^VEN(7.64," F  S DA=$O(^VEN(7.64,"AC",TID,0)) Q:'DA  D ^DIK ; INITIAL CLEANUP OF HOLDING FILES
 D PASS1(KBCIEN,DFN,.KB) I '$D(KB) Q  ; BUILD THE RAW DATA ARRAY
 D PASS2(TID,.KB) ; GET LAST VALUES AND FILE ITEMS IN KB ITEM TRANSACTION FILE
 Q
 ;
PASS1(KBCIEN,DFN,KB) ; EP - GET KB ARRAY
 N K,CAT,HDT,CNT,AGE,AGEFLAG,GESTFLAG,KIEN,LAGFLAG,MOD,SEXFLAG,START,STG,STOP,TITLE,TMP,TOT,HDR
 S CNT=0,TMP="KB"
 D GETKBI^VENPCCK(KBCIEN)
 Q
 ;
PASS2(TID,KB) ; EP - STORE THE KB ITEMS IN THE TRANSACTION FILE AND KB ITEM MODIFIER TRANSACTION FILE
 N DIC,DIE,DA,X,Y,CAT,%,DR,KBCIEN,VIEN,DFN,FLD,IIEN,STG,TXT,INT,EXT,FILE,FIELD
 N LASTDT,LASTRES,MAXIDT,TS,PATIENT,CNT,TITLE,MOD
 N AUPNDAYS,AUPNDOB,AUPNDOD,AUPNPAT,AUPNSEX
 S KBCIEN=+TID,VIEN=$P(TID,"_",2),DFN=$P($G(^AUPNVSIT(VIEN,0)),U,5)
 S TS=$$HTE^XLFDT($H),CNT=0
 S PATIENT=$P($G(^DPT(DFN,0)),U) I '$L(PATIENT) Q
 S CAT=$P($G(^VEN(7.11,KBCIEN,0)),U,1) I '$L(CAT) Q
 S MAXIDT=9999999-(DT-20000) ; GO BACK UP TO 2 YEARS
 S DR=".02///^S X=KBCIEN;.03///^S X=TID;.04///^S X=TS;.05///^S X=IIEN;"
 S DR=DR_".06///^S X=PATIENT;.07///^S X=CAT;.08////^S X=VIEN;"
 S DR=DR_"1.01///^S X=TITLE;1.02///^S X=INT;1.03///^S X=EXT;"
 S DR=DR_"2.01////0;2.02///^S X=CNT;2.03////0"
 S FLD="" F  S FLD=$O(KB(FLD)) Q:FLD=""  D
 . S IIEN=$G(KB(FLD,"IX")) I 'IIEN Q
 . S STG=$G(^VEN(7.12,IIEN,0)) I '$L(STG) Q
 . S INT=$P(STG,U,3),EXT=$P(STG,U,4)  ; CODES
 . S CNT=CNT+1
 . S (TITLE,TXT)=KB(FLD)
 . I $E(TXT,1,3)="__ " S (TITLE,TXT)=$E(TXT,4,999)
 . S KB(FLD)=TXT ; ITEM TITLE
GETLAST . ; GET LAST DATE/RESULT IF POSSIBLE
 . D LAST(IIEN,KBCIEN,TXT,DFN,MAXIDT,.LASTDT,.LASTRES)
 . I $G(LASTDT) D  ; APPEND LAST DATE & RESULT TO THE ITEM
 .. S %=$$FMTE^XLFDT(LASTDT,"2D")
 .. I $L($G(LASTRES)) S %=%_"  "_LASTRES
 .. S TITLE=TITLE_" ("_%_")"
 .. Q
KBFILE . ; FILE RESULTS IN TRANSACTION FILE: VEN EHP OBJ KB
 . S DIC="^VEN(7.64,",DIC(0)="L",DIE=DIC,DLAYGO=19707.64
 . S X="""`"_DFN_""""
 . D ^DIC I Y=-1 Q
 . S DA=+Y
 . L +^VEN(7.64,DA):1 I  D ^DIE L -^VEN(7.64,DA)
 . Q
 K KB
 D ^XBFMK ; CLEANUP
 Q
 ;
TXSTUB(OUT,IN) ; EP - RPC (VEN GUI TX) ; IN=SCHEMA NAME_VISIT IEN_USER IEN ; OUT = TX FILE IEN
 ; CREATE THE TRANSACTION FILE STUB
 N TFILE,DIC,DIE,DA,DR,X,Y,DFN,PATIENT,TS,VIEN,USER,UIEN,TXID,SIEN,SCHEMA
 S OUT=""
 I $P($G(IN),"_")="VEN WELL CHILD PT ED" S TFILE=9000010.16 G STUB ; GENERIC PATIENT ED MODIFIERS
 I $P($G(IN),"_")="VEN WELL CHILD ASQ" S TFILE=9000010.16 G STUB ; ASQ SCORES
 ; 
STUB I '$D(^DD(+$G(TFILE),.01,0)) Q  ; A VALID TARGET FILE MUST BE DEFINED
 S VIEN=+$P($G(IN),"_",2) I '$D(^AUPNVSIT(VIEN,0)) Q
 S DFN=$P($G(^AUPNVSIT(+$G(VIEN),0)),U,5) I 'DFN Q
 S PATIENT=$P($G(^DPT(DFN,0)),U) I '$L(PATIENT) Q
 S UIEN=$P(IN,"_",3) I 'UIEN Q
 S USER=$P($G(^VA(200,UIEN,0)),U) I '$L(USER) Q
 S TS=$$HTE^XLFDT($H,2)
 S SCHEMA=$P(IN,"_") I '$L(SCHEMA) Q
 S SIEN=$O(^BMXADO("B",SCHEMA,0)) I 'SIEN Q
 S TXID=SIEN_"_"_VIEN
 S DIC="^VEN(7.65,",DIC(0)="L",DLAYGO=19707.65
 S X=TXID ; TRANSACTION ID
 D ^DIC I Y=-1 Q
 S DA=+Y,DIE=DIC
 S DR=".02////^S X=TFILE;.03////^S X=VIEN;.04///^S X=TS;.05///^S X=PATIENT;.06////^S X=DFN;"
 S DR=DR_".07////^S X=USER;.08///^S X=UIEN;.09///^S X=SCHEMA;.1////^S X=SIEN"
 I TFILE=9000010.16 S DR=DR_";1.01////^S X=UIEN;1.02////^S X=USER"
 L +^VEN(7.65,DA):1 I  D ^DIE L -^VEN(7.65,DA)
 S OUT=TXID
 D ^XBFMK,KILL^AUPNPAT
 Q
 ; 
GEN(OUT,TXID) ; EP - FILE INFO FOR THIS TRANSACTION
 N TFIEN,TXIEN,VIEN
 S OUT=""
 S VIEN=+$P($G(TXID),"_",2) I '$D(^AUPNVSIT(VIEN,0)) Q  ; VISIT IEN
 S TXIEN=$O(^VEN(7.65,"B",TXID,0)) I 'TXIEN Q  ; TRANSACTION IEN
 S TFIEN=$P($G(^VEN(7.65,TXIEN,0)),U,2) I 'TFIEN Q  ; TARGET FILE
 I TFIEN=9000010.16 D GPTED Q  ; PT ED
 Q
 ; 
GPTED ; EP - FILE THE GENERIC PT ED INFO
 ; ONLY TO BE RUN AFTER ALL PT ED CATEGORIES AND TOPICS HAVE BEEN ENTERED
 N PRV,PRVIEN,IG,LOU,TT,AT,CNT,PEIEN,DIE,DA,DR
 S %=$G(^VEN(7.65,TXIEN,1)) I '$L(%) Q  ; MUST HAVE DATA TO FILE, OR QUIT NOW
 S PRVIEN=+%,TT=$P(%,U,3),IG=$P(%,U,4),LOU=$P(%,U,5)
 S CNT=0,PEIEN=0
 F  S PEIEN=$O(^AUPNVPED("AD",VIEN,PEIEN)) Q:'PEIEN  S CNT=CNT+1
 I 'CNT Q
 S AT=TT\CNT
 S DIE="^AUPNVPED(",DR=".05////^S X=PRVIEN;.06///^S X=LOU;.07///^S X=IG;.08///^S X=AT"
 S DA=0 F  S DA=$O(^AUPNVPED("AD",VIEN,DA)) Q:'DA  L ^AUPNVPED(DA):1 I  D ^DIE L -^AUPNVPED(DA)
 S DA=$O(^AUPNVWC("AD",VIEN,0)) I 'DA Q
 S DIE="^AUPNVWC(",DR=".05///^S X=TT;.06///^S X=LOU;.04////^S X=PRVIEN"
 L ^AUPNVWC(DA):1 I  D ^DIE L -^AUPNVWC(DA)
 S OUT=DA
 S DA=TXIEN,DIK="^VEN(7.65," D ^DIK,^XBFMK,KILL^AUPNPAT ; CLEANUP
 Q
 ; 
LAST(IIEN,CIEN,TXT,DFN,MAXIDT,LASTDT,LASTRES) ; EP - GET LAST VALUE
 N %,FILE,FIELD,RFILE,FREF,AAREF,FCREF,AACREF,SUBFILE,IDT,IEN,REFIEN
 N SFREF,SFCREF,SFIEN,TREF,TCREF,INTITEM,INTIEN,ITEM,SS,PCE,RFIELD,RESSS,RESPC
 S (LASTDT,LASTRES)="" ; INITIALIZE THE OUTPUT VALUES
 S %=$G(^VEN(7.11,CIEN,4)) I '$L(%) Q  ; GET FILING PARAMETERS
 S FILE=$P(%,U),FIELD=$P(%,U,2),RFILE=$P(%,U,3),RFIELD=$P(%,U,4)
 I FILE,FIELD
 E  Q
 S FREF=$$ROOT^DILFD(FILE,"1,") I '$L(FREF) Q
 S FCREF=$$CREF^DILF(FREF)
 S AAREF=FREF_"""AA"","_DFN_","
SUB S SUBFILE=$P($G(^DD(FILE,FIELD,0)),U,2)
 I SUBFILE D SUBFILE Q  ; ITEM IS STORED IN A SUBFILE
 S RFIEN=$P($G(^VEN(7.12,IIEN,2)),U) ; ITEM'S IEN IN THE REFERENCE FILE
 I RFIEN,RFILE,RFIELD S TXT=RFIEN,AAREF=AAREF_TXT_"," ; EXTEND "AA" INDEX AND CHANGE TEXT TO POINTER VALUE
TOPFILE ; DATA IS STORED AT THE TOP LEVEL OF A FILE (E.G., V MEASUREMENT)
 S AACREF=$$CREF^DILF(AAREF)
 S %=$P($G(^DD(FILE,FIELD,0)),U,4) I '$L(%) Q  ; INVALID FILE DEFINITION
 S SS=$P(%,";"),PCE=$P(%,";",2),IDT=0
 I $G(GUIFLAG) S IDT=9999999-DT ; FOR GUI, EXCLUDE TODAY'S RESULTS: THEY ARE "CURRENT" - NOT "LAST"
 F  S IDT=$O(@AACREF@(IDT)) Q:'IDT  Q:IDT>MAXIDT  D  I $G(LASTDT) Q  ; FIND V-FILE ENTRIES IN THE DATE RANGE
 . S IEN=0
 . F  S IEN=$O(@AACREF@(IDT,IEN)) Q:'IEN  D  I $G(LASTDT) Q  ; FIND EVERY V-FILE ENTRY ON THAT DATE
 .. S ITEM=$P($G(@FCREF@(IEN,SS)),U,PCE)
 .. I ITEM'=TXT Q  ; V-FILE VALUE MUST MATCH KB ITEM
 .. S LASTDT=9999999-IDT ; DATE: LAST TIME ITEM WAS ENTERED IN V-FILE
 .. I RFIELD S LASTRES=$$GET1^DIQ(FILE,(IEN_","),RFIELD)
 .. Q
 . Q
 Q
 ; 
SUBFILE  ; EP - DATA IS STORED IN THE FIRST NODE OF A SUBFILE (E.G., PATIENT ED)
 I $O(^DD(FILE,"SB",SUBFILE,0))'=FIELD Q  ; INVALID SUBFILE
 S IDT=0
 S AACREF=$$CREF^DILF(AAREF)
 F  S IDT=$O(@AACREF@(IDT)) Q:'IDT  Q:IDT>MAXIDT  D  I $G(LASTDT) Q  ; FIND PT'S V-FILE DATES IN TIMEFRAME
 . S IEN=0
 . F  S IEN=$O(@AACREF@(IDT,IEN)) Q:'IEN  D  I $G(LASTDT) Q  ; FIND ALL THE PT'S V-FILE ENTRIES ON EA. DATE
 .. S SFREF=$$ROOT^DILFD(SUBFILE,"1,"_IEN_",")
 .. S SFCREF=$$CREF^DILF(SFREF)
 .. S SFIEN=0
 .. F  S SFIEN=$O(@SFCREF@(SFIEN)) Q:'SFIEN  D  I $G(LASTDT) Q  ; LOOP THROUGH THE SUBFILE OT CHECK ITEMS
 ... S ITEM=$P($G(@SFCREF@(SFIEN,0)),U)
 ... I TXT,ITEM'=TXT Q  ; V-FILE VALUE MUST MATCH KB ITEM
 ... I 'TXT,$$UP^XLFSTR(ITEM)'=$$UP^XLFSTR(TXT) Q  ; V-FILE VALUE MUST MATCH KB ITEM
 ... S LASTDT=9999999-IDT ; DATE: LAST TIME ITEM WAS ENTERED IN V-FILE
 ... I $G(RFIELD) S LASTRES=$$GET1^DIQ(SUBFILE,(SFIEN_","_IEN_","),RFIELD)
 ... Q
 .. Q
 . Q
 Q
 ;
VFILE(OUT,TID) ; EP - RPC: VEN PCC+ FILE KB ITEMS
 ; FILE A SET OF TRANSACTION FILE ENTRIES IN V-FILES AND RETURN A CONFIRMATION MSG IN "OUT"
 S OUT=""
 N KBIEN,VIEN,DFN,VFILE,VFIELD,REFILE,REFIELD,VFILE2,VFIELD2,SUB,CODE,CIEN,TIEN,%
 I '$L($G(TID)) Q
 I '$D(^VEN(7.64,"AC",TID)) G VX  ; MUST HAVE AT LEAST ONE ENTRY IN THE TRANSACTION FILE
 S KBIEN=+TID I '$D(^VEN(7.11,+$G(KBCIEN))) G VX  ; MUST BE A VALID KB CATEGORY
 S VIEN=+$P(TID,"_",2) I '$D(^AUPNVSIT(VIEN,0)) G VX  ; MUST BE A VALID VISIT
 S DFN=+$P(^AUPNVSIT(VIEN,0),U,5) I '$D(^DPT(DFN,0)) G VX  ; MUST BE A VALID PATIENT
 S %=$G(^VEN(7.11,KBIEN,4)) I '$L(%) G VX
 S VFILE=+% I 'VFILE G VX
 S VFIELD=$P(%,U,2) I 'VFIELD G VX
 S REFILE=$P(%,U,3),REFIELD=$P(%,U,4)
 S %=$P($G(^DD(VFILE,VFIELD,0)),U,2)
 I %,$D(^DD(VFILE,"SB",%,VFIELD)) D VSUB G SEC ; RESULTS ARE STORED IN A SUBFILE
SEC ; POPULATE SECONDARY FILE, IF NECESSARY
 S VFILE2=$P($G(^VEN(7.11,KBIEN,5)),U,1) I 'VFILE2 G VX
 I VFILE2 S VFIELD2=$P(^VEN(7.11,KBIEN,5),U,2) I 'VFIELD G VX
 D V2(VIEN,VFILE2,VFIELD2)
VX ; CLEANUP TRANSACTION ENTRIES FOR THIS TID
 S DIK="^VEN(7.64,",DA=0
 F  S DA=$O(^VEN(7.64,"AC",TID,DA)) Q:'DA  D ^DIK
 D ^XBFMK
 Q
 ; 
VSUB ; EP - MANAGE RESULTS STORED IN A VFILE SUBFILE
 I VFILE=9000010.16 D PTED Q  ; PT ED IS STORED IN A SUBFILE
 Q
 ; 
V2(VIEN,VFILE2,VFIELD2) ; EP - SECONDARY V FILE ENTRY
 ; CURRENTLY ONLY WELL CHILD PT ED TOPIC ARE STORED IN 2 PLACES.
 I VFILE2=9000010.46 D VWB(VIEN,VFIELD2)
 Q
 ; 
VWB(VIEN,FLD) ; EP - POPULATE V WELL CHILD FILE
 N WIEN,DIC,IEN,DA,X,Y,%
 I '$D(^AUPNVSIT(+$G(VIEN),0)) Q
 I '$D(^DD(9000010.46,+$G(FLD),0)) Q
 S WIEN=$O(^AUPNVWC("AD",VIEN,0))
 I 'WIEN D  I '$G(WIEN) Q  ; NEED TO MAKE THE V WELL CHILD STUB BEFORE APPENDING DATA
 . S DIC="^AUPNVWC(",DIC(0)="L",DLAYGO=9000010.46,X=""""_0_""""
 . D ^DIC I Y=-1 Q
 . S WIEN=+Y
 . S DIE="^AUPNVWC(",DA=WIEN,DR=".02////^S X=DFN;.03////^S X=VIEN"
 . L +^AUPNVWC(DA):1 I  D ^DIE L -^AUPNVWC(DA)
 . Q
 S DA(1)=WIEN,DIC="^AUPNVWC("_DA(1)_","_FLD_",",DIC(0)="L"
 S (DLAYGO,DIC("P"))=$P($G(^DD(9000010.46,FLD,0)),U,2)
 S IEN=0
 F  S IEN=$O(^VEN(7.64,"AC",TID,IEN)) Q:'IEN  D  ; ENTER EACH TOPIC AS FREE TEXT
 . I '$P($G(^VEN(7.64,IEN,2)),U) Q  ; ITEM MUST BE SELECTED
 . S X=$P($G(^VEN(7.64,IEN,1)),U) I '$L(X) Q
 . D ^DIC
 . Q
 D ^XBFMK
 Q
 ; 
VPE(VIEN,DFN,RFIEN) ; EP - GET PARENT IEN IN V PATEINT ED
 N IEN,DIC,DIE,DR,DA,PFIEN,X,Y,%
 S PFIEN="",IEN=0
 F  S IEN=$O(^AUPNVPED("AD",VIEN,IEN)) Q:'IEN  D  I PFIEN Q  ; DOES PARENT FILE ALREADY EXIST?
 . S %=+$G(^AUPNVPED(IEN,0)) I '% Q  ; GET EDU TOPIC
 . I %=RFIEN S PFIEN=IEN
 . Q
 I PFIEN Q PFIEN ; PCODE ALREADY EXISTS
 ; CREATE A NEW PARENT ENTRY IN V PATIENT ED
 S DIC="^AUPNVPED(",DLAYGO=900010.01,DIC(0)="L",X="""`"_RFIEN_""""
 D ^DIC I Y=-1 Q "" ; MAKE PARENT FILE ENTRY
 S DIE=DIC,(PFIEN,DA)=+Y,DR=".02////^S X=DFN;.03////^S X=VIEN"
 L ^AUPNVPED(DA):1 I  D ^DIE L -^AUPNVPED(DA)
 D ^XBFMK
 Q PFIEN
 ; 
PTED ; EP - FILE PATIENT ED RESULTS IN V PATIENT ED
 N PCODE,PFIEN,DIC,DIE,DA,DR,X,Y,%,TIEN,RFIEN,PRV,TIME,IG,LOU,TXT,IEN
 S PFIEN="",TIEN=0
 F  S TIEN=$O(^VEN(7.64,"AC",TID,TIEN)) Q:'TIEN  D  ; SUBFILE ENTRIES
 . I '$P($G(^VEN(7.64,TIEN,2)),U) Q  ; ITEM MUST BE SELECTED
 . S X=$G(^VEN(7.64,TIEN,1)) I '$L(X) Q  ; TEXT OF PT ED ITEM
 . S TXT=$P(X,U,1) I '$L(TXT) Q
 . S PCODE=$P(X,U,3) I '$L(PCODE) Q
 . S RFIEN=$O(^AUTTEDT("C",PCODE,0)) I 'RFIEN Q  ; GET EDUCATION TOPIC IEN
 . I 'PFIEN S PFIEN=$$VPE(VIEN,DFN,RFIEN) I 'PFIEN Q  ; FIND/MAKE PRIMARY ENTRY IN THE PARENT FILE
 . K DA S DA(1)=PFIEN,DIC="^AUPNVPED("_DA(1)_",1,",DIC(0)="L",DLAYGO=9000010.161,X=TXT
 . S DIC("P")=$P(^DD(9000010.16,1,0),U,2)
 . D ^DIC ; MAKE SUBFILE ENTRY
 . Q
 Q
 ; 
