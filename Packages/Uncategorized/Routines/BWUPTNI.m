BWUPTNI ;IHS/ANMC/MWR - UPLOAD: TABLE FROM CORNING;15-Feb-2003 22:13;PLS
 ;;2.0;WOMEN'S HEALTH;**8**;MAY 16, 1996
 ;;* MICHAEL REMILLARD, DDS * ALASKA NATIVE MEDICAL CENTER *
 ;;  CONVERT AND STORE NICHOL'S ABBREVIATIONS TABLE.
 ;;  CALLED BY BWUPLD.
 ;
 ;
 ;---> CONVERSION AND TRANSFER OF RESULTS TABLE FOR * CORNING LAB *.
 ;---> THIS ROUTINE IS CALLED BY ^BWUPLD. "CVT" CONVERTS CORNING LAB
 ;---> FIXED LENGTH ASCII TEXT LAB RESULTS ABBREVIATIONS AND TEXT
 ;---> INTO "^" DELIMITED VARIABLE LENGTH RECORDS.
 ;---> EACH CORNING RECORD CONTAINS AN ABBREVIATION, TEXT,
 ;---> AND A SEQUENCE NUMBER.  MULTIPLE RECORDS RECORDS (OR LINES)
 ;---> FORMING ONE RESULT TEXT AND REPRESENTED BY THE SAME ABBREVIATION
 ;---> ARE GROUPED BY VIRTUE OF HAVING THE SAME ABBREVIATION, AND ARE
 ;---> ORDERED BY THEIR SEQUENCE NUMBERS.
 ;--->
 ;---> "TRANS" FORMATS AND TRANSFERS ENTRIES IN "BW UPLD TABLE TEMP
 ;---> (CORNING)" FILE INTO THE "BW UPLD TABLE PERM (ALL)" FILE.
 ;---> AS THE ENTRIES ARE TRANSFERED, RECORDS OF TEXT BELONGING TO
 ;---> THE SAME ABBREVIATION ARE GROUPED AND STORED IN A WP FIELD
 ;---> UNDER A SINGLE ENTRY FOR THAT ABBREVIATION (.01 FIELD).
 ;
CVT(LINE,PIECE) ;EP
 ;---> COVERT FIXED LENGTH INTO "^" DELIMITED VARIABLE LENGTH.
 ;---> PIECE=0 SAYS DO NOT PIECE BWLINE WHEN RETURNED TO BWUPLD.
 I '$D(LINE) S LINE="" Q
 N Y,Z
 ;---> TRANSLATE ALL "^" INTO "`".
 S Y=$TR(LINE,"^","`")
 ;---> EXTRACT THE ABBREVIATION.
 S Z=$$TRIM($E(Y,1,6))
 ;---> EXTRACT THE SEQUENCE NUMBER.
 S Z=Z_U_$$TRIM2($E(Y,9,12))
 ;---> EXTRACT AND CONCATENATE THE TEXT.
 S LINE=Z_U_$$TRIM($E(Y,13,66)),PIECE=0
 S:$P(LINE,U,3)=""&($P(LINE,U)'="YYES") LINE=""
 Q
 ;
TRIM(X) ;EP
 ;---> TRIM OFF ANY TRAILING SPACES.
 Q:'$D(X) ""
 N L S L=$L(X)
 F  Q:$E(X,L)'=" "  S L=L-1
 Q $E(X,1,L)
 ;
TRIM2(X) ;EP
 ;---> TRIM OFF ANY LEADING SPACES.
 Q:'$D(X) ""
 N I,L S L=$L(X)
 F I=1:1 Q:$E(X,I)'=" "
 Q $E(X,I,L)
 ;
 ;
TRANS ;EP
 ;---> * FOR CORNING LAB *
 ;---> GROUP AND COPY ENTRIES FROM "BW UPLD TABLE TEMP (CORNING)"
 ;---> FILE INTO THE "BW UPLD TABLE FINAL (CORNING)" FILE.
 D SETVARS^BWUTL5
 ;---> ZERO OUT PREVIOUS DATA IN TABLE FINAL FILE.
 D ZGBL^BWUTL8("^BWTFNI")
 W !?5,"Transferring to ""BW UPLD TABLE FINAL (CORNING)"" FILE..." H 1
 S BWABBV=0
 F  S BWABBV=$O(^BWTNI("B",BWABBV)) Q:BWABBV=""  D
 .;---> DIC LOOKUP/ADD OF NEW ABBREVIATION IN FINAL FILE.
 .D DIC^BWFMAN(9002086.85,"QML",.Y,"","","",BWABBV)
 .I Y<0 W ?10,"FAILED TO ADD/EDIT ",BWABBV,"!" Q
 .;---> NOW COPY TEMP TABLE ENTRIES INTO NEW FINAL TABLE ENTRY.
 .S (M,N)=0,Y=+Y
 .F  S N=$O(^BWTNI("B",BWABBV,N)) Q:'N  D
 ..S M=M+1,^BWTFNI(Y,1,M,0)=$P(^BWTNI(N,0),U,3)
 .S ^BWTFNI(Y,1,0)="^^"_M_U_M_U_DT
 Q
