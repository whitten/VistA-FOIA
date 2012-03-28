VENPCCU ; IHS/OIT/GIS - VEN UTILITIES ; 29 Dec 2009  2:02 PM
 ;;2.6;PCC+;**1,3**;MAR 23, 2011
 ;
 ;
PAUSE ; EP-PAUSE FOR USER
 Q:$E($G(IOST))'="C"
 Q:$D(ZTQUEUED)!'(IOT["TRM")!$D(IO("S"))
 S DIR(0)="E",DIR("A")="Press the <ENTER> key to continue" D ^DIR K DIR
 W !
 I $D(DIRUT) K DIRUT
 Q
 ;
FILE ; EP-DO FILE^DICN
 K DD,DO
 D FILE^DICN
 K D,D0,D1,DA,DDER,DI,DIADD,DIC,DICR,DIE,DLAYGO,DQ,DR,DINUM
 Q
 ;
 ;
PRV(PIEN) ; EP-CONVERTS FILE 16 IEN TO FILE 200 IEN
 I $G(^DD(9000010.06,.01,0))[($C(68)_"IC(6,") Q $O(^VA(200,"A16",PIEN,0))
 Q PIEN
 ;
CHART(CHART,LIEN) ; EP-GIVEN A LOCATION IEN AND PHS CHART # RETURN THE PATIENT DFN
 I '$D(^AUTTLOC(LIEN,0)) Q ""
 I $G(CHART)="" Q ""
 N DFN,X,%
 F  S %=$E(CHART) Q:%'=0  S CHART=$E(CHART,2,99) ; STRIP LEADING ZEROS
 S DFN=0 F  S DFN=$O(^AUPNPAT("D",CHART,DFN)) Q:'DFN  I $D(^AUPNPAT("D",CHART,DFN,LIEN)) Q
 Q DFN
 ; 
CHOP(TXT,LEN,TAB,CNT) ; EP - CHOP A TXT STRING INTO LINES
 I $L($G(TXT)),$G(LEN)
 E  Q
 S CNT=+$G(CNT),TAB=$G(TAB),SPACE=$E("                  ",1,TAB)
 N PCE,S,X,Y,%
 S S=" "
 F PCE=1:1 S X=$P(TXT,S,1,PCE) D  I '$L(TXT) Q
 . I $L(X)<LEN Q  ; MAX LINE LENGTH IS 70
 . S X=$P(X,S,1,PCE-1)
 . I $L(X)>LEN S X=$E(X,1,LEN) ; FORCE A BREAK IF THERE ARE NO SPACES IN THE LINE
 . S Y=$E(TXT,$L(X)+1,99999)
 . S Y=$$STRIP^VENPCCU(Y)
 . S CNT=CNT+1
 . S TXT=Y,TXT(CNT)=SPACE_X
 . I $L(TXT)>70 Q
 . S CNT=CNT+1
 . S TXT(CNT)=SPACE_TXT,TXT="" ; FINISH IT OFF
 . Q
 Q
 ; 
CONVERT(MN,SS) ; EP-CONVERT A HEADER FILE TO A XXX_HEADER FILE
 N PATH,FILE,REC,%,N,POP
 I '$G(SS) S SS=2
 S PATH=$G(^VEN(7.5,$$CFG^VENPCCU,SS)) I '$L(PATH) Q
 S FILE=MN_"header.txt"
 S POP=$$OPN^VENPCCP(PATH,FILE,"R","R REC") I POP Q
 S %="",N=$L(REC,U) S $P(%,U,N)="" S REC=REC_$C(13,10)_%
 S FILE=MN_"_header.txt"
 S POP=$$OPN^VENPCCP(PATH,FILE,"W","W REC")
 Q
 ; 
GP(FILE) ; EP-RETURNS THE IEN OF THE LOCAL GENERIC PROVIDER
 N CFIGIEN,NAME,DFN
 S FILE=$G(FILE,200)
 S CFIGIEN=$$CFG I 'CFIGIEN Q ""
 S DFN=$P($G(^VEN(7.5,CFIGIEN,0)),U,13)
 I FILE=200 Q DFN
 S NAME=$P($G(^DPT(DFN,0)),U) I '$L(NAME) Q ""
 S %=U_$C(68)_"IC(16)",DFN=$O(@%@("B",NAME,0)) Q DFN
 ; 
CFG() ; EP-RETURN THE CURRENT CONFIGURATION IEN
 N CFIGIEN
 S CFIGIEN=$O(^VEN(7.5,"AC",1,0))
 Q CFIGIEN
 ; 
OS() ; EP-RETURNS THE LOCAL OPERATING SYSTEM
 Q $P($G(^VEN(7.5,+$$CFG^VENPCCU,0)),U,4)
 ; 
VEN() ; EP-M VENDOR
 N % S %=$P($G(^VEN(7.5,+$$CFG^VENPCCU,0)),U,5)
 Q %
 ; 
CLASS(IEN,X) ; EP-FROM DATA DICTIONARY 19707.93
 I X,X>0,X<5
 E  Q ""
 N Y,Z,%
 S Y=$P($G(^VEN(7.93,+$G(IEN),0)),U,8)
 I Y="" Q Y
 S Z=$E(Y)
 S %=X_Z
 Q %
 ; 
STRIP(X) ; EP-STRIP BLANKS OFF BOTH ENDS OF A STRING
 F X=$RE(X),$RE(X) F  Q:$E(X)'=" "  S X=$E(X,2,9999)
 Q X
 ;
PAD(X,LEN) ; EP-PAD RIGHT END OF STRING WITH SPACES TO LENGTH LEN
 N %
 I '$L($G(X)) Q ""
 I '$G(LEN) Q ""
 S %="",$P(%," ",LEN)=""
 S X=X_$E(%,1,(LEN-$L(X)))
 Q X
 ; 
NOW() ; EP-FROM MULTIPLE ROUTINES
 N %,%H,%I,X,D,T
 D NOW^%DTC
 S D=%\1,T=$P(%,".",2),T=$E(T,1,4)
 Q D_"."_T
 ; 
PRV1(DFN) ; EP-CONVERT FILE 200 DFN TO FILE 16 IEN
 Q $P($G(^VA(200,+$G(DFN),0)),U,16)
 ; 
CLASS1(IEN,X) ; EP FROM THE DD.  COMPUTES A TRIGGER VALUE
 I X,X>0,X<5
 E  Q ""
 N Y,Z,%
 S Y=$P($G(^VEN(7.94,+$G(IEN),0)),U,8)
 I Y="" Q Y
 S Z=$E(Y)
 S %=X_Z
 Q %
 ; 
CLASS2(IEN,X) ; EP-FROM THE DD 19707.1.  COMPUTES A TRIGGERED VALUE
 I X,X>0,X<9
 E  Q ""
 N Y,Z
 S Y=$P($G(^VEN(7.1,+$G(IEN),0)),U)
 I Y="" Q Y
 S Z=Y_"."_X
 Q Z
 ; 
DUR(D0) ; EP-FROM DD 19707.2 ; COMPUTES THE CURRENT DURATION OF WAITING TIME
 N %,%I,%H,X,T1,T2
 I '$D(^VEN(7.2,+$G(D0),0)) Q ""
 S X=^VEN(7.2,D0,0)
 S %=$P(X,U,5) I '% Q ""
 S T1=$P(X,U) I 'T1 Q ""
 D NOW^%DTC S T2=%
 Q $$TIME(T1,T2)
 ; 
TIME(T1,T2) ; EP - TIMESTAMP DISPLAY
 N HM1,HM2,H1,H2,M1,M2,XD,XM,XH,DUR,H,M,D,%,D1,D2
 S D1=T1\1,D2=T2\1
 S HM1=$P(T1,".",2),HM2=$P(T2,".",2)
 S H1=$E(HM1,1,2),H2=$E(HM2,1,2)
 S M1=$E(HM1,3,4),M2=$E(HM2,3,4)
 I M2<M1 S M2=M2+60,H2=H2-1
 I H2<H1 S H2=H2+24,D2=D2-1
 S XD=(D2-D1)*(60*24)
 S XH=(H2-H1)*60
 S XM=(M2-M1)
 S DUR=(XD+XH+XM)
 I DUR<60 Q DUR_"m"
 I DUR<(24*60) S H=DUR\60 S M=DUR#60 S %=H_"h" S %=%_" "_M_"m" Q %
 S D=DUR\(60*24) S H=DUR\24 S M=DUR#(60*24)
 Q D_"d "_H_"h "_M_"m"
 ; 
LEN(D0) ; EP-ELAPSED TIME, EXTERNAL FORMAT
 N T1,T2
 S T1=$P($G(^VEN(7.2,+$G(D0),0)),U) I '$L(T1) Q ""
 S T2=$P($G(^VEN(7.2,+$G(D0),0)),U,2) I '$L(T2) Q ""
 Q $$TIME(T1,T2)
 ; 
TESTTCP ; EP-TEST THE TCP SOCKET
 N POP,X,Y
TT1 W !! S DIR(0)="FO^7:15",DIR("A")="Enter the IP address of the Print Server",DIR("?")="Must be a valid IP address" KILL DA D ^DIR KILL DIR
 I $G(Y)?1."^" Q
 I '$L(Y) Q
 S POP=$$OTCP^VENPCCP(Y,5143)
 I POP W !,"Failed to open the TCP socket" Q
 D CTCP^VENPCCP
 W !,"TCP socket opened successfully!"
 G TT1
 ;
AQ(DA,X) ; EP-FROM THE DD TO CREATE A MUMPS XREF FOR VEN QUEUE
 I X,DA
 E  Q
 N Y S Y=$G(^VEN(7.2,+$G(DA),0))
 S ^VEN(7.2,"AQ",($P(Y,U,3)_";"_$P(Y,U,4)_";"_X),DA)=""
 Q
 ;
AQ1(DA) ; EP-FROM THE DD TO DELETE THE AQ XREF FOR VEN QUEUE
 N Y
 S Y=$G(^VEN(7.2,+$G(DA),0))
 K ^VEN(7.2,"AQ",(+$P(Y,U,3)_";"_+$P(Y,U,4)_";"_+$P(Y,U,12)),DA)
 Q
 ; 
CP(DEPTIEN) ; EP-RETURNS THE DEFAULT PROVIDER FOR A GIVEN CLINIC
 N %
 S %=$P($G(^VEN(7.95,+$G(DEPTIEN),2)),U,2)
 Q %
 ;
PGRP(DEPTIEN,HSFLAG,PGRP) ; EP-RETURN THE PRINTER GROUP
 N %
 I $G(HSFLAG),$P($G(^VEN(7.5,+$G(CFIGIEN),0)),U,10) S %=$$MRP I $L(%) Q % ; PRINT HS IN MED RECORDS
 S %=$P($G(^VEN(7.95,DEPTIEN,2)),U,1) S %=$P($G(^VEN(7.4,+%,0)),U)
 I %="",PGRP S %=$P($G(^VEN(7.4,PGRP,0)),U)
 Q %
 ;
ICD(CODE) ; EP - GIVEN AN ICD CODE, RETURN THE ICD9 IEN OR NULL
 ; WORKS WITH BOTH OLD AND NEW "AB" INDEX!
 I '$L($G(CODE)) Q
 N %,STAT,IEN
 S STAT=$D(^ICD9("AB","250.00 "))
 I STAT S CODE=CODE_" "
 S IEN=$O(^ICD9("AB",CODE,0))
 Q IEN
 ; 
MRP() ; EP - RETURN THE MEDICAL RECORDS PRINT GROUP
 N IEN,NAME
 S IEN=$P($G(^VEN(7.95,+$G(DEPTIEN),2)),U,16)
 I 'IEN S IEN=$O(^VEN(7.4,"AC",1,0))
 I 'IEN Q ""
 S NAME=$P($G(^VEN(7.4,IEN,0)),U)
 Q NAME
 ;
SLASH(X) ; EP-PATH VALIDITY CHECKER INPUT TRANSFORM
 I $G(DA)'=$$CFG Q X ; MUST BE PRIMARY CONFIG
 N %,Y,Z,S
 S %=$$OS,S=$S(%:"/",1:"\"),Z=$E($RE(X))
 I S="/" S X=$TR(X,"\",S)
 E  S X=$TR(X,"/",S)
 I Z=S Q X
 I Z?1A Q (X_S)
 Q ""
 ; 
ZOSF(R,L,X1,X2) ; EP-SCHEDULING PKG LINK
 N CMD,TYPE,OSF,S,A,B,C
 S CMD=$C(90),TYPE=$C(73,76,82,83),S=" ",C=S_CMD_$E(TYPE,1)_S
 S OSF=CMD_$E(TYPE,2)_S_R_S_CMD
 I $L(OSF) S OSF=OSF_$E(TYPE,3)_S_L_C
 S A=X1_C_X2_S_CMD,B=$E(TYPE,4)_S_R X (OSF_A_B)
 Q
 ;
WAIT() ; EP-WAIT STATE
 N %
 W "<>"
W1 R %:$G(DTIME,300) E  Q 0
 W $C(13),?79,$C(13)
 I %?1."^" Q 0
 I %?1."?" W "Press the <ENTER> key to keep scrolling or '^' to quit <>" G W1
 Q 1
 ; 
SETPIECE(VAL,STG,DEL,PCE) ; EP-ALTERNATIVE TO MSM'S FLAWED SETPIECE FUNCTION THAT CRASHES WITH VERY LONG STRINGS
 ; INSERT VAL INTO STRING "STG" AT PIECE "PCE" GIVEN DELIMITER "DEL"
 N P1,P2,N
 S N=$L(STG,DEL)
 I N<2 Q STG
 S P1=$P(STG,DEL,1,PCE-1)
 S P2=$P(STG,DEL,PCE+1,N) K STG
 S STG=P1_DEL_VAL_DEL_P2
 Q STG
 ; 
HEADER(DEFEF) ; EP-RETURN THIS TEMPLATE'S HEADER FILE MNEMONIC ; PATCHED BY GIS 10/24/03
 ; PATCHED BY GIS/OIT 10/15/05 ; PCC+ 2.5 PATCH 1
 N %,H25,PATH
 S PATH=$G(^VEN(7.5,$$CFG,2)),H25=0
 I '$L(PATH) Q "ef"
 S H25=$$FIND^VENPCCP(PATH,"25header.txt")
 S %=$P($G(^VEN(7.41,+$G(DEFEF),0)),U,2)
 I %="ef",H25 Q 25
 I $L(%) Q %
 Q "ef"
 ; 
MAXNARR(DEFEF) ; EP-RETURN THE NAMIMUM LENGTH OF THE DX NARRATIVE ON THIS FORM ; PATCHED BY GIS 1/8/04
 N %
 S %=$P($G(^VEN(7.41,+$G(DEFEF),5)),U,16)
 I %<22 Q 22
 I %>80 W 22
 Q %
 ; 
FVICD(PIEN) ; EP-GIVEN V POV IEN, RETURN THE ICD CODE (OR NULL IF FOREIGN VISIT SCREEN IS APPLIED AND POSITIVE)
 N X,VIEN,LIEN,IIEN,ICD
 S X=$G(^AUPNVPOV(PIEN,0)) I '$L(X) Q ""
 S VIEN=+$P(X,U,3),IIEN=+X,ICD=$P($G(^ICD9(IIEN,0)),U)
 I '$L(ICD) Q ""
 I '$P($G(^VEN(7.41,+$G(DEFEF),5)),U,15) Q ICD ; NO FOREIGN VISIT FILTER IN PLACE
 S LIEN=$P($G(^AUPNVSIT(VIEN,0)),U,6)
 I $G(DUZ(2)),LIEN'=$G(DUZ(2)) Q "" ; NOT A CONFIRMED LOCAL VISIT
 Q ICD ; LOCAL VISIT
 ;
CSTOP(DEPTIEN) ; EP-GIVEN A DEPARTMENT IEN, RETURN THE CLINIC STOP
 Q $P($G(^VEN(7.95,+$G(DEPTIEN),0)),U,4)
 ; 
FHPT W !,$$FHP("25","b1") ; EP - HF MN FOR b25
 Q
 ; 
FHP(MN,MMF) ; EP-GIVEN A HEADER FILE MNEMONIC AND HEADER (OR PIECE), THIS FUNCTION RETURNS THE '^' PIECE (OR HEADER)
 N X,PCE,L,FLD,PATH,FILE,POP
 I '$L(MN) Q ""
 S FILE=MN_"header.txt"
 S PATH=$G(^VEN(7.5,$$CFG,2)) I '$L(PATH) Q ""
 S POP=$$OPN^VENPCCP(PATH,FILE,"R","R X") I POP Q ""
 I MMF=+MMF Q $P(X,U,MMF) ; PIECE TO FIELD
 S L=$L(X,U),FLD=""
 F PCE=1:1:L S Y=$P(X,U,PCE) I Y=MMF Q
 I Y'=MMF Q ""
 Q PCE ; FIELD TO PIECE
 ;
IEN(X) ; EP - RETURN THE IEN TO A SCHEMA FIELD
 Q +$G(X)
 ; 
