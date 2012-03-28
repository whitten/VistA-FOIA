VENPCC2A ; IHS/OIT/GIS - HEALTH SUMMARY GENERATOR ;
 ;;2.6;PCC+;;NOV 12, 2007
 ;
 ; VER 2.2 HS GENERATOR
 ; SUPPORTS OUTPUT FOR NEW PRINT SERVICE
 ; 
HS(APCHSPAT,APCHSTYP,VISIT,DEPTIEN) ; EP-GENERATE A HEALTH SUMMARY
 N PATH2,PATH3
 I $D(^AUPNPAT(+$G(APCHSPAT),0)),$D(^APCHSCTL(+$G(APCHSTYP),0)),$D(^VEN(7.95,+$G(DEPTIEN),0))
 E  Q
 I '$L($T(EN1^APCHS)) N ERR S ERR="Missing APCHS/APCHS0 patches.  Can't print health summary!" D ERR^VENPCC1(ERR) Q
 S %=$G(^VEN(7.5,+$G(CFIGIEN),2)) I %="" D ERR7^VENPCC1 Q
 S PATH2=%,%=$G(^VEN(7.5,+$G(CFIGIEN),3)) I %="" D ERR7^VENPCC1 Q
 S PATH3=%
NEW NEW %,%FN,%I,%T,%Y,APCHSFNM,APCHSIC,APCHSICD,APCHSICL,APCHSIR,APCHSNRQ,APCHSPDN,APCHSSGY,APCHSMTY,APCHSTAT,ARRAY,FILE,HEAD,I,POP,STOP,TAB,TMP,X,Y,APCHSERR,APCHSBWR
 S TMP="^TMP(""VEN HS"",$J)",FILE="T"_VISIT_".TXT" K @TMP
 S IOST="P-PRINTER",IOSL=99999,IOM=80
 D DEL^VENPCCP1(PATH3,FILE) ; CLEAN UP THE TMP FILE ; PATCHED BY GIS 2/20/07
 S POP=$$OPN^VENPCCP(PATH3,FILE,"W","N IO S IO=DEV D EN1^APCHS") K ^TMP($J,"APCHSMED") I POP Q
 S POP=$$OPN^VENPCCP(PATH3,FILE,"R","D LINE^VENPCC2A") I POP Q
 D DEL^VENPCCP1(PATH3,FILE) ; CLEAN UP THE TMP FILE AGAIN
PASS ; FIRST PASS THROUGH THE ARRAY
 NEW %,I,PATIENT,SCNT,SNO,STOP,X
 S SCNT=0
 S %=$G(@TMP@(0,2)) I '$L(%) S ERR="Unable to generate health summary.  Directory may be full!" D ERR^VENPCC1(ERR) Q  ; /usr directory or descendent is full
 S %=$P(%,"** ",2),%=$P(%," **") S @TMP@(2,"H1")=%
 S %=$P(@TMP@(0,3),"** ",2),@TMP@(2,"H2")="  "_$P(%,"pg")
 S @TMP@(2,"HEADER")="HS",@TMP@(2,"TEMPLATE")="HS2",@TMP@(2,"PRINTER")=""
 S (%,PATIENT)=$P($G(^DPT(APCHSPAT,0)),U) I '$L(%) D ERR3^VENPCC1 S STOP=1 Q
 S X=$P(%,",",2,99)_" "_$P(%,",")
 I $G(DFN),$G(DEPTIEN) S X=X_"  #"_$$CHART^VENPCC1A(DEPTIEN,DFN)
 S @TMP@(2,"FOOTER")=X
 S I=0 F  S I=$O(@TMP@(0,I)) Q:'I  S X=^(I) D
 . I X["** END" K @TMP@(0,I-1),^(I),^(I+1)
 . I X[("**"_$E(PATIENT,1,20)) K @TMP@(0,I) Q
 . I X["CONFIDENTIAL PATIENT" K @TMP@(0,I) Q
 . I $L(X)>50,$E(X)="-",$E(X,$L(X))="-" S %=$$STRIP^VENPCCU(X) S @TMP@(1,I)=% K @TMP@(0,I-1),^(I+1)
 . Q
 S SNO=0 F  S SNO=$O(@TMP@(1,SNO)) Q:'SNO  D SEC(SNO) I $G(STOP) Q
 D EXP
 K @TMP
 D AUDIT(APCHSPAT,DUZ,$G(DEPTIEN),$G(VISIT))
 Q
 ;
LINE F I=1:1 R X:30 Q:$$STATUS^%ZISH!(X["** END ")  S @TMP@(0,I)=X
 Q
 ; 
SEC(SNO) ; EP-PROCESS A SECTION
 NEW %,COMP,INO,NAME,REF,X,Y,LINE,LNO,STOP
 S NAME=@TMP@(1,SNO),COMP=$P(NAME," (")
 S SCNT=SCNT+1,STOP=0
SEC1 S @TMP@(2,("S"_SCNT))=NAME,INO=0,LNO=SNO
 F  S LNO=$O(@TMP@(0,LNO)) Q:'LNO  S LINE=^(LNO) D  I STOP Q
 . I $E(LINE)="-",$E(LINE,$L(LINE))="-" S STOP=1 Q  ; QUIT WHEN YOU REACH THE START OF NEXT SECTION
 . D SET($C(9)_LINE) ; ATTACH A VALUE TO A FIELD.  ALL FIELDS MUST START WITH A TAB CHARACTER
 . Q
 D SET($C(9))
 Q
 ;
SET(X) ; EP-SAVE TMP GLOBALS
 I X[U S X=$TR(X,U,"")
 S INO=INO+1
 I INO=44,SCNT=10 S @TMP@(2,"S1044")="",INO=45  ; FIXES PRINT SERVICE PROBLEM
 ; I INO>50 S:INO=51 @TMP@(2,("S"_(SCNT*100+50)))="<<Space constraints prevent display of additional lines in this section!!!>>" Q
 S @TMP@(2,("S"_(SCNT*100+INO)))=X
 I INO=50 S SCNT=SCNT+1,INO=0 ; OVERLAP INTO THE NEXT SECTION
 Q
 ;
EXP ; EP-EXPORT THE MAIL MERGE FILE
 I $P($G(^VEN(7.5,CFIGIEN,13)),U) D  Q  ; IN VER 2.5, GO DIRECTLY DATA FILE BUILDER
 . N VER25,HSFLAG
 . S VER25=1 S HSFLAG=1
 . D TCP^VENPCC1 Q
 . Q
 ; TRADITIONAL HS EXPORT...
 NEW %,%FN,FILE,I,LINE,LNO,NAME,X,Y,Z,PATH
 S @TMP@(3,1)="HEADER",^(2)="TEMPLATE",^(3)="GROUP",^(4)="PRINTER",^(5)="H1",^(6)="H2"
 S I=6 F X=1:1:25 F Y=1:1:50 D
 . I Y=1 S I=I+1,@TMP@(3,I)="S"_X,Z=X*100
 . S I=I+1,@TMP@(3,I)="S"_(Z+Y)
 . Q
 S @TMP@(3,I+1)="FOOTER"
 N HSFLAG S HSFLAG=1
 D TCP^VENPCC1 ; TCP PRINT SERVICE
 Q
 ;
AUDIT(PAT,USER,SITE,VISIT)   ; EP-UPDATE AUDIT FILE
 NEW %,%DT,%I,%Q,%Y,D,D0,DA,DI,DIC,DIE,DQ,DR,X,Y,DLAYGO
 D NOW^%DTC S X=%
 S DIC="^VEN(7.8,",DIC(0)="L",DLAYGO=19707.8 D ^DIC
 I Y=-1 Q
 S DIE=DIC,DA=+Y
 S DR=".02////^ S X=$G(PAT);.03////^S X=$G(SITE);.04////^S X=USER"
 I '$G(VFLAG),$L($G(VISIT)),'$G(NOVISIT) S DR=DR_";.05////^S X=VISIT"
 I $G(APPDATE) S DR=DR_";.06////^S X=APPDATE"
 L +^VEN(7.8):0 I $T D ^DIE L -^VEN(7.8)
 Q
 ; 
