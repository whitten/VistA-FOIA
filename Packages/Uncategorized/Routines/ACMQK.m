ACMQK ; IHS/TUCSON/TMJ - EDIT/PRINT/SORT CONTROLS ; [ 03/11/2002  6:59 PM ]
 ;;2.0;ACM CASE MANAGEMENT SYSTEM;**5,8**;JAN 10, 1996
 ;ACMCTRLE OR ACMCTRLS OR ACMCTRLP OR ACMCTRLX VARIABLES
EN ;PEP - SORT CONTROLS
 Q:'$D(ACMRG)!$D(XQUIT)
 D SLCT
EXIT K ACMX,ACMRTN,ACMLINER,ACMSLCT,ACMENTRY,ACMDATA,ACMEN,ACMJ,ACM,ACMQKI,ACMLI,ACMEND,ACMQK1,ACMCNT,ACMTITLE,ACMOUT,ACMENDD,%
 Q
 ;
SLCT S ACMTITLE=$S($D(ACMES):"LSS",$D(ACMEP):"PDE",$D(ACMPS):"PSD",$D(ACMPP):"PR",1:"HEAD")
 D @ACMTITLE^ACMMENU
 W !
 S ACMCTRL=$S($D(ACMEP):ACMCTRLE,$D(ACMES):ACMCTRLS,$D(ACMPP):ACMCTRLP,$D(ACMPS):ACMCTRLX,1:"")
 Q:ACMCTRL=""
 I ACMCTRLS=";AD",$D(ACMES) W !,?5,"The "_ACMRGNA_" Register does NOT include any Data Type Component",!,?5,"which requires a Supporting List to be built!",! H 8 Q
 S ACMEND=$L(ACMCTRL,";"),ACMENDD=ACMEND\2+(ACMEND#2)
 K ACM
 F %(1)=1:1:ACMEND S ACMEN=$P(ACMCTRL,";",%(1)),ACMDATA=$T(@ACMEN^ACMCTRL1),ACMSLCT=$P(ACMDATA,";;",2),ACM(%(1))=ACMDATA
 F %(1)=1:1:ACMENDD D
 .S ACMDATA=ACM(%(1)),ACMSLCT=$P(ACMDATA,";;",2) W !?10,$J(%(1)_")",3),?$X+2,ACMSLCT
 .S %(2)=%(1)+ACMENDD
 .Q:'$D(ACM(%(2)))
 .S ACMDATA=ACM(%(2)),ACMSLCT=$P(ACMDATA,";;",2) W ?45,$J(%(2)_")",3),?$X+2,ACMSLCT
SLCT1 W !
 W:'$D(ACMPP) !?10,"To select several options separate them with commas.",!?10,"For example:  ==> 1,3,7,9  "
 W !
 S DIR(0)="LOA^1:"_ACMEND,DIR("A")="          "_$S($D(ACMES)!$D(ACMEP):"Enter",$D(ACMPS)!$D(ACMPP):"Report")_" Option"_$S('$D(ACMPP):"(s)",1:"")_" ==> ",DIR("?")="Type a number from 1 to "_ACMEND
 D ^DIR K DIR
 I U[$E(X)!(Y="") K:$D(ACMES) ACMCTRL S ACMQUIT="" Q
 S ACMQK=Y
 S:$E(ACMQK,$L(ACMQK))="," ACMQK=$E(ACMQK,1,$L(ACMQK)-1)
 I $D(ACMPP) S ACMENTRY=ACM(ACMQK) D ^ACMSRT G EN
 I ACMQK=ACMEND D ALL G EN
 D LOOP
 G EN
 ;
LOOP S ACMCNT=$L(ACMQK,","),ACMQK1=ACMQK
 F ACMLI=1:1:ACMCNT S ACMQK=$P(ACMQK1,",",ACMLI) D:ACMQK?1N.2N&(ACMQK'>(ACMEND-1)) SET Q:$D(ACMOUT)
 Q
 ;
SET Q:ACMQK=ACMEND
 S ACMENTRY=ACM(ACMQK),ACMRTN=$S($D(ACMES):"^ACMESDT",$D(ACMEP):"^ACMEP",$D(ACMPS)!$D(ACMPP):"^ACMSRT")
 I $P(ACMENTRY," ;;")="APPT" S ACMRTN="^ACMAPPT"
 I $P(ACMENTRY," ;;")="CT" S ACMCT=""
 I $P(ACMENTRY," ;;")="CH" S ACMCH=""
 I $P(ACMENTRY," ;;")="CR" S ACMCR=""
 I $P(ACMENTRY," ;;")="CP" S ACMRTN="^ACMPLAN"
 I $P(ACMENTRY," ;;")="CMGT" S ACMRTN="CMS^CIMTYKC" ;IHS/CIM/THL PATCH 5
 D @ACMRTN
 K ACMRTN,ACMCT,ACMCH,ACMCR
 Q
 ;
ALL ;
 F ACMQK=1:1:(ACMEND-1) D SET Q:$D(ACMOUT)
 Q
