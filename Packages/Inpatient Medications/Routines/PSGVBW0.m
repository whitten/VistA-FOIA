PSGVBW0 ;BIR/CML3,MV-SHOW NON-VERFIED ORDERS GATHERED IN PSGVBW ;17 SEP 97 /  1:41 PM
 ;;5.0; INPATIENT MEDICATIONS ;**29,39,53,56,95,80**;16 DEC 97
 ;
 ; Reference to ^PSSLOCK is supported by DBIA #2789
 ; Reference to ^DIR is supported by DBIA 10026
 ; Reference to ^VALM is supported by DBIA 10118
 ;
START ;
 S (LINE,PSGOEA,PSGOEAV)="",$P(LINE,"-",81)=""
 K ^TMP("PSJLIST",$J) D:PSGSS'="P" DISPLAYW Q:'$O(^TMP("PSJSELECT",$J,0))
PROCESS ; Loop through selected patients and display profile/orders.
 K DIR,PSJPNV S PSJPNV=1
 I $P(PSJSYSU,";")=3 S X=$O(^TMP("PSJSELECT",$J,1)),DIR(0)="Y",DIR("A")="Do you want to print a profile for the"_$S(X:"se",1:"")_" patient"_$S(X:"s",1:""),DIR("B")="NO" D
 .D ^DIR K DIR I Y D ^PSJHVARS,^PSGVBWP,RESTORE^PSJHVARS
 .W !!,"Select profile type for order processing.",!!
 D ENL^PSGOU Q:"SNL"'[PSGOL
 F PSJCNT=0:0 S PSJCNT=$O(^TMP("PSJSELECT",$J,PSJCNT)) Q:'PSJCNT  D PROCESS1 S PSGOP=PSGP D ENQL^PSGLW:$P(PSJSYSL,"^",2)]"" Q:$G(PSJGOTO)="E"  I $D(^TMP("PSJSELECT",$J,+$G(PSJGOTO))) S PSJCNT=PSJGOTO-1
 Q
PROCESS1 ;
 S PSJPN=$G(^TMP("PSJSELECT",$J,PSJCNT)) K PSJGOTO
 S PSJLK=$$L^PSSLOCK($P(PSJPN,U,2),1) I 'PSJLK W !,$C(7),$P(PSJLK,U,2) Q
 K PSJGOTO D:PSJPN]"" GTORDERS
 I PSJLK D UL^PSSLOCK($P(PSJPN,U,2))
 I $G(PSGPXN),$P(PSJSYSW0,U,29)]"" D  K PSGPXPT S PSGPXN=0
 .S PSGPXPT=PSGP
 .N DFN,PSGP S (PSGP,DFN)=PSGPXPT D ^PSGPER,ENCV^PSGSETU,^PSIVXU
 Q
 ;
DISPLAYW ; Allow selection of patients on each ward selected.
 K ^TMP("PSJSELECT",$J) S PSJCNT=1,PSGVBWN="" F  S PSGVBWN=$O(^TMP("PSGVBW",$J,PSGVBWN)) Q:PSGVBWN=""  D DISPLAYT
 Q
 ;
DISPLAYT ;
 NEW PSGPICK  ;PSGPICK=1-->user selected order, stop display the profile
 D HEADER S PSGVBTM="",PSGVBY=0 F  S PSGVBTM=$O(^TMP("PSGVBW",$J,PSGVBWN,PSGVBTM))  Q:(PSGVBTM=""!$G(PSGPICK))  D V2
 I PSJASK,(PSGVBY>0) D ASK
 Q
 ;
GTORDERS ;
 S (PSGP,DFN)=$P(PSJPN,U,2) K PSJACNWP D ^PSJAC
 I PSGOL'="N" D PROFILE Q
 D ENGORD^PSGVBWU
 ;S PSJPRIO="" F  S PSJPRIO=$O(^TMP("PSJON",$J,PSJPRIO)) Q:PSJPRIO=""  S PSJON="" F  S PSJON=$O(^TMP("PSJON",$J,PSJPRIO,PSJON)) Q:PSJON=""  D DISACTIO^PSJOE(DFN,$P(PSJON,U,2),1) Q:$D(PSJGOTO)
 S PSJPRIO="" F  S PSJPRIO=$O(^TMP("PSJON",$J,PSJPRIO)) Q:PSJPRIO=""  S PSJON="" D
 . F  S PSJON=$O(^TMP("PSJON",$J,PSJPRIO,PSJON)) Q:PSJON=""  D
 .. I '$$LS^PSSLOCK(DFN,$P(PSJON,U,2)) D DISPORD(DFN,$P(PSJON,U,2)) Q
 .. D DISACTIO^PSJOE(DFN,$P(PSJON,U,2),1) Q:$D(PSJGOTO)  D UNL^PSSLOCK(DFN,$P(PSJON,U,2))
 Q
 ;
PROFILE ; Display the patient's profile and allow order selection.
 S PSGP=DFN,PSJOL=PSGOL F  D EN^VALM("PSJ LM PNV") Q:'$G(PSJORD)&'$G(PSJNEWOE)  S PSJNEWOE=0
 Q
 ;
DONE ;
 K ^TMP("PSGVBW",$J),^TMP("PSJON",$J)
 K CF,DA,LINE,NP,POP,PPN,PR,PSGCANFL,PSGION,PSGOL,PSGOEAV,PSGOENOF,PSGON,PSGONC,PSGONR,PSGLMT,PSGORD,PSGPRF,PSGVBA,PSGVBAF,PSGVBON,PSGVBPN,PSGVBQ,PSGVBQ1,PSGVBSD,PSGVBSS,PSGVBST,PSGVBTM,PSGVBW,PSGVBWN,PSGVBY,QQ,Z
 Q
 ;
V2 ;
 S PSGVBPN="" F  S PSGVBPN=$O(^TMP("PSGVBW",$J,PSGVBWN,PSGVBTM,PSGVBPN)) Q:(PSGVBPN=""!$G(PSGPICK))  S PSGP=$P(PSGVBPN,"^",2),PPN=$P(PSGVBPN,"^") S:PPN="" PPN=PSGP_";DPT(" D WRT
 Q
 ;
WRT ;
 S PSGVBY=PSGVBY+1,PSJASK=1
 W !,$J(PSGVBY,4),?6,$S(PSGVBTM'="zz":PSGVBTM,1:"Not Found"),?27,PPN," (",$P(PSGVBPN,U,3),")" S ^TMP("PSJLIST",$J,PSGVBY)=PSGVBWN_U_PSGVBTM_U_PPN_U_PSGP
 ;I $Y+1>IOSL,(PSGVBY>0) D ASK S PSJASK=0
 ;I $Y+1>IOSL,(PSGVBY>0) D ASK S PSJASK=0 W @IOF
 I $Y+1>IOSL,(PSGVBY>0) NEW DIR S DIR(0)="EA",DIR("A")=" '^' TO QUIT " D ^DIR D
 . I X="^" S PSGPICK=1  Q
 . S PSJASK=0 W @IOF
 Q
 ;
ASK ;
 N DIR,PSGDFN S DIR(0)="LOA^1:"_PSGVBY,DIR("A")="Select 1 - "_PSGVBY_": " D ^DIR I $D(DUOUT)!$D(DTOUT) K ^TMP("PSGVBW",$J) Q
 S:Y]"" PSGPICK=1
 F PSJINDEX=1:1:$L(Y,",")-1 D
 . S X=$G(^TMP("PSJLIST",$J,$P(Y,",",PSJINDEX))),PSGDFN=$P(X,"^",4)_"^"_$P(X,"^",3)
 . D CHK^PSJDPT(.PSGDFN,1) I PSGDFN=-1 Q
 . S:X]"" ^TMP("PSJSELECT",$J,PSJCNT)=$P(X,U,3,4),^TMP("PSJSELECT",$J,"B",$P(X,U,3),PSJCNT)="",PSJCNT=PSJCNT+1
 ;K ^TMP("PSJLIST",$J)
 Q
 ;
H2 ;
 W !!?2,"Select patients either singularly separated by commas (1,2,3), by a range of",!,"patients separated by a dash (1-3), or a combination (1,2,4-6).  To select all",!,"patients, enter 'ALL' or a dash ('-').  You can also enter '-n' to"
 W " select the",!,"first patient through the 'nth' patient or enter 'n-' to select the 'nth'",!,"patient through the last patient.  If a patient is selected more than once,"
 W !,"only the first selection is used.  (Entering '1,2,1' would return '1,2'.)" Q
 ;
HEADER ;
 W:$Y @IOF W !,"ORDERS NOT VERIFIED BY A ",$S($P(PSJSYSU,";",3)>1:"PHARMACIST",1:"NURSE")," - ",$S(PSGVBWN="ZZ":"^OTHER",1:PSGVBWN)
 W !!," No.",?7,"TEAM",?32,"PATIENT",!,LINE K PSGVBY S PSGVBY=0 Q
 Q
 ;
NP ;
 W $C(7) R !!,"ENTER AN '^' TO SELECT ORDERS NOW, OR PRESS THE RETURN KEY TO CONTINUE. ",NP:DTIME E  S NP="^"
 Q
DISPORD(DFN,ON)     ;Display the order that being lock by another user
 NEW PSJLINE,PSJOC,X
 S PSJLINE=1
 D DSPLORDU^PSJLMUT1(DFN,ON)
 W ! F X=0:0 S X=$O(PSJOC(ON,X)) Q:'X  W !,PSJOC(ON,X)
 Q
        
