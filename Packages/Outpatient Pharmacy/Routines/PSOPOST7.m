PSOPOST7 ;BIR/EJW,JLC-Post install routine ;10/04/02
 ;;7.0;OUTPATIENT PHARMACY;**115**;DEC 1997
 ;External reference to ^DPT supported by DBIA 10035
 ;External reference to ^PS(55 supported by DBIA 2228
 ; POST-INSTALL ROUTINE FOR PATCH PSO*7*115 - TO RESET MISSING ENTRIES INTO THE PHARMACY PATIENT FILE (#55)
 S ZTDTH=""
 I $D(ZTQUEUED) S ZTDTH=$H
 L +^XTMP("PSOPOST7"):0 I '$T D  Q
 . I ZTDTH="" W !,"Clean up job is already running.  Halting..."
 L -^XTMP("PSOPOST7")
 I ZTDTH="" D
 .W !,"The background job to search for missing ^PS(55 entries must be queued."
 .W !,"If no start date/time is entered when prompted, the background job will be"
 .W !,"queued to run NOW."
 .W !
 .D BMES^XPDUTL("Queuing background job to search for missing ^PS(55 entries")
 S ZTRTN="RES^PSOPOST7",ZTIO="",ZTDESC="Background job to search for missing ^PS(55 entries" D ^%ZTLOAD K ZTDTH,ZTRTN,ZTIO,ZTDESC
 W:$D(ZTSK)&('$D(ZTQUEUED)) !!,"Task Queued !",!
 Q
RES ;
 L +^XTMP("PSOPOST7"):0 I '$T S:$D(ZTQUEUED) ZTREQ="@" Q
 I '$G(DT) S DT=$$DT^XLFDT
 I '$D(^XTMP("PSOPOST7")) S X1=DT,X2=+30 D C^%DTC S ^XTMP("PSOPOST7",0)=$G(X)_"^"_DT
 S PSODT2=DT-20000
 D NOW^%DTC S ^XTMP("PSOTIMEX","START")=%
 D BMES^XPDUTL("Searching for missing ^PS(55 entries...")
SRCH ; SEARCH THROUGH PRESCRIPTIONS
 N RXP,RX0,PSODFN,PSODT,PSOCTP,PSOCTPA
 S (PSOCTP,PSOCTPA)=0
 S RXP=0 F  S RXP=$O(^PSRX(RXP)) Q:'RXP  S RX0=$G(^PSRX(RXP,0)),PSODT=$P(RX0,"^",13) I PSODT>PSODT2 S PSODFN=$P(RX0,"^",2) I PSODFN D
 .I '$D(^DPT(PSODFN,0)) Q
 .D PS55P
 .D PS55PA
 I $O(^XTMP("PSOPOST7",$J,""))'="" D RESET
 N DFN,PSJORD,PSSTART,PSSTOP,PSSTATUS,A
 S DFN=0
 F  S DFN=$O(^PS(55,DFN)) Q:'DFN  D
 . S PSJORD=0 F  S PSJORD=$O(^PS(55,DFN,5,PSJORD)) Q:'PSJORD  D
 .. S PSSTATUS=$P($G(^PS(55,DFN,5,PSJORD,0)),U,9),PSSTART=$P($G(^PS(55,DFN,5,PSJORD,2)),U,2),PSSTOP=$P($G(^(2)),U,4) I PSSTOP]"",$D(^PS(55,"AUD",PSSTOP,DFN,PSJORD)) Q
 .. K DR S DIE="^PS(55,"_DFN_",5,",DA=PSJORD,DA(1)=DFN,DR="10////^S X=PSSTART;28////^S X=PSSTATUS;34////^S X=PSSTOP"
 .. D ^DIE
 .. S ^XTMP("PSOPOST7",$J,"UD",DFN,PSJORD)="" K DIE,DR,DA
 . S PSJORD=0 F  S PSJORD=$O(^PS(55,DFN,"IV",PSJORD)) Q:'PSJORD  D
 .. S A=$G(^PS(55,DFN,"IV",PSJORD,0)) Q:A=""
 .. S PSSTART=$P(A,"^",2),PSSTOP=$P(A,"^",3),PSSTATUS=$P(A,"^",17)
 .. I PSSTOP]"",$D(^PS(55,"AIV",PSSTOP,DFN,PSJORD)) Q
 .. K DR S DIE="^PS(55,"_DFN_",""IV"",",DA=PSJORD,DA(1)=DFN,DR=".02////^S X=PSSTART;.03////^S X=PSSTOP;100////S X=PSSTATUS"
 .. D ^DIE
 .. S ^XTMP("PSOPOST7",$J,"IV",DFN,PSJORD)="" K DIE,DR,DA
MAIL ;
 N CNT
 D NOW^%DTC S PSOTIMEB=%
 S Y=$G(^XTMP("PSOTIMEX","START")) D DD^%DT S PSOTIMEA=Y
 S Y=$G(PSOTIMEB) D DD^%DT S PSOTIMEB=Y
 S XMDUZ="Patch PSO*7*115",XMY(DUZ)="",XMSUB="PHARMACY PATIENT File (#55) search for missing entries"
 K PSOTEXT S PSOTEXT(1)="Patch PSO*7*115 PHARMACY PATIENT File (#55) search and clean-up is complete.",PSOTEXT(2)="It started on "_$G(PSOTIMEA)_".",PSOTEXT(3)="It ended on "_$G(PSOTIMEB)_"."
 S PSOTEXT(4)=" "
 S PSOTEXT(5)=PSOCTP_" patients had missing ""P"" cross-references"_$S(PSOCTP>0:" and have been reset.",1:".")
 S PSOTEXT(6)=PSOCTPA_" ^PS(55,PSODFN,""P"",""A"" cross-references were missing"_$S(PSOCTPA>0:" and have been reset.",1:".")
 S PSOTEXT(7)=" "
 S CNT=7
 I PSOCTP S CNT=CNT+1,PSOTEXT(CNT)="The global ^XTMP(""PSOPOST7"","_$J_",PSODFN,ISSUE DATE,RXIEN) contains" D
 .S CNT=CNT+1,PSOTEXT(CNT)="the missing ""P"" cross-reference entries. "_"("_$J_" is the job number.)"
 S CNT=CNT+1,PSOTEXT(CNT)=" "
 I PSOCTPA S CNT=CNT+1,PSOTEXT(CNT)="The global ^XTMP(""PSOPOST7"","_$J_",PSODFN,""P,A"",EXP. DATE,RXIEN) contains" D
 .S CNT=CNT+1,PSOTEXT(CNT)="the missing ""P"",""A"" cross-reference entries. "_"("_$J_" is the job number.)"
 I $D(^XTMP("PSOPOST7",$J,"UD")) D
 . S CNT=CNT+1,PSOTEXT(CNT)="The global ^XTMP(""PSOPOST7"","_$J_",""UD"",DFN,PSJORD) contains"
 . S CNT=CNT+1,PSOTEXT(CNT)="the unit dose orders missing stop dates."
 I $D(^XTMP("PSOPOST7",$J,"IV")) D
 . S CNT=CNT+1,PSOTEXT(CNT)="The global ^XTMP(""PSOPOST7"","_$J_",""IV"",DFN,PSJORD) contains"
 . S CNT=CNT+1,PSOTEXT(CNT)="the IV orders missing stop dates."
 S XMTEXT="PSOTEXT(" N DIFROM D ^XMD
 K PSOTIMEA,PSOTIMEB,XMDUZ,XMSUB,PSOTEXT,XMTEXT,PSODT2
 L -^XTMP("PSOPOST7")
 S:$D(ZTQUEUED) ZTREQ="@"
 Q
 ;
PS55P ; CHECK FOR MISSING "P" CROSS=REFERENCES
 N PSOSQ
 S PSOSQ=0 F  S PSOSQ=$O(^PS(55,PSODFN,"P",PSOSQ)) Q:'PSOSQ  I $G(^PS(55,PSODFN,"P",PSOSQ,0))=RXP Q
 I PSOSQ Q
 S ^XTMP("PSOPOST7",$J,PSODFN,PSODT,RXP)=""
 Q
 ;
PS55PA ; CHECK FOR MISSING "P","A" CROSS-REFERENCES
 N PSODT
 S PSODT="" F  S PSODT=$O(^PS(55,PSODFN,"P","A",PSODT)) Q:'PSODT  I $D(^PS(55,PSODFN,"P","A",PSODT,RXP)) Q
 I 'PSODT D
 . N PSOEXP
 . S PSOEXP=$P($G(^PSRX(RXP,2)),"^",6) I PSOEXP="" S PSOEXP=$P($G(^PSRX(RXP,3)),"^",5)
 .I PSOEXP="" Q
 .S ^XTMP("PSOPOST7",$J,PSODFN,"P,A",PSOEXP,RXP)=""
 .D CHKPS
 .S ^PS(55,PSODFN,"P","A",PSOEXP,RXP)="",PSOCTPA=PSOCTPA+1
 Q
 ;
CHKPS ; SEE IF ^PS(55,PSODFN EXISTS - IF NOT SET TOP LEVEL AT LEAST
 I '$D(^PS(55,PSODFN,0)) S ^PS(55,PSODFN,0)=PSODFN_"^^^^^2"
 Q
 ;
RESET ; RESET "P" CROSS-REFERENCE BY BUILDING ^TMP GLOBAL IN ISSUE DATE SEQUENCE FOR ALL ENTRIES, THEN RESETTING THE "P" SUBSCRIPT
 N PSOIDT,PSOSQ,CNT
 S PSODFN="" F  S PSODFN=$O(^XTMP("PSOPOST7",$J,PSODFN)) Q:'PSODFN  S PSOCTP=PSOCTP+1 D
 .K ^TMP("PSOPOST7",$J)
 .S CNT=0
 .I '$O(^XTMP("PSOPOST7",$J,PSODFN,"")) Q  ; quit if only "P,A" entries
 .L +^PS(55,PSODFN)
 .S PSODT="" F  S PSODT=$O(^XTMP("PSOPOST7",$J,PSODFN,PSODT)) Q:'PSODT    S RXP="" F  S RXP=$O(^XTMP("PSOPOST7",$J,PSODFN,PSODT,RXP)) Q:'RXP  D
 ..S PSOIDT=$P($G(^PSRX(RXP,0)),"^",13) I PSOIDT'="" I '$D(^TMP("PSOPOST7",$J,PSOIDT,RXP)) S ^TMP("PSOPOST7",$J,PSOIDT,RXP)="",CNT=CNT+1
 .S PSOSQ=0 F  S PSOSQ=$O(^PS(55,PSODFN,"P",PSOSQ)) Q:'PSOSQ  D  ; NOW ADD ALL EXISTING ENRIES TO ^TMP GLOBAL
 ..S RXP=$G(^PS(55,PSODFN,"P",PSOSQ,0)) I RXP="" Q
 ..S PSOIDT=$P($G(^PSRX(RXP,0)),"^",13) I PSOIDT'=""  I '$D(^TMP("PSOPOST7",$J,PSOIDT,RXP)) S ^TMP("PSOPOST7",$J,PSOIDT,RXP)="",CNT=CNT+1
 .I $O(^PS(55,PSODFN,"P",CNT)) D
 ..S PSOSQ=CNT F  S PSOSQ=$O(^PS(55,PSODFN,"P",PSOSQ)) Q:'PSOSQ  K ^PS(55,PSODFN,"P",PSOSQ) ; REMOVE SEQUENCE NUMBERS THAT ARE GREATER THAN THE NUMBER OF "P" ENTRIES
 .S CNT=0,PSOIDT="" F  S PSOIDT=$O(^TMP("PSOPOST7",$J,PSOIDT)) Q:'PSOIDT  S RXP=""  F  S RXP=$O(^TMP("PSOPOST7",$J,PSOIDT,RXP)) Q:'RXP  S CNT=CNT+1,^PS(55,PSODFN,"P",CNT,0)=RXP
 .I CNT>0 S ^PS(55,PSODFN,"P",0)="^55.03PA^"_CNT_"^"_CNT
 .L -^PS(55,PSODFN)
 K ^TMP("PSOPOST7",$J)
 Q
 ;
