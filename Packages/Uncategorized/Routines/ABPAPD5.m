ABPAPD5 ;DISPLAY PAYMENT EDIT SCREEN CONT.; [ 07/09/91  7:42 AM ]
 ;;1.4;AO PVT-INS TRACKING;*0*;IHS-OKC/KJR;JULY 25, 1991
A0 K ABPA("QF"),SA,ABPAPSDT,ABPARECV,ABPAPOST,ABPADATA,ABPA("CHK")
 S ABPAPAMT=0,ABPA("STDAMT")=0,ABPA("UP","N")=0,ABPA("UP","D")=0
 S ABPA("UP","S")=0,ABPARECV=$P(^ABPVAO(ABPATDFN,"P",ABPADDFN,0),"^")
 S ABPARECV=$E(ABPARECV,4,5)_"/"_$E(ABPARECV,6,7)_"/"_$E(ABPARECV,2,3)
 S ABPAPOST=$P(^ABPVAO(ABPATDFN,"P",ABPADDFN,0),"^",3)
 S ABPAPOST=$S(ABPAPOST="N":"NO",ABPAPOST="Y":"YES",1:"???")
 S ABPADATA=$P(^ABPVAO(ABPATDFN,"P",ABPADDFN,0),"^",2)
 I +ABPADATA>0 D
 .S ABPAPSDT=$E(ABPADATA,4,5)_"/"_$E(ABPADATA,6,7)_"/"
 .S ABPAPSDT=ABPAPSDT_$E(ABPADATA,2,3)
 S:$D(ABPAPSDT)=0 ABPAPSDT=""
 S ABPA("CHK")=$P(^ABPVAO(ABPATDFN,"P",ABPADDFN,0),"^",6)
LOOP S (REFUND,DA)=0 F ABPA("I")=0:0 D  Q:$D(ABPA("QF"))=1
 .S DA=$O(^ABPVAO(ABPATDFN,"P",ABPADDFN,"A",DA))
 .I +DA=0 S ABPA("QF")="" Q
 .Q:$D(^ABPVAO(ABPATDFN,"P",ABPADDFN,"A",DA,0))'=1  S ABPADATA=^(0)
 .W !,$J(DA,3),?7,ABPARECV S ABPAPCOD=$P(ABPADATA,"^",2)
 .S ABPACDFN=$P(ABPADATA,"^",3) W ?18,$J(+ABPADATA,9,2),?32,ABPAPCOD
 .I ABPACDFN]"" I $D(^ABPVAO(ABPATDFN,1,ABPACDFN,0))=1 D
 ..W ?36,$J($P(^ABPVAO(ABPATDFN,1,ABPACDFN,0),"^",2),8)
 ..S ABPA("AP",ABPACDFN,DA)=ABPADATA
 .I ABPACDFN']""&("NDS"[ABPAPCOD) D
 ..S ABPA("UP",ABPAPCOD)=ABPA("UP",ABPAPCOD)+(+ABPADATA)
 .I ABPAPCOD="S" W ?46,$J(ABPA("CHK"),15) D
 ..S ABPA("STDAMT")=ABPA("STDAMT")+(+ABPADATA)
 ..I ABPA("CHK")']"" S REFUND=REFUND+(+ABPADATA*-1)
 .W ?65,ABPAPSDT S ABPAPAMT=ABPAPAMT+(+ABPADATA)
ENDLOOP K ABPAX S $P(ABPAX,"=",81)="" W !,ABPAX
 K DIR S DIR("A")="Select ACTION (1-File, 2-Edit, 3-Cancel): "
 S DIR(0)="SOAB^1:File;2:Edit;3:Cancel;",DIR("B")=3 D ^DIR
 G ^ABPAPD7:+Y=1,^ABPAPD6:+Y=2,^ABPAPD8
 ;
PROMPT W ?5,"Select FILE, EDIT or CANCEL (F/E/C)// " D SBRS
 I $D(DFOUT)!$D(DUOUT)!$D(DLOUT)!$D(DQOUT) D  G PROMPT
 .W *7,!,"Please enter ""F"", ""E"", or ""C"".",!
 I $D(DTOUT) G ^ABPAPD8
 S X=Y I "FEC"'[X D  G PROMPT
 .W *7,!,"Please enter ""F"", ""E"", or ""C"".",!
 I X="E" W "dit" G ^ABPAPD6
 I X="C" W "ancel" G ^ABPAPD8
 W "ile" G ^ABPAPD7
 ;
SBRS K DFOUT,DTOUT,DUOUT,DQOUT,DLOUT
 R Y:DTIME I '$T W *7 R Y:5 G SBRS:Y="." I '$T S (DTOUT,Y)="" Q
 I Y="/.," S (DFOUT,Y)="" Q
 I Y="" S DLOUT="" Q
 I Y="^" S (DUOUT,Y)="" Q
 I Y?1"?".E!(Y["^") S (DQOUT,Y)="" Q
 Q
