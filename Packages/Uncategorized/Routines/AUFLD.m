%AUFLD ;PRINT IHS DICTIONARY FIELDS [ 06/16/89  1:15 PM ]
 ;IHS-OKLA CITY AREA OFFICE-LMD
 ;CHANGED LINE L6 TO COMMENT TO ALLOW REQUEST FOR NEXT FILE
 ;CHANGED LINE END TO GOTO S1.  THIS ALLOWS CLOSE OF DEVICE
 ;SO THAT SLAVE PRINTERS WILL FUNCTION TJF 5/18/88
 ;LINE 'END' TO NOT WRITE @IOF IF OUTPUT TO TERMINAL; RPMS/GTH; 20DEC88
 ;CHANGED 3 OCCURRENCES OF X]"@" TO X'=+X MFD 6/8/89
 ;MODIFIED (COMPRESSED) HEADER PRINTING MFD 6/16/89
S1 W !! S DIC="^DIC(",DIC(0)="QAZEM" D ^DIC G:Y<0 QUIT S (DIC,MG("DIC"))=+Y
L1 G QUE
 W !! S IOP="Q" D ^%ZIS
L2 S PG=0 K MGIO S:$D(AU("PRINT")) MGIO=+$I S IOP=IO D ^%ZIS U IO D HEADING
L4 W ! S (D,SK1,SK2,SK3)=0 K MG("PAT") S:DIC=9000001 MG("PAT")=""
L5 S D=$O(^DD(DIC,D)) G L6:D'=+D S MF=($P(^DD(DIC,D,0),"^",2)) G L7:+MF>0 S Q=$P(^DD(DIC,D,0),"^",4),Q1=$P(Q,";",1),Q2=$P(Q,";",2) W:Q1'=SK1 ! S SK1=Q1 W ?5,D,?15,$P(^DD(DIC,D,0),"^",1)
 I $D(^DD(DIC,D,9.01))&$D(MG("PAT")) I $P(^DD(DIC,D,9.01),"^",1)=2 W "/",$P($P(^DD(DIC,D,9.01),"^",2),";",1)
L5A G L5B:Q1=" " W ?45,"DFN,",Q1,?61,Q2,?74,$S(MF["D":"D",MF["P":"P",MF["F":"F",MF["S":"S",MF["W":"W",MF["N":"N",1:"?") G L5C
L5B W ?45,"COMP"
L5C W ! D HEADING:$Y>(IOSL-3) G S1:Y="^",L5
L6 ;I $D(MGIO) I MGIO=IO K MG,MGIO G S1
END W:IOSL>24 @IOF X ^%ZIS("C") K MG,MGIO G S1
QUIT K %DT,AU,C,D,DD,DDD,DDDD,DIC,EE,EEE,EEEE,G,GEE,GG,GGG,HR,I,IO,IOBS,IOF,IOM,ION,IOP,IOPAR,IOS,IOSL,IOST,IOT,MF,MF,MG,MGIO,MIN,MMF,MMMF,PG,POP,Q,Q1,Q2,SK1,SK2,SK3,TITLE,TM,TME,U,USER,X,Y
 Q
L7 W !,?5,D,?15,$P(^DD(DIC,D,0),"^",1),"  (",+MF,")",! S DD=0,GG=$P(($P(^DD(DIC,D,0),"^",4)),";",1)
L8 S DD=$O(^DD(+MF,DD)) G L10:DD'=+DD S MMF=$P(^DD(+MF,DD,0),"^",2) G L20:+MMF>0 S G=$P(^DD(+MF,DD,0),"^",4),DDD=$P(G,";",1),DDDD=$P(G,";",2)
L9 W:SK2'=DDD ! S SK2=DDD W ?7,DD,?17,$P(^DD(+MF,DD,0),"^",1) W:DDD=" " ?47,"COMP" G L9A:DDD=" "
 W ?47,"DFN,",GG,",I,",DDD,?63,DDDD,?76,$S(MMF["D":"D",MMF["P":"P",MMF["F":"F",MMF["S":"S",MMF["W":"W",MMF["N":"N",1:"?")
L9A W ! I $Y>(IOSL-3) D HEADING G:Y="^" S1 W ?5,D,?15,$P(^DD(DIC,D,0),"^",1),"  (cont.)",!
L9B G L8
L10 G L5
L20 W !,?7,DD,?17,$P(^DD(+MF,DD,0),"^",1),"  (",+MMF,")",! S EE=0,GGG=$P(($P(^DD(+MF,DD,0),"^",4)),";",1)
L21 S EE=$O(^DD(+MMF,EE)) G L25:EE'=+EE S MMMF=$P(^DD(+MMF,EE,0),"^",2),GEE=$P(^DD(+MMF,EE,0),"^",4),EEE=$P(GEE,";",1),EEEE=$P(GEE,";",2)
L22 W:SK3'=EEE ! S SK3=EEE W ?9,EE,?19,$P(^DD(+MMF,EE,0),"^",1) W:EEE=" " ?49,"COMP" G L23:EEE=" " W:EEEE=0 "   (",+$P(^DD(+MMF,EE,0),"^",2),")",?49,"MUL" G L23:EEEE=0
 W ?49,"DFN,",GG,",1,",GGG,",I,",EEE,?67,EEEE,?78,$S(MMMF["D":"D",MMMF["P":"P",MMMF["F":"F",MMMF["S":"S",MMMF["W":"W",MMMF["N":"N",1:"?")
L23 W ! I $Y>(IOSL-3) D HEADING G:Y="^" S1 W ?5,D,?15,$P(^DD(DIC,D,0),"^",1),"  (cont.)",!,?7,DD,?17,$P(^DD(+MF,DD,0),"^",1),"  (cont.)",!
L24 G L21
L25 G L8
HEADING I PG'=0,IOSL=24 R !,"Press RETURN...",Y:999 Q:Y="^"
 I PG'=0 S PG=PG+1 W # G H2
 S PG=PG+1,TITLE="I.H.S.  DICTIONARY FIELDS",TM=$P($H,",",2),HR=TM\3600,MIN=TM#3600\60 S:MIN<10 MIN="0"_MIN S TME=HR_":"_MIN,USER=""
 W #,!,TME,?80-$L(TITLE)\2,TITLE,!,?80-$L(^DD("SITE"))\2,^DD("SITE"),!
UCI X ^%ZOSF("UCI") S MG("UCI")="UCI: "_$P(Y,",",1) W ?80-$L(MG("UCI"))\2,MG("UCI")
 I '$D(DT) S %DT="",X="T" D ^%DT S DT=Y
 S Y=DT X ^DD("DD") W !,?80-$L("as of "_Y)\2,"as of ",Y,!!
H2 W ?80-$L("FILE: "_$P(^DIC(DIC,0),"^",1))\2,"FILE: ",$P(^DIC(DIC,0),"^",1),!,?80-$L("GLOBAL: "_^DIC(DIC,0,"GL"))\2,"GLOBAL: ",^DIC(DIC,0,"GL"),?70,"page ",PG,!,?80-$L("FILE #: "_DIC)\2,"FILE #: ",DIC,!
H3 W ?5,"FIELD #",?15,"FIELD NAME",?43,"SUBSCRIPT",?60,"PIECE #",?72,"TYPE",!,"=====1 2 3" F I=1:1:69 W "="
 W ! Q
PRQ S DIC=^%ZTSK(ZTSK,"DIC")
PRQ1 S:$D(^%ZTSK(ZTSK,"SITE")) DUZ(2)=^%ZTSK(ZTSK,"SITE") S U="^" K ^%ZTSK(ZTSK) G L2
QUE S:'$D(DUZ) DUZ=0 D ^%AUQUE G L2:$D(AU("PRINT")),QUIT:'$D(AU("QUE"))
QUE1 S ^%ZTSK(ZTSK,0)="PRQ^%AUFLD"_^%ZTSK(ZTSK,0),^("DIC")=MG("DIC")
QUEEND K ZTSK G S1
