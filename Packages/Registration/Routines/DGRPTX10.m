DGRPTX10 ; ;10/29/04
 D DE G BEGIN
DE S DIE="^DPT(",DIC=DIE,DP=2,DL=1,DIEL=0,DU="" K DG,DE,DB Q:$O(^DPT(DA,""))=""
 I $D(^(.21)) S %Z=^(.21) S %=$P(%Z,U,3) S:%]"" DE(1)=% S %=$P(%Z,U,4) S:%]"" DE(3)=% S %=$P(%Z,U,5) S:%]"" DE(5)=% S %=$P(%Z,U,6) S:%]"" DE(6)=% S %=$P(%Z,U,7) S:%]"" DE(7)=% S %=$P(%Z,U,9) S:%]"" DE(9)=% S %=$P(%Z,U,11) S:%]"" DE(10)=%
 I $D(^(.22)) S %Z=^(.22) S %=$P(%Z,U,7) S:%]"" DE(8)=%
 I $D(^(.33)) S %Z=^(.33) S %=$P(%Z,U,1) S:%]"" DE(18)=% S %=$P(%Z,U,10) S:%]"" DE(13)=%
 K %Z Q
 ;
W W !?DL+DL-2,DLB_": "
 Q
O D W W Y W:$X>45 !?9
 I $L(Y)>19,'DV,DV'["I",(DV["F"!(DV["K")) G RW^DIR2
 W:Y]"" "// " I 'DV,DV["I",$D(DE(DQ))#2 S X="" W "  (No Editing)" Q
TR R X:DTIME E  S (DTOUT,X)=U W $C(7)
 Q
A K DQ(DQ) S DQ=DQ+1
B G @DQ
RE G PR:$D(DE(DQ)) D W,TR
N I X="" G NKEY:$D(^DD("KEY","F",DP,DIFLD)),A:DV'["R",X:'DV,X:D'>0,A
RD G QS:X?."?" I X["^" D D G ^DIE17
 I X="@" D D G Z^DIE2
 I X=" ",DV["d",DV'["P",$D(^DISV(DUZ,"DIE",DLB)) S X=^(DLB) I DV'["D",DV'["S" W "  "_X
T G M^DIE17:DV,^DIE3:DV["V",P:DV'["S" X:$D(^DD(DP,DIFLD,12.1)) ^(12.1) I X?.ANP D SET I 'DDER X:$D(DIC("S")) DIC("S") I  W:'$D(DB(DQ)) "  "_% G V
 K DDER G X
P I DV["P" S DIC=U_DU,DIC(0)=$E("EN",$D(DB(DQ))+1)_"M"_$E("L",DV'["'") S:DIC(0)["L" DLAYGO=+$P(DV,"P",2) G:DV["*" AST^DIED D NOSCR^DIED S X=+Y,DIC=DIE G X:X<0
 G V:DV'["N" D D I $L($P(X,"."))>24 K X G Z
 I $P(DQ(DQ),U,5)'["$",X?.1"-".N.1".".N,$P(DQ(DQ),U,5,99)["+X'=X" S X=+X
V D @("X"_DQ) K YS
Z K DIC("S"),DLAYGO I $D(X),X'=U D:$G(DE(DW,"INDEX")) SAVEVALS G:'$$KEYCHK UNIQFERR^DIE17 S DG(DW)=X S:DV["d" ^DISV(DUZ,"DIE",DLB)=X G A
X W:'$D(ZTQUEUED) $C(7),"??" I $D(DB(DQ)) G Z^DIE17
 S X="?BAD"
QS S DZ=X D D,QQ^DIEQ G B
D S D=DIFLD,DQ(DQ)=DLB_U_DV_U_DU_U_DW_U_$P($T(@("X"_DQ))," ",2,99) Q
Y I '$D(DE(DQ)) D O G RD:"@"'[X,A:DV'["R"&(X="@"),X:X="@" S X=Y G N
PR S DG=DV,Y=DE(DQ),X=DU I $D(DQ(DQ,2)) X DQ(DQ,2) G RP
R I DG["P",@("$D(^"_X_"0))") S X=+$P(^(0),U,2) G RP:'$D(^(Y,0)) S Y=$P(^(0),U),X=$P(^DD(X,.01,0),U,3),DG=$P(^(0),U,2) G R
 I DG["V",+Y,$P(Y,";",2)["(",$D(@(U_$P(Y,";",2)_"0)")) S X=+$P(^(0),U,2) G RP:'$D(^(+Y,0)) S Y=$P(^(0),U) I $D(^DD(+X,.01,0)) S DG=$P(^(0),U,2),X=$P(^(0),U,3) G R
 X:DG["D" ^DD("DD") I DG["S" S %=$P($P(";"_X,";"_Y_":",2),";") S:%]"" Y=%
RP D O I X="" S X=DE(DQ) G A:'DV,A:DC<2,N^DIE17
I I DV'["I",DV'["#" G RD
 D E^DIE0 G RD:$D(X),PR
 Q
SET N DIR S DIR(0)="SV"_$E("o",$D(DB(DQ)))_U_DU,DIR("V")=1
 I $D(DB(DQ)),'$D(DIQUIET) N DIQUIET S DIQUIET=1
 D ^DIR I 'DDER S %=Y(0),X=Y
 Q
SAVEVALS S @DIEZTMP@("V",DP,DIIENS,DIFLD,"O")=$G(DE(DQ)) S:$D(^("F"))[0 ^("F")=$G(DE(DQ))
 I $D(DE(DW,"4/")) S @DIEZTMP@("V",DP,DIIENS,DIFLD,"4/")=""
 E  K @DIEZTMP@("V",DP,DIIENS,DIFLD,"4/")
 Q
NKEY W:'$D(ZTQUEUED) "??  Required key field" S X="?BAD" G QS
KEYCHK() Q:$G(DE(DW,"KEY"))="" 1 Q @DE(DW,"KEY")
BEGIN S DNM="DGRPTX10",DQ=1
1 S DW=".21;3",DV="FX",DU="",DLB="K-STREET ADDRESS [LINE 1]",DIFLD=.213
 S DE(DW)="C1^DGRPTX10"
 G RE
C1 G C1S:$D(DE(1))[0 K DB
 S X=DE(1),DIC=DIE
 X "S DGXRF=.213 D ^DGDDC Q"
 S X=DE(1),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
C1S S X="" G:DG(DQ)=X C1F1 K DB
 S X=DG(DQ),DIC=DIE
 ;
 S X=DG(DQ),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
C1F1 Q
X1 K:X[""""!($A(X)=45) X I $D(X) K:$L(X)>30!($L(X)<3) X I $D(X) S DFN=DA D K1^DGLOCK2
 I $D(X),X'?.ANP K X
 Q
 ;
2 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=2 D X2 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X2 S:X="" Y=.216
 Q
3 D:$D(DG)>9 F^DIE17,DE S DQ=3,DW=".21;4",DV="FX",DU="",DLB="K-STREET ADDRESS [LINE 2]",DIFLD=.214
 S DE(DW)="C3^DGRPTX10"
 G RE
C3 G C3S:$D(DE(3))[0 K DB
 S X=DE(3),DIC=DIE
 X "S DGXRF=.214 D ^DGDDC Q"
 S X=DE(3),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
C3S S X="" G:DG(DQ)=X C3F1 K DB
 S X=DG(DQ),DIC=DIE
 ;
 S X=DG(DQ),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
C3F1 Q
X3 K:X[""""!($A(X)=45) X I $D(X) K:$L(X)>30!($L(X)<3) X I $D(X) S DFN=DA D K1^DGLOCK2
 I $D(X),X'?.ANP K X
 Q
 ;
4 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=4 D X4 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X4 S:X="" Y=.216
 Q
5 D:$D(DG)>9 F^DIE17,DE S DQ=5,DW=".21;5",DV="FX",DU="",DLB="K-STREET ADDRESS [LINE 3]",DIFLD=.215
 G RE
X5 K:$L(X)>30!($L(X)<3) X I $D(X) S DFN=DA D K1^DGLOCK2
 I $D(X),X'?.ANP K X
 Q
 ;
6 S DW=".21;6",DV="FX",DU="",DLB="K-CITY",DIFLD=.216
 S DE(DW)="C6^DGRPTX10"
 G RE
C6 G C6S:$D(DE(6))[0 K DB
 S X=DE(6),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
C6S S X="" G:DG(DQ)=X C6F1 K DB
 S X=DG(DQ),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
C6F1 Q
X6 K:$L(X)>30!($L(X)<3) X I $D(X) S DFN=DA D K1^DGLOCK2
 I $D(X),X'?.ANP K X
 Q
 ;
7 D:$D(DG)>9 F^DIE17,DE S DQ=7,DW=".21;7",DV="P5'X",DU="",DLB="K-STATE",DIFLD=.217
 S DE(DW)="C7^DGRPTX10"
 S DU="DIC(5,"
 G RE
C7 G C7S:$D(DE(7))[0 K DB
 S X=DE(7),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
C7S S X="" G:DG(DQ)=X C7F1 K DB
 S X=DG(DQ),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
C7F1 Q
X7 I $D(X) S DFN=DA D K1^DGLOCK2
 Q
 ;
8 D:$D(DG)>9 F^DIE17,DE S DQ=8,DW=".22;7",DV="FOX",DU="",DLB="K-ZIP+4",DIFLD=.2207
 S DQ(8,2)="S Y(0)=Y D ZIPOUT^VAFADDR"
 S DE(DW)="C8^DGRPTX10"
 G RE
C8 G C8S:$D(DE(8))[0 K DB
 S X=DE(8),DIC=DIE
 D KILL^DGREGDD1(DA,.218,.21,8,$E(X,1,5))
C8S S X="" G:DG(DQ)=X C8F1 K DB
 S X=DG(DQ),DIC=DIE
 D SET^DGREGDD1(DA,.218,.21,8,$E(X,1,5))
C8F1 Q
X8 K:X[""""!($A(X)=45) X I $D(X) S DFN=DA D K1^DGLOCK2 I $D(X) K:$L(X)>15!($L(X)<5) X I $D(X) D ZIPIN^VAFADDR
 I $D(X),X'?.ANP K X
 Q
 ;
9 D:$D(DG)>9 F^DIE17,DE S DQ=9,DW=".21;9",DV="FX",DU="",DLB="K-PHONE NUMBER",DIFLD=.219
 S DE(DW)="C9^DGRPTX10"
 G RE
C9 G C9S:$D(DE(9))[0 K DB
 S X=DE(9),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".219;" D AVAFC^VAFCDD01(DA)
 S X=DE(9),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
C9S S X="" G:DG(DQ)=X C9F1 K DB
 S X=DG(DQ),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".219;" D AVAFC^VAFCDD01(DA)
 S X=DG(DQ),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
C9F1 Q
X9 K:$L(X)>20!($L(X)<4) X I $D(X) S DFN=DA D K1^DGLOCK2
 I $D(X),X'?.ANP K X
 Q
 ;
10 D:$D(DG)>9 F^DIE17,DE S DQ=10,DW=".21;11",DV="F",DU="",DLB="K-WORK PHONE NUMBER",DIFLD=.21011
 G RE
X10 K:$L(X)>20!($L(X)<4) X
 I $D(X),X'?.ANP K X
 Q
 ;
11 S DQ=12 ;@30
12 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=12 D X12 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X12 I $S('$D(^DPT(DFN,.21)):1,$P(^(.21),U,1)="":1,1:0) S Y=.331
 Q
13 S DW=".33;10",DV="RSX",DU="",DLB="E-EMER. CONTACT SAME AS NOK?",DIFLD=.3305
 S DU="Y:YES;N:NO;"
 S Y="NO"
 G Y
X13 I $D(X),X="Y" D K1^DGLOCK2
 Q
 ;
14 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=14 D X14 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X14 I X'="Y" S Y=.331
 Q
15 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=15 D X15 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X15 S X=$S($D(^DPT(DA,.21)):^(.21),1:"") S:X'="" ^(.33)=$P(X_"^^^^^^^^^^^",U,1,9)_U_$P(^(.33),U,10)_U_$P(X,U,11)
 Q
16 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=16 D X16 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X16 S:$D(^DPT(DFN,.22)) $P(^(.22),U,1)=$P(^(.22),U,7)
 Q
17 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=17 D X17 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X17 S Y=.33011
 Q
18 S DW=".33;1",DV="FX",DU="",DLB="E-NAME",DIFLD=.331
 S DE(DW)="C18^DGRPTX10",DE(DW,"INDEX")=1
 G RE
C18 G C18S:$D(DE(18))[0 K DB
 S X=DE(18),DIC=DIE
 X "S DGXRF=.331 D ^DGDDC Q"
C18S S X="" G:DG(DQ)=X C18F1 K DB
 D ^DGRPTX11
C18F1 N X,X1,X2 S DIXR=46 D C18X1(U) K X2 M X2=X D C18X1("O") K X1 M X1=X
 I $G(X(1))]"" D
 . I '$G(XUNOTRIG) N XUNOTRIG S XUNOTRIG=1 D DELCOMP^XLFNAME2(2,.DA,.331,1.07) Q
 K X M X=X2 I $G(X(1))]"" D
 . I '$G(XUNOTRIG) N XUNOTRIG S XUNOTRIG=1,DG20NAME=X D NARY^DPTNAME(.DG20NAME),UPDCOMP^XLFNAME2(2,.DA,.331,.DG20NAME,1.07,+$P($G(^DPT(DA,"NAME")),U,7),"CL35") K DG20NAME Q
 G C18F2
C18X1(DION) K X
 S X(1)=$G(@DIEZTMP@("V",2,DIIENS,.331,DION),$P($G(^DPT(DA,.33)),U,1))
 S X=$G(X(1))
 Q
C18F2 Q
X18 K:$L(X)>35!($L(X)<3) X I $D(X) K DG20NAME S DG20NAME=X,(X,DG20NAME)=$$FORMAT^DPTNAME(.DG20NAME,3,35) K:'$L(X) X,DG20NAME
 I $D(X),X'?.ANP K X
 Q
 ;
19 D:$D(DG)>9 F^DIE17,DE S Y=U,DQ=19 D X19 D:$D(DIEFIRE)#2 FIREREC^DIE17 G A:$D(Y)[0,A:Y=U S X=Y,DIC(0)="F",DW=DQ G OUT^DIE17
X19 S:X="" Y="@40"
 Q
20 D:$D(DG)>9 F^DIE17 G ^DGRPTX12
