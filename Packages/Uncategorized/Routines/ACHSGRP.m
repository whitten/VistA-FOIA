ACHSGRP ; IHS/ITSC/PMF - CHS DRG GROUPER MODIFIED FROM AICDGRP & AICDGRP1 ;   [ 10/16/2001   8:16 AM ]
 ;;3.1;CONTRACT HEALTH MGMT SYSTEM;;JUN 11, 2001
ARR ;EP
 S Y=ACHSICDX(ACHSICDJ),Y(0)=ACHSICDX(ACHSICDJ,0),ACHSMDC=$P(Y(0),U,5) I ACHSMDC=469 Q
CD K RG
 S ACHSPD=$P(Y(0),U,2),RG=0 I ACHSMDC=12 S ACHSMDC=$S(ACHSSEX="F":13,1:12)
 F ACHSNDR=1:1 S RG=$O(^ICD9(+Y,"DR",RG)) Q:RG'>0  S RG(RG)=""
MORE I ACHSMDC=5,'ACHSNOR!(ACHSOR'["O") D MI,WRT:RG>0 Q
 I ACHSMDC=18,ACHSOR["O" S RG=415 D WRT Q
 I ACHSMDC=19,ACHSOR["O" S RG=424 D WRT Q
 I ACHSMDC=23,ACHSOR["O" S RG=461 D WRT Q
 I ACHSMDC=14 D DGDRG14,WRT Q
 I ACHSMDC=20,ACHSDAM S RG=433 D WRT Q
 I ACHSMDC=22 S:'$D(ACHSTAC) ACHSTAC=0 S RG=$S(ACHSTAC:456,ACHSPD["*"!(ACHSSD["*"):457,ACHSOR'["O":460,1:0) I RG D WRT Q
 I '$D(ACHSTRS) S ACHSTRS=""
 I '$D(ACHSEXP) S ACHSEXP=""
 I ACHSMDC=15,ACHSTRS!ACHSEXP S RG=385 D WRT Q
 I ACHSMDC=15,ACHSSD1 S RG=391 D WRT Q
 I 'ACHSNOR,ACHSNDR<3 S RG=$O(RG(0)) D:RG'>0 469 D WRT Q
 I 'ACHSNOR S RG=$O(RG(0)) X:$D(^ICD(RG,"MC")) ^ICD(RG,"MC") D WRT Q
 ;
 D DGDRG6:ACHSMDC=6,DGDRG8:ACHSMDC=8,DGDRG2:ACHSMDC=2,DGDRG3:ACHSMDC=3 S RG=$O(ACHSORG(ACHSMDC,0)) G:RG'>0 NOP X:$D(^ICD(RG,"MC")) ^ICD(RG,"MC") D WRT Q
 ;
NOP I ACHSOR["O",ACHSMDC'=20 D 468 Q
D S RG=$O(RG(0)) X:$D(^ICD(RG,"MC")) ^ICD(RG,"MC") D:RG'>0 469 D WRT Q
WRT D:'$D(RG) 469 Q:RG<0  S ACHSGCAL=$G(^ICD(RG,0)),ACHSICDN=$P(ACHSGCAL,U,7) I ACHSICDN'="" D SETDRG
 Q
SETDRG I '$D(ACHSICDE(9-ACHSICDN)) S ACHSICDE(9-ACHSICDN)=ACHSGCAL,ACHSICDE(9-ACHSICDN,1)=Y(0) Q
 I ACHSGCAL'=ACHSICDE(9-ACHSICDN) S ACHSICDN=ACHSICDN+.000001 G SETDRG
 F I=1:1 I '$D(ACHSICDE(9-ACHSICDN,I)) S ACHSICDE(9-ACHSICDN,I)=Y(0) Q
 Q
 ;
469 S RG=469 W *7,!!,"DRG= 469 PDX INVALID AS DISCHARGE DIAGNOSIS" Q
468 ;
 S ACHSOR="",ACHSNOR=0
 D CD
 Q
MI I ACHSPD["I"!(ACHSSD["I") S RG=$S($S($D(ACHSEXP):ACHSEXP,1:0):123,ACHSPD["V"!(ACHSSD["V"):121,1:122) Q
KILLS ;EP
 K ACHSICDX,ACHSICDE,ACHSICDI,ACHSICDJ,ACHSICDK,ACHSICDL,ACHSICDN,ACHSICDT
 K ACHSDGFL,ACHSDAM,ACHSPT,Q,RG,ACHSSD,ACHSSD1,T,ACHSTAC,Y,DIC,ACHSGCAL,I,L,ACHSMDC,ACHSNDR,ACHSNOR,ACHSOR,ACHSPD,%,%DT,ACHSSEX,ACHSEXP,ACHSORG,ACHSTRS,ACHSNSD,C,ACHSWD2,W,S,AGE
 Q
DGDRG2 ;
 Q:$O(ACHSORG(ACHSMDC,0))'>0  K JJ F JJ=0:0 S JJ=$O(ACHSORG(ACHSMDC,JJ)) Q:JJ'>0  D F
 G END
 ;
F ;
 I JJ=36 S JJ(1)=JJ Q
 I JJ=37 S JJ(2)=JJ Q
 I JJ=38 S JJ(5)=JJ Q
 I JJ=39 S JJ(4)=JJ Q
 I JJ=40 S JJ(6)=JJ Q
 I JJ=42 S JJ(3)=JJ
 Q
DGDRG3 ;
 Q:$O(ACHSORG(ACHSMDC,0))'>0  K JJ F JJ=0:0 S JJ=$O(ACHSORG(ACHSMDC,JJ)) Q:JJ'>0  D F3
 G END
F3 ;
 I JJ=49 S JJ(1)=JJ Q
 I JJ=50 S JJ(2)=JJ Q
 I JJ=51 S JJ(3)=JJ Q
 I JJ=52 S JJ(4)=JJ Q
 I JJ=53 S JJ(5)=JJ Q
 I JJ=55 S JJ(6)=JJ Q
 I JJ=56 S JJ(7)=JJ Q
 I JJ=57 S JJ(9)=JJ Q
 I JJ=59 S JJ(8)=JJ Q
 I JJ=61 S JJ(10)=JJ Q
 I JJ=63 S JJ(11)=JJ
 Q
DGDRG6 ;
 Q:$O(ACHSORG(ACHSMDC,0))'>0  K JJ F JJ=0:0 S JJ=$O(ACHSORG(ACHSMDC,JJ)) Q:JJ'>0  D F6
 G END
F6 ;
 I JJ=146 S JJ(2)=JJ Q
 I JJ=148 S JJ(3)=JJ Q
 I JJ=150 S JJ(7)=JJ Q
 I JJ=152 S JJ(4)=JJ Q
 I JJ=154 S JJ(1)=JJ Q
 I JJ=157 S JJ(8)=JJ Q
 I JJ=159 S JJ(6)=JJ Q
 I JJ=161 S JJ(6.1)=JJ Q
 I JJ=164 S JJ(5)=JJ Q
 I JJ=166 S JJ(5.5)=JJ Q
 I JJ=168 S JJ(9)=JJ Q
 I JJ=170 S JJ(10)=JJ
 Q
DGDRG8 ;
 Q:$O(ACHSORG(ACHSMDC,0))'>0  K JJ F JJ=0:0 S JJ=$O(ACHSORG(ACHSMDC,JJ)) Q:JJ'>0  D F8
 G END
F8 ;
 I JJ=209 S JJ(1)=JJ Q
 I JJ=210 S JJ(2)=JJ Q
 I JJ=213 S JJ(3)=JJ Q
 I JJ=214 S JJ(4)=JJ Q
 I JJ=216 S JJ(5)=JJ Q
 I JJ=217 S JJ(6)=JJ Q
 I JJ=218 S JJ(7)=JJ Q
 I JJ=221 S JJ(8)=JJ Q
 I JJ=223 S JJ(10)=JJ Q
 I JJ=225 S JJ(11)=JJ Q
 I JJ=226 S JJ(12)=JJ Q
 I JJ=228 S JJ(13)=JJ Q
 I JJ=229 S JJ(13.5)=JJ Q
 I JJ=230 S JJ(9)=JJ Q
 I JJ=231 S JJ(9.5)=JJ Q
 I JJ=232 S JJ(14)=JJ Q
 I JJ=233 S JJ(15)=JJ
 Q
END ;
 S JJ=$O(JJ(0)) Q:JJ'>0  S JJ=JJ(JJ) K ACHSORG S ACHSORG(ACHSMDC,JJ)="" K JJ Q
 ;
DGDRG14 ;
 G POST:ACHSPD'["D" I ACHSOR["c" S RG=$S(ACHSSD["C":370,1:371) Q
NOV I ACHSOR["s"!(ACHSOR["g") S RG=$S(ACHSOR["s":374,1:375) Q
 S RG=$S(ACHSSD["n"!(ACHSPD["n"):372,1:373) Q
 ;
POST I ACHSPD["d" S RG=$S(ACHSOR["O":377,1:376) Q
 S RG=$O(RG(0)) I RG'>0 S RG=469 Q
 X ^ICD(RG,"MC") Q
