APSER ; GENERATED FROM 'APS PREPACK LOG EXPIRATION' PRINT TEMPLATE (#2819) ; 02/14/03 ; (FILE 9009031, MARGIN=80)
 G BEGIN
N W !
T W:$X ! I '$D(DIOT(2)),DN,$D(IOSL),$S('$D(DIWF):1,$P(DIWF,"B",2):$P(DIWF,"B",2),1:1)+$Y'<IOSL,$D(^UTILITY($J,1))#2,^(1)?1U1P1E.E X ^(1)
 S DISTP=DISTP+1,DILCT=DILCT+1 D:'(DISTP#100) CSTP^DIO2
 Q
DT I $G(DUZ("LANG"))>1,Y W $$OUT^DIALOGU(Y,"DD") Q
 I Y W $P("JAN^FEB^MAR^APR^MAY^JUN^JUL^AUG^SEP^OCT^NOV^DEC",U,$E(Y,4,5))_" " W:Y#100 $J(Y#100\1,2)_"," W Y\10000+1700 W:Y#1 "  "_$E(Y_0,9,10)_":"_$E(Y_"000",11,12) Q
 W Y Q
M D @DIXX
 Q
BEGIN ;
 S:'$D(DN) DN=1 S DISTP=$G(DISTP),DILCT=$G(DILCT)
 I $D(DXS)<9 F X=0:0 S X=$O(^DIPT(2819,"DXS",X)) Q:'X  S Y=$O(^(X,"")) F X=X:0 Q:Y=""  S DXS(X,Y)=^(Y),Y=$O(^(Y))
 D T Q:'DN  D N W ?0 X DXS(1,9) K DIP K:DN Y
 S X=$G(^APSPP(31,D0,0)) D N:$X>41 Q:'DN  W ?41,$E($P(X,U,1),1,7)
 D N:$X>50 Q:'DN  W ?50 S Y=$P(X,U,6) D DT
 D N:$X>63 Q:'DN  W ?63 S Y=$P(X,U,9) W:Y]"" $J(Y,10,0)
 S I(1)=15,J(1)=9009031.15 F D1=0:0 Q:$O(^APSPP(31,D0,15,D1))'>0  X:$D(DSC(9009031.15)) DSC(9009031.15) S D1=$O(^(D1)) Q:D1'>0  D:$X>75 T Q:'DN  D A1
 G A1R
A1 ;
 S X=$G(^APSPP(31,D0,15,D1,0)) D N:$X>4 Q:'DN  W ?4 S Y=$P(X,U,1) S Y=$S(Y="":Y,$D(^APSPP(31.2,Y,0))#2:$P(^(0),U,1),1:Y) W $E(Y,1,45)
 D N:$X>63 Q:'DN  W ?63 S Y=$P(X,U,2) W:Y]"" $J(Y,10,0)
 Q
A1R ;
 K Y
 Q
HEAD ;
 W !,?41,"PREPACK",?50,"EXPIRATION",?64,"NUMBER OF"
 W !,?0,"DRUG",?41,"NO.",?50,"DATE",?65,"PREPACKS"
 W !,?67,"NO. OF"
 W !,?65,"PREPACKS"
 W !,?4,"LOCATION",?67,"ISSUED"
 W !,"--------------------------------------------------------------------------------",!!
