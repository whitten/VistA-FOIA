ACMRNT ; GENERATED FROM 'ACM RG NEXT REVIEW' PRINT TEMPLATE (#1389) ; 05/13/96 ; (FILE 9002241, MARGIN=80)
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
 D T Q:'DN  D N D N:$X>4 Q:'DN  W ?4 W "**********************    CASE REVIEW DATES    ****************************"
 D N:$X>4 Q:'DN  W ?4 W "LAST REVIEW DATE:"
 S X=$G(^ACM(41,D0,"DT")) W ?23 S Y=$P(X,U,8) D DT
 D N:$X>39 Q:'DN  W ?39 W "NEXT REVIEW DATE:"
 W ?58 S Y=$P(X,U,9) D DT
 K Y
 Q
HEAD ;
 W !,"--------------------------------------------------------------------------------",!!
