APSP416 ;IHS/ITSC/ENM - FIX 50.416 SUB DATA [ 11/22/2002  3:51 PM ]
 ;;6.0;IHS PHARMACY MODIFICATIONS;;11/22/02
EP1 ;EP TO CHECK AND FIX DRUG IDENTIFIER XREF'S
 W !!,?10,"START DRUG INGREDIENT SUB-FILE FIX......",!
 S A1=0,A2=0
 F  S A1=$O(^PS(50.416,A1)) Q:'A1  D EMCK
 W !,"Done!",!
 Q
EMCK ;
 S A2=0 F  S A2=$O(^PS(50.416,A1,1,A2)) Q:'A2  D EMWR
 Q
EMWR I '$D(^PS(50.416,A1,1,0)) D EMFIX
 Q
EMFIX ;
 S SUBCT=0,A2=0 F  S A2=$O(^PS(50.416,A1,1,A2)) Q:'A2  D COUNT
 S ^PS(50.416,A1,1,0)="^50.4161A^"_SUBCT_"^"_SUBCT
 S SUBCT=0
 Q
COUNT S SUBCT=SUBCT+1
 Q
