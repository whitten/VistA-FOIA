VEN8INI1 ; IHS/OIT/GIS - NO DESCRIPTION PROVIDED 20 Nov 2008 9:09 AM ; 
 ;;2.6;PCC+;**1,3**;MAR 23, 2011
 ; LOADS AND INDEXES DD'S
 ;
 K DIF,DIK,D,DDF,DDT,DTO,D0,DLAYGO,DIC,DIDUZ,DIR,DA,DFR,DTN,DIX,DZ D DT^DICRW S %=1,U="^",DSEC=1
 S NO=$P("I 0^I $D(@X)#2,X[U",U,%) I %<1 K DIFQ Q
ASK S DSEC=1 S %="^VEN(7.17)" K @%
 I $D(DIFKEP) F DIDIU=0:0 S DIDIU=$O(DIFKEP(DIDIU)) Q:DIDIU'>0  S DIU=DIDIU,DIU(0)=DIFKEP(DIDIU) D EN^DIU2
 D DT^DICRW K ^UTILITY(U,$J),^UTILITY("DIK",$J) D WAIT^DICD
 S DN="^VEN8I" F R=1:1:78 D @(DN_$$B36(R)) W "."
 F  S D=$O(^UTILITY(U,$J,"SBF","")) Q:D'>0  K:'DIFQ(D) ^(D) S D=$O(^(D,"")) I D>0  K ^(D) D IX
DATA W "." S (D,DDF(1),DDT(0))=$O(^UTILITY(U,$J,0)) Q:D'>0
 I DIFQR(D) S DTO=0,DMRG=1,DTO(0)=^(D),Z=^(D)_"0)",D0=^(D,0),@Z=D0,DFR(1)="^UTILITY(U,$J,DDF(1),D0,",DKP=DIFQR(D)'=2 F D0=0:0 S D0=$O(^UTILITY(U,$J,DDF(1),D0)) S:D0="" D0=-1 Q:'$D(^(D0,0))  S Z=^(0) D I^DITR
 K ^UTILITY(U,$J,DDF(1)),DDF,DDT,DTO,DFR,DFN,DTN G DATA
 ;
W S Y=$P($T(@X),";",2) W !,"NOTE: This package also contains "_Y_"S",! Q:'$D(DIFQ(0))
 S %=1 W ?6,"SHALL I WRITE OVER EXISTING "_Y_"S OF THE SAME NAME" D YN^DICN I '% W !?6,"Answer YES to replace the current "_Y_"S with the incoming ones." G W
 S:%=2 DIFQ(X)=0 K:%<0 DIFQ
 Q
 ;
OPT ;OPTION
RTN ;ROUTINE DOCUMENTATION NOTE
FUN ;FUNCTION
BUL ;BULLETIN
KEY ;SECURITY KEY
HEL ;HELP FRAME
DIP ;PRINT TEMPLATE
DIE ;INPUT TEMPLATE
DIB ;SORT TEMPLATE
DIS ;FORM
REM ;REMOTE PROCEDURE
 ;
SBF ;FILE AND SUB FILE NUMBERS
IX W "." S DIK="A" F %=0:0 S DIK=$O(^DD(D,DIK)) Q:DIK=""  K ^(DIK)
 S DA(1)=D,DIK="^DD("_D_"," D IXALL^DIK
 I $D(^DIC(D,"%",0)) S DIK="^DIC(D,""%""," G IXALL^DIK
 Q
B36(X) Q $$N(X\(36*36)#36+1)_$$N(X\36#36+1)_$$N(X#36+1)
N(%) Q $E("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",%)
