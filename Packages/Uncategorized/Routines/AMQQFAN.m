AMQQFAN ; IHS/CMI/THL - FREE TEXT ANALYTIC ROUTINES ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;-----
VAR S AMQQSYMB=$P(AMQQX,";")
 S AMQQVAL1=$P(AMQQX,";",2)
 S AMQQVAL2=$P(AMQQX,";",3)
 K AMQQNOT
SYMBOL I $E(AMQQSYMB)="'" S AMQQNOT="",AMQQSYMB=$E(AMQQSYMB,2,99)
 S %=$F("[]=$-#?",AMQQSYMB)
 I '%
 E  D @("A"_(%-1))
 I ('$D(AMQQNOT)=$T)
EXIT K %,AMQQSYMB,AMQQVAL1,AMQQVAL2,AMQQVALU,AMQQNOT,AMQQX
 Q
 ;
A1 I AMQQVALU[AMQQVAL1
 Q
 ;
A2 I AMQQVALU]AMQQVAL1
 Q
 ;
A3 I AMQQVALU=AMQQVAL1
 Q
 ;
A4 I $E(AMQQVALU,1,$L(AMQQVAL1))=AMQQVAL1
 Q
 ;
A5 I AMQQVALU]AMQQVAL1,AMQQVALU']AMQQVAL2
 Q
 ;
A6 I $E(AMQQVALU,$L(AMQQVALU)-$L(AMQQVAL1)+1,250)=AMQQVAL1
 Q
 ;
A7 X ("I AMQQVALU?"_AMQQVAL1)
 Q
 ;
SER ; ENTRY POINT FOR COMPUTING SEARCH EFFICIENCY RATING
 ; ENTRY POINT FROM EXECUTING ^AMQQ(1,D0,3)
 N AMQQSYMB,AMQQVALU,AMQQVAL1,AMQQVAL2,AMQQNOT,%
 S AMQQSYMB=AMQQESBL,AMQQVALU=AMQQEVAL,AMQQVAL1=$P(AMQQECPR,";"),AMQQVAL2=$P(AMQQECPR,";",2)=""
 D SYMBOL
 Q
 ;
NAME ; ENTRY POINT FROM METADICTIONARY (PATIENT;NAME)
 N AMQQSYMB,AMQQVALU,AMQQVAL1,AMQQVAL2,AMQQNOT,%
 S AMQQSYMB=AMQP(.11)
 S AMQQVALU=AMQP(.1)
 S AMQQVAL1=AMQP(.101)
 S AMQQVAL2=""
 D SYMBOL
 Q
 ;
TEXT ; ENTRY POINT FROM AMQQMULT
 N X,Y,Z,%,AMQQSYMB,AMQQNOT
 S X=AMQQVAL1
 S Y=AMQQVAL2
 S Z=AMQQVALU
 N AMQQVAL1,AMQQVAL2
 S AMQQSYMB=$P(X,":")
 I X="'<:'>" S AMQQSYMB="-"
 S AMQQVAL1=$P(Y,":")
 S AMQQVAL2=$P(Y,":",2)
 D SYMBOL
 I  S AMQQVALU=Z
 Q
 ;
START ; ENTRY POINT FROM METADICTIONARY
 N X,Y,Z,%
 S Y=AMQP(.11)
 I Y'="$",Y'="=" Q
 S %=AMQP(.101)
 S X=$E(%,$L(%))
 S X=$A(X)-1
 S X=$C(X)
 S AMQP(.1)=$E(%,1,$L(%)-1)_X
 I Y="$" S AMQP(.111)=AMQP(.101)_"~" Q
 S AMQP(.111)=AMQP(.101)
 Q
 ;
