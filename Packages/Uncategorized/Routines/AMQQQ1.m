AMQQQ1 ; IHS/CMI/THL - SCRIPT INTERPRETER FOR MULTIPLES ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;-----
 I AMQQFTYP="G"!(AMQQFTYP="L") D TAX,CHECK I  Q
VAR S AMQQSQAA=AMQQUATN
 S AMQQSQSN=AMQQATN
 S AMQQSQST=AMQQFTYP
 S AMQQZZFN=0
 S AMQQZZNN=$D(AMQQXXNN)
RUN F  S AMQQXXGN=$O(@AMQQXXGG) Q:'AMQQXXGN  S AMQQZZZZ=@AMQQXXGI I "+*"'[$E(AMQQZZZZ) D LEV I  Q
 I '$D(AMQQFAIL),$D(@AMQQXXND)=11,AMQQZZNN S @AMQQXXND@(AMQQZZFN+1,1)="NULL"
EXIT K AMQQZZZZ,AMQQZZNN,AMQQXXNN,AMQQZZAA,AMQQZZFN,AMQQXXMN
 I AMQQXXGN="" S AMQQXXGN=999
 Q
 ;
LEV F I=0:1 S %=$E(AMQQZZZZ,I+1) Q:%'="."
 I I<AMQQXXLV S AMQQXXGN=AMQQXXGN-1,AMQQXXLV=I Q
 I I>AMQQXXLV S AMQQFAIL=10 I 1 Q
 S AMQQZZZZ=$E(AMQQZZZZ,AMQQXXLV+1,999)
 D ANAL
CHECK I $D(AMQQQUIT)!($D(AMQQFAIL))
 Q
 ;
ANAL S (AMQQZZAA,X)=$P(AMQQZZZZ,";")
 I AMQQXXLV>1,X="BETWEEN" S @AMQQXXND=@AMQQXXND_";"_$P(AMQQZZZZ,";",2,3) Q
 I $E(X)="-" S AMQQZZMN="",(X,AMQQZZAA)=$E(X,2,999)
 S AMQQSQNN=AMQQUSQN
 I $G(AMQQZZAA)="NULL" S AMQQZZFN=AMQQZZFN+1,@AMQQXXND@(AMQQZZFN,1)="NULL" Q
 D EN1^AMQQSQA0
 I $D(AMQQXX),'$D(Y),X="NULL" G SET
 I $G(Y)=-1 K AMQQZZMN S AMQQFAIL=10 Q
 S AMQQZZFN=AMQQZZFN+1
 S AMQQSQCT=$P(^AMQQ(5,+Y,0),U,20)
 I AMQQSQCT="L" S AMQQZZAA=Y D LINK Q
 I AMQQSQCT="M" D MULT Q
 S AMQQZZCC=Y
 S %=$P(^AMQQ(5,+Y,0),U,21)
 S AMQQSYMB=$P(^(0),U,6)
 S AMQQNOCO=$P(^(0),U,8)
 S AMQQFTYP=$P(^AMQQ(4,%,0),U)
 I $P(^AMQQ(5,+Y,0),U,20)="C" S AMQQCOMP="" G SET
 D VALUE
 I $D(AMQQFAIL) Q
SET S @AMQQXXND@(AMQQZZFN,1)=AMQQZZCC_";"_AMQQCOMP
 Q
 ;
TAX D TAX^AMQQQ0
 D CHECK
 I  Q
 S $P(@AMQQXXND,";",2,3)="MTAX;"_AMQQURGN
 I 0
 Q
 ;
LINK S @AMQQXXND@(AMQQZZFN,1)=Y
 S AMQQZZZZ=$P(AMQQZZZZ,";",2,9)
 D ATT
 I $D(AMQQFAIL) Q
 Q
 ;
MULT ; SUBQUERIES
 S %=AMQQXXND
 N AMQQXXND,AMQQATN,AMQQXXNN,AMQQFTYP
 S %=$E(%,1,$L(%)-1)_","_AMQQZZFN_",1)"
 S AMQQXXND=%
 S AMQQATN=+Y
 S %=^AMQQ(5,AMQQATN,0)
 S %=$P(%,U,5)
 S %=^AMQQ(1,%,0)
 S %=$P(%,U,5)
 S AMQQFTYP=$P(^AMQQ(4,%,0),U)
 I $D(AMQQZMN) K AMQQZZMN S AMQQXXNN=""
 I AMQQFTYP="G"!(AMQQFTYP="L") N AMQQXXXX S AMQQXXXX=AMQQZZZZ,AMQQXXMT=Y D TAX,CHECK I  Q
 I $D(AMQQXXMT) S Y=AMQQXXMT
 S @AMQQXXND=Y_$S($D(AMQQXXMT):";MTAX;",1:";MULT;")_AMQQURGN
 K AMQQXXMT
RECURSE S AMQQXXLV=AMQQXXLV+1
 N AMQQZZAA,AMQQZZFN,AMQQZZMN,AMQQZZNN,AMQQZZZZ,AMQQSQAA,AMQQSQSN,AMQQSQST
 D VAR
 S AMQQXXLV=AMQQXXLV-1
 S AMQQXXGN=AMQQXXGN-1
 Q
 ;
VALUE S Y=AMQQZZCC
 S X=$P(AMQQZZZZ,";",2,3)
 S AMQQATNM=$P(Y,U,2)
 K AMQQCOMP
 I AMQQFTYP="U" S AMQQCOMP=$P(AMQQZZZZ,";",2) Q
 D ^AMQQAV
 I '$D(AMQQCOMP) S AMQQFAIL=10
 Q
 ;
ATT N AMQQFTYP,Y,AMQQCOND
 S Y=AMQQZZAA
 S %=$P(^AMQQ(5,+Y,0),U,5)
 S %=$P(^AMQQ(1,%,0),U,5)
 S AMQQFTYP=$P(^AMQQ(4,%,0),U)
 S X=$P(AMQQZZZZ,";")
 D CONDIC^AMQQAC
 I Y=-1 S AMQQFAIL=10 Q
 N AMQQZZCC,AMQQCOMP
 S AMQQZZCC=Y
 N Y,AMQQNOCO
 S AMQQNOCO=+$P(^AMQQ(5,+AMQQZZCC,0),U,8)
 D VALUE
 I Y=-1 S AMQQFAIL=10 Q
 S @AMQQXXND@(AMQQZZFN,1,1,1)=AMQQZZCC_";"_AMQQCOMP
 Q
 ;
