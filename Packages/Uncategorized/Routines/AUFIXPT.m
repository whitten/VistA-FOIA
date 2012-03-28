%AUFIXPT ; FIX ALL "PT" NODES [ 07/29/87  9:19 AM ]
 ;
 ; This routine fixes all "PT" nodes for files 1 through the
 ; highest file number in the current UCI.
 ;
 W !!,"This routine insures the ""PT"" node of each FileMan file is correct.",!
 W !!,"Now checking false positives.",!
 S U="^"
 S AUFFILE=.99999999 F AUFL=0:0 S AUFFILE=$O(^DD(AUFFILE)) Q:AUFFILE'=+AUFFILE  I $D(^DD(AUFFILE,0,"PT")) W !,AUFFILE D FPOS
 W !!,"Now checking false negatives.",!
 D FNEG
 K AUFFILE,AUFL
 W !!,"DONE",!
 Q
 ;
FPOS ; CHECK FOR FALSE POSITIVES
 S AUFPFILE="" F AUFL=0:0 S AUFPFILE=$O(^DD(AUFFILE,0,"PT",AUFPFILE)) Q:AUFPFILE=""  S AUFPFLD="" F AUFL=0:0 S AUFPFLD=$O(^DD(AUFFILE,0,"PT",AUFPFILE,AUFPFLD)) Q:AUFPFLD=""  D CHKIT
 K AUFPFILE,AUFPFLD,AUFX
 Q
 ;
CHKIT ;
 W "."
 I '$D(^DD(AUFPFILE)) W "|" K ^DD(AUFFILE,0,"PT",AUFPFILE) Q
 I '$D(^DD(AUFPFILE,AUFPFLD)) W "|" K ^DD(AUFFILE,0,"PT",AUFPFILE,AUFPFLD) Q
 S AUFX=$P(^DD(AUFPFILE,AUFPFLD,0),U,2)
 I AUFX["P",AUFX[AUFFILE Q
 I AUFX["V",$D(^DD(AUFPFILE,AUFPFLD,"V","B",AUFFILE)) Q
 W "|" K ^DD(AUFFILE,0,"PT",AUFPFILE,AUFPFLD)
 Q
 ;
FNEG ; CHECK FOR FALSE NEGATIVES
 S AUFFILE=.99999999 F AUFL=0:0 S AUFFILE=$O(^DD(AUFFILE)) Q:AUFFILE'=+AUFFILE  W !,AUFFILE S AUFFLD=0 F AUFL=0:0 S AUFFLD=$O(^DD(AUFFILE,AUFFLD)) Q:AUFFLD'=+AUFFLD  D:$D(^(AUFFLD,0))#2 PTRCHK
 K AUFFILE,AUFFLD,AUFX,AUFI
 Q
 ;
PTRCHK ;
 S AUFX=$P(^(0),U,2)
 I AUFX["V" D PTRCHK2 Q
 Q:AUFX'["P"
 F AUFI=1:1:$L(AUFX)+1 Q:$E(AUFX,AUFI)?1N
 Q:AUFI>$L(AUFX)
 S AUFX=$E(AUFX,AUFI,999),AUFX=+AUFX
 Q:'AUFX
 Q:AUFX<1  ;*** DOES NOT MESS WITH FILE NUMBERS < 1 ***
 W "."
 Q:'$D(^DIC(AUFX))
 Q:'$D(^DD(AUFX,0))
 I '$D(^DD(AUFX,0,"PT",AUFFILE,AUFFLD)) W "|" S ^(AUFFLD)=""
 Q
 ;
PTRCHK2 ; VARIABLE POINTER CHECK
 S AUFX="" F AUFL=0:0 S AUFX=$O(^DD(AUFFILE,AUFFLD,"V","B",AUFX)) Q:AUFX=""  I '$D(^DD(AUFX,0,"PT",AUFFILE,AUFFLD)) W "|" S ^(AUFFLD)=""
 Q
