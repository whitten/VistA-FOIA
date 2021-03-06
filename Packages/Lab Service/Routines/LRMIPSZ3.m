LRMIPSZ3 ;SLC/CJS/BA- MICRO PATIENT REPORT - STERILITY, PARASITES, VIRUS ; 6/22/87  16:15 ;
 ;;5.2;LAB SERVICE;;Sep 27, 1994
STER ;from LRMIPSZ1
 W:$L($P(^LR(LRDFN,"MI",LRIDT,1),U,7)) !,"STERILITY CONTROL: ",$S($P(^(1),U,7)="N":"NEGATIVE",1:"POSITIVE")
 S DIC="^LR("_LRDFN_",""MI"",",DA=LRIDT,DR=31 D EN^DIQ S:$D(DTOUT)!($D(DUOUT)) LREND=1
 Q
PARA ;from LRMIPSZ1
 I '$L($P(^LR(LRDFN,"MI",LRIDT,5),U)) Q:'$D(LRWRDVEW)  Q:LRSB'=5
 S LRTUS=$P(^LR(LRDFN,"MI",LRIDT,5),U,2),DZ=$P(^(5),U,3),Y=$P(^(5),U) D D^LRU
 W:LRHC ! W !,"* PARASITOLOGY ",$S(LRTUS="F":"FINAL",LRTUS="P":"PRELIMINARY",1:"")," REPORT => ",Y,"   TECH CODE: ",DZ
 S LRPRE=21 D PRE^LRMIPSU
 I $D(^LR(LRDFN,"MI",LRIDT,24)) W:LRHC ! W !,"PARASITOLOGY SMEAR/PREP:" S LRMYC=0 F I=0:0 S LRMYC=+$O(^LR(LRDFN,"MI",LRIDT,24,LRMYC)) Q:LRMYC<1  W !?5,^(LRMYC,0)
 S LRPAR=0 F I=0:0 S LRPAR=+$O(^LR(LRDFN,"MI",LRIDT,6,LRPAR)) Q:LRPAR<1  W:LRHC ! W !,"Parasite: ",$E($P(^LAB(61.2,^(LRPAR,0),0),U),1,25),?30," " D STG
 I $D(^LR(LRDFN,"MI",LRIDT,7,0)),$P(^(0),U,4)>0 W:LRHC ! W !,"Parasitology Remark(s):" S LRPAR=0 F I=0:0 S LRPAR=+$O(^LR(LRDFN,"MI",LRIDT,7,LRPAR)) Q:LRPAR<1  W !,?3,^(LRPAR,0)
 Q
STG S LRBUG(LRPAR)=^LR(LRDFN,"MI",LRIDT,6,LRPAR,0),S1=6,LRTA=LRPAR
 I $D(^LR(LRDFN,"MI",LRIDT,S1,LRTA,1,0)) S B=0 F I=0:0 S B=+$O(^LR(LRDFN,"MI",LRIDT,S1,LRTA,1,B)) Q:B<1  S Y=^(B,0),Y1=$P(Y,U,2) W !,"   Stage: " D SET W:$L(Y1) !,"   Quantity: ",Y1 D LIST1
 Q
SET S LRSET=$P(^DD(63.35,.01,0),U,3),%=$P($P(";"_LRSET,";"_$P(Y,U)_":",2),";") W:%]"" %
 Q
LIST1 W !,"   Comment: " S C=0 F I=0:0 S C=+$O(^LR(LRDFN,"MI",LRIDT,S1,LRTA,1,B,1,C)) Q:C<1  W ?13,^(C,0),!
 Q
VIR ;from LRMIPSZ1
 I '$L($P(^LR(LRDFN,"MI",LRIDT,16),U)) Q:'$D(LRWRDVEW)  Q:LRSB'=16
 S LRTUS=$P(^LR(LRDFN,"MI",LRIDT,16),U,2),DZ=$P(^(16),U,3),Y=$P(^(16),U) D D^LRU
 W:LRHC ! W !,"* VIROLOGY ",$S(LRTUS="F":"FINAL",LRTUS="P":"PRELIMINARY",1:"")," REPORT => ",Y,"   TECH CODE: ",DZ
 S LRPRE=20 D PRE^LRMIPSU
 S LRPAR=0 F I=0:0 S LRPAR=+$O(^LR(LRDFN,"MI",LRIDT,17,LRPAR)) Q:LRPAR<1  W:LRHC ! W !,"Virus: ",$P(^LAB(61.2,$P(^(LRPAR,0),U),0),U) S LRBUG(LRPAR)=^LR(LRDFN,"MI",LRIDT,17,LRPAR,0)
 I $D(^LR(LRDFN,"MI",LRIDT,18,0)),$P(^(0),U,4)>0 W:LRHC ! W !,"Virology Remark(s):" S LRPAR=0 F I=0:0 S LRPAR=+$O(^LR(LRDFN,"MI",LRIDT,18,LRPAR)) Q:LRPAR<1  W !,?3,^(LRPAR,0)
 Q
