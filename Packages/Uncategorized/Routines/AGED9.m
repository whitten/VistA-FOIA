AGED9 ; IHS/ASDS/EFG - EDIT - PAGE 9 (DIRECTIONS TO PATIENT'S HOME) ;  
 ;;7.1;PATIENT REGISTRATION;;AUG 25,2005
VAR D DRAW
 W !,"Do you wish to ",$S($Y>8:"edit",1:"enter")," the patient's LOCATION OF HOME? (Y/N) NO// "
 D READ^AGED1
 Q:$D(DFOUT)!$D(DTOUT)
 G END:$D(DLOUT)!(Y["N"),UP:$D(DUOUT),EDIT:Y["Y" G:$D(AG("ED"))&'$D(AGXTERN) @("^AGED"_AG("ED")) D YN^AG H 2 G VAR
DRAW ;EP
 S AG("PG")=9 D ^AGED Q:'$O(^AUPNPAT(DFN,12,0))
 W !! F I=0:0 S I=$O(^AUPNPAT(DFN,12,I)) Q:'I  W ^(I,0),!
 Q
EDIT S DIE="^AUPNPAT(",DR=1201,DA=DFN D ^DIE,UPDATE1^AGED(DUZ(2),DFN,9,"")
 G VAR
END K AG,DUOUT,DFOUT,DQOUT,DTOUT,DLOUT,DA,DIC,DIE,DR,DRENT,AG("DRENT1"),AGL,AG("LKERR"),AG("LKPRINT"),Y
 Q:$D(AGXTERN)
 G ^AGED10
UP K AG
 Q:$D(AGXTERN)
 G ^AGED8
ADD ;EP - Add a Patient.
 I AGOPT(6)="Y" W ! S DIE="^AUPNPAT(",DA=DFN,DR=1201 D ^DIE
 G ADD^AGED11
