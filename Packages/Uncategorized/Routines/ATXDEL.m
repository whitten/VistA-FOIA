ATXDEL ; IHS/OHPRD/TMJ -  ICD CODES FOR A GIVEN TAXONOMY BEFORE MODIFICATION ; 
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;
 S U="^",ATXDEL="",ATXFLG="" ;ATXDEL ALLOWS DELETING OF A TAXONOMY
 ;SINCE A DELETE NODE FOR THE TAXONOMY FILE CHECKS $D(ATXDEL)
 F ATXL=0:0 D START Q:ATXSTP
 D EOJ
 Q
 ;
START ;
 D ASK Q:ATXSTP
 Q
 ;
ASK ;
 S ATXSTP=0
 S DIC="^ATXAX(",DIC(0)="AEMQ",DIC("DR")="",DIC("S")="I $P(^(0),U,5)=DUZ,$P(^(0),U,4)="""",$P(^(0),U,8)" D ^DIC K DIC
 I Y<1 S ATXSTP=1 Q
 I $D(^TMP("ATXTAX",+Y)) W !,$C(7),"Taxonomy now",^(+Y)," Try later." G ASK
 W !,$C(7),"Are you sure you want to delete this entire taxonomy" S %=1 D YN^DICN
 I %=1 S ATXX=+Y,ATXQT="" D TSKMN Q
 I %=2 Q
 I %=0 G ASK
 I %=-1 S ATXSTP=1 Q
 Q
 ;
TSKMN ;
 S ^TMP("ATXTAX",ATXX)=" being deleted."
 K ZTSAVE F %="ATXX","ATXQT" S ZTSAVE(%)=""
 W !!,$C(7),"The taxonomy will now be deleted in background by Taskman!",!
 S ZTRTN="ZTM^ATXDEL",ZTDESC="DELETE TAXONOMY",ZTIO="",ZTDTH=DT D ^%ZTLOAD K ZTSK
 Q
 ;
ZTM ;ENTRY POINT FOR TASKMAN
 D DFNS
 D CALLDIK
 K ^TMP("ATXTAX",ATXX)
 I $D(ZTQUEUED) S ZTREQ="@"
 D EOJ
 Q
 ;
DFNS ;GET LO AND HIGH DFNS FOR THIS TAXONOMY
 S ATXSS=0 F ATXL=0:0 S ATXSS=$O(^ATXAX(ATXX,21,ATXSS)) Q:ATXSS'=+ATXSS  S ATXLOV=$P(^(ATXSS,0),U) S:ATXLOV'[" " ATXLOV=ATXLOV_" " S ATXHIV=$P(^(0),U,2) S:ATXHIV'[" " ATXHIV=ATXHIV_" " D GETVAL
 Q
 ;
GETVAL ;GET RANGE OF DFNS
 S ATXDFN=$O(^ICD9("BA",ATXLOV,"")) D DIEDELT
 Q:'ATXHIV!(ATXHIV=ATXLOV)
 F  S ATXLOV=$O(^ICD9("BA",ATXLOV)) Q:ATXLOV](ATXHIV)!(ATXLOV="")  S ATXDFN=$O(^ICD9("BA",ATXLOV,"")) D DIEDELT
 Q
 ;
DIEDELT ;
 Q:'$D(^ICD9(ATXDFN,9999999.41,ATXX,0))#2
 S DIE="^ICD9(",DR="9999999.41///"_"`"_ATXX,DR(2,80.999999941)=".01///@",DA=ATXDFN D ^DIE K DIE
 Q
 ;
CALLDIK ;
 I $P(^ATXAX(ATXX,0),U,7) S DIK="^XMB(3.6,",DA=$P(^(0),U,7) D ^DIK K DIK
 S DIK="^ATXAX(",DA=ATXX D ^DIK K DIK
 S DIK="^ATXPAT(",DA=ATXX D ^DIK K DIK
 Q
 ;
EOJ ;
 K ATXLOV,ATXHIV,ATXLO,ATXHI,ATXDFN,ATXL,ATXQT,ATXX,ATXDEL,ATXSS,ATXSTP,ATXFLG
 Q
 ;
