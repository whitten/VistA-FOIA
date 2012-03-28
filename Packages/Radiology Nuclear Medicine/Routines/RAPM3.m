RAPM3 ;HOIFO/SM-Radiology Performance Monitors/Indicator ;5/28/03  10:37
 ;;5.0;Radiology/Nuclear Medicine;**37**;Mar 16, 1998
 Q
EN1 ;entry/edit OUTLOOK mail groups
 N RA1,RA2
 S RA1=$O(^RA(79,0)) Q:'RA1
 I $O(^RA(79,RA1,1,0)) D
 . W !?5,"OUTLOOK mail groups previously entered:",!
 . S RA2=0
 . F  S RA2=$O(^RA(79,RA1,1,RA2)) Q:'RA2  W !?8,^(RA2,0)
 . Q
 W !!?5,"You may add another mail group."
 W !,?5,"To edit or replace a mail group, you must delete the old one first.",!
 L +^RA(79,RA1,1):1 I '$T W !,$C(7),"Can't lock ^RA,",RA1,",1) now, try again later." Q
 S DIE="^RA(79,",DA=RA1,DR="175"
 D ^DIE
 L -^RA(79,RA1,1)
 Q
