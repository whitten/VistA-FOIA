RAMAINP1 ;HISC/GJC AISC/TMP,RMO-Utility Files Print ;9/22/98  15:26
 ;;5.0;Radiology/Nuclear Medicine;**3**;Mar 16, 1998
18 ;;Parent Procedure List
 N RA1,RA2,RA3
 D KILL^RAMAINP N RAX,RAY S RAX=$$IMG^RAUTL12() Q:'RAX
 S DIR(0)="S^A:Active;I:Inactive;B:Both",DIR("A")="Select Procedure Status",DIR("B")="A"
 S DIR("?",1)="Enter 'A' for active procedures, 'I' for inactive proceduRes,"
 S DIR("?")="or 'B' for both active and inactive procedures."
 W ! D ^DIR  Q:$D(DIRUT)
 S RASTAT=Y K DIR,DIROUT,DIRUT,DTOUT,DUOUT
 S DIC="^RAMIS(71,",L=0,FLDS="[RA PARENT PROCEDURE LIST]"
 S BY="12,.01",DHD=$S(RASTAT="B":"Active/Inactive",RASTAT="A":"Active",1:"Inactive")_" Parent Procedure List"
 S:RASTAT="B" DIS(0)="I $P($G(^RAMIS(71,D0,0)),U,6)=""P"",$$IMG^RAMAINP(D0)"
 S:RASTAT="A" DIS(0)="I $P($G(^RAMIS(71,D0,0)),U,6)=""P"",$$IMG^RAMAINP(D0),(+$G(^RAMIS(71,D0,""I""))=0!(+$G(^RAMIS(71,D0,""I""))>DT))"
 S:RASTAT="I" DIS(0)="I $P($G(^RAMIS(71,D0,0)),U,6)=""P"",$$IMG^RAMAINP(D0),+$G(^RAMIS(71,D0,""I""))>0,+$G(^RAMIS(71,D0,""I""))'>DT"
 S (FR,TO)="" K RASTAT S DHIT="S $P(RALINE,""-"",(IOM+1))="""" W !,RALINE"
 W ! D 132^RAMAINP S RAPOP=$$ZIS^RAMAINP("Rad/Nuc Med Parent Procedure Listing")
 I +RAPOP D HOME^%ZIS,KILL^RAMAINP Q  ; device selection failed
 I +$P(RAPOP,"^",2) D KILL^RAMAINP Q
 E  D ENTASK^RAMAINP
 Q
