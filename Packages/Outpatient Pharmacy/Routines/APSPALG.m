APSPALG ;IHS/DSD/ENM - POC ALLERGY DRUG CHECK [ 06/07/2001  10:53 AM ]
 ;;6.0;IHS PHARMACY MODIFICATIONS;**3**;01/15/98
 ;This routine was developed by Patrick Cox @Oklahoma Area
 ;in December, 1997.
 Q
EN ;EP ENTRY POINT FROM PSODRG
 W !,"CHECK FOR DRUG ALLERGIES..."
 Q:'$D(PSODFN)
 Q:'$D(^GMR(120.8,"B",PSODFN))
 Q:$O(^GMR(120.8,"ANKA",PSODFN,""))="n"  ;PT HAS NKA XREF
 S APSZQUIT=0 ;APSZQUIT=0=XREF APSZQUIT=1=NONDF APSZQUIT=2=NOCLASS
 ;APSZXREF IS SPECIAL XREF OF DRUG INGREDIENT FILE
 S APSZXREF=$G(PSODRUG("NDF"),0) S:APSZXREF="" APSZXREF=0,APSZQUIT=1
 S:'(APSZXREF?1.N1"A"1.N) APSZXREF=0,APSZQUIT=1 ;GOT TO KNOW WHEN NO NDF
 S APSZCLASS=$G(PSODRUG("VA CLASS"),0) S:APSZCLASS="" APSZCLASS=0
 S:APSZCLASS=0 APSZQUIT=2
 D XREF:APSZQUIT=0,CLASS:APSZQUIT=1,DRUG:APSZQUIT=2
 ;0 SPECIAL XREF 1 CLASS 2 JUST A DRUG NAME
 ;WORK THRU THE PATIENT ALLERGY FILE USING ABOVE
 D FULL:APSZQUIT=0,HALF:APSZQUIT=1,QUAR:APSZQUIT=2,NONE:APSZQUIT=3
 D END
 Q
 ;SUBROUTINES
XREF ;GET THE DRUG COMPONENTS TO BE CHECKED
 S APSZDRCOM=0 F I=1:1 S APSZDRCOM=$O(^PS(50.416,"APD",APSZXREF,APSZDRCOM)) Q:APSZDRCOM=""  D
 .I $P(^PS(50.416,APSZDRCOM,0),U,2)]"" S APSZDREF(I)=$P(^(0),U,2)
 .;THE ABOVE IS FOR SOME INGREDIENTS POINTING TO OTHER INGREDIENTS
 .E  S APSZDREF(I)=APSZDRCOM
 .Q
 I '$D(APSZDREF) S APSZ("MESS")="THERE IS NO DRUG COMPONENT (NO NDF ENTRY) FOR THE DRUG "_PSODRUG("NAME")_".  IT CANNOT BE CHECKED WITH CERTAINTY FOR ALLERGIES.",APSZQUIT=1 ;D CLASS ;POC CHECK THIS
 Q
 ;
CLASS ;CHECKS FOR CLASS IF NO ENTRY IN NDF
 S APSZ("MESS")="THERE IS NO DRUG COMPONENT (NO NDF ENTRY) FOR THE DRUG "_PSODRUG("NAME")_". IT CANNOT BE CHECKED WITH CERTAINTY FOR ALLERGIES."
 Q
 ;
DRUG ;GO HERE IF NO NDF OR CLASS
 S APSZ("MESS")="THERE IS NO DRUG COMPONENT (NO NDF ENTRY) OR CLASS FOR THE DRUG "_PSODRUG("NAME")_".  IT CANNOT BE CHECKED PROPERLY FOR ALLERGIES."
 Q
 ;
FULL S APSZALLD=0 F  S APSZALLD=$O(^GMR(120.8,"API",PSODFN,APSZALLD)) Q:APSZALLD=""  D
 .S APSZALLI=$O(^GMR(120.8,"API",PSODFN,APSZALLD,""))
 .Q:$$TEST(APSZALLI)
 .S APSZALLDR=$S($P(^PS(50.416,APSZALLD,0),U,2)["":$P(^(0),U,2),1:APSZALLD) ;SOME DRUG INGREDIENTS POINT TO OTHER DRUG INGREDIENTS
 .S I=0 F  S I=$O(APSZDREF(I)) Q:I=""  D  ;NOW LOOP THRU OUR DRUG BEING CHECKED APSZDREF(I)'S
 ..I APSZDREF(I)=APSZALLD S APSZHIT=1,APSZ("HITE"_$P(^GMR(120.8,APSZALLI,0),U,2))="THE DRUG "_PSODRUG("NAME")_" IS 'MATCHED' WITH PATIENT'S ALLERGY FILE NAME"_$P(^GMR(120.8,APSZALLI,0),U,2)
 ..;THIS WAY GOING GET ONE MESSAGE FOR COMBINATION DRUGS
 ..Q
 .Q
 D:$G(APSZHIT) ASK
 K APSZHIT
 Q
 ;
HALF S APSZALLC=0 F  S APSZALLC=$O(^GMR(120.8,"APC",PSODFN,APSZALLC)) Q:APSZALLC=""  D
 .S APSZALLI=$O(^GMR(120.8,"APC",PSODFN,APSZALLC,""))
 .Q:$$TEST(APSZALLI)
 .I APSZCLASS=APSZALLC S APSZHIT=1,APSZ("HITC"_APSZALLI)="THE DRUG "_PSODRUG("NAME")_" IS 'CLASSED' WITH PATIENT ALLERGY FILE AS "_$P(^GMR(120.8,APSZALLI,0),U,2)
 .Q
 D:$G(APSZHIT) ASK
 K APSZHIT
 QUIT
 ;
QUAR S APSZDRUG=PSODRUG("IEN") ;OUR DRUG TO COMPARE WITH ALLERGIES
 S APSZDRUGC=0 F  S APSZDRUGC=$O(^GMR(120.8,"B",PSODFN,APSZDRUGC)) Q:APSZDRUGC=""  D
 .Q:$$TEST(APSZDRUGC)
 .I $P($P(^GMR(120.8,APSZDRUGC,0),U,3),";",2)="PSDRUG(" S APSZDRUGI=+$P(^(0),U,3) I $G(APSZDRUGI)=APSZDRUG D
 ..S APSZHIT=1,APSZ("HITD"_$P(^GMR(120.8,APSZDRUGC,0),U,2))="THE DRUG "_PSODRUG("NAME")_" IS 'IDENTICAL' WITH PATIENT ALLERGY FILE NAME "_$P(^GMR(120.8,APSZDRUGC,0),U,2)
 ..Q
 .Q:$G(APSZHIT)
 .I $P(^GMR(120.8,APSZDRUGC,0),U,3)="1;GMRD(120.82," S APSZDRUGN=$P(^(0),U,2) I PSODRUG("NAME")[$G(APSZDRUGN) D
 ..S APSZHIT=1,APSZ("HITS"_$P(^GMR(120.8,APSZDRUGC,0),U,2))="THE DRUG "_PSODRUG("NAME")_" IS 'PART OF AN ENTRY' PATIENT ALLERGY FILE NAME "_$P(^GMR(120.8,APSZDRUGC,0),U,2)
 ..Q
 .Q
 D:$G(APSZHIT) ASK
 K APSZHIT
 Q
 ;
ASK S X="IORVON;IORVOFF" D ENDR^%ZISS
 ;WRITE THE MESSAGES
 S I="" F  S I=$O(APSZ(I)) Q:I=""  W:I="MESS" IORVON W !,*7,APSZ(I) W:I="MESS" IORVOFF
 ;S DIR("A")="WHAT IS YOUR POISON?  "
 S DIR("A")="WHAT IS YOUR CHOICE?  "
 S DIR("A",1)="1  DO YOU WANT TO DELETE THIS DRUG?"
 S DIR("A",2)="2  DO AN INTERVENTION?",DIR("A",3)="3  DO BOTH 1 AND 2?"
 S DIR("A",4)="4  JUST CONTINUE?",DIR("B")=1,DIR(0)="N^1:4:0"
 D ^DIR
 I $D(DIRUT)!($D(DIROUT)) S Y=1
 K DIR,DTOUT,DIRUT,DUOUT,DIROUT
 D @Y
 Q
 ;
TEST(APSZIT)        ;CHECK FOR ERRORS AND VERIFED STATUS
 S APSZCHECK=0
 S:$D(^GMR(120.8,APSZIT,"ER")) APSZCHECK=1
 S:$P(^GMR(120.8,APSZIT,0),U,16)'="1" APSZCHECK=1
 ;NOTE APSZCHECK=1 MEANS STOP
 Q APSZCHECK
 ;
1 ;DELETE THE DRUG
 S PSORX("DFLG")=1,DGI=""
 Q
2 ;DO AN INTERVENTION
 S PSORX("INTERVENE")=3,DGI=""
 Q
3 ;DO BOTH 1 AND 2
 D 2,1
 Q
4 ;DO NOTHING
 Q
NONE ;SHOULD NOT GET HERE
END ;CLEAN UP TIME
 D EN^XBVK("APSZ")
 Q
