RAORD6 ;HISC/CAH,FPT,GJC AISC/RMO-Print A Request Cont. ;2/2/98  15:28
 ;;5.0;Radiology/Nuclear Medicine;**5,10,15,18,27**;Mar 16, 1998
 D HD Q:RAX["^"
 N RAPRGST S RAPRGST=$P(RAORD0,"^",13)
 I RAPRGST]"",(RAPRGST'="n") D
 . W !!?18,$S(RAPRGST="y":"**** Patient is Pregnant ****",1:"**** Pregnancy Status is Unknown ****")
 . Q
 W:$P(RAORD0,"^",24)="y" !!?12,"*** Universal Isolation Precautions ***"
 W:$D(RA("VDT")) !!?8,"** Note Request Associated with Visit on ",RA("VDT")," **"
 W !!,"Requested:",?18,RA("PRC INFO"),!
 I $D(^TMP($J,"RA DIFF PRC")),('$D(RAFOERR)),('$D(RAOPT("REG"))),('$D(RAOPT("ORDEREXAM"))),('$D(RAOPT("ADDEXAM"))) D  Q:RAX["^"
 . ; don't print registered procedure info (CPT, Proc Type, Imaging
 . ; Type) if entering through 'Request An Exam', 'Register Patient
 . ; for Exams' or 'Add Exams To Last Visit'.  Don't print if ordered
 . ; through ANY version of OE/RR.  If ordered through OE/RR, RAFOERR
 . ; will be defined. (Set in RAORD1 & RAO7RO)
 . N RAT,RA18NLIN S RAT="",RA18NLIN=0 W "Registered:"
 . F  S RAT=$O(^TMP($J,"RA DIFF PRC",RAT)) Q:RAT=""  D  Q:RAX["^"
 .. S:($Y+6)>IOSL RA18NLIN=1
 .. D HD:($Y+6)>IOSL Q:RAX["^"
 .. I RA18NLIN'=0 W ! S RA18NLIN=0
 .. W ?12,RAT,!
 .. Q
 . Q
 W:$D(RA("MOD")) "Procedure Modifiers:",?22,RA("MOD")
 I RA("PRC MSG") D  Q:RAX["^"
 . N A,B,C,X S (A,C)=0 W !,"Procedure Message: ",!
 . F  S A=$O(^RAMIS(71,+$P(RAORD0,"^",2),3,A)) Q:A'>0!(RAX["^")  D
 .. S B=+$G(^RAMIS(71,+$P(RAORD0,"^",2),3,A,0))
 .. S X=$G(^RAMIS(71.4,B,0))
 .. W:'C ?3,"-" W:C !?3,"-"
 .. D OUTTEXT^RAUTL9(X,"",5,80,4,"","!")
 .. D HD:($Y+6)>IOSL S C=C+1
 .. Q
 . Q
 W !,"Request Status:",?22,$E(RA("OST"),1,24)
 I $P(RAORD0,"^",5)=1!($P(RAORD0,"^",5)=3) D  Q:RAX["^"
 . W !,"Reason ",$S($P(RAORD0,"^",5)=1:"Cancelled",1:"Held"),":"
 . W ?22,$S($D(^RA(75.2,+$P(RAORD0,"^",10),0)):$E($P(^(0),"^"),1,50),$P(RAORD0,"^",27)]"":$E($P(RAORD0,"^",27),1,50),1:"UNKNOWN")
 . D HD:($Y+6)>IOSL Q:RAX["X"
 . I $D(^RAO(75.1,RAOIFN,1)) D  Q:RAX["^"
 .. N X,I,RAXX
 .. K ^UTILITY($J,"W")
 .. W !,"Hold Description:",!
 .. S I=0 F  S I=$O(^RAO(75.1,RAOIFN,1,I)) Q:'I  S (RAXX,X)=^(I,0) D HD:($Y+6)>IOSL Q:RAX["^"  S X=RAXX D ^DIWP
 .. Q:RAX["^"
 .. D HD:($Y+6)>IOSL Q:RAX["X"
 .. D ^DIWW:$D(RAXX)
 .. D HD:($Y+6)>IOSL Q:RAX["X"
 . I $P(RAORD0,"^",5)=1 D
 .. W !!,?(IOM-(IOM/2+15)),"*********************",!,?(IOM-(IOM/2+15)),"* C A N C E L L E D *",!,?(IOM-(IOM/2+15)),"*********************"
 W:$P(RAORD0,"^",5)=6&($D(RA("ST"))) !,"Exam Status:",?22,RA("ST")
 W:$P(RAORD0,"^",5)=8&($D(RA("SDT"))) !,"Exam Scheduled:",?22,RA("SDT")
 D HD:($Y+6)>IOSL Q:RAX["^"
 W !!,"Requester:",?22,$E(RA("PHY"),1,20)
 W:RA("PHY")'="UNKNOWN" !?1,"Tel/Page/Dig Page: ",$G(RA("RPHOINFO"))
 D HD:($Y+6)>IOSL Q:RAX["^"
 W !,"Attend Phy Current:",?22,$E(RA("ATTEN"),1,20)
 W:RA("ATTEN")'="UNKNOWN" !?1,"Tel/Page/Dig Page: ",$G(RA("APHOINFO"))
 D HD:($Y+6)>IOSL Q:RAX["^"
 W !,"Prim Phy Current:",?22,$E(RA("PRIM"),1,20)
 W:RA("PRIM")'="UNKNOWN" !?1,"Tel/Page/Dig Page: ",$G(RA("PPHOINFO"))
 K RAPASS1,RAPASS2
 S RAPASS1=RA("ATTEN"),RAPASS2=RA("OATTEN")
 D HD:($Y+6)>IOSL Q:RAX["^"
 I $$ID^RAORD6(RAPASS1,RAPASS2) D
 . W !,"Attend Phy At Order:",?22,$E(RA("OATTEN"),1,20)
 . W:RA("OATTEN")'="UNKNOWN" !?1,"Tel/Page/Dig Page: ",$G(RA("OAPHOINFO"))
 . Q
 S RAPASS1=RA("PRIM"),RAPASS2=RA("OPRIM")
 I $$ID^RAORD6(RAPASS1,RAPASS2) D
 . W !,"Prim Phy At Order:",?22,$E(RA("OPRIM"),1,20)
 . W:RA("OPRIM")'="UNKNOWN" !?1,"Tel/Page/Dig Page: ",$G(RA("OPPHOINFO"))
 . Q
 K RAPASS1,RAPASS2
 I +$P(RAORD0,"^",8) D
 . N RAPPRAD S RAPPRAD=+$P(RAORD0,"^",8)
 . S:$P($G(^VA(200,RAPPRAD,20)),"^",2)]"" RAPPRAD=$P(^(20),"^",2)
 . S:RAPPRAD=+RAPPRAD RAPPRAD=$P(^VA(200,RAPPRAD,0),"^")
 . W !,"Approved by: ",?22,RAPPRAD
 . Q
 D HD:($Y+6)>IOSL Q:RAX["^"
 W !,"Date/Time Ordered:",?22,$S($D(RA("ODT")):RA("ODT"),1:""),"  by ",$E(RA("USR"),1,20)
 W:$D(RA("RDT")) !,"Date Desired:",?22,RA("RDT")
 D:$P(RAORD0,"^",5)=1 USERCAN^RAORD3
 D HD:($Y+6)>IOSL Q:RAX["^"
 W:$D(RA("PDT")) !,"Pre-op Date/Time:",?22,RA("PDT"),!!?26,"**** P R E - O P ****",!
BAR ;Print bar-coded SSN on request form if term type has bar code setup
 I $G(RASSN)'?3N1"-"2N1"-".E G CONT
 S X3=$E(RASSN,1,3)_$E(RASSN,5,6)_$E(RASSN,8,11)
 D PSET^%ZISP I IOBARON]"",(IOBAROFF]"") W !?49,@IOBARON,X3,@IOBAROFF,!
 D PKILL^%ZISP
 ;
CONT D HD:($Y+6)>IOSL Q:RAX["^"  K ^UTILITY($J,"W"),^(1) W !!,"Clinical History:",! K RAXX F RAV=0:0 S RAV=$O(^RAO(75.1,RAOIFN,"H",RAV)) Q:'RAV  I $D(^(RAV,0)) S RAXX=^(0) D HD:($Y+6)>IOSL Q:RAX["^"  S X=RAXX D ^DIWP
 Q:RAX["^"  D HD:($Y+6)>IOSL Q:RAX["^"  D ^DIWW:$D(RAXX),HD:($Y+6)>IOSL Q:RAX["^"  D WORK ;always print bottom section of form 012601
 W ! S BOT=IOSL-($Y+4) S:($E(IOST,1,6)="P-BROW"&($D(DDBRZIS))) BOT=5 F BT=1:1:BOT W !
 K BOT,BT
 ;W !,"VA Form 519a-ADP"  ;IHS/ITSC/CLS 02/09/2004
 Q
 ;
WORK W !,RALNE,!,"Date Performed: ________________________",?46
 I $O(^RADPT("AO",RAOIFN,0))="" W "Case No.: ______________________"
 E  W "Case No.: ______see above_______"
 D HD:($Y+6)>IOSL Q:RAX["^"
 W !,"Technologist Initials: _________________"
 D HD:($Y+6)>IOSL Q:RAX["^"
 W !?46,"Number/Size Films: _____________",!,"Interpreting Phys. Initials: ___________",?65,"_____________",!?65,"_____________",!
 D HD:($Y+6)>IOSL Q:RAX["^"
 W !,"Comments:"
 ;
TC D EN30^RAO7PC1(RAOIFN)
 N RA18FL,RA18ARR,RA18EX,RA18CNI,RA18DTI,RA18PRC,RA18ND,RA18TC S RA18EX=0,RA18CNI=0
 S RA18DTI=$O(^RADPT("AO",RAOIFN,RADFN,0)) G:RA18DTI="" DASHLN
 F  S RA18CNI=$O(^TMP($J,"RAE2",RADFN,RA18CNI)) Q:+RA18CNI=0  D  Q:RAX["^"
 . S RA18PRC=""
 . F  S RA18PRC=$O(^TMP($J,"RAE2",RADFN,RA18CNI,RA18PRC)) Q:RA18PRC=""  D  Q:RAX["^"
 .. ;case info
 .. W !,"Case No: "_$P($G(^RADPT(RADFN,"DT",RA18DTI,"P",RA18CNI,0)),"^")
 .. S RA18FL=0,RA18ARR("FT")=""
 .. S RA18TC=0 F  S RA18TC=$O(^RADPT(RADFN,"DT",RA18DTI,"P",RA18CNI,"F",RA18TC)) Q:RA18TC=""  S RA18ARR("F")=$G(^RADPT(RADFN,"DT",RA18DTI,"P",RA18CNI,"F",RA18TC,0),0) D  Q:$L(RA18ARR("FT"))>32
 ... I RA18ARR("F")'=0 S RA18ARR("FT")=RA18ARR("FT")_$P($G(RA18ARR("F")),"^",2)_"-"_$P($G(^RA(78.4,$P($G(RA18ARR("F")),"^",1),0)),"^",1)_";"
 .. S RA18TC=0 F  S RA18TC=$O(^RADPT(RADFN,"DT",RA18DTI,"P",RA18CNI,"TC",RA18TC)) Q:RA18TC=""  S RA18ARR("T",RA18TC,0)=$G(^RADPT(RADFN,"DT",RA18DTI,"P",RA18CNI,"TC",RA18TC,0),0) D
 ... I RA18ARR("T",RA18TC,0)'=0 W:RA18FL>0 ! W ?14,"Tech: " I RA18ARR("T",RA18TC,0)'="" S RA18ARR("N")=$$GET1^DIQ(200,RA18ARR("T",RA18TC,0),.01) W $E($P(RA18ARR("N"),"^",1),1,18)
 ... W:(RA18FL'>0) ?38,"  Film: "_$E(RA18ARR("FT"),1,32)
 ... S RA18FL=RA18FL+1
 .. I '$D(RA18ARR("T")) W ?14,"Tech: ",?38,"  Film: "_$E(RA18ARR("FT"),1,32)
 .. K RA18ARR("T"),RA18ARR("F"),RA18ARR("N")
 .. I $O(^TMP($J,"RAE2",RADFN,RA18CNI,RA18PRC,"TCOM",0))>0 D  Q
 ... ;tech comm
 ... W !
 ... S RA18EX=$$TXTOUT^RAUTL11(^TMP($J,"RAE2",RADFN,RA18CNI,RA18PRC,"TCOM",1),1,70,-1,"",4,1,1,1)
 ... D HD:($Y+6)>IOSL
 Q:RAX["^"
 ;
DASHLN W ! F I=1:1:5 D HD:($Y+6)>IOSL Q:RAX["^"  W !,RALNE ;P18
 W:($Y+6)'>IOSL !!,RALNE1 S RACONT="" D HD:($Y+8)>IOSL Q:RAX["^"  D EXAM^RADEM1 K RACONT W !,RALNE1 ;IHS/ITSC/CLS 02/09/2004 print last 5 exams/orders
 Q
 ;
HD S:'$D(RAPGE) RAPGE=0 D CRCHK Q:$G(RAX)["^"  S RATAB=$S($D(RA("ILC")):1,1:16)
 W:$Y @IOF W !?RATAB,">>"_$S($D(RACRHD):"Discontinued ",1:"")_"Rad/Nuc Med Consultation" W:$D(RA("ILC")) " for ",$E(RA("ILC"),1,17) W "<<" S X="NOW",%DT="T" D ^%DT K %DT D D^RAUTL W ?52,Y ;P18
 S RAPGE=RAPGE+1 W ?71,"Page ",RAPGE ;P18
 W !,RALNE1,!,"Name         : ",RA("NME"),?46,"Urgency    : ",RA("OUG") W:$D(RA("PORTABLE")) "  *PORTABLE*"
 W !,"Pt ID Num    : ",RASSN,?46,"Transport  : ",RA("TRAN")
 S Y=RA("DOB") D D^RAUTL W !,"Date of Birth: ",Y,?46,"Patient Loc: ",$E(RA("HLC"),1,20)
 W !,"Age          : ",RA("AGE"),?46,"Phone Ext  : ",RA("HPH")
 W !,"Sex          : ",$S(RA("SEX")="M":"MALE",1:"FEMALE") W:$D(RA("ROOM-BED")) ?46,"Room-Bed   : ",RA("ROOM-BED") W !,RALNE1
 W:$P(RAORD0,U,5)=1 !,"***C A N C E L L E D***",?56,"***C A N C E L L E D***"
 Q
 ;
CRCHK I RAPGE,$E(IOST)="C" W !!,*7,"Press RETURN to continue or '^' to stop " R X:DTIME S RAX=X
 Q
ID(X,Y) ; Checks for the following condition:
 ; 1) Attending Phy. Current & Attending Phy. At Order are the same.
 ; 2) Primary Phy. Current & Primary Phy. At Order are the same.
 ; Input Variables:
 ; 'X'-> Attending/Primary Phy. Current
 ; 'Y'-> Attending/Primary Phy. At Order
 I X']""!(Y']"") Q 0
 I $$UP^XLFSTR(X)="UNKNOWN",($$UP^XLFSTR(Y)="UNKNOWN") Q 0
 N A,B,Z S A=+$O(^VA(200,"B",X,"")),B=+$O(^VA(200,"B",Y,""))
 I A>0,(B>0),(A=B) S Z=0
 E  S Z=1
 Q Z ; $S(Z=1:"different physician",Z=0:"same physician")
