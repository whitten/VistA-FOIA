AMHPVDSG  ; IHS/CMI/LAB -VISIT DISPLAY ; 
 ;;4.0;IHS BEHAVIORAL HEALTH;;MAY 14, 2010
 ;
EP(AMHVIEN,AMHARRY) ;EP
 I $G(AMHARRY)="" S AMHARRY="^TMP(""AMHPVDSG"",$J)"
 Q:'$D(AMHVIEN)
 Q:'AMHVIEN
 Q:'$D(^AUPNVSIT(AMHVIEN,0))
 D BUILD
 D XIT
 Q
 ;
SET ;set array
 S AMHCTR=AMHCTR+1
 S @AMHARRY@(AMHCTR,0)=AMHSTR
 S AMHSTR=""
 Q
BUILD ; build array
 K AMHAR
 D TERM^VALM0
 S AMHVREC=^AUPNVSIT(AMHVIEN,0)
 S Y=$P(AMHVREC,U,5) D ^AUPNPAT
 S AMHSTR="",AMHCTR=0
 S AMHH="Patient Name",AMHV=IOINHI_$E($P(^DPT($P(AMHVREC,U,5),0),U),1,20)_IOINORM D BUILD1
 S AMHH="Chart #",AMHV=IOINHI_$S($D(^AUPNPAT($P(AMHVREC,U,5),41,DUZ(2),0)):$P(^(0),U,2),1:"None")_IOINORM D BUILD1
 S AMHH="Date of Birth" S Y=AUPNDOB D DD^%DT S AMHV=Y D BUILD1
 S AMHH="Sex",AMHV=AUPNSEX D BUILD1
 S AMHSTR="" D SET
VISIT ;
 S AMHSTR="=============== "_IOINHI_"VISIT FILE"_IOINORM_" ===============",X=(80-$L(AMHSTR)\2) D SET ;$J("",X)_AMHSTR D SET
 D ENP^XBDIQ1(9000010,AMHVIEN,".01:999999","AMHAR(","E")
 S F=0 F  S F=$O(AMHAR(F)) Q:F'=+F  I AMHAR(F)]"" D
 .S AMHH=$P(^DD(9000010,F,0),U)
 .S AMHV=AMHAR(F)
 .D BUILD1
 S AMHSTR="" D SET
 Q:'$P(AMHVREC,U,9)
VFILES ;set up array of all v file entries
 NEW DA,D0,DIC,DIQ,DR,DI
 S AMHVFLE=9000010 F  S AMHVFLE=$O(^DIC(AMHVFLE)) Q:AMHVFLE>9000010.99!(AMHVFLE'=+AMHVFLE)  D VF2
 D XIT
 Q
 ;
VF2 ;
 S AMHVNM=$P(^DIC(AMHVFLE,0),U),AMHVDG=^DIC(AMHVFLE,0,"GL"),AMHVIGR=AMHVDG_"""AD"",AMHVIEN,AMHVDFN)",AMHVDFN=""
 F AMHVI=1:1 S AMHVDFN=$O(@AMHVIGR) Q:AMHVDFN=""  D VF3
 Q
 ;
VF3 ;
 I AMHVI<2 S AMHSTR="" D SET S AMHSTR="=============== "_IOINHI_$P(AMHVNM,"V ",2)_"s"_IOINORM_" ===============",X=(80-$L(AMHSTR)\2) D SET ;$J("",X)_AMHSTR D SET
 K AMHAR D ENP^XBDIQ1(AMHVFLE,AMHVDFN,".01:.019999;.04:999999","AMHAR(","E")
 I AMHVI>1 S AMHSTR="" D SET
 S F=0 F  S F=$O(AMHAR(F)) Q:F'=+F  I AMHAR(F)]"" D
 .S AMHH=$P(^DD(AMHVFLE,F,0),U)
 .S AMHV=AMHAR(F)
 .D BUILD1
 Q
BUILD1 ;
 S AMHSTR=$E(AMHH,1,21)_":",AMHSTR=$$SETSTR^VALM1(AMHV,AMHSTR,24,$L(AMHV))
 D SET
 Q
 I $L(AMHSTR)>39 D SET
 S AMHV=" "_AMHV_" ",X=AMHH_": "_AMHV
 I $L(AMHSTR),$L(X)>40 D SET
 I $L(AMHSTR) S AMHSTR=$$SETSTR^VALM1(X,AMHSTR,40,$L(X))
 I '$L(AMHSTR) S AMHSTR=X
 K AMHV,AMHH,X
 Q
XIT ;
 K AMHAR,AMHARRY,AMHCTR,AMHH,AMHSTR,AMHV,AMHVDFN,AMHVDG,AMHVFLE,AMHVI,AMHVIEN,AMHVIGR,AMHFL,AMHVNM,AMHVREC,AMHH
 K DO,D0,DA,DI,DIC,DIQ,DR,F,X,Y,Z
 Q
