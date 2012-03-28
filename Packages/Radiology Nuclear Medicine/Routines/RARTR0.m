RARTR0 ;HISC/GJC-Queue/Print Radiology Rpts utility routine. ;1/8/97  08:07
 ;;5.0;Radiology/Nuclear Medicine;**8,26**;Mar 16, 1998
EN1 ; Called from RARTR
 S RARPT(0)=$G(^RARPT(+$G(RARPT),0)) Q:RARPT(0)']""
 S RARPT(10)=$P(RARPT(0),"^",10)
 S RAVERF=+$P(RARPT(0),U,9),RAPVERF=+$P(RARPT(0),U,13)
 S RAPIR=+$P(RALB,"^",12),RAPIS=+$P(RALB,"^",15)
 S RAWHOVER=+$P(RARPT(0),"^",17)
 I RAVERF,((RAPIR=RAVERF)!(RAPIS=RAVERF)) D
 . S RAVERFND="" ; Set verifier found flag
 . Q
 I RAPIS D  Q:$D(RAOOUT)
 . S RALBS=$E($P($G(^VA(200,RAPIS,20)),"^",2),1,25)
 . S:RALBS']"" RALBS=$E($P($G(VA(200,RAPIS,0)),"^"),1,25)
 . S RALBST=$P($G(^VA(200,RAPIS,20)),"^",3) ; max: 50 chars
 . I RALBST']"" S RALBST=$$TITLE^RARTR0(RAPIS)
 . I '$D(RAUTOE) D:($Y+RAFOOT+4)>IOSL HANG^RARTR2 Q:$D(RAOOUT)
 . I '$D(RAUTOE) D HD^RARTR:($Y+RAFOOT+4)>IOSL
 . I '$D(RAUTOE) D
 .. W !,"Primary Interpreting Staff:"
 .. W !?2,$S(RALBS]"":RALBS,1:"Unknown"),", ",$E(RALBST,1,((IOM-$X)-16))
 .. ; The '-16' above is derived from $L("(Pre-Verifier)")+1 FORMATTING
 .. Q
 . E  D
 .. S ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))="Primary Interpreting Staff:"
 .. S ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))="  "_$S(RALBS]"":RALBS,1:"Unknown")
 .. N RALEN S RALEN=$L(^TMP($J,"RA AUTOE",RAACNT))
 .. S ^TMP($J,"RA AUTOE",RAACNT)=^TMP($J,"RA AUTOE",RAACNT)_", "_$E(RALBST,1,((80-RALEN)-16))
 .. Q
 . I $D(RAVERFND)&(RAPIS=RAVERF) D
 .. I $G(RARPT(10))']"",('$D(RAUTOE)) D  Q
 ... W:RAWHOVER=RAPIS !?10,"(Verifier, no e-sig)"
 ... W:RAWHOVER'=RAPIS !?10,"Verified by transcriptionist for "_RALBS  ;Removed RA*5*8 _", M.D."
 ... Q
 .. I $G(RARPT(10))']"",($D(RAUTOE)) D  Q
 ... S:RAWHOVER=RAPIS ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))="          (Verifier, no e-sig)"
 ... S:RAWHOVER'=RAPIS ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))="          Verified by transcriptionist for "_RALBS  ;Removed RA*5*8 _", M.D."
 ... Q
 .. W:'$D(RAUTOE) " (Verifier)"
 .. S:$D(RAUTOE) ^TMP($J,"RA AUTOE",RAACNT)=^TMP($J,"RA AUTOE",RAACNT)_" (Verifier)"
 .. Q
 . I RAPIS=RAPVERF,'$D(RAUTOE) W " (Pre-Verifier)"
 . I RAPIS=RAPVERF,$D(RAUTOE) S ^TMP($J,"RA AUTOE",RAACNT)=^TMP($J,"RA AUTOE",RAACNT)_" (Pre-Verifier)"
 . Q
 D SECSTF^RARTR1 Q:$D(RAOOUT)  ; Print secondary interp'ting staff now
 I RAPIR D  Q:$D(RAOOUT)
 . S RALBR=$E($P($G(^VA(200,RAPIR,20)),"^",2),1,25)
 . S:RALBR']"" RALBR=$E($P($G(VA(200,RAPIR,0)),"^"),1,25)
 . S RALBRT=$P($G(^VA(200,RAPIR,20)),"^",3) ; max: 50 chars
 . I RALBRT']"" S RALBRT=$$TITLE^RARTR0(RAPIR)
 . I '$D(RAUTOE) D:($Y+RAFOOT+4)>IOSL HANG^RARTR2 Q:$D(RAOOUT)
 . I '$D(RAUTOE) D HD^RARTR:($Y+RAFOOT+4)>IOSL
 . W:'$D(RAUTOE) !,"Primary Interpreting Resident:"
 . W:'$D(RAUTOE) !?2,$S(RALBR]"":RALBR,1:"Unknown")_", ",$E(RALBRT,1,((IOM-$X)-16))
 . ; The '-16' above is derived from $L("(Pre-Verifier)")+1 FORMATTING
 . I $D(RAUTOE) D
 .. S ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))="Primary Interpreting Resident:"
 .. S ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))="  "_$S(RALBR]"":RALBR,1:"Unknown")
 .. N RALEN S RALEN=$L(^TMP($J,"RA AUTOE",RAACNT))
 .. S ^TMP($J,"RA AUTOE",RAACNT)=^TMP($J,"RA AUTOE",RAACNT)_", "_$E(RALBRT,1,((80-RALEN)-16))
 .. Q
 . I $D(RAVERFND)&(RAPIR=RAVERF) D
 .. I $G(RARPT(10))']"",('$D(RAUTOE)) D  Q
 ... W:RAWHOVER=RAPIR !?10,"(Verifier, no e-sig)"
 ... W:RAWHOVER'=RAPIR !?10,"Verified by transcriptionist for "_RALBR  ;Removed RA*5*8 _", M.D."
 ... Q
 .. I $G(RARPT(10))']"",($D(RAUTOE)) D  Q
 ... S:RAWHOVER=RAPIR ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))="          (Verifier, no e-sig)"
 ... S:RAWHOVER'=RAPIR ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))="          Verified by transcriptionist for "_RALBR  ;Removed RA*5*8 _", M.D."
 ... Q
 .. W:'$D(RAUTOE) " (Verifier)"
 .. S:$D(RAUTOE) ^TMP($J,"RA AUTOE",RAACNT)=^TMP($J,"RA AUTOE",RAACNT)_" (Verifier)"
 .. Q
 . I RAPIR=RAPVERF,('$D(RAUTOE)) W " (Pre-Verifier)"
 . I RAPIR=RAPVERF,($D(RAUTOE)) S ^TMP($J,"RA AUTOE",RAACNT)=^TMP($J,"RA AUTOE",RAACNT)_" (Pre-Verifier)"
 . Q
 D SECRES^RARTR1 ; Print out secondary interp'ting resident now
 Q
TITLE(X) ; Determine an individuals title
 ; 'X' is the IEN of the Primary Interpreting Resident i.e, ^DD(70.03,12
 ; -OR-
 ; 'X' is the IEN of the Primary Interpreting Staff i.e, ^DD(70.03,15
 N Y
 I $D(^VA(200,"ARC","R",X)) S Y="Resident Physician" Q Y
 I $D(^VA(200,"ARC","S",X)) S Y="Staff Physician" Q Y
 S Y=""
 Q Y
HEAD ; Set up header info for e-mail message (called from INIT^RARTR)
 N RAGE,RATPHY,RACSE,RAILOC,RANME,RAPRIPHY,RAPTLOC,RAREQPHY,RASERV,RASEX
 N RASPACE,RASSN,X1,X2 S:'$D(RAACNT) RAACNT=0
 ;S RANME=$P(RAY0,"^"),RASSN=$P(RAY0,"^",9)
 S RANME=$P(RAY0,"^"),RASSN=$$SSN^RAUTL  ;IHS/ITSC/CLS 08/10/2003 standard call for SSN
 S RASEX=$$UP^XLFSTR($P(RAY0,"^",2))
 S RACSE=$P($G(^RARPT(RARPT,0)),"^")_"@"_$P($$FMTE^XLFDT($P(RAY2,"^")),"@",2)
 ;S RAGE=$$FMDIFF^XLFDT(DT,$P(RAY0,"^",3),3),RAGE=RAGE\365.25
 S DFN=RADFN D DEM^VADPT S RAGE=VADM(4)  ;IHS/ITSC/CLS 08/10/2003 IHS printable age
 S RAREQPHY=$$XTERNAL^RAUTL5($P(RAY3,"^",14),$P($G(^DD(70.03,14,0)),"^",2))
 S RAPTLOC=$$PTLOC^RAUTL12() S:RAREQPHY']"" RAREQPHY="Unknown"
 S RASERV=$$XTERNAL^RAUTL5($P(RAY3,"^",7),$P($G(^DD(70.03,7,0)),"^",2))
 S RATPHY=$$ATND^RAUTL5(RADFN,DT),RAPRIPHY=$$PRIM^RAUTL5(RADFN,DT)
 S RAILOC=$$XTERNAL^RAUTL5($P(RAY2,"^",4),$P($G(^DD(70.02,4,0)),"^",2))
 S:RAILOC']"" RAILOC="Unknown" S:RASERV']"" RASERV="Unknown"
 S RANME=$E(RANME,1,20)_"  "
 ;S RASSN=$E(RASSN,1,3)_"-"_$E(RASSN,4,5)_"-"_$E(RASSN,6,9)_"    "
 S RASSN=RASSN_"    "  ;IHS/ITSC/CLS 08/10/2003 don't format SSN
 ;S RAGE=RAGE_" yr. old "_$S(RASEX="F":"female",RASEX="M":"male",1:"unknown")
 I RAGE["DYS"!(RAGE["MOS") S RAGE=RAGE_" old "_$S(RASEX="F":"female",RASEX="M":"male",1:"unknown")  ;IHS/ITSC/CLS 08/10/2003 IHS printable age
 E  S RAGE=RAGE_" yr. old "_$S(RASEX="F":"female",RASEX="M":"male",1:"unknown")  ;IHS/ITSC/CLS 08/10/2003 
 S $P(RASPACE," ",(22-$L(RAGE)))=""
 S RAGE=RAGE_RASPACE,RACSE="Case: "_RACSE
 S RAREQPHY="Req Phys: "_$E(RAREQPHY,1,28)
 S RASPACE="",$P(RASPACE," ",(42-$L(RAREQPHY)))=""
 S RAREQPHY=RAREQPHY_RASPACE
 S RAPTLOC="Pat Loc: "_$S(RAPTLOC]"":$E(RAPTLOC,1,30),1:"Unknown")
 S RATPHY="Att Phys: "_$E(RATPHY,1,28)
 S RASPACE="",$P(RASPACE," ",(42-$L(RATPHY)))=""
 S RATPHY=RATPHY_RASPACE
 S RAILOC="Img Loc: "_$E(RAILOC,1,30)
 S RAPRIPHY="Pri Phys: "_$E(RAPRIPHY,1,28)
 S RASPACE="",$P(RASPACE," ",(42-$L(RAPRIPHY)))=""
 S RAPRIPHY=RAPRIPHY_RASPACE
 S RASERV="Service: "_$E(RASERV,1,30)
 S ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))=RANME_RASSN_RAGE_RACSE
 S ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))=RAREQPHY_RAPTLOC
 S ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))=RATPHY_RAILOC
 S ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))=RAPRIPHY_RASERV
 S:$D(RAERRFLG) ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))="         "_$$AMENRPT^RARTR2()
 S ^TMP($J,"RA AUTOE",$$INCR^RAUTL4(RAACNT))=""
 Q
