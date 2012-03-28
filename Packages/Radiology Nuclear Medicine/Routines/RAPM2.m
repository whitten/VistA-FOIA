RAPM2 ;HOIFO/TH-Radiology Performance Monitors/Indicator; ;6/9/03  12:49
 ;;5.0;Radiology/Nuclear Medicine;**37**;Mar 16, 1998
 ;
DETAIL ; Print Detail report
 I ($Y+5)>IOSL!(RARPT="B") D
 . I IO=IO(0),($E(IOST,1,2)="C-") D
 . . R !,"Press RETURN to continue. ",X:DTIME
 D HDR("D")
 D PRTTOT
 D DHDR
 D DRPT
 D DFOOTER
 Q
 ;
PRTTOT ; Print total number of reports
 S RATOTCNT=+$G(^TMP($J,"RAPM","TOTAL"))
 W !,"Total number of reports expected for procedures performed during specified date range: ",$J(RATOTCNT,$L(RATOTCNT))
 Q
 ;
DHDR ; Header
 I ($Y+5)>IOSL D
 . S RAPG=RAPG+1,RAHD(0)="Detail Performance Indicator Report"
 . W @IOF,!?(RAIOM-$L(RAHD(0))\2),RAHD(0),?(RAIOM-10),"Page: ",$G(RAPG)
 W !!,?32,"Date/Time",?48,"Date/Time",?64,"Date/Time",?105,"Cat"
 W ?109,"Rpt",?116,"Imaging"
 W !,"Patient Name",?18,"Case #",?32,"Registered",?48,"Transcribed",?63,"Hrs"
 W ?68,"Verified",?83,"Hrs",?90,"Radiologist",?105,"Exm",?109,"Sts"
 W ?116,"Type",!
 Q
 ;
DRPT ; Read records
 I '$D(^TMP($J,"RAPM2")) W !!?30,"No data to print...",!!!!! Q
 S D1="" F  S D1=$O(^TMP($J,"RAPM2",D1)) Q:D1=""  D
 . S D2="" F  S D2=$O(^TMP($J,"RAPM2",D1,D2)) Q:D2=""  D
 . . S D3="" F  S D3=$O(^TMP($J,"RAPM2",D1,D2,D3)) Q:D3=""  D
 . . . D SRT
 Q
 ;
SRT ; Read records
 I RASORT="C"!(RASORT="P") S RAREC=$G(^TMP($J,"RAPM2",D1,D2,D3)) D DET Q
 S D4="" F  S D4=$O(^TMP($J,"RAPM2",D1,D2,D3,D4)) Q:D4=""  D
 . S RAREC=$G(^TMP($J,"RAPM2",D1,D2,D3,D4)) D DET
 Q
 ;
DET ; Print detail records
 ; use Transcription elasped hr for all sorts, except if sort by Verif.
 S RAVAL=$S(RASORT="V":$P(RAREC,U,13),1:$P(RAREC,U,12))
 ; remove symbols before comparison
 S:$E(RAVAL)="<" RAVAL=.5 S:$E(RAVAL)=">" RAVAL=999
 ; include PENDING and those with hours > RASINCE
 I RAVAL'="",RAVAL<RASINCE Q
 I ($Y+5)>IOSL D
 . I IO=IO(0) D
 . . I $E(IOST,1,2)="C-" R !,"Press RETURN to continue. ",X:DTIME
 . D DHDR
 W !,$E($P(RAREC,U,2),1,15)
 W ?18,$P(RAREC,U,1)
 W ?32,$P($$FMTE^XLFDT($P(RAREC,U,3),"2S"),":",1,2)
 W ?48,$P($$FMTE^XLFDT($P(RAREC,U,4),"2S"),":",1,2),?63,$J($P(RAREC,U,12),4)
 W ?68,$P($$FMTE^XLFDT($P(RAREC,U,5),"2S"),":",1,2),?83,$J($P(RAREC,U,13),4)
 I $P(RAREC,U,6)'="" W ?88,$E($P(RAREC,U,6),1,16)
 W ?106,$P(RAREC,U,7),?110,$P(RAREC,U,8)
 W:$P(RAREC,U,11)="" ?113,"*D"
 W ?116,$E($P(RAREC,U,9),1,16)
 Q
 ;
DFOOTER ; Footer
 I ($Y+5)>IOSL D
 . I IO=IO(0) D
 . . I $E(IOST,1,2)="C-" R !,"Press RETURN to continue. ",X:DTIME
 . D DHDR
 W !!,"Note: Category of Exam: 'I' for Inpatient; 'O' for Outpatient; "
 W "'C' for Contract; 'S' for Sharing; 'E' for Employee; 'R' for Research"
 W !,"      Report Status:    'V' for Verififed; 'R' for Released/Not "
 W "Verified; 'PD' for Problem Draft; 'D' for Draft"
 W:RANODIV !," *D = Division is missing"
 Q
 ;
STORE ; Store detail information
 Q:RARPT="S"
 ; for storage subscript: if no rpt dt, set to neg
 S RADHT=$S(RARPTDT="":-1,1:RATDFHR)
 S RADHV=$S(RAVERDT="":-1,1:RAVDFHR)
 ; for display: ronnd off and remove decimal portion of hours
 S:RATDFHR'="" RATDFHR=(RATDFHR+.5)\1
 S:RAVDFHR'="" RAVDFHR=(RAVDFHR+.5)\1
 S RATDFHR=$S(RATDFHR="":"",RATDFHR<1:"<1",RATDFHR>999:">999",1:RATDFHR)
 S RAVDFHR=$S(RAVDFHR="":"",RAVDFHR<1:"<1",RAVDFHR>999:">999",1:RAVDFHR)
 S RAREC1=RACN_U_RAPATNM
 S RAREC1=RAREC1_U_RADTE_U_RARPTDT_U_RAVERDT_U_RAPRIMNM_U_RACAT_U
 S RAREC1=RAREC1_RARPTST_U_RAIMGTYP_U_RADFN_U_RACHKDIV_U_RATDFHR_U_RAVDFHR
 I RASORT="C" S ^TMP($J,"RAPM2",$P(RADTE,"."),RACN,RAPATNM)=RAREC1
 I RASORT="P" S ^TMP($J,"RAPM2",RAPATNM,$P(RADTE,"."),RACN)=RAREC1
 I RASORT="I" S ^TMP($J,"RAPM2",RAIMGTYP,$P(RADTE,"."),RACN,RAPATNM)=RAREC1
 I RASORT="E" S ^TMP($J,"RAPM2",RACAT,$P(RADTE,"."),RACN,RAPATNM)=RAREC1
 I RASORT="R" S ^TMP($J,"RAPM2",RAPRIMNM,$P(RADTE,"."),RACN,RAPATNM)=RAREC1
 I RASORT="T" S ^TMP($J,"RAPM2",RADHT,RADTE,RACN,RAPATNM)=RAREC1
 I RASORT="V" S ^TMP($J,"RAPM2",RADHV,RADTE,RACN,RAPATNM)=RAREC1
 Q
EMAIL ; Ask if ready to email the summary report
 N RA1
 W ! S DIR(0)="Y"
 S DIR("A")="Send summary report to local mail group ""G.RAD PERFORMANCE INDICATOR"""
 S DIR("B")="Yes"
 D ^DIR
 Q:$D(DIRUT)
 S RAANS=Y
 S RA1=$O(^RA(79,0)) Q:'RA1
 I '$O(^RA(79,RA1,1,0)) D  Q
 . W !!,?5,"No OUTLOOK mail group(s) have been entered yet."
 . Q
 W ! S DIR(0)="Y"
 S DIR("A")="Send summary report to OUTLOOK mail group(s)"
 S DIR("B")="Yes"
 D ^DIR
 S RAANS2=Y
 Q
SEND ; Send summary report to mail group
 I RAANS=0,RAANS2=0 Q
 N RA1,RA2,RASVSUB,RASVTEXT
 S XMSUB="Radiology Summary Performance Indicator"
 S XMDUZ=DUZ
 S XMTEXT="^TMP($J,""RAPM"","
 S RASVSUB=XMSUB,RASVTEXT=XMTEXT
 I RAANS=1 D
 . S XMY("G.RAD PERFORMANCE INDICATOR")=""
 . D ^XMD
 . K XMY
 . Q
 I RAANS2=1 D
 . S RA1=$O(^RA(79,0)) Q:'RA1
 . S XMSUB=RASVSUB,XMTEXT=RASVTEXT
 . S RA2=0
 . F  S RA2=$O(^RA(79,RA1,1,RA2)) Q:'RA2  S XMY($P(^(RA2,0),U))=""
 . D ^XMD
 . K XMY
 . Q
 K XMDUZ
 Q
HDR(RATYP) ; Print appropriate header
 U IO S RAPG=$G(RAPG)+1
 I $G(RAPG)!($E(IOST,1,2)="C-") W @IOF
 I $E(IOST,1,2)="P-",(RAPG>1) W @IOF
 S RAHD(0)=$S(RATYP="S":"Summary",RATYP="D":"Detail",1:"")
 S RAHD(0)=RAHD(0)_" Performance Indicator Report"
 S RAIOM=$S(RATYP="S":80,1:IOM)
 W !?(RAIOM-$L(RAHD(0))\2),RAHD(0),?(RAIOM-10),"Page: ",$G(RAPG),!
 I RATYP="S" S RAN=1 D
 . S ^TMP($J,"RAPM",RAN)="                     Summary Performance Indicator Report          Page: "_$G(RAPG) S RAN=RAN+1
 . S ^TMP($J,"RAPM",RAN)="",RAN=RAN+1
 W !,"Facility: ",$$NAME^XUAF4(DUZ(2))
 I RATYP="S" S ^TMP($J,"RAPM",RAN)="Facility: "_$$NAME^XUAF4(DUZ(2)) S RAN=RAN+1
 W !,"Division: "
 I RATYP="S" S ^TMP($J,"RAPM",RAN)="Division: "
 D DIV
 S:(RATYP="S") RAN=RAN+1
 ;
 W !,"Exam Date Range: "
 W $$FMTE^XLFDT(BDATE,"2D")," - ",$$FMTE^XLFDT(EDATE,"2D")
 I RATYP="S" S ^TMP($J,"RAPM",RAN)="Exam Date Range: "_$$FMTE^XLFDT(BDATE,"2D")_" - "_$$FMTE^XLFDT(EDATE,"2D") S RAN=RAN+1
 ;
 W !,"Imaging Type(s): "
 I RATYP="S" S ^TMP($J,"RAPM",RAN)="Imaging Type(s): "
 D IMG
 S:RATYP="S" RAN=RAN+1
 ;
 ; Run date and time
 S NOW=$$NOW^XLFDT,NOW=$P(NOW,".",1)_"."_$E($P(NOW,".",2),1,4)
 W !,"Run Date/Time: ",$$FMTE^XLFDT(NOW,"2P"),!
 I RATYP="S" S ^TMP($J,"RAPM",RAN)="Run Date/Time: "_$$FMTE^XLFDT(NOW,"2P")
 I (RARPT="D"!(RARPT="B")),(RATYP'="S") D
 . S RASRT=$S(RASORT="C":"Case Number",RASORT="E":"Cateory of Exam",RASORT="I":"Imaging Type",RASORT="P":"Patient Name",RASORT="R":"Radiologist",RASORT="T":"Hrs to Transcription",RASORT="V":"Hrs to Verification",1:"")
 . W !,"Sorted by: ",RASRT,?45,"Min. hours elasped to "_$S(RASORT="V":"Verification",1:"Transcription")_": "_RASINCE
 Q
DIV ; List selected Division
 Q:'$D(^TMP($J,"RA D-TYPE"))
 S RADIV="" F I=1:1 S RADIV=$O(^TMP($J,"RA D-TYPE",RADIV)) Q:RADIV=""  D
 . I $X'>(RAIOM-$L("Division(s): ")) D
 . . W RADIV_$S($O(^TMP($J,"RA D-TYPE",RADIV))]"":", ",1:"")
 . . I RATYP="S" S ^TMP($J,"RAPM",RAN)=^TMP($J,"RAPM",RAN)_RADIV_$S($O(^TMP($J,"RA D-TYPE",RADIV))]"":", ",1:"")
 . I $X>(RAIOM-$L("Division(s): ")) D
 . . W !?($X+$L("Division(s): "))
 . . I RATYP="S" S RAN=RAN+1,^TMP($J,"RAPM",RAN)="         "
 Q
IMG ; List selected Imaging Type(s)
 Q:'$D(^TMP($J,"RA I-TYPE"))
 S RAIMG="" F J=1:1 S RAIMG=$O(^TMP($J,"RA I-TYPE",RAIMG)) Q:RAIMG=""  D
 . I $X'>(RAIOM-$L("Imaging Type(s): ")) D
 . . W RAIMG_$S($O(^TMP($J,"RA I-TYPE",RAIMG))]"":", ",1:"")
 . . I RATYP="S" S ^TMP($J,"RAPM",RAN)=^TMP($J,"RAPM",RAN)_RAIMG_$S($O(^TMP($J,"RA I-TYPE",RAIMG))]"":", ",1:"")
 . I $X>(RAIOM-$L("Imaging Type(s): ")) D
 . . W !?($X+$L("Imaging Type(s): "))
 . . I RATYP="S" S RAN=RAN+1,^TMP($J,"RAPM",RAN)="                 "
 Q
