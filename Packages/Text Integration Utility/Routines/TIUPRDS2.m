TIUPRDS2 ;SLC/SBW - Header & Footer for Form 10-1000 ; 11/29/02
 ;;1.0;TEXT INTEGRATION UTILITIES;**7,55,52,148**;Jun 20, 1997
 ;IHS/ITSC/LJF 02/26/2003 removed VA form # from print
 ;                        put name of document under patient for footer
 ;                        shortened header
 ;
FOOTER(TIUDA,TIU,TIUFLAG,TIUPAGE,TIUCOPY,TIUHDR) ; Control Pagination
 ; position, write footer when appropriate
 ; IF TIUHDR=1, HEADER WILL NOT BE PRINTED
 S TIUCONT=1 G:$Y+TIUY'>IOSL FOOTEXIT
 ;
 ;IHS/ITSC/LJF 02/26/2003 IHS version of footer
 ;I (IOT'="HFS")!(IOSL<250) F  Q:$Y+4>IOSL  W ! ;moves ftr to pg bottom
 ;I $E(IOST)="P" D
 ;. W !,"PATIENT: ",^TMP("TIULQ",$J,TIUDA,.02,"E")
 ;. W ?47,"VA FORM 10-1000 DISCHARGE SUMMARY"
 ;. W !,TIU("SSN"),"  DOB: ",$$DATE^TIULS(+$G(TIU("DOB")),"MM/DD/CCYY")
 I (IOT'="HFS")!(IOSL<250) F  Q:$Y+5>IOSL  W ! ;moves ftr to pg bottom
 I $E(IOST)="P" D
 . W !,$$REPEAT^XLFSTR("-",80)
 . W !,"PATIENT: ",^TMP("TIULQ",$J,TIUDA,.02,"E")
 . W "  ",TIU("SSN"),"   #",TIU("HRCN")
 . W !,$$GET1^DIQ(8925,+TIUDA,.01)
 ;IHS/ITSC/LJF 02/26/2003
 ;
 W ?40,$J(TIUCOPY,39)
 I $E(IOST)="C" S TIUCONT=$$STOP^TIUU G FOOTEXIT:'TIUCONT
 W @IOF
 D:'+$G(TIUHDR) HEADER(TIUDA,.TIU,TIUFLAG,.TIUPAGE)
FOOTEXIT ;
 Q TIUCONT
HEADER(TIUDA,TIU,TIUFLAG,TIUPAGE) ; Header
 N TIULINE,TIUADT,TIUENTDT S $P(TIULINE,"-",80)=""
 S TIUENTDT=+$G(^TIU(8925,+TIUDA,12))
 S TIUADT=$S(+$G(TIU("LDT")):TIU("LDT"),+TIUENTDT:TIUENTDT,+$G(TIU("EDT")):TIU("EDT"),1:0)
 I +$G(TIU("DOB")),+TIUADT S TIU("AGE")=$$AGE(TIUADT,+$G(TIU("DOB")))
 W:'+TIUFLAG&($E(IOST)="P") ?26,"** WORK COPY - NOT FOR MEDICAL RECORD **"
 W !,$E($P($G(TIU("DIV")),U,2),1,37),?$X+3,$S(^TMP("TIULQ",$J,TIUDA,.09,"I")="P":^("E"),1:"")
 W ?50,$$DATE^TIULS($$NOW^TIULC,"MM/DD/CCYY HR:MIN"),?71,"Page: ",$J(TIUPAGE,2)
 W !,$$REPEAT^XLFSTR("_",80) S TIUPAGE=TIUPAGE+1 Q     ;IHS/ITSC/LJF 02/26/2003 shortened header by quitting here
 W !,TIULINE
 ; Removed RACE from header **148**
 W !,"PATIENT NAME",?33,"| AGE | SEX |    SSN       | CLAIM NUMBER"
 W !,^TMP("TIULQ",$J,TIUDA,.02,"E"),?33,"| ",$J($G(TIU("AGE")),3),?39,"|",?42,$P($G(TIU("SEX")),U),?45,"| ",$P($G(TIU("SSN")),U),?60,"| ",$G(TIU("CLAIM"))
 W !,TIULINE
 G:$E(IOST)="C"&(TIUPAGE>1) HEADERX ;Next lines not displayed on screen if page > 0
 W !?2,"ADM DATE   |  DISC DATE   | TYPE OF RELEASE   | INP | ABS | WARD NO"
 W !,$P(^TMP("TIULQ",$J,TIUDA,.07,"E"),"@"),?13,"| ",$P(^TMP("TIULQ",$J,TIUDA,.08,"E"),"@"),?28,"| "
 W $S($G(^TMP("TIULQ",$J,TIUDA,.08,"E"))]"":$$DISPTYP($P($G(TIU("MTYPE")),U,2)),1:" "),?48,"|"
 W ?49,$J($P($G(TIU("DAYS")),U),4),?54,"|",?55,$J($P($G(TIU("DAYS")),U,4),4),?60,"| ",$E($P($G(TIU("WARD")),U,2),1,18)
 W !,TIULINE
HEADERX ;Header exit
 S TIUPAGE=TIUPAGE+1
 Q
AGE(TIUDT,TIUDOB) ; Compute patient's age as of discharge (or entry)
 N Y S Y=$$FMDIFF^XLFDT(TIUDT,TIUDOB)\365.25
 Q Y
DISPTYP(TEXT) ; NOIS SHE-1098-52553
 N TIUY
 I TEXT="NON-SERVICE CONNECTED (OPT-NSC)" S TIUY="NON-SERV (OPT-NSC)"
 E  I TEXT="CONTINUED ASIH (OTHER FACILITY)" S TIUY="CONTINUED ASIH"
 E  I TEXT="DISCHARGE FROM NHCU/DOM WHILE ASIH" S TIUY="DISC NHCU/DOM ASIH"
 E  S TIUY=$E(TEXT,1,18)
 Q TIUY
