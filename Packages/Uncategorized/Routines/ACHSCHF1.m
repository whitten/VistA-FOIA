ACHSCHF1 ; IHS/ITSC/TPF/PMF - PRINT C H E F REIMBURSEMENT REQUEST ;   
 ;;3.1;CONTRACT HEALTH MGMT SYSTEM;**15,16**;JUN 11, 2001
 ;ACHS*3.1*15 12.15.2008 IHS/OIT/FCJ NEW SUBTOTALS, COMMENTS AND MED PRIORITY CHANGE
 ;ACHS*3.1*16 11.6.2009 IHS.OIT.FCJ ADDED BLANKET DISPLAY, % requested, AMENDMENT TOTALS 
 ;AND NEW CALC FOR ADVANCES TO DATE, CFR UPDATE
 ;
 D SEL^ACHSCHF2
 Q:+Y<1
 S ACHSCHEF=+Y
DEV ; Select device/ztload.
 S %=$$PB^ACHS
 I %=U!$D(DTOUT)!$D(DUOUT) D K Q
 I %="B" D VIEWR^XBLM("START^ACHSCHF1"),EN^XBVK("VALM"),K Q
 S %ZIS="OPQ"
 D ^%ZIS
 I POP D HOME^%ZIS D K Q
 G:'$D(IO("Q")) START
 K IO("Q")
 I $D(IO("S"))!($E(IOST)'="P") W *7,!,"Please queue to system printers." D ^%ZISC G DEV
 S ZTRTN="START^ACHSCHF1",ZTDESC="PRINT C H E F REIMBURSEMENT REQUEST, CASE "_$P($G(^ACHSCHEF(DUZ(2),1,ACHSCHEF,0)),U)
 F %="ACHSCHEF" S ZTSAVE(%)=""
 D ^%ZTLOAD
 G:'$D(ZTSK) DEV
 K ZTSK
 D K
 Q
 ;
START ;EP - From TaskMan.
 D ^ACHSVAR
 K ^TMP("ACHSCHF",$J)
 S ACHS("IPD")=0  ;ACHS*3.1*15 12.15.2008 IHS/OIT/FCJ ADDED NEW LINE FOR IHS PAID SUB TOTAL
 S ACHSTODA=$$PARM^ACHS(2,27),ACHSPO="",ACHS("-")=$$REPEAT^XLFSTR("-",77),ACHSTAB=20,(ACHSDRG,ACHSMPRI)="",ACHSEOCB=9999999,ACHSEOCE="0000000"
 F  S ACHSPO=$O(^ACHSCHEF(DUZ(2),1,ACHSCHEF,1,"B",ACHSPO)) Q:ACHSPO=""  D
 . S ACHSDIEN=$O(^ACHSF(DUZ(2),"D","B",1_$E(ACHSPO)_$P(ACHSPO,"-",3),0))
 . S ACHSMPRI=ACHSMPRI_","_$$DOC^ACHS(3,6)
 . I $$DOC^ACHS(8,1) S ACHSDRG=ACHSDRG_","_$P($G(^ICD($$DOC^ACHS(8,1),0)),U)
 . I $$DOC^ACHS(3,1)<ACHSEOCB S ACHSEOCB=$$DOC^ACHS(3,1)
 . I $$DOC^ACHS(3,2)>ACHSEOCE S ACHSEOCE=$$DOC^ACHS(3,2)
 . S ACHSDOCR=$G(^ACHSF(DUZ(2),"D",ACHSDIEN,0))
 . S ACHSTIEN=0
 . F  S ACHSTIEN=$O(^ACHSF(DUZ(2),"D",ACHSDIEN,"T",ACHSTIEN)) Q:'ACHSTIEN  S ACHSTRAN=$P($G(^ACHSF(DUZ(2),"D",ACHSDIEN,"T",ACHSTIEN,0)),U,2) D SET^ACHSCHF
 .Q
 S ACHSPCNT=.01*$P(^ACHSCHEF(DUZ(2),1,ACHSCHEF,0),U,4) S:ACHSPCNT=0 ACHSPCNT=1  ;ACHS*3.1*16 11.9.2009 IHS.OIT.FCJ NEW LINE FOR %
 ;
B1 ;BLANKETS AND SLO  ;ACHS*3.1*16 11.9.2009 IHS.OIT.FCJ
 I $D(^ACHSCHEF(DUZ(2),1,ACHSCHEF,3)) D
 .S ACHS=0
 .F  S ACHS=$O(^ACHSCHEF(DUZ(2),1,ACHSCHEF,3,ACHS)) Q:ACHS'?1N.N  D
 ..S ACHSPO=$O(^ACHSCHEF(DUZ(2),1,ACHSCHEF,3,"B",ACHSPO)) Q:ACHSPO=""
 ..S ACHSDIEN=$O(^ACHSF(DUZ(2),"D","B",1_$E(ACHSPO)_$P(ACHSPO,"-",3),0))
 ..W !,ACHSPO,"  ",ACHSDIEN
 ..D SETBS^ACHSCHF
 ;
A1 ;TOTAL AMENDMENTS            ;ACHS*3.1*16 11.9.2009 IHS.OIT.FCJ
 S (ACHSTOTA,ACHSTOTR)=0
 I $D(^ACHSCHEF(DUZ(2),1,ACHSCHEF,4)) D
 .S ACHS=0
 .F  S ACHS=$O(^ACHSCHEF(DUZ(2),1,ACHSCHEF,4,ACHS)) Q:ACHS'?1N.N  D
 ..S ACHSST=$P(^ACHSCHEF(DUZ(2),1,ACHSCHEF,4,ACHS,0),U,4),ACHSAMT=$P(^(0),U,2)
 ..I ACHSST="P" S ACHSTOTR=ACHSTOTR+ACHSAMT
 ..E  S ACHSTOTA=ACHSTOTA+ACHSAMT
 ;
 S DFN=0
 U IO
P1 ;
 S DFN=$O(^TMP("ACHSCHF",$J,DFN))
 G:'DFN END
 D HDR
 G:$G(ACHSQUIT) END
 S (ACHSDIEN,ACHS("TAO"),ACHS("PD"))=0
P2 ;
 S ACHSDIEN=$O(^TMP("ACHSCHF",$J,DFN,ACHSDIEN))
 ;ACHS*3.1*16 11.9.2009 IHS.OIT.FCJ ADDED PB TO NXT LINE
 I 'ACHSDIEN D PB,BOT G P1
 S ACHSDOCR=$G(^ACHSF(DUZ(2),"D",ACHSDIEN,0))
 S (ACHSTIEN,ACHSTAO,ACHSPD,ACHSP3RD)=0,ACHSDOS=""
P3 ;
 S ACHSTIEN=$O(^TMP("ACHSCHF",$J,DFN,ACHSDIEN,ACHSTIEN))
 I 'ACHSTIEN D POSUM^ACHSCHF G P2
 S ACHSTRAN=$O(^TMP("ACHSCHF",$J,DFN,ACHSDIEN,ACHSTIEN,""))
 I ACHSTRAN="P" S ACHSDOS=$$FMTE^XLFDT($P($G(^ACHSF(DUZ(2),"D",ACHSDIEN,"T",ACHSTIEN,0)),U,10))
 I "IS"[ACHSTRAN S ACHSTAO=ACHSTAO+$P($G(^TMP("ACHSCHF",$J,DFN,ACHSDIEN,ACHSTIEN,ACHSTRAN)),U)
 I "C"[ACHSTRAN S ACHSTAO=ACHSTAO-$P($G(^TMP("ACHSCHF",$J,DFN,ACHSDIEN,ACHSTIEN,ACHSTRAN)),U)
 I "IS"'[ACHSTRAN S ACHSPD=ACHSPD+$P($G(^TMP("ACHSCHF",$J,DFN,ACHSDIEN,ACHSTIEN,ACHSTRAN)),U)
 I "C"[ACHSTRAN S ACHSPD=ACHSPD-$P($G(^TMP("ACHSCHF",$J,DFN,ACHSDIEN,ACHSTIEN,ACHSTRAN)),U)
 S ACHSP3RD=ACHSP3RD+$P($G(^TMP("ACHSCHF",$J,DFN,ACHSDIEN,ACHSTIEN,ACHSTRAN)),U,3)
 G P3
 ;
PB ;SECTION TO PRINT BLANKET INFORMATION  ;ACHS*3.1*16 IHS.OIT.FCJ
 S ACHS=0
 F  S ACHS=$O(^TMP("ACHSCHF",$J,DFN,"BLK",ACHS)) Q:ACHS'?1N.N  D
 .S ACHSDOS=""
 .S ACHSPO=$P(^TMP("ACHSCHF",$J,DFN,"BLK",ACHS),U),ACHSPROV=$P(^(ACHS),U,5)
 .S ACHSTAO=$P(^TMP("ACHSCHF",$J,DFN,"BLK",ACHS),U,2)
 .S ACHSPD=$P(^TMP("ACHSCHF",$J,DFN,"BLK",ACHS),U,3),ACHSDTP=$P(^(ACHS),U,4)
 .I 'ACHSPD S ACHSPD=ACHSTAO
 .D POSUM2^ACHSCHF
 Q
END ; Ask RTRN, write IOF. 
 D RTRN^ACHS
 W @IOF
K ;EP - Kill vars, do ERPT, quit.
 D EN^XBVK("ACHS"),^ACHSVAR
 K DA,DFN,DR,^TMP("ACHSCHF",$J),ACHSREM,X  ;ACHS*3.1*15 12.15.2008 IHS/OIT/FCJ ADDED ACHSREM AND X
 D ERPT^ACHS
 Q
 ;
HDR ; Write header of CHEF sheet.
 W @IOF,!,"|",ACHS("-"),"|",!,"|",$$C^XBFUNC("CATASTROPHIC HEALTH EMERGENCY FUND REIMBURSEMENT REQUEST",76),?78,"|"
 D LN
 ;
 W !,"|1. AREA",?30,"|2. ORDERING FACILITY",?60,"|3. CHEF NUMBER",?78,"|"
 W !,"|",$$VAL^XBDIQ1(9999999.06,DUZ(2),.04)
 W ?30,"|",$$VAL^XBDIQ1(9999999.06,DUZ(2),.01)
 S DA(1)=DUZ(2),DA=ACHSCHEF
 W ?60,"|",$$VAL^XBDIQ1(9002064.11,.DA,.01)
 W ?78,"|"
 D LN
 ;
 W !,"|4. PATIENT NAME",?30,"|5. DATE OF BIRTH",?60,"|6. SEX M/F",?78,"|"
 W !,"|",$$VAL^XBDIQ1(2,DFN,.01)
 W ?30,"|",$$DOB^AUPNPAT(DFN,"E")
 W ?60,"|",$$SEX^AUPNPAT(DFN),?78,"|"
 D LN
 ;
 ;ACHS*3.1*15 12.15.2008 IHS/OIT/FCJ CHANGED NXT 2 LINES FOR FORMAT CHANGE
 W !,"|7. TRIBE: " I $P($G(^AUPNPAT(DFN,11)),U,08)'="" W $P(^AUTTTRI($P(^AUPNPAT(DFN,11),U,08),0),U,2)
 W ?20,"|8. EPISODE OF CARE  ",$$FMTE^XLFDT(ACHSEOCB)," TO ",$$FMTE^XLFDT(ACHSEOCE),?78,"|"
 D LN
 ;ACHS*3.1*15 12.15.2008 IHS/OIT/FCJ CHANGED NXT LINE FOR FORMAT CHANGE
 W !,"|9. DX,ICD-9 CM,OR, DRG#",?25,"|10. CAT. CODE",?39,"|11. TRAUMA CD",?54,"|12. MEDICAL PRIORITY",?78,"|"
 ;ACHS*3.1*15 12.15.2008 IHS/OIT/FCJ CHANGED MEDICAL PRIORITY FIELD TO THE FIRST PO ONLY
 ;W !,"|",$E(ACHSDRG,2,23),?25,"|",?39,"|",?54,"|",$E(ACHSMPRI,2,23),?78,"|"
 W !,"|",$E(ACHSDRG,2,23),?25,"|",?39,"|",?54,"|",?64,$P(ACHSMPRI,",",2),?78,"|"
 D LN
 ;
 D RTRN^ACHS
 Q:$G(ACHSQUIT)
 ;
 S ACHSTAB=0
 ;ACHS*3.1*15 IHS.OIT.FCJ COMMENTED OUT NXT 3 LINES
 ; W !,"|12. ALTERNATE RESOURCES",?26,"|13. CONTRACT SOURCE",?48,"|14. EPISODE OF CARE",?78,"|"
 ;W !,"| (See Below)",?26,"|Provider marked w/'*'",?48,"|",$$FMTE^XLFDT(ACHSEOCB)," TO ",$$FMTE^XLFDT(ACHSEOCE),?78,"|"
 ;D LN
 D EN^ACHSRP31,LN
 ;
 S ACHSTAB=20
 D H^ACHSCHF
 Q
 ;
BOT ; Print bottom of CHEF.
 S ACHSTOTL=$P($G(^ACHSCHEF(DUZ(2),1,ACHSCHEF,0)),U,3)
 D PAT^ACHSCHF
 Q:$G(ACHSQUIT)
 S ACHSREM=""
 I $D(^ACHSCHEF(DUZ(2),1,ACHSCHEF,2)) D
 .S X=0 F  S X=$O(^ACHSCHEF(DUZ(2),1,ACHSCHEF,2,X)) Q:X'?1N.N  D
 ..S ACHSREM=ACHSREM_" "_^ACHSCHEF(DUZ(2),1,ACHSCHEF,2,X,0) Q:$L(ACHSREM)>79
 D LN
 W !,"|I hereby certify that the information and costs listed are associated with",?78,"|",!,"|this catastrophic illness/incident, and that case management has been",?78,"|"
 W !,"|performed. 42.CFR SEC 136 HAS BEEN MET.",?78,"|"    ;ACHS*3.1*16 IHS.OIT.FCJ ADDED 1 TO THE SEC 36
 D LN
 W !,"|26. SRVC UNIT DIRECTOR / Date",?30,"|27. CASE MANAGER / Date",?57,"|28. AREA CERT / Date",?78,"|"
 W !,"|",?30,"|",?57,"|",?78,"|"
 D LN
 ;ACHS*3.1*15 12.15.2008 IHS/OIT/FCJ CHANGED NXT SECTION FOR FORMAT CHANGE AND ADDED REMARKS
 ;REMARKS LIMITED TO 33 ON FIRST LINE AND 46 ON THE SECOND
 ;W !,"|39. AREA CHSO APPROVAL / Date",?30,"|30. 42.CFR SEC.36 MET",?57,"|31. REMARKS",?78,"|" 
 W !,"|29. AREA CHSO APPROVAL / Date",?30,"|30. REMARKS: ",$E(ACHSREM,1,33)
 W ?78,"|"
 W !,"|",?30,"|",$E(ACHSREM,34,79)
 W ?78,"|"
 W !,"|",$$REPEAT^XLFSTR("-",77),?78,"|"
 W !,"|TRAUMA CAUSE CODE:  MV=MOTORVEHICLE, F=FALL, S=SUICIDE,",?78,"|",!,"|A=ASSAULT, B=BURN, D=DROWNING, O=OTHER, U=UNKNOWN",?78,"|"
 W !,"|* indicates provider is a contract source",?78,"|"
 D LN
 Q
 ;
LN ;
 W !,"|",ACHS("-"),"|"
 Q
 ;
