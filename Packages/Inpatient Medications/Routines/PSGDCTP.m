PSGDCTP ;BIR/DAV,MLM-SORT AND PRINT DRUG PROFILE DATA ;1 NOV 95 / 8:55 AM
 ;;5.0; INPATIENT MEDICATIONS ;;16 DEC 97
 ;
 ; Resort data by if by amt or cost.
 K ^TMP($J,"S2") S PSGP2=$S(PSGDCT=1:0,1:$D(PSGDISP)),PSGWO=$S('$D(PSGDCLW):0,1:'PSGP2),S1=""
 F  S S1=$O(^TMP($J,"S1",S1)) Q:S1=""!(S1="RST")  S ND=$G(^(S1,0)) I '$$EXCLUDE(ND) D
 .S RST1=$$SETRST(PSGDCTS,ND),$P(ND,U,4)=$P(S1,U),PSG1=$E($P(S1,U),1,20)_U_$P(S1,U,2),^TMP($J,"S2",RST1,PSG1)=ND
 .S S2=0  F  S S2=$O(^TMP($J,"S1",S1,S2)) Q:S2=""  S ND=$G(^(S2,0)) I ND>0 D
 ..S RST3=$$SETRST(PSGDCTS,ND) I $D(PSGDISP)!PSGWO S PSG2=$E($P(S2,U),1,20)_U_$P(S2,U,2),$P(ND,U,4)=$P(S2,U),^TMP($J,"S2",RST1,PSG1,RST3,PSG2)=ND
 ..S S3=0 F  S S3=$O(^TMP($J,"S1",S1,S2,S3)) Q:S3=""  S ND=$G(^(S3,0)) I ND>0 D
 ...S RST5=$$SETRST(PSGDCTS,ND),$P(ND,U,4)=$P(S3,U),^TMP($J,"S2",RST1,PSG1,RST3,PSG2,RST5,$E($P(S3,U,2),1,20))=ND
 D START
 Q
 ;
DONE ;Kill and EXIT.
 W:CML&($Y) @IOF
DONE1 D ENCV^PSGSETU K ^TMP($J),CML,DRG,FD,HLP,LN1,ND,ND50,NP,OI,PD,PR,PSG,PSG1,PSG2,PSG3,PSG4,PSG5,PSG6,PSGASUM,PSGCLW,PSGCSUM,PSGCTL,PSGCTS,PSGDCLW,PSGDCSUM
 K PSGDCT,PSGDCTA,PSGDCTD,PSGDCTL,PSGDCTS,PSGDISP,PSGERR,PSGP2,PSGSASUM,PSGSCSUM,PSGWO,RST1,RST3,RST5,RTN,S1,S2,S3,SD,ST,STOP,STRT,TYP,W,WD,X,Y
 K OIND,PSGDASUM,PSGDT,PSGID,PSJSYSL,PSJSYSP,PSJSYSP0,PSJSYSU
 Q
EXCLUDE(X) ; Check if drug data should be included.
 S C=$P(ND,U,2)
 I (ND!C),(PSGDCTL=""!(C'<PSGDCTL)),(PSGDCTA=""!(ND'<PSGDCTA)) Q 0
 Q 1
 ;
SETRST(X,ND) ; Set RSTx subscripts
 Q $S("CA"'[X:"ZZ",X="C":-(+$P(ND,U,2)),1:-(+ND))
 ;
START ;
 D NOW^%DTC S PSGDT=%,CML=IO'=IO(0)!(IOST'["C-"),(NP,LN1)="",$P(LN1,"-",81)=""
 U IO D HDR I '$D(^TMP($J,"S2")) W !!?25,"*** NO DRUG COST DATA FOUND ***" D DONE Q
 ;
PRINT ;Print Data
 S (PSGCSUM,PSGASUM)=0,PSG1="" F  S PSG1=$O(^TMP($J,"S2",PSG1)) Q:PSG1=""!$D(DIRUT)  D
 .S PSG2="" F  S PSG2=$O(^TMP($J,"S2",PSG1,PSG2)) Q:PSG2=""!$D(DIRUT)  D  D:PSGP2!$D(PSGDCLW) SUM(PSGSASUM,PSGSCSUM,$S(PSGDCT=1:"D",1:"S"))
 ..S ND=$G(^TMP($J,"S2",PSG1,PSG2)),PSGSASUM=+ND,PSGSCSUM=$P(ND,U,2),PSGASUM=PSGASUM+PSGSASUM,PSGCSUM=PSGCSUM+PSGSCSUM
 ..W ! W:PSGDCT=1&$P(ND,U,3) "**" W ?2,$P(ND,U,4) D:'PSGP2&'$D(PSGDCLW) WRTAC
 ..S PSG3="" F  S PSG3=$O(^TMP($J,"S2",PSG1,PSG2,PSG3)) Q:PSG3=""!$D(DIRUT)  D
 ...S PSG4="" F  S PSG4=$O(^TMP($J,"S2",PSG1,PSG2,PSG3,PSG4)) Q:PSG4=""!$D(DIRUT)  S ND=$G(^(PSG4)) D  D:$D(PSGDCLW)&'PSGWO SUM(PSGDASUM,PSGDCSUM,"D")
 ....S PSGDASUM=+ND,PSGDCSUM=$P(ND,U,2) I PSGP2!PSGWO W !?3,$S($P(ND,U,3)="":"  ",1:"**"),$P(ND,U,4) D:'$D(PSGDCLW)!PSGWO WRTAC
 ....S PSG5="" F  S PSG5=$O(^TMP($J,"S2",PSG1,PSG2,PSG3,PSG4,PSG5)) Q:PSG5=""!$D(DIRUT)  D
 .....S PSG6="" F  S PSG6=$O(^TMP($J,"S2",PSG1,PSG2,PSG3,PSG4,PSG5,PSG6)) Q:PSG6=""!$D(DIRUT)  S ND=$G(^(PSG6)) D
 ......W !,?10,$P(ND,U,4) D WRTAC
 D:'$D(DIRUT) SUM(PSGASUM,PSGCSUM,"L")
 Q
 ;
WRTAC ; Print amt, cost line.
 W ?50,$J(+ND,8,3),?70,$J($P(ND,U,2),8,4) D:$Y+7>IOSL EOP
 Q
 ;
HDR ;Report Header.
 W:$Y @IOF W !!?28,"UNIT DOSE DRUG COST REPORT",?63,$$ENDTC^PSGMI(PSGDT),!?25,"FROM ",STRT," THROUGH ",STOP,!!
 D:'PSGP2&'$D(PSGDCLW) HDR1
 W PSGDCT(1) D:$D(PSGDCLW)&'PSGP2 HDR1
 I PSGP2 W !?5,"DISPENSED DRUG"
 D:$D(PSGDCLW)&$D(PSGDISP) HDR1
 W:$D(PSGDCLW) ?(5+(PSGP2*5)),"WARD" W ?50,"DISPENSED",?74,"COST",!,LN1,!
 Q
HDR1 W ?48,"TOTAL UNITS",?73,"TOTAL",!
 Q
 ;
SUM(AMT,CST,TYP) ;Print totals and subtotals
 Q:$D(DIRUT)
 W !?51,"---------------------------",!,?22 W $J($S(TYP="S":PSGDCT(1),TYP="D":"DISPENSE DRUG",1:""),13)," " W:TYP'="L" "Sub-" W ?39,"Total:",?50,$J(AMT,8,3),?70,$J(CST,8,4),!
 I TYP="L",NP'["^",CML W !!?54,"(** = NON-FORMULARY ITEM)"
 I $Y+7>IOSL,(TYP'="L") D EOP
 Q
 ;
EOP ;Check for end of page.
 I 'CML K DIR S DIR(0)="E" D ^DIR Q:$D(DIRUT)
 I CML W !!?54,"(** = NON-FORMULARY ITEM) "
 D HDR
 Q
