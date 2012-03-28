PSJMP ;BIR/CML3-PATIENT LOOK-UP ;12 JAN 94 / 2:00 PM
 ;;5.0; INPATIENT MEDICATIONS ;**53**;16 DEC 97
 ;
 ; Reference to ^VADPT is supported by DBIA 10061
 ;
ENDPT ;*** get patient ***
 K DIC,PSGP,Y W !!,"Select "_$S($D(PSGDICA):PSGDICA_" ",1:"")_"PATIENT: " R X:DTIME I "^"[X S (Y,PSGP)=-1 G DONE
 D EN^PSJDPT
 I Y'>0 G ENDPT
 K DIC
 ;
CHK ;*** Check patient status ***
 S PPN=$P(Y,U,2),(DFN,PSGP)=+Y,VA200=1 D INP^VADPT
 I VAIN(4) S PSJPWD=+VAIN(4) G DONE
 S PSJPCAF="",VAIP("D")="L" D IN5^VADPT I 'VAIP(13,1) W $C(7),!!?3,"PATIENT HAS NEVER BEEN ADMITTED." G ENDPT
 S PSJPAD=VAIP(13,1),PSGID=+VAIP(3),X=+VAIP(4)=12!(+VAIP(4)=38),PSGOD=$$ENDTC^PSGMI(PSGID) W $C(7),!!?3,"PATIENT IS FOUND TO BE D",$P("ISCHARG^ECEAS",U,X+1),"ED AS OF ",PSGOD,"." G:'$D(PSGRETF) ENDPT
 ;
DONE ;
 K DA,DIC,NB,ND,NS,PSGID,PSGOD,VA200,VADM,VAIN,VAIP,VAMT,X,Y(0),Y(0,0) Q
