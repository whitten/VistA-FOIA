ADGDODP ; IHS/ADC/PDW/ENM - INPATIENT DEATHS LISTING (PRINT) ; [ 09/22/2000  11:14 AM ]
 ;;5.0;ADMISSION/DISCHARGE/TRANSFER;**5**;MAR 25, 1999
 ;
 ;***> initialize variables
 S DGPAGE=0,DGSTOP="",DGDUZ=$P(^VA(200,DUZ,0),U,2)
 S DGSITE=$P(^DIC(4,DUZ(2),0),U)    ;set site
 S DGLIN="",$P(DGLIN,"=",80)=""
 S DGLIN2="",$P(DGLIN2,"-",80)=""
 ;
 G DATE:DGTYP=1,SERV:DGTYP=2,NAME:DGTYP=3  ;what sort order?
 ;
DATE ;***> discharge date order
 S DGDT=0 D HEAD
DT1 S DGDT=$O(^TMP("DGZDOD",$J,DGDT)) G END:DGDT="" S DGNM=0
DT2 S DGNM=$O(^TMP("DGZDOD",$J,DGDT,DGNM)) G DT1:DGNM="" S DFN=0
DT3 S DFN=$O(^TMP("DGZDOD",$J,DGDT,DGNM,DFN)) G DT2:DFN=""
 S DGS=^TMP("DGZDOD",$J,DGDT,DGNM,DFN),DGT=$P(DGS,U),DGSV=$P(DGS,U,2)
 D LIN G END1:DGSTOP=U G DT3
 ;
SERV ;***> discharge service order
 S DGSV=0 D HEAD
SV1 S DGSV=$O(^TMP("DGZDOD",$J,DGSV)) G END:DGSV="" S DGDT=0
SV2 S DGDT=$O(^TMP("DGZDOD",$J,DGSV,DGDT)) G SV1:DGDT="" S DGNM=0
SV3 S DGNM=$O(^TMP("DGZDOD",$J,DGSV,DGDT,DGNM)) G SV2:DGNM="" S DFN=0
SV4 S DFN=$O(^TMP("DGZDOD",$J,DGSV,DGDT,DGNM,DFN)) G SV3:DFN=""
 ;IHS/ASDST/ENM 09/22/00 NEXT LINE COPIED/MOD
 ;S DGT=^TMP("DGZDOD",$J,DGSV,DGDT,DGNM,DFN) D LIN G END1:DGSTOP=U G SV4
 S DGT=+^TMP("DGZDOD",$J,DGSV,DGDT,DGNM,DFN) D LIN G END1:DGSTOP=U G SV4
 ;
NAME ;***> alpha order by patient name
 S DGNM=0 D HEAD
NM1 S DGNM=$O(^TMP("DGZDOD",$J,DGNM)) G END:DGNM="" S DFN=0
NM2 S DFN=$O(^TMP("DGZDOD",$J,DGNM,DFN)) G NM1:DFN="" S DGDT=0
NM3 S DGDT=$O(^TMP("DGZDOD",$J,DGNM,DFN,DGDT)) G NM2:DGDT=""
 S DGS=^TMP("DGZDOD",$J,DGNM,DFN,DGDT),DGT=$P(DGS,U),DGSV=$P(DGS,U,2)
 D LIN G END1:DGSTOP=U G NM3
 ;
 ;
END ;***> eoj
 I IOST["C-" K DIR S DIR(0)="E" D ^DIR
END1 W @IOF D KILL^ADGUTIL
 D ^%ZISC K ^TMP("DGZDOD") Q
 ;
 ;
LIN ;***> subrtn to print patient data
 W !,$E(DGNM,1,20) S DGX=$P(^AUPNPAT(DFN,41,DUZ(2),0),U,2)  ;name
 W ?23,$J(DGX,6),?33,$E(DGSV,1,3)     ;chart # and service
 W ?40,$E(DGDT,4,5)_"/"_$E(DGDT,6,7)_"/"_$E(DGDT,2,3)  ;date of death
 W ?52,$E($P(^DIC(42.2,DGT,0),U),1,25)     ;discharge type
 I $Y>(IOSL-6) D NEWPG
 Q
 ;
NEWPG ;***> subrtn for end of page control
 I IOST'?1"C-".E D HEAD S DGSTOP="" Q
 K DIR S DIR(0)="E" D ^DIR S DGSTOP=X
 I DGSTOP'=U D HEAD
 Q
 ;
HEAD ;***> subrtn to print heading
 I (IOST["C-")!(DGPAGE>0) W @IOF
 W !,DGLIN S DGPAGE=DGPAGE+1
 W !?11,"*****Confidential Patient Data Covered by Privacy Act*****"
 W !,DGDUZ,?80-$L(DGSITE)/2,DGSITE S DGTY="INPATIENT DEATHS"
 W ! D TIME^ADGUTIL W ?80-$L(DGTY)/2,DGTY,?70,"Page: ",DGPAGE
 S Y=DT X ^DD("DD") W !,Y
 S DGX="SORTED BY "_$S(DGTYP=1:"DATE",DGTYP=2:"SERVICE",1:"PATIENT NAME") W ?80-$L(DGX)/2,DGX
 W !,DGLIN,!,"Patient Name",?24,"Chart #",?33,"SRV"
 W ?38,"Date of Death",?55,"Discharge Type"
 W !,DGLIN2,!
 Q
