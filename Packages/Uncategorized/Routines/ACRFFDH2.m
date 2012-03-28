ACRFFDH2 ;IHS/OIRM/DSD/AEF - PRINT FUNDS DISTRIBUTION ACCOUNTS HIERARCHICAL STRUCTURE FOR SELECTED APPROPRIATIONS [ 10/27/2004   4:18 PM ]
 ;;2.1;ADMIN RESOURCE MGT SYSTEM;**13**;NOV 05, 2001
 ;;SPECIAL DIAGNOSTIC REPORTS
 ;
 ;
 ;This routine loops through the FMS Department, FMS Sub-Allowance, FMS
 ;Allowance and FMS Appropriation file for individual appropriations and
 ;produces a report showing the hierarchical structure of the accounts
 ;
EN ;EP -- MAIN ENTRY POINT TO PRINT ONE OR MORE INDIVIDUAL APPROPRIATIONS
 ;
 N APP,X,Y,ZTDESC,ZTRTN,ZTSAVE
 D ^XBKVAR,HOME^%ZIS
 D SELECT
 I '$D(APP) W !,"No appropriations were selected" Q
 S ZTSAVE("APP(")=""
 D PRINT
 Q
SELECT ;----- SELECT WHICH APPROPRIATION(S) TO PRINT
 ;
 N DIC,OUT,X,Y
 S D="C"
 S DIC(0)="AEMNQ"
 S DIC="^ACRAPP("
 S DIC("A")="Select APPROPRIATION: "
 F  D  Q:$G(OUT)
 . D ^DIC
 . I $D(DTOUT)!($D(DUOUT))!(Y'>0) S OUT=1 Q
 . S APP(+Y)=Y
 . I $D(APP) S DIC("A")="Select Another APPROPRIATION: "
 Q
PRINT ;----- PRINT THE REPORT
 ;
 D QUE^ACRFUTL("DQ^ACRFFDH2",.ZTSAVE,"FUNDS DISTRIBUTION REPORT") ;ACR*2.1*13.02 IM13574
 Q
DQ ;----- QUEUED JOB STARTS HERE
 ;
 N ALLOW,APPROP,DATA,DEPT,OUT,PAGE,SUBALLOW,X,Y
 K ^TMP("ACRFFDH",$J)
 D DEPT^ACRFFDH,SUB^ACRFFDH,ALLOW^ACRFFDH,APP^ACRFFDH
 S APP=0 F  S APP=$O(APP(APP)) Q:'APP  D ONE^ACRFFDH1(APP) Q:$G(OUT)
 D QUIT
 Q
QUIT ;----- CLEAN UP, CLOSE DEVICE, QUIT JOB
 ;
 K ZTSAVE
 K ^TMP("ACRFFDH",$J)
 D ^%ZISC
 Q
