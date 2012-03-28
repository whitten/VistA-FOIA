ASURM17P ; IHS/ITSC/LMH - REPORT 17 AREA MONTHLY SUB-SUB-ACTIVITY ; 
 ;;4.2T2;Supply Accounting Mgmt. System;;JUN 30, 2000
 ;;Y2K/OK AEF/2970311
 ;This routine produces report #17, Area Monthly Sub-Sub-Activity
 ;Report
 ;
 ;
EN ;EP -- MAIN ENTRY POINT (USER INTERACTIVE)
 ;
 N ASUDT,ASUTYP
 D ^XBKVAR,HOME^%ZIS
 D SELXTRCT^ASUUTIL G QUIT:'$G(ASUDT)
 W !,*7,"THIS REPORT REQUIRES 132 COLUMNS!"
 S (ZTSAVE("ASUDT"),ZTSAVE("ASUTYP"))=""
 D QUE^ASUUTIL("DQ^ASURM17P",.ZTSAVE,"SAMS RPT #17 - AREA MONTHLY SUB-SUB-ACTIVITY REPORT")
 D QUIT
 Q
EN1(ASUDT,ASUTYP)  ;EP
 ;----- ENTRY POINT CALLED BY ^ASURMSTD (NON-USER INTERACTIVE)
 ;
DQ ;EP -- QUEUED JOB STARTS HERE
 ;
 ;      ASUDT  =  report extract date or month
 ;      ASUTYP =  type of report, I=individual extract, M=monthly
 ;
 N ASU,ASUD
 D ^XBKVAR
 D GET,PRT,QUIT
 Q
GET ;EP ; GATHER DATA
 ;
 ;      Main loop through ASUTRN ISSUE, ASUTRN DIRECT ISSUE, and
 ;      ASUTRN RECEIPTS files
 ;
 ;      ASU("DT","BEG") = beginning date of fiscal year
 ;      ASU("DT","END") = ending date of fiscal year
 ;      ASU("DT","FY")  = fiscal year
 ;      ASU0            = transaction type where:
 ;                        2 = RECEIPTS
 ;                        3 = ISSUE
 ;                        7 = DIRECT ISSUE
 ;      ASU1            = extracted date in 'AX' crossreference
 ;      ASU2            = internal file entry number
 ;      ASUD("TRANS")   = transaction type
 ;      ASUD("STATUS")  = transaction status
 ;         
 N ASU0,ASU1,ASU2
 K ^XTMP("ASUR","R17")
 D DT^ASUUTIL(.ASUDT,ASUTYP)
 Q:'$D(ASUDT("DXTRACT"))
 S (ASU("DT","BEG"),ASU("DT","END"))=$E(ASU("DT","FY"),1,3)
 S ASU("DT","BEG")=ASU("DT","BEG")-1_"0999"
 S ASU("DT","END")=ASU("DT","END")_"0999"
 D TC16^ASUUTIL
 S ASU1=ASU("DT","BEG")
 F  S ASU1=$O(^ASUTH("AX",ASU1)) Q:'ASU1  Q:ASU1>ASU("DT","END")  D
 . S ASU2=0 F  S ASU2=$O(^ASUTH("AX",ASU1,ASU2)) Q:'ASU2  D
 . . S ASUD("TRANS")=$P($G(^ASUH(ASU2,1)),U),ASU0=$E(ASUD("TRANS")) S:ASU0=0 ASU0=7
 . . I ASU0'=2&(ASU0'=3)&(ASU0'=7) Q
 . . D DATA16^ASUUTIL(ASU2)
 . . Q:'$D(ASU("TC",ASUD("TRANS")))
 . . Q:ASUD("STATUS")=""
 . . Q:"UX"'[ASUD("STATUS")
 . . D SET
 Q
SET ;----- SETS DATA INTO ^XTMP("ASUR","R17") GLOBAL
 ;
 ;      Sorts and totals the transaction data and sets it into the
 ;      ^XTMP("ASUR","R17") global
 ;
 ;      ASU      = array where date and transaction code data is stored
 ;      ASUD     = array where transaction data is stored
 ;      ASU1     = transaction date
 ;      ASUPC    = piece designation in ^TMP global where totals are
 ;                 stored, the piece corresponds to the column on the
 ;                 report
 ;      ASUPCM   = piece in ^TMP global to put monthly totals (1-7)
 ;      ASUPCY   = piece in ^TMP global to put yearly todays (8-14)
 ;      ASUDT("DXTRACT")  = array containing extract dates
 ;      ASUD("VAL")       = transaction amount
 ;
 N ASUPC,ASUPCM,ASUPCY
 S ASUPCY=ASU("TC",ASUD("TRANS"))+7
 S ASUPCM=0 S:$D(ASUDT("DXTRACT",ASU1)) ASUPCM=ASU("TC",ASUD("TRANS"))
 F ASUPC=ASUPCM,ASUPCY D
 . S $P(^XTMP("ASUR","R17",2,ASUD("AREA"),ASUD("ACC"),0),U,ASUPC)=$P($G(^XTMP("ASUR","R17",2,ASUD("AREA"),ASUD("ACC"),0)),U,ASUPC)+ASUD("VAL")
 . S $P(^XTMP("ASUR","R17",1,ASUD("AREA"),0),U,ASUPC)=$P($G(^XTMP("ASUR","R17",1,ASUD("AREA"),0)),U,ASUPC)+ASUD("VAL")
 . S $P(^XTMP("ASUR","R17",1,ASUD("AREA"),ASUD("SSA"),0),U,ASUPC)=$P($G(^XTMP("ASUR","R17",1,ASUD("AREA"),ASUD("SSA"),0)),U,ASUPC)+ASUD("VAL")
 . S $P(^XTMP("ASUR","R17",1,ASUD("AREA"),ASUD("SSA"),ASUD("ACC"),0),U,ASUPC)=$P($G(^XTMP("ASUR","R17",1,ASUD("AREA"),ASUD("SSA"),ASUD("ACC"),0)),U,ASUPC)+ASUD("VAL")
 Q
PRT ;----- PRINT THE DATA     
 ;
 ;      ASUL         =  loop counter array
 ;      ASUPAGE      =  report page number
 ;      ASUOUT       =  '^' to escape controller
 ;      ASUDATA      =  temporary data storage
 ;      ASUD("ACC")  =  general ledger account number
 ;      ASUHDR       =  array containing report header segments
 ;
 N ASUL,ASUPAGE,ASUOUT,ASUHDR
 S ASUOUT=0
 I '$D(^XTMP("ASUR","R17")) W !!,"NO DATA FOR REPORT 17" Q
 ;
 S ASUHDR(1)="REPORT #17 AREA MONTHLY SUB-SUB-ACTIVITY REPORT"
 S ASUHDR(2)="AREA "_ASUD("AREA")
 S ASUHDR(4)="SUB   G L"
 S ASUHDR(5)="SUB   ACC"
 S ASUHDR(6)="ACT  CODE"
 ;
 D LOOPS
 Q
LOOPS ;----- LOOPS THROUGH ^XTMP("ASUR","R17") GLOBAL AND PRINTS THE   
 ;      REPORT
 ;
1 ;----- LOOP THROUGH AREA SUBSCRIPT
 ;
 N ASUDATA
 S ASUL(1)="" F  S ASUL(1)=$O(^XTMP("ASUR","R17",1,ASUL(1))) Q:ASUL(1)']""  D  Q:ASUOUT
 . Q:ASUL(1)=0
 . D HDR16^ASUUTIL(ASUDT,ASUTYP,.ASUPAGE,.ASUHDR,.ASUOUT)
 . D 2 Q:ASUOUT
 . D HDR16^ASUUTIL(ASUDT,ASUTYP,.ASUPAGE,.ASUHDR,ASUOUT)
 . S ASUD("ACC")="" F  S ASUD("ACC")=$O(^XTMP("ASUR","R17",2,ASUL(1),ASUD("ACC"))) Q:ASUD("ACC")']""  D  Q:ASUOUT
 . . I $Y>(IOSL-5) D HDR16^ASUUTIL(ASUDT,ASUTYP,.ASUPAGE,.ASUHDR,.ASUOUT)
 . . W !,"ALL ACCT ",$P(ASUD("ACC"),".",2)
 . . S ASUDATA=^XTMP("ASUR","R17",2,ASUL(1),ASUD("ACC"),0)
 . . D WRITE16^ASUUTIL(ASUDATA)
 . I $Y>(IOSL-5) D HDR16^ASUUTIL(ASUDT,ASUTYP,.ASUPAGE,.ASUHDR,.ASUOUT)
 . W !,"AREA TOTAL"
 . S ASUDATA=^XTMP("ASUR","R17",1,ASUL(1),0)
 . D WRITE16^ASUUTIL(ASUDATA)
 Q
2 ;----- LOOP THROUGH SUB-SUB-ACTIVITY SUBSCRIPT
 ;
 S ASUL(2)="" F  S ASUL(2)=$O(^XTMP("ASUR","R17",1,ASUL(1),ASUL(2))) Q:ASUL(2)']""  D  Q:ASUOUT
 . Q:ASUL(2)=0
 . D 3 Q:ASUOUT
 Q
3 ;----- LOOP THROUGH GENERAL LEDGER ACCOUNT SUBSCRIPT
 ;
 N ASUDATA
 S ASUL(3)="" F  S ASUL(3)=$O(^XTMP("ASUR","R17",1,ASUL(1),ASUL(2),ASUL(3))) Q:ASUL(3)']""  D  Q:ASUOUT
 . Q:ASUL(3)=0
 . S ASUDATA=^XTMP("ASUR","R17",1,ASUL(1),ASUL(2),ASUL(3),0)
 . I $Y>(IOSL-5) D HDR16^ASUUTIL(ASUDT,ASUTYP,.ASUPAGE,.ASUHDR,.ASUOUT) Q:ASUOUT
 . W !?1,$S(ASUL(2)="UNK":"",1:$P(ASUL(2)," ")),?8,$S(ASUL(3)="UNK":"",1:$P(ASUL(3),".",2))
 . D WRITE16^ASUUTIL(ASUDATA)
 Q
QUIT ;----- CLEAN UP VARIABLES, CLOSE DEVICE, QUIT
 ;
 K ZTSAVE
 K ^XTMP("ASUR","R17")
 I $G(ASUK("PTRSEL"))]"" W @IOF Q
 D ^%ZISC
 Q
