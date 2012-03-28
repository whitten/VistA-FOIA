ABSPOSPE ; IHS/OIT/RAN - Pharmacy EXPENSE report modeled after ABSPOSEX Patient Expense report
 ;;1.0;PHARMACY POINT OF SALE;**38,40**;MAR 8, 2010
  Q
    ;
MAIN(ABSPTRNS) ;PHAREX
 N ABSPPNAM,ABSPPINF,ABSPPHRM,ABSPRXDT,ABSPARRY,ABSPDOB,ABSPTMP,ABSPPHRN,ABSPEND
 N ABSPPDOB,ABSPSDAT,ABSPSDAT,ABSPSTRT,ABSPEND,ABSPPRMI,ABSPFROM,ABSPTO,ABSPPROV
 N ABSPDTOT,ABSPDINS,ABSPDDUE,ABSPRUN,OK,IO
 S ABSPPIEN=$P(^ABSPTL(ABSPTRNS,0),U,6)
 Q:ABSPPIEN=""
 S ABSPPHRM=$P(^ABSPTL(ABSPTRNS,1),U,7)
 Q:ABSPPHRM=""
 S ABSPRUN=$$CHKPARMS(ABSPPIEN,ABSPPHRM) ;MAKE SURE THEY HAVE ASKED TO RUN THESE REPORTS
 Q:'ABSPRUN
 S ABSPPNAM=$P(^DPT(ABSPPIEN,0),U,1)  ;VA(200 patient name
 S ABSPPDOB=$$DOB^AUPNPAT(ABSPPIEN,"E")
 S ABSPPHRN=$$HRN^AUPNPAT(ABSPPIEN,DUZ(2))
 S OK=0
 S OK=$$DEVSEL()
 Q:'OK  ;Even if they chose to run this report, if they didn't set up a device don't bother
 D GETINFO(ABSPPIEN,ABSPTRNS)
 S ABSPSDAT=""
 U IO W !,"PATIENT: "_ABSPPNAM_"   DOB: "_ABSPPDOB_"    HRN: "_ABSPPHRN
 F  S ABSPSDAT=$O(ABSPTMP(ABSPSDAT)) Q:ABSPSDAT=""  D
 . Q:ABSPTMP(ABSPSDAT)=""
 . S Y=ABSPSDAT D DD^%DT
 . U IO W !!?10,"RELEASE DATE: "_Y
 . S ABSPPHRM=""
 . F  S ABSPPHRM=$O(ABSPTMP(ABSPSDAT,ABSPPHRM)) Q:ABSPPHRM=""  D
 . . S ABSPDTOT=0,ABSPDINS=0,ABSPDDUE=0
 . . U IO W !!?8,"PHARMACY: "_$P($G(^ABSP(9002313.56,ABSPPHRM,0)),"^",1)
 . . S ABSPPRMI=""
 . . F  S ABSPPRMI=$O(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI)) Q:ABSPPRMI=""  D
 . . . U IO W !!,"RX #/REFILL: `"_ABSPPRMI_"/"_$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),U,1)
 . . . S Y=$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",2) D DD^%DT
 . . . U IO W !?0,"TRANSACTION DATE: "_Y,?40,"TRANSACTION TYPE: "_$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",4)
 . . . U IO W !?5,"DRUG NAME: "_$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",5),?50,"NDC#: "_$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",6)
 . . . U IO W !?5,"QTY: "_$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",9),?50,"D/S: "_$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",10)
 . . . S ABSPPROV=$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",7)
 . . . U IO W !?5,"PROVIDER NAME: "_$P(^VA(200,ABSPPROV,0),"^",1),?50,"PROVIDER NPI#: "_$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",8)
 . . . U IO W !?0,"TOTAL PRICE: "_$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",11),?25,"INSURER PAID: "_$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",12),?53,"AMOUNT DUE: "_$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",13)
 . . . S ABSPDTOT=ABSPDTOT+$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",11)
 . . . S ABSPDINS=ABSPDINS+$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",12)
 . . . S ABSPDDUE=ABSPDDUE+$P(ABSPTMP(ABSPSDAT,ABSPPHRM,ABSPPRMI),"^",13)
 . U IO W !!,?0,"TOTAL: "_ABSPDTOT,?25,"INS PAID: "_ABSPDINS,?53,"DUE: "_ABSPDDUE
 D ZEND
 Q
 ;
GETINFO(ABSPPIEN,ABSPTRNS)  ;GET PRESCRIPTION INFO
 N ABSPRDT,ABSPPHRM,ABSPCTYP,ABSPDAT,ABSPTRXI,ABSPDRGP,ABSPDRGN,ABSPNDC,ABSPPROV
 N ABSPPNPI,ABSPQTY,ABSPDAYS,ABSPCPAY,ABSPDAYS,ABSPTDAT,ABSPDONE,ABSPCTYN,ABSPTPAT,ABSPRXR,ABSPRXN
 N ABSPRESP,ABSPPSTN,ABSPNET,RESP
 S ABSPTPAT=$P($G(^ABSPTL(ABSPTRNS,0)),U,6) ;TRANSACTION PATIENT
 Q:ABSPTPAT'=ABSPPIEN  ;NOT SELECTED PATIENT
 S ABSPPHRM=$P($G(^ABSPTL(ABSPTRNS,1)),U,7)
 S:ABSPPHRM="" ABSPPHRM=0
 S FILENUM=9002313.57
 S ABSPPSTN=$$GET1^DIQ(FILENUM,ABSPTRNS_",",14)
 S ABSPRESP=$$GET1^DIQ(FILENUM,ABSPTRNS_",",4,"I")
 ;S ABSPCTYP=$$RESP1000^ABSPOSQ4(RESP,POS,"I")
 ;IHS/OIT/CNI/RAN 9/20/2010 Patch 40 Fix for Non-Ben Patients which don't have a response file associated - BEGIN
 I +$G(ABSPRESP)'=0 S ABSPCTYP=$$RESP1000^ABSPOSQ4(ABSPRESP,ABSPPSTN,"I")
 E  S ABSPCTYP="PAPER"
 ;IHS/OIT/CNI/RAN Patch 40 Fix for Non-Ben Patients which don't have a response file associated - END
 S ABSPRICE=$P($G(^ABSPTL(ABSPTRNS,5)),U,5)
 S ABSPTDAT=$P($P($G(^ABSPTL(ABSPTRNS,0)),U,8),".",1) ;TRANSACTION DATE
 S ABSPTRXI=$P($P($G(^ABSPTL(ABSPTRNS,0)),U,1),".",1) ;POINTER TO PRESCRIPTION FILE
 S ABSPCTYN=""
 S ABSPNET=0
 I ABSPCTYP="R" D
 . S ABSPCTYN="REJECTED"
 . S ABSPPAID=0
 . S ABSPCPAY=ABSPRICE
 ;IHS/OIT/CNI/RAN Patch 40 Fix for Non-Ben Patients which don't have a response file associated - BEGIN
 I ABSPCTYP="PAPER" D
 . S ABSPCTYN="PAPER"
 . S ABSPPAID=0
 . S ABSPCPAY=ABSPRICE
 ;IHS/OIT/CNI/RAN Patch 40 Fix for Non-Ben Patients which don't have a response file associated - END
 I (ABSPCTYP="P")!(ABSPCTYP="DP") D
 . S ABSPCTYN="E PAYABLE"
 . Q:ABSPRESP=""
 . Q:ABSPPSTN=""
 . S ABSPCPAY=$$505^ABSPOS03(ABSPRESP,ABSPPSTN) ;PATIENT PAY AMOUNT
 . S ABSPPAID=$$509^ABSPOS03(ABSPRESP,ABSPPSTN) ;(#509) Total Amount Paid
 Q:ABSPCTYN=""
 S ABSPTRXR=+$P($G(^PSRX(ABSPTRXI,1,0)),U,4)    ;REFILL NUMBER
 S ABSPTRXN=$P($G(^PSRX(ABSPTRXI,0)),U,1)  ;EXTERNAL PRESCRIPTION NUMBER
 S ABSPDRGP=$P($G(^PSRX(ABSPTRXI,0)),U,6) ;POINTER TO DRUG FILE
 S ABSPDRGN=$P($G(^PSDRUG(ABSPDRGP,0)),U,1) ;DRUG NAME
 S ABSPDSYN=$P($G(^PSDRUG(ABSPDRGP,0)),U,1) ;DRUG NAME
 S ABSPNDC=$P($G(^ABSPTL(ABSPTRNS,1)),U,2)  ;NDC NUMBER
 S ABSPPROV=$P($G(^PSRX(ABSPTRXI,0)),U,4) ;POINTER TO NEW PERSON FILE (PROVIDER)
 S ABSPPNPI=$P($$NPI^XUSNPI("Individual_ID",ABSPPROV),U)  ;PROVIDER NPI
 S ABSPQTY=$P($G(^PSRX(ABSPTRXI,0)),U,7) ;PRESCRIPTION QUANTITY
 S ABSPDAYS=$P($G(^PSRX(ABSPTRXI,0)),U,8) ;PRESCRIPTION DAYS SUPPLY
 S ABSPCPAY=$FNUMBER(ABSPCPAY,"",2)
 S ABSPPAID=$FNUMBER(ABSPPAID,"",2)
 S ABSPRICE=$FNUMBER(ABSPRICE,"",2)
 S ABSPTMP(ABSPTDAT)=1
 S ABSPTMP(ABSPTDAT,ABSPPHRM,ABSPTRXI)=ABSPTRXR_"^"_ABSPTDAT_"^"_ABSPCTYP_"^"_ABSPCTYN_"^"_ABSPDRGN_"^"_ABSPNDC_"^"_ABSPPROV_"^"_ABSPPNPI_"^"_ABSPQTY_"^"_ABSPDAYS_"^"_ABSPRICE_"^"_ABSPPAID_"^"_ABSPCPAY
 Q
 ;
CHKPARMS(ABSBPATI,ABSPPHRM)  ;CHECK PARAMETERS TO SEE IF THIS SHOULD RUN
 ;ABSP PHARMACIES FILE=$P(^ABSP(9002313.56,ABSPPHRM,"REP"),U,3)
 ; 1="All Patients"
 ; 0="No Patients"
 ; NB="Only Non-Ben Patients"
 N OK
 S OK=0
 I $P($G(^ABSP(9002313.56,ABSPPHRM,"REP")),U,3)=0 Q 0 ;THEY DONT WANT THESE REPORTS FOR THIS PHARMACY
 I $P($G(^ABSP(9002313.56,ABSPPHRM,"REP")),U,3)=1 S OK=1
 I ($P($G(^ABSP(9002313.56,ABSPPHRM,"REP")),U,3)="NB"),($P(^AUPNPAT(ABSBPATI,11),U,12)="I")  S OK=1  ;NON BENIFICIERY
 Q OK
 ;
DEVSEL()  ;SELECT DEVICE
 N ABSPSTOP,IOP,OK
 S OK=0
 S IOP=$P($G(^ABSP(9002313.56,1,"REP")),U,4)
 Q:IOP="" 0
 S IOP="`"_IOP
 S %ZIS("HFSMODE")="W"  ;Just in case the Device is a flat file
 S ABSPSTOP=0
 D ^%ZIS
 I POP D
 . D ^%ZIS
 I $D(DUOUT) D
 . D ^%ZISC
 . S ABSPSTOP=1
 Q:ABSPSTOP 0
 I 'POP S OK=1
 Q OK
 ;
ZEND  ;CLOSE DEVICE
 D ^%ZISC
 Q
 ;
