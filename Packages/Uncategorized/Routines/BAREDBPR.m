BAREDBPR ; IHS/SD/SDR - AR ERA Batch/Item matching ; 01/30/2009
 ;;1.8;IHS ACCOUNTS RECEIVABLE;**20**;OCT 26,2005
 Q
EN ;
 I $G(DUZ(2))="" D  Q
 . W !!,"Check your DUZ setup."
 . D EOP^BARUTL(1)
 W !,"Matching ERA 835 to A/R Collection Batch & Items..."
 D SELFL^BAREDP00
 I Y'>0 Q
 W !,"I will begin matching the following items:"
 H 1
 I TRNAME["HIPAA" D  Q:'+BARCKIEN
 . S BARCKIEN=$$CHECK^BAREDP09(IMPDA)
 D CLNUP
 Q
 ; *********************************************************************
 ;
CLNUP ; Cleanup variables
 I $G(IMPDA) L -^BAREDI("I",IMPDA)  ;BAR*1.8*5 SRS-80 IHS/SD/TPF
 K XBDIR,X,Y,HSTFILE,ANS,IMPDA,TRDA,DATM,SEQ,TNAME
 K HSTIME,BARCOL,BARITM
 Q
