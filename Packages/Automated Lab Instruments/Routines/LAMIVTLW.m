LAMIVTLW ;DAL/HOAK VITEK ARANGER FOR LART [ 01/12/98  11:20 AM ]
 ;;5.2;LA;**1001**;DEC 10, 1997
 ;;5.2;AUTOMATED LAB INSTRUMENTS;**12,30**;Sep 27,1994
 ;
 Q
 S LRDZ1=0
 F  S LRDZ1=$O(LART("a3",LRDZ1)) Q:LRDZ1'>0  D
 .  I '$D(LART("a4",LRDZ1)) S LART("a4",LRDZ1)=LART("a3",LRDZ1)
NA ;
 ;QUIT
 Q:'$G(LRLL)
 S LRX09="^LAH(LRLL,1)"
 F  S LRX09=$Q(@LRX09) Q:LRX09'[LRLL  D
 .  I @LRX09["NA" S @LRX09=$P(@LRX09,"NA")_$P(@LRX09,"NA",2)
 .  ;I $P(@LRX09,U,2)="NA" S $P(@LRX09,U,2)=$P(@LRX09,U)
 Q
 ;
NA1 ;
 ;
 QUIT
 Q:'$D(^LR(LRDFN,LRSUB,LRIDT))
 S LRX09="^LR(LRDFN,LRSUB,LRIDT)"
 D UPDATE
 QUIT
 ;
UPDATE ;
 ;
 F  S LRX09=$Q(@LRX09) Q:LRX09'[LRLL  D
 .  I $P(@LRX09,U)="NA" S $P(@LRX09,U)=$P(@LRX09,U,2)
 QUIT
