BCHRLP2 ; IHS/TUCSON/LAB - PRINT GEN RET ;  [ 12/06/00  3:47 PM ]
 ;;1.0;IHS RPMS CHR SYSTEM;**7,11**;OCT 28, 1996
 ;IHS/CMI/LAB - tmp to xtmp
 ;
DONE ;EP
 D DONE^BCHUTIL1,XIT^BCHRPTU
 K ^XTMP("BCHRL",BCHJOB,BCHBT)
 D DEL^BCHRL
 K BCHBD,BCHSD,BCHED,BCHEDD,BCHBDD,BCHRPT,BCHHEAD,BCHLINE,BCHL,BCHRCNT,BCHI,BCHCRIT,BCHR,BCHRREC,BCHJOB,BCHBT,BCHBTH,BCHQUIT,BCHHDR,BCHDASH,BCHLENG,BCHPCNT,BCHTCW,BCHODAT,BCHPG,AUPNDAYS,AUPNPAT,AUPNDOD,AUPNDOB,AUPNSEX
 K BCHSORT,BCHSRT,BCHSORX,BCHFILE,BCHFIEL,BCHPRNT,BCHX,BCHTYPE,BCHFOUN,D0,J,K,L,BCHPRNM,BCHTEST,BCHSEAT,BCHLHDR,BCHFRST
 Q
HEAD ;ENTRY POINT
 I 'BCHPG G HEAD1
 I $E(IOST)="C",IO=IO(0) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S BCHQUIT="" Q
HEAD1 ;EP
 W:$D(IOF) @IOF S BCHPG=BCHPG+1
 W ?16,"**********  CONFIDENTIAL PATIENT INFORMATION  **********"
 I $G(BCHTITL)="" S BCHTEXT="CHR "_$S(BCHPTVS="V":"ENCOUNTER",1:"PATIENT")_" LISTING",BCHLENG=$L(BCHTEXT) W !?((BCHTCW-BCHLENG)/2),BCHTEXT,?(BCHTCW-8),"Page ",BCHPG
 I $G(BCHTITL)]"" S BCHLENG=$L(BCHTITL) W !?((BCHTCW-BCHLENG)/2),BCHTITL,?(BCHTCW-8),"Page ",BCHPG
 W !,$$CTR^BCHRLU($$LOC^BCHRLU)
 I BCHTYPE="D" S BCHLENG=46 S:BCHTCW<BCHLENG BCHLENG=BCHTCW W !?((BCHTCW-BCHLENG)/2),"Record Dates:  ",BCHBDD," and ",BCHEDD,!
 I BCHTYPE="S" S BCHLENG=16+$L($P(^DIBT(BCHSEAT,0),U)) S:BCHTCW<BCHLENG BCHLENG=BCHTCW  W !?((BCHTCW-BCHLENG)/2),"Search Template: ",$P(^DIBT(BCHSEAT,0),U),!
 I BCHCTYP="S" S BCHLENG=$L(BCHSORV)+23 W !?((BCHTCW-BCHLENG)/2),$S(BCHPTVS="V":"ENCOUNTER",1:"PATIENT")," SUB-TOTALS BY:  ",BCHSORV,!
 I $G(BCHSPAG) S BCHLENG=$L(BCHSRTR)+$L(BCHSORV)+2 S:BCHTCW<BCHLENG BCHLENG=BCHTCW W !?((BCHTCW-BCHLENG)/2),BCHSORV,":  ",BCHSRTR,!
 I BCHHEAD]"" W !,BCHHEAD,!
 W BCHDASH,!
 I BCHCTYP="S" W !,BCHSORV,":"
 Q
