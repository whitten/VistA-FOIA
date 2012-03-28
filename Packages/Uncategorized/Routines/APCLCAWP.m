APCLCAWP ; IHS/CMI/LAB -print tally of walkin and appt clinic visits ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
START ;
 S APCL80S="-------------------------------------------------------------------------------"
 D NOW^%DTC S Y=X D DD^%DT S APCLDT=Y
 S Y=APCLBD D DD^%DT S APCLBDD=Y S Y=APCLED D DD^%DT S APCLEDD=Y
 S (APCLTOT,APCLPG,APCLVLOC)=0 D HEAD
 K APCLQUIT
 S (APCLGTOT,APCLGTUN,APCLGTAP,APCLGTWI)=0 F  S APCLVLOC=$O(^XTMP("APCLCAW",APCLJOB,APCLBT,"LOCTOT",APCLVLOC)) Q:APCLVLOC=""!($D(APCLQUIT))  S (APCLLTOT,APCLUNLT,APCLWILT,APCLAPLT)=0 D SORT
 G:$D(APCLQUIT) DONE
 I $Y>(IOSL-5) D HEAD G:$D(APCLQUIT) DONE
 W !?18,"Totals:  ",?30,$J(APCLGTOT,6),?38,$J(APCLGTAP,6),?45,$J(((APCLGTAP/APCLGTOT)*100),5,1),?52,$J(APCLGTWI,6),?59,$J(((APCLGTWI/APCLGTOT)*100),5,1),?65,$J(APCLGTUN,6),?72,$J(((APCLGTUN/APCLGTOT)*100),5,1),!!
DONE ;
 D DONE^APCLOSUT
 K ^XTMP("APCLCAW",APCLJOB,APCLBT)
 Q
SORT ;
 I $Y>(IOSL-6) D HEAD Q:$D(APCLQUIT)
 W !,$P(^DIC(4,APCLVLOC,0),U),!
 S APCLSORT="" F  S APCLSORT=$O(^XTMP("APCLCAW",APCLJOB,APCLBT,"LOCTOT",APCLVLOC,APCLSORT)) Q:APCLSORT=""!($D(APCLQUIT))  D P
 Q:$D(APCLQUIT)
 W !?15,"Sub total:  ",?30,$J(APCLLTOT,6),?38,$J(APCLAPLT,6),?45,$J(((APCLAPLT/APCLLTOT)*100),5,1),?52,$J(APCLWILT,6),?59,$J(((APCLWILT/APCLLTOT)*100),5,1),?65,$J(APCLUNLT,6),?72,$J(((APCLUNLT/APCLLTOT)*100),5,1),!!
 S APCLGTOT=APCLLTOT+APCLGTOT,APCLGTAP=APCLGTAP+APCLAPLT,APCLGTUN=APCLGTUN+APCLUNLT,APCLGTWI=APCLGTWI+APCLWILT
 Q
P ;
 S (APCLCLNT,APCLUNST,APCLWIT,APCLAPPT)=0
 I $Y>(IOSL-5) D HEAD Q:$D(APCLQUIT)
 S APCLSRT2=$O(^XTMP("APCLCAW",APCLJOB,APCLBT,"LOCTOT",APCLVLOC,APCLSORT,"")),APCLPRNT=APCLSORT
 I $D(^XTMP("APCLCAW",APCLJOB,APCLBT,"LOCTOT",APCLVLOC,APCLSORT,APCLSRT2,"U")) S APCLUNST=APCLUNST+^("U"),APCLCLNT=APCLCLNT+^("U")
 I $D(^XTMP("APCLCAW",APCLJOB,APCLBT,"LOCTOT",APCLVLOC,APCLSORT,APCLSRT2,"W")) S APCLWIT=APCLWIT+^("W"),APCLCLNT=APCLCLNT+^("W")
 I $D(^XTMP("APCLCAW",APCLJOB,APCLBT,"LOCTOT",APCLVLOC,APCLSORT,APCLSRT2,"A")) S APCLAPPT=APCLAPPT+^("A"),APCLCLNT=APCLCLNT+^("A")
 W !?2,$E(APCLPRNT,1,20),?23,APCLSRT2,?30,$J(APCLCLNT,6),?38,$J(APCLAPPT,6),?45,$J(((APCLAPPT/APCLCLNT)*100),5,1),?52,$J(APCLWIT,6),?59,$J(((APCLWIT/APCLCLNT)*100),5,1),?65,$J(APCLUNST,6),?72,$J(((APCLUNST/APCLCLNT)*100),5,1)
 S APCLLTOT=APCLCLNT+APCLLTOT,APCLUNLT=APCLUNLT+APCLUNST,APCLAPLT=APCLAPLT+APCLAPPT,APCLWILT=APCLWILT+APCLWIT
 Q
HEAD I 'APCLPG G HEAD1
 I $E(IOST)="C",IO=IO(0) W ! S DIR(0)="EO" D ^DIR K DIR I Y=0!(Y="^")!($D(DTOUT)) S APCLQUIT="" Q
HEAD1 ;
 W:$D(IOF) @IOF S APCLPG=APCLPG+1
 W !?58,APCLDT,?72,"Page ",APCLPG,!
 W ?10,"TALLY OF CLINIC VISITS:  WALK-IN, APPOINTMENT, UNSPECIFIED",!
 S APCLLOCT=$S(APCLLOC=0:"ALL",1:"SELECTED")
 S APCLLENG=21+$L(APCLLOCT)
 W ?((80-APCLLENG)/2),"LOCATION OF VISITS:  ",APCLLOCT,!
 S APCLCLNT=$S($G(APCLCLN)="A":"ALL",1:"Selected Clinics") ;IHS/CMI/LAB
 S APCLLENG=8+$L(APCLCLNT)
 W ?((80-APCLLENG)/2),"CLINIC:  ",APCLCLNT,!
 W ?18,"VISIT DATES:  ",APCLBDD,"  TO  ",APCLEDD,!
 W !,"LOCATION OF VISIT"
 W !?22,"CLINIC",?30,"TOTAL",?39,"APPOINTMENTS",?53,"  WALK-INS",?66,"UNSPECIFIED"
 W !?5,"CLINIC",?22,"CODE",?29,"# VISITS",?42,"#     %",?56,"#     %",?69,"#     %",!
 W APCL80S,!
 Q
