BGP0GR1 ; IHS/CMI/LAB - BGPG Visual CRS Reports 12/30/2004 12:29:35 PM ;
 ;;10.0;IHS CLINICAL REPORTING;**1**;JUN 18, 2010
 ;
DEBUG(RETVAL,BGPSTR) ;run the debugger
 D DEBUG^%Serenji("NTLSUM^BGP0GR1(.RETVAL,.BGPSTR)")
 Q
 ;
ELD ;EP
 S X="MERR^BGP0GU",@^%ZOSF("TRAP") ; m error trap
 N BGPI,BGPJ,BGPDATA,BGPDA,P,BGPERR,BGPCT,BGPOT,BGPOPT,BGPRT,P,R,A,BGPFN
 N BGPTP,BGPQTR,BGPRDT,BGPRE,BGPBAS,BGPPATT,BGPLIST,BGPPRV,BGPPROV,BGPEXP,BGPIND
 S P="|",R="~",A="*"
 I $G(BGPSTR)="" D CATSTR^BGP0GR(.BGPSTR,.BGPSTR)
 S BGPI=0
 S BGPERR=""
 S BGPCT=$P($P(BGPSTR,P),R)
 I $P(BGPCT,R)'?.N S BGPCT=$O(^ATXAX("B",BGPCT,0))
 S BGPTP=$P(BGPSTR,P,2)
 S BGPQTR=$P(BGPSTR,P,3)
 S BGPRDT=$P(BGPSTR,P,4)
 S BGPRE=$P(BGPSTR,P,5)
 S BGPOPT="CRS 10 ELDER CARE REPORT"
 S BGPBAS=$P(BGPSTR,P,6)
 S BGPPATT=$P(BGPSTR,P,7)
 S BGPLIST=$P(BGPSTR,P,8)
 S BGPPRV=$P($P(BGPSTR,P,9),R)
 S BGPPROV=$P($P(BGPSTR,P,9),R,2)
 S BGPEXP=$P(BGPSTR,P,10)
 S BGPOT=$P(BGPSTR,P,11)
 S BGPINDI=$P(BGPSTR,P,13)
 S BGPLSTI=$P(BGPSTR,P,12)
 S BGPMFITI=$P(BGPSTR,P,14)
 S BGPFN=$P(BGPSTR,P,15)
 N I
 F I=2:1 D  Q:$P(BGPLSTI,A,I)=""
 . Q:$P(BGPLSTI,A,I)=""
 . N BGPL
 . S BGPL=$P($P(BGPLSTI,A,I),R)
 . S BGPLIST(BGPL)=""
 N J
 F J=2:1 D  Q:$P(BGPINDI,A,J)=""
 . Q:$P(BGPINDI,A,J)=""
 . N BGPL
 . S BGPL=$P($P(BGPINDI,A,J),R)
 . S BGPIND(BGPL)=""
 K ^BGPTMP($J)
 S RETVAL="^BGPTMP("_$J_")"
 S ^BGPTMP($J,BGPI)="T00250DATA"_$C(30)
 D EP^BGP0GELD(.BGPERR,DUZ,DUZ(2),BGPOPT,BGPCT,BGPTP,.BGPIND,BGPQTR,BGPRE,BGPBAS,BGPPATT,BGPLIST,.BGPLIST,BGPPRV,BGPPROV,BGPEXP,BGPOT,BGPRDT,BGPMFITI,BGPFN)
 S BGPI=BGPI+1
 S ^BGPTMP($J,BGPI)=$G(BGPERR)_$C(30)
 S ^BGPTMP($J,BGPI+1)=$C(31)
 D EN^XBVK("BGP")
 Q
 ;
EO(RETVAL,BGPSTR) ;EP
 S X="MERR^BGP0GU",@^%ZOSF("TRAP") ; m error trap
 N BGPI,BGPJ,BGPDATA,BGPDA,P,BGPERR,BGPCT,BGPOT,BGPOPT,BGPRT,P,R,A,BGPFN
 N BGPTP,BGPQTR,BGPRDT,BGPRE,BGPBAS,BGPPATT,BGPLIST,BGPPRV,BGPPROV,BGPEXP,BGPIND,BGPHC
 S P="|",R="~",A="*"
 I $G(BGPSTR)="" D CATSTR^BGP0GR(.BGPSTR,.BGPSTR)
 S BGPI=0
 S BGPERR=""
 S BGPCT=$P($P(BGPSTR,P),R)
 I $P(BGPCT,R)'?.N S BGPCT=$O(^ATXAX("B",BGPCT,0))
 S BGPTP=$P(BGPSTR,P,2)
 S BGPQTR=$P(BGPSTR,P,3)
 S BGPRDT=$P(BGPSTR,P,4)
 S BGPRE=$P(BGPSTR,P,5)
 S BGPOPT="CRS 10 EO REPORT"
 S BGPBAS=$P(BGPSTR,P,6)
 S BGPPATT=$P(BGPSTR,P,7)
 S BGPLIST=$P(BGPSTR,P,8)
 S BGPPRV=$P($P(BGPSTR,P,9),R)
 S BGPPROV=$P($P(BGPSTR,P,9),R,2)
 S BGPEXP=$P(BGPSTR,P,10)
 S BGPOT=$P(BGPSTR,P,11)
 S BGPINDI=$P(BGPSTR,P,13)
 S BGPLSTI=$P(BGPSTR,P,12)
 S BGPMFITI=$P(BGPSTR,P,14)
 S BGPHC=$P(BGPSTR,P,15)
 S BGPFN=$P(BGPSTR,P,16)
 N I
 F I=2:1 D  Q:$P(BGPLSTI,A,I)=""
 . Q:$P(BGPLSTI,A,I)=""
 . N BGPL
 . S BGPL=$P($P(BGPLSTI,A,I),R)
 . S BGPLIST(BGPL)=""
 N J
 F J=2:1 D  Q:$P(BGPINDI,A,J)=""
 . Q:$P(BGPINDI,A,J)=""
 . N BGPL
 . S BGPL=$P($P(BGPINDI,A,J),R)
 . S BGPIND(BGPL)=""
 K ^BGPTMP($J)
 S RETVAL="^BGPTMP("_$J_")"
 S ^BGPTMP($J,BGPI)="T00250DATA"_$C(30)
 D EP^BGP0GEO(.BGPERR,DUZ,DUZ(2),BGPOPT,BGPCT,BGPTP,.BGPIND,BGPQTR,BGPRE,BGPBAS,BGPPATT,BGPLIST,.BGPLIST,BGPPRV,BGPPROV,BGPEXP,BGPOT,BGPRDT,BGPMFITI,BGPHC,BGPFN)
 S BGPI=BGPI+1
 S ^BGPTMP($J,BGPI)=$G(BGPERR)_$C(30)
 S ^BGPTMP($J,BGPI+1)=$C(31)
 D EN^XBVK("BGP")
 Q
 ;
NTL9(RETVAL,BGPSTR) ;-- queue National GPRA Report
 S X="MERR^BGP0GU",@^%ZOSF("TRAP") ;m error trap
 N BGPI,BGPJ,BGPDATA,BGPDA,P,BGPERR,BGPCT,BGPYN,BGPOT,BGPOPT,BGPRT,R,BGPOM,BGPFN
 S P="|",R="~"
 S BGPI=0
 S BGPERR=""
 S BGPCT=$P($P(BGPSTR,P),R)
 I $P(BGPCT,R)'?.N S BGPCT=$O(^ATXAX("B",BGPCT,0))
 S BGPYN=$P(BGPSTR,P,2)
 S BGPOT=$P(BGPSTR,P,3)
 S BGPOPT=$P(BGPSTR,P,4)
 S BGPOPT="CRS 10 NATIONAL GPRA RPT FOR 2011"  ;8.0 p2
 S BGPRT=$P(BGPSTR,P,5)
 S BGPMFITI=$P(BGPSTR,P,6)
 S BGPYWCHW=$P(BGPSTR,P,7)
 S BGPOM=$P(BGPSTR,P,8)
 S BGPFN=$P(BGPSTR,P,9)
 K ^BGPTMP($J)
 S RETVAL="^BGPTMP("_$J_")"
 S ^BGPTMP($J,BGPI)="T00250DATA"_$C(30)
 D EP^BGP0GNT9(.BGPERR,DUZ,DUZ(2),BGPOPT,BGPCT,BGPYN,BGPOT,BGPRT,BGPMFITI,BGPYWCHW,BGPOM,BGPFN)
 S BGPI=BGPI+1
 S ^BGPTMP($J,BGPI)=$G(BGPERR)_$C(30)
 S ^BGPTMP($J,BGPI+1)=$C(31)
 D EN^XBVK("BGP")
 Q
NTLSUM(RETVAL,BGPSTR) ;-- queue National GPRA Report PERFORMANCE SUMMARIES
 S X="MERR^BGP0GU",@^%ZOSF("TRAP") ;m error trap
 N BGPI,BGPJ,BGPDATA,BGPDA,P,BGPERR,BGPCT,BGPYN,BGPOT,BGPOPT,BGPRT,R,BGPOM,BGPSUMON,BGPFN
 S P="|",R="~"
 S BGPI=0
 S BGPERR=""
 S BGPCT=$P($P(BGPSTR,P),R)
 I $P(BGPCT,R)'?.N S BGPCT=$O(^ATXAX("B",BGPCT,0))
 S BGPYN=$P(BGPSTR,P,2)
 S BGPOT=$P(BGPSTR,P,3)
 S BGPOPT=$P(BGPSTR,P,4)
 S BGPOPT="CRS 10 NATIONAL GPRA PERF SUMM"  ;8.0 p2
 S BGPRT=$P(BGPSTR,P,5)
 S BGPMFITI=$P(BGPSTR,P,6)
 S BGPYWCHW=$P(BGPSTR,P,7)
 S BGPOM=$P(BGPSTR,P,8)
 S BGPFN=$P(BGPSTR,P,9)
 S BGPSUMON=1
 K ^BGPTMP($J)
 S RETVAL="^BGPTMP("_$J_")"
 S ^BGPTMP($J,BGPI)="T00250DATA"_$C(30)
 D EPSUM^BGP0GNTS(.BGPERR,DUZ,DUZ(2),BGPOPT,BGPCT,BGPYN,BGPOT,BGPRT,BGPMFITI,BGPYWCHW,BGPOM,BGPSUMON,BGPFN)
 S BGPI=BGPI+1
 S ^BGPTMP($J,BGPI)=+$G(BGPERR)_$C(30)
 S ^BGPTMP($J,BGPI+1)=$C(31)
 D EN^XBVK("BGP")
 Q
 ;
DPRV(RETVAL,BGPSTR) ;-- queue National GPRA Report - BY PROVIDER
 S X="MERR^BGP0GU",@^%ZOSF("TRAP") ;m error trap
 N BGPI,BGPJ,BGPDATA,BGPDA,P,BGPERR,BGPCT,BGPYN,BGPOT,BGPOPT,BGPRT,R,BGPOM,BGPDPRV,BGPFN
 S P="|",R="~"
 S BGPI=0
 S BGPERR=""
 S BGPCT=$P($P(BGPSTR,P),R)
 I $P(BGPCT,R)'?.N S BGPCT=$O(^ATXAX("B",BGPCT,0))
 S BGPYN=0
 S BGPOT=$P(BGPSTR,P,3)
 S BGPOPT=$P(BGPSTR,P,4)
 S BGPOPT="CRS 10 NATIONAL GPRA RPT DESG P"  ;8.0 p2
 S BGPRT=$P(BGPSTR,P,5)
 S BGPMFITI=$P(BGPSTR,P,6)
 S BGPYWCHW=0
 S BGPOM=$P(BGPSTR,P,8)
 S BGPDPRV=$P(BGPSTR,P,9)  ;ien of designated provider selected by the user
 S BGPFN=$P(BGPSTR,P,10)
 K ^BGPTMP($J)
 S RETVAL="^BGPTMP("_$J_")"
 S ^BGPTMP($J,BGPI)="T00250DATA"_$C(30)
 D EP^BGP0GNTP(.BGPERR,DUZ,DUZ(2),BGPOPT,BGPCT,BGPYN,BGPOT,BGPRT,BGPMFITI,BGPYWCHW,BGPOM,BGPDPRV,BGPFN)
 S BGPI=BGPI+1
 S ^BGPTMP($J,BGPI)=+$G(BGPERR)_$C(30)
 S ^BGPTMP($J,BGPI+1)=$C(31)
 D EN^XBVK("BGP")
 Q
 ;
NTL10(RETVAL,BGPSTR) ;-- queue National GPRA Report
 S X="MERR^BGP0GU",@^%ZOSF("TRAP") ;m error trap
 N BGPI,BGPJ,BGPDATA,BGPDA,P,BGPERR,BGPCT,BGPYN,BGPOT,BGPOPT,BGPRT,R,BGPOM,BGPFN
 S P="|",R="~"
 S BGPI=0
 S BGPERR=""
 S BGPCT=$P($P(BGPSTR,P),R)
 I $P(BGPCT,R)'?.N S BGPCT=$O(^ATXAX("B",BGPCT,0))
 S BGPYN=$P(BGPSTR,P,2)
 S BGPOT=$P(BGPSTR,P,3)
 S BGPOPT=$P(BGPSTR,P,4)
 S BGPOPT="CRS 10 NATIONAL GPRA RPT FOR 2011"  ;10.0p1
 S BGPRT=$P(BGPSTR,P,5)
 S BGPMFITI=$P(BGPSTR,P,6)
 S BGPYWCHW=$P(BGPSTR,P,7)
 S BGPOM=$P(BGPSTR,P,8)
 S BGPFN=$P(BGPSTR,P,9)
 K ^BGPTMP($J)
 S RETVAL="^BGPTMP("_$J_")"
 S ^BGPTMP($J,BGPI)="T00250DATA"_$C(30)
 D EP^BGP0GNT9(.BGPERR,DUZ,DUZ(2),BGPOPT,BGPCT,BGPYN,BGPOT,BGPRT,BGPMFITI,BGPYWCHW,BGPOM,BGPFN)
 S BGPI=BGPI+1
 S ^BGPTMP($J,BGPI)=$G(BGPERR)_$C(30)
 S ^BGPTMP($J,BGPI+1)=$C(31)
 D EN^XBVK("BGP")
 Q
