BEHOVMRP ;IHS/MSC/MGH - OE/RR REPORTS FOR MEASUREMNTS AND I&O;26-Mar-2010 15:42;PLS
 ;;1.1;BEH COMPONENTS;**001004,001005**;March 20,2007
 ;
VITALS(ROOT,ORDFN,ID,ALPHA,OMEGA,ORDTRNG,REMOTE,ORMAX,ORFHIE) ; -- get vitals report
 D START^ORWRP(132,"VITALSB^BEHOVMRP(.ROOT,.ORDFN,.ID,.ALPHA,.OMEGA,.ORDTRNG,.REMOTE,.ORMAX,.ORFHIE)")
 Q
VITALSB(ROOT,ORDFN,ID,ALPHA,OMEGA,ORDTRNG,REMOTE,ORMAX,ORFHIE) ; -- build vitals report
 N ORVP,XQORNOD,ORSSTRT,ORSSTOP
 Q:'$G(ORDFN)
 I $L(ORDTRNG),'$G(ALPHA) S ALPHA=$$FMADD^XLFDT(DT,-ORDTRNG),OMEGA=$$NOW^XLFDT
 Q:'$G(ALPHA)  Q:'$G(OMEGA)
 I '$P(OMEGA,".",2) S OMEGA=OMEGA_".2359"
 S ORVP=ORDFN_";DPT(",XQORNOD=1,ORSSTRT(XQORNOD)=ALPHA,ORSSTOP(XQORNOD)=OMEGA
 D VITCUM
 Q
VITCUM ; Print Vitals Cumulative Report
 N %,DFN,BEH1ST,BEHDAT,BEHDATE,BEHSH,BEHDT,BEHLN,BEHOUT,BEHPDT,BEHPG
 N BEHSITE,BEHSP,BEHVDA,BEHVFDT,BEHVITY,BEHVSDT,BEHVTY,GMTVX,BEHY,I
 N VA,VADM
 S DFN=+ORVP,BEHVSDT=$G(ORSSTRT(+XQORNOD)),BEHVFDT=$G(ORSSTOP(+XQORNOD))
 D EN3^BEHOVMC(DFN,BEHVSDT,BEHVFDT) G:BEHOUT VCOUT
VCOUT ;
 Q
IANDO(ROOT,ORDFN,ID,ALPHA,OMEGA,ORDTRNG,REMOTE,ORMAX,ORFHIE) ; -- get Intake and output report
 D START^ORWRP(132,"IANDOSB^BEHOVMRP(.ROOT,.ORDFN,.ID,.ALPHA,.OMEGA,.ORDTRNG,.REMOTE,.ORMAX,.ORFHIE)")
 Q
IANDOSB(ROOT,ORDFN,ID,ALPHA,OMEGA,ORDTRNG,REMOTE,ORMAX,ORFHIE) ; -- build I&O report
 N ORVP,XQORNOD,ORSSTRT,ORSSTOP
 Q:'$G(ORDFN)
 I $L(ORDTRNG),'$G(ALPHA) S ALPHA=$$FMADD^XLFDT(DT,-ORDTRNG),OMEGA=$$NOW^XLFDT
 Q:'$G(ALPHA)  Q:'$G(OMEGA)
 I '$P(OMEGA,".",2) S OMEGA=OMEGA_".2359"
 S ORVP=ORDFN_";DPT(",XQORNOD=1,ORSSTRT(XQORNOD)=ALPHA,ORSSTOP(XQORNOD)=OMEGA
 D IOCUM
 Q
IOCUM ; Print I&O Cumulative Report
 N VA,VADM,DFN,%,GMREDB,GMRSTRT,GMRFIN,GMRCOL,GMROUT,GRPT
 S DFN=+ORVP,GMRSTRT=$G(ORSSTRT(+XQORNOD)),GMRFIN=$G(ORSSTOP(+XQORNOD))
 S GMREDB="P",GRPT=1,GMROUT=0
 S GMRCOL=6*(3+$P(^GMRD(126.56,0),"^",4)+$P(^GMRD(126.58,0),"^",4))
 D START^GMRYRP0 G VCOUT
 Q
VERR(ROOT,ORDFN,ID,ALPHA,OMEGA,ORDTRNG,REMOTE,ORMAX,ORFHIE) ; -- get error report
 D START^ORWRP(132,"VERRSB^BEHOVMRP(.ROOT,.ORDFN,.ID,.ALPHA,.OMEGA,.ORDTRNG,.REMOTE,.ORMAX,.ORFHIE)")
 Q
VERRSB(ROOT,ORDFN,ID,ALPHA,OMEGA,ORDTRNG,REMOTE,ORMAX,ORFHIE) ; -- build vitals error report
 N ORVP,XQORNOD,ORSSTRT,ORSSTOP
 Q:'$G(ORDFN)
 I $L(ORDTRNG),'$G(ALPHA) S ALPHA=$$FMADD^XLFDT(DT,-ORDTRNG),OMEGA=$$NOW^XLFDT
 Q:'$G(ALPHA)  Q:'$G(OMEGA)
 I '$P(OMEGA,".",2) S OMEGA=OMEGA_".2359"
 S ORVP=ORDFN_";DPT(",XQORNOD=1,ORSSTRT(XQORNOD)=ALPHA,ORSSTOP(XQORNOD)=OMEGA
 D ERRPT
 Q
ERRPT ; Print Vitals Entered in error Report
 N VA,VADM,DFN,%,GMREDB,GMRVSDT,GMRVFDT,GMRCOL,PARAM,DATA
 S DFN=+ORVP,BEHVSDT=$G(ORSSTRT(+XQORNOD)),BEHVFDT=$G(ORSSTOP(+XQORNOD))
 S PARAM="BEHOVM ERROR RPT"
 D GETPAR^CIAVMRPC(.DATA,PARAM)
 I DATA=0 W !,"You are not authorized to view the error reprort" Q
 I DATA=1 D EN1^BEHOVMER
 Q
