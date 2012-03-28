AZFMENV ;IHS/OIT/ENM - PHARMACY ENVIRONMENT CHECK [ 02/13/2006  2:54 PM ]
 ;;IHS PHARMACY MODIFICATIONS;;
ALLPKG ;LOOP ON PACKAGE FILE "B" XREF
 S AZFMSITE="",PKRN="",AZASUFAC="",AZFMCT=1
 I $G(DUZ)'>0!('$D(DUZ(0))) W !,"YOUR DUZ VARIABLES ARE NOT DEFINED! AT THE PROMPT, ENTER 'D ^XUP'",!,"AND RUN THIS PROGRAM AFTERWARDS.",! Q
 I $G(DUZ(2))>0 S AZFMSITE=$P($G(^DIC(4,DUZ(2),0)),"^",1),AZASUFAC=$P($G(^AUTTLOC(DUZ(2),0)),"^",10) W !,"SITE NAME: ",AZFMSITE_" "_AZASUFAC,!
 W !,"SOURCE: Package File - Package Name - Current Version - Last Patch",!
 W !,"PACKAGE NAME",?40,"CURRENT",?50,"LATEST",!,?40,"VERSION",?50,"PATCH/DATE",!,"------------",?40,"---------------",?50,"----------"
 S AZFM="",LPATCH="",LPAT=""
 F NA=0:0 S AZFM=$O(^DIC(9.4,"B",AZFM)) Q:AZFM=""  D PKGWR
 ;D XBC ;MOVE XTMP GBL TO UNIX
 D OUT
 Q
PKGWR S AZVER=$$VERSION^XPDUTL(AZFM),LPATCH=$$LAST^AZFMENV2(AZFM,AZVER),LPATCH=$S(LPATCH'[-1:LPATCH,1:"") D DTFIX
 I LPATCH="" S LPAT=""
 I LPATCH]"" S LPAT=$P(LPATCH,"^",1)_" / "_$P(LPATCH,"^",2)
 ;GRAB THE PREFIX
 S PKRN=$O(^DIC(9.4,"B",AZFM,"")) S:'PKRN PKRN=""
 S PREFIX=$P($G(^DIC(9.4,PKRN,0)),"^",2)
 W !,AZFM_"-("_PREFIX_")",?40,AZVER,?50,LPAT I AZFM="IHS V FILES 200 CONVERSION" D PCC2
 S ^AZFMENV(AZFMCT)=AZASUFAC_"^"_AZFM_"^"_AZVER_"^"_LPAT
 S AZFMCT=AZFMCT+1
 Q
PCC2 S PCCF200=$P($G(^AUTTSITE(1,0)),"^",22) I PCCF200="" S PCCF200="No"
 I PCCF200=1 S PCCF200="YES"
 W !,?30,"IHS V Files 200 Conversion Done?// ",PCCF200
 Q
DOONE ;DO ONE RECORD FOR TESTING ONLY
 S AZFM="ADVERSE REACTION TRACKING" D PKGWR
 D OUT
 Q
DTFIX Q:'+LPATCH
 S Y=$P(LPATCH,"^",2) X ^DD("DD") S $P(LPATCH,"^",2)=" "_Y
 Q
OUT ;
 W !!,"END OF PACKAGE FILE PROFILE CHECK!"
 K NA,AZFM,AZVER,LPATCH,PCCF200,AZASUFAC
 Q
PFIX S AZFMSITE="",PKRN=""
 I $G(DUZ)'>0!('$D(DUZ(0))) W !,"YOUR DUZ VARIABLES ARE NOT DEFINED! AT THE PROMPT, ENTER 'D ^XUP'",!,"AND RUN THIS PROGRAM AFTERWARDS.",! Q
 I $G(DUZ(2))>0 S AZFMSITE=$P($G(^DIC(4,DUZ(2),0)),"^",1) W !,"SITE NAME: ",AZFMSITE,!
 W !,"SOURCE: Package File - Package Name - Current Version - Last Patch",!
 W !,"PACKAGE NAME",?40,"CURRENT",?50,"LATEST",!,?40,"VERSION",?50,"PATCH/DATE",!,"------------",?40,"---------------",?50,"----------"
 S AZFM="",LPATCH="",LPAT=""
 F NA=0:0 S AZFM=$O(^DIC(9.4,"B",AZFM)) Q:AZFM=""  D PGWR
 D OP2
 D OUT
 Q
PGWR S AZVER=$$VERSION^XPDUTL(AZFM),LPATCH=$$LAST^AZFMENV2(AZFM,AZVER),LPATCH=$S(LPATCH'[-1:LPATCH,1:"") D DTFIX
 I LPATCH="" S LPAT=""
 I LPATCH]"" S LPAT=$P(LPATCH,"^",1)_" / "_$P(LPATCH,"^",2)
 ;GRAB THE PREFIX
 S PKRN=$O(^DIC(9.4,"B",AZFM,"")) S:'PKRN PKRN=""
 S PREFIX=$P($G(^DIC(9.4,PKRN,0)),"^",2)
 ;W !,AZFM_"-("_PREFIX_")",?40,AZVER,?50,LPAT I AZFM="IHS V FILES 200 CONVERSION" D PCC2
 S ENM(PREFIX)=AZFM_"^"_AZVER_"^"_LPAT
 Q
OP2 ;
 S PFX="",AZFM1="",AZVER1="",APAT1=""
 F  S PFX=$O(ENM(PFX)) Q:'PFX  S AZFM1=$P($G(ENM(PFX)),"^",1),AZVER1=$P($G(ENM(PFX)),"^",2),APAT1=$P($G(ENM(PFX)),"^",3) D LS
 Q
LS ;
 W !,AZFM1_"-("_PFX_")",?40,AZVER1,?50,LPAT1 I AZFM1="IHS V FILES 200 CONVERSION" D PCC2
 Q
 ;SAVE PACKAGE STATUS DATA TO A UNIX FILE
XBC ;XBGL=GLOBAL NAME,XBUF=UNIX DIRECTORY,XBFN=UNIX FILE NAME
 ;Next line will try to send file to 92.145 with ftpsend
 ;
 ;S XBGL="AZFMENV",XBUF="127.0.0.1",XBFN="AZFMENV"_AZASUFAC_".G",XBMED="F"
 S XBGL="AZFMENV(",XBFN="AZFMENV"_AZASUFAC_".G",XBMED="F"
 S XBS1="AZFMSITETRACK"
 ;S XBGL="AZFMENV",XBUF="/usr2/ihs/emoore",XBFN="AZFMENV"_AZASUFAC_".G",XBMED="F"
 D ^XBGSAVE
 Q
