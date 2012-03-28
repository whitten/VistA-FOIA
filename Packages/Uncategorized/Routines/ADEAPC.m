ADEAPC ;IHS/HQW/MJL - DENTAL PCC LINK  [ 08/26/2008  7:16 AM ]
 ;;6.0;ADE;**12,16,20**;APRIL 1999
 ;IHS/SET/HMW 20050414 Rewrote this routine to call BSDAPI
 ;IHS/CMI/LAB 20080826 Added interactive PCC Link
 ;
 ;------->INITIALIZE
 Q:'$D(^ADEPARAM(+^AUTTSITE(1,0),0))  ;Q:$P(^(0),U,5)'="y"  ;IHS/SET/HMW 2-6-2003 **12** PCC Link mandatory
 K APCDALVR
 S AUPNTALK=1,APCDANE=1,APCDAUTO=1
 S APCDALVR("APCDDATE")=$P(^ADEPCD(ADEDFN,0),U,2) S APCDALVR("APCDLOC")="`"_$P(^(0),U,3)
 S APCDALVR("APCDPAT")=ADEPAT
 S APCDALVR("APCDCLN")=$O(^DIC(40.7,"C",56,0))
 S:APCDALVR("APCDCLN") APCDALVR("APCDCLN")="`"_APCDALVR("APCDCLN")
 S APCDALVR("APCDACS")=""
 S APCDALVR("APCDTYPE")="I"
 S ADEAPC("ADELOC")=$P(APCDALVR("APCDLOC"),"`",2)
 S ADEAPC("ADELOC")=$O(^ADEPARAM(+^AUTTSITE(1,0),1,"B",ADEAPC("ADELOC"),0))
 S ADEAPC("ADETYP")=""
 I ADEAPC("ADELOC")]"",$D(^ADEPARAM(+^AUTTSITE(1,0),1,ADEAPC("ADELOC"),0)) S ADEAPC("ADETYP")=$P(^ADEPARAM(+^AUTTSITE(1,0),1,ADEAPC("ADELOC"),0),U,3)
 I ADEAPC("ADETYP")]"" S APCDALVR("APCDTYPE")=ADEAPC("ADETYP")
 K ADEAPC("ADELOC"),ADEAPC("ADETYP")
 ;------->IF CONTRACT VISIT SET APPROPRIATE APC VARIABLES
 D CONTRACT
 ;------->CREATE VISIT OR APPEND TO EXISTING
 D DSERV
 I 'ADENEWVS D
 . Q:'$D(^ADEPCD(ADEDFN,"PCC"))
 . D ^ADEAPC2
 . D VMOD
 . D ^ADEAPC1
 E  D
 . N ADEV,ADEADD
 . S ADEV=0,ADEADD=""
 . D VISIT(.ADEV,.ADEADD)
 . I '+ADEV D BULLT Q
 . D REV(ADEV) ;IHS/OIT/HMW 9-22-2005 **16**Mark visit as reviewed 
 . S APCDALVR("APCDVSIT")=ADEV
 . I ADEADD="ADD" D
 . . ;MAKE POV, PRV AND VDEN ENTRIES
 . . N ADEV
 . . D ^ADEAPC1
 . E  D
 . . ;APPENDING TO EXISTING VISIT SO MAKE VDEN ENTRIES
 . . N ADEW,XMB,ADEHIT
 . . S ADEHIT=0
 . . D DENTRY^ADEAPC1
 . . D:$D(ADEW) DENTRY3^ADEAPC1
 . . D:$D(XMB) BULLT^ADEAPC1
 . . ;modify any existing primary provider on the visit to be secondary
 . . ;unless it's the same as the ADE-entered provider
 . . I $D(^AUPNVPRV("AD",ADEV)) D
 . . . N ADEVPRV,APCDALVR,DA,DIE,ADEPRV,ADEDA
 . . . S ADEPRV=$S($P(^DD(9000010.06,.01,0),U,2)["P6":$P(^ADEPCD(ADEDFN,0),U,4),1:^DIC(16,$P(^ADEPCD(ADEDFN,0),U,4),"A3"))
 . . . S ADEDA=0 F  S ADEDA=$O(^AUPNVPRV("AD",ADEV,ADEDA)) Q:'+ADEDA  D
 . . . . N DA
 . . . . S DA=ADEDA
 . . . . Q:'$D(^AUPNVPRV(DA,0))
 . . . . Q:$P(^AUPNVPRV(DA,0),U,4)'="P"
 . . . . I $P(^AUPNVPRV(DA,0),U)=ADEPRV S ADEHIT=1 Q  ;If prim provider is ADE provider, do nothing
 . . . . S DIE=9000010.06
 . . . . S DR=".04////S" ;otherwise, make it secondary
 . . . . D ^DIE,MOD^AUPNVSIT
 . . . Q
 . . ;Add the ADE-entered dental provider as the primary, unless it was already there
 . . I 'ADEHIT D VPRV^ADEAPC1
 . . ;Add a VPOV entry if none already exists on the visit.
 . . I '$D(^AUPNVPOV("AD",ADEV)) D VPOV^ADEAPC1
 . . Q
 . D ADDPCC^ADEAPC2("301///"_ADEV,ADEDFN)
 . Q
 ;
 ;------->END
END K APCDALVR,ADEOP,ADEADA,ADEQTY,ADEI,ADEC,ADESER,ADEX,ADEV,ADEY,Y,XMB
 K ADETSUR
 K ADETFEE ;IHS/SET/HMW 2-6-2003 **12**
 Q
 ;
REV(ADEV) ;IHS/OIT/HMW 9-22-2005 **16**Mark visit as reviewed 
 N ADEFDA,ADEIEN,ADEMSG
 S ADEFDA(9000010,ADEV_",",1111)="R"
 D UPDATE^DIE(,"ADEFDA","ADEIEN","ADEMSG")
 Q
 ;
VISIT(ADEV,ADEADD) ;
 ;
 N ADEBSD,ADEBSOUT,ADEHIT
 S ADEV=0,ADEADD="",ADEHIT=0
 ;
 ;If GETVISIT API not installed, call APCDALV directly and add a new visit
 I $T(GETVISIT^APCDAPI4)="" D  Q
 . D EN^APCDALV
 . I $D(APCDALVR("APCDAFLG")) S ADEV=0 Q
 . S ADEV=APCDALVR("APCDVSIT")
 . S ADEADD="ADD"
 . Q
 ;
 S ADEBSD("PAT")=ADEPAT
 S ADEBSD("VISIT DATE")=APCDALVR("APCDDATE")
 S ADEBSD("SITE")=$P(APCDALVR("APCDLOC"),"`",2)
 S ADEBSD("VISIT TYPE")=APCDALVR("APCDTYPE")
 S ADEBSD("SRV CAT")="A"
 S ADEBSD("TIME RANGE")=-1
 S ADEBSD("CLINIC CODE")=$P(APCDALVR("APCDCLN"),"`",2)
 S ADEBSD("USR")=$G(DUZ)
 N APCDALVR
 D GETVISIT^APCDAPI4(.ADEBSD,.ADEBSOUT)
 ;
 ;IHS/CMI/LAB - added line below to allow a site to have an interactive pcc link
 I $P($G(^ADEPARAM(+^AUTTSITE(1,0),0)),U,11) D INTERACT Q
 ;IHS/CMI/LAB - end mods
 ;
 S ADEV=0 F  S ADEV=$O(ADEBSOUT(ADEV)) Q:'+ADEV  D  Q:ADEHIT
 . I ADEBSOUT(ADEV)="ADD" S ADEHIT=1,ADEADD="ADD" Q
 . ;SKIP IF VISIT HAS EXISTING V DENTAL ENTRIES
 . Q:$D(^AUPNVDEN("AD",ADEV))
 . ;VISIT MATCHES AND HAS NO V DENTAL ENTRIES, SO USE IT
 . S ADEHIT=1,ADEADD="" Q
 ;
 ;IF NONE OF THE RETURNED VISITS WORKED, THEN FORCE AN ADD
 I '+ADEV D
 . S ADEBSD("FORCE ADD")=1
 . D GETVISIT^APCDAPI4(.ADEBSD,.ADEBSOUT)
 . Q:'+ADEBSOUT(0)
 . S ADEV=$O(ADEBSOUT(0))
 . S ADEADD="ADD"
 ;
 Q
 ;
VMOD S APCDALVR("APCDATMP")="[APCDALVR 9000010 (MOD)]" D EN^APCDALVR
 I $D(APCDALVR("APCDAFLG")) S XMB(7)="PCC VISIT"
 Q
DSERV S ADESER=0
 F ADEI=1:1 S ADESER=$O(^ADEPCD(ADEDFN,"ADA",ADESER)),ADEC=ADEI-1 Q:'+ADESER  D
 . N ADENOD
 . S ADENOD=^ADEPCD(ADEDFN,"ADA",ADESER,0)
 . Q:$P($G(^AUTTADA($P(ADENOD,U),0)),U,7)=1  ;IHS/SET/HMW 4-13-2005 **16**
 . S ADEADA(ADEI)=$P(ADENOD,U)
 . S ADEQTY(ADEI)=1
 . S ADEOP(ADEI)=$P(ADENOD,U,2)
 . S ADETFEE(ADEI)=$P(ADENOD,U,3) ;IHS/SET/HMW 2-6-2003 **12**
 . S ADETSUR(ADEI)=$P(ADENOD,U,4)
 ;F ADEI=1:1:ADEC S:$P(^AUTTADA(ADEADA(ADEI),0),U,7)=1 Y=0 ;IHS/OIT/HMW 4-13-2005 **16**
 Q
CONTRACT ;IHS/SET/HMW 2-6-2003 Modified this subroutine to append ` to APCDALVR("APCDCLN")
 I ADECON D
 . S APCDALVR("APCDTYPE")="CONTRACT"
 . S APCDALVR("APCDCLN")=$O(^DIC(40.7,"C",99,""))
 . S:APCDALVR("APCDCLN") APCDALVR("APCDCLN")="`"_APCDALVR("APCDCLN")
 . S APCDALVR("APCDTNQ")="CONTRACT DENTAL/ORAL HEALTH VISIT"
 Q
BULLT S %DT="",X="T" D ^%DT X ^DD("DD")
 S XMB(1)=$P(^DPT(ADEPAT,0),U,1)_" Patient DFN= "_ADEPAT,XMB(2)=Y,XMB(6)="A PCC visit could not be created for this patient",XMB="ADEVISIT" S XMDUZ=.5 D ^XMB
 Q
 ;
INTERACT ;
 ;IHS/CMI/LAB - interactive pcc link
 ;first, if 1 visit passed back and it was an add use it and quit
 I $P(ADEBSOUT(0),U)=1 S V=$O(ADEBSOUT(0)) I ADEBSOUT(V)="ADD" S ADEV=V,ADEHIT=1,ADEADD="ADD" Q
 ;since more than one passed back display them to the user and quit
SELECT ; SELECT EXISTING VISIT
 NEW ADEV1,ADEC,ADEA,ADEX,ADEA,ADEB,ADEVLT,ADEVLOC
 S ADEV=""
 W !!,"PATIENT: ",$P(^DPT(ADEPAT,0),U)," has one or more VISITs on this date.",!,"If one of these is your visit, please select it",!
 K ADEV1 S (ADEC,ADEA,ADEX)="",ADEV1=0 F  S ADEV1=$O(ADEBSOUT(ADEV1)) Q:ADEV1'=+ADEV1  S ADEX=$G(^AUPNVSIT(ADEV1,0)),ADEX11=$G(^AUPNVSIT(ADEV1,11)) D WRITE
 S ADEC=ADEC+1 W !,ADEC,"  Create New Visit",!
 K DIR
 S DIR(0)="N^1:"_ADEC,DIR("A")="Select" KILL DA D ^DIR KILL DIR
 I $D(DIRUT) S ADEBSD("FORCE ADD")=1 D BSDADD1 Q
 I ADEC=Y S ADEBSD("FORCE ADD")=1 D BSDADD1 Q
 S ADEV=ADEX1(Y)
 Q
 ;
WRITE ; WRITE VISITS FOR SELECT
 S ADEC=ADEC+1,ADEX1(ADEC)=ADEV1
 S ADEVLT=$P(+ADEX,".",2),ADEVLT=$S(ADEVLT="":"<NONE>",$L(ADEVLT)=1:ADEVLT_"0:00 ",1:$E(ADEVLT,1,2)_":"_$E(ADEVLT,3,4)_$E("00",1,2-$L($E(ADEVLT,3,4)))_" ")
 S ADEVLOC=""
 I $P(ADEX,U,6),$D(^AUTTLOC($P(ADEX,U,6),0)) S ADEVLOC=$P(^(0),U,7),ADEVLOC=ADEVLOC_$E("    ",1,4-$L(ADEVLOC))
 S:ADEVLOC="" ADEVLOC="...."
 W !,ADEC,"  TIME: ",ADEVLT,"LOC: ",ADEVLOC," TYPE: ",$P(ADEX,U,3)," CAT: ",$P(ADEX,U,7)," CLINIC: ",$S($P(ADEX,U,8)]"":$E($P(^DIC(40.7,$P(ADEX,U,8),0),U),1,8),1:"<NONE>") D
 .W ?57,"DEC: ",$S($P(ADEX,U,9):$P(ADEX,U,9),1:0),$S($P(ADEX11,U,3)]"":" VCN:"_$P(ADEX11,U,3),1:"")
 .I $P(ADEX,U,22) W !?3,"Hospital Location: ",$P($G(^SC($P(ADEX,U,22),0)),U)
 .S ADETIU=$$PRIMPROV^APCLV(ADEV1,"N") I ADETIU]"" W !?3,"Provider on Visit: ",ADETIU
 .S ADEA=0,ADEB="" F  S ADEA=$O(^AUPNVDEN("AD",ADEV1,ADEA)) Q:ADEA'=+ADEA  S ADEB=ADEB_$$VAL^XBDIQ1(9000010.05,ADEA,.01)_" ; "
 .I ADEB]"" W !?3,"Dental ADA Codes:  ",ADEB
 Q
 ;
BSDADD1 ;
 ;IF NONE OF THE RETURNED VISITS WORKED, THEN FORCE AN ADD
 I '+ADEV D
 . S ADEBSD("FORCE ADD")=1
 . D GETVISIT^APCDAPI4(.ADEBSD,.ADEBSOUT)
 . Q:'+ADEBSOUT(0)
 . S ADEV=$O(ADEBSOUT(0))
 . S ADEADD="ADD"
 ;
 Q
