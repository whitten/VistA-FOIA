BGPGTXE ;cmi/anch/maw - ATX Gui Save Utilities 4/28/2005 9:22:40 AM
 ;;9.0;IHS CLINICAL REPORTING;**1**;JUL 01, 2009
 ;
 ;
 ;
 ;
 ;this routine will save from GUI applications
DEBUG(ATXRET,ATXSTR) ;-- call serenji debugger
 ;D DEBUG^%Serenji("TAX^ATXGE(.ATXRET,.ATXSTR)")
 Q
 ;
 Q
TAX(ATXRET,ATXSTR) ;EP -- save taxonomy
 S X="MERR^BGPGTXU",@^%ZOSF("TRAP") ; m error trap
 N P,ATXFL,ATXTAXN,ATXTAXF,ATXTAX,ATXI,ATXP,ATXERR,ATXTAXM,ATXTXG,ATXTXF,ATXTAXPF,ATXLOOK,ATXTX,ATXTXM
 I ATXSTR="" D CATSTR^BGPGTXU(.ATXSTR,.ATXSTR)
 K ^ATXTMP($J)
 S ATXRET="^ATXTMP("_$J_")"
 S ATXI=0
 S ATXERR=""
 S ^ATXTMP($J,ATXI)="T00080TAXONOMYSAVE"_$C(30)
 S ATXI=ATXI+1
 I $G(ATXSTR)="" S ^ATXTMP($J,ATXI)="Error Concatenating String for Taxonomy"_$C(30) Q
 S P="|"
 S ATXTAXN=$P(ATXSTR,P)
 S ATXTAXF=$P(ATXSTR,P,2)
 S ATXTXG="^ATXAX(""B"")"
 S ATXTX="^ATXAX("
 S ATXTXF=9002226
 S ATXTXM=9002226.02101
 I ATXTAXF="Lab" D
 . S ATXTXG="^ATXLAB(""B"")"
 . S ATXTX="^ATXLAB("
 . S ATXTXF=9002228
 . S ATXTXM=9002228.02101
 . S ATXFL=60
 S ATXTAX=$O(@ATXTXG@(ATXTAXN,0))
 S ATXLOOK=0
 I 'ATXTAX S ^ATXTMP($J,ATXI)="Taxonomy does not exist on System"_$C(30) Q
 S ATXLOOK=0
 I '$$GET1^DIQ(ATXTXF,ATXTAX,.13) S ATXLOOK=1
 D CLEANTAX(ATXTX,ATXTAX)
 F ATXP=3:1 S ATXTAXM=$P(ATXSTR,P,ATXP) Q:$G(ATXTAXM)=""  D
 . N ATXIENS,ATXFDA
 . S ATXIENS="+2,"_ATXTAX_","
 . I ATXLOOK D
 .. N ATXGLF
 .. I '$G(ATXFL) S ATXFL=$P($G(^ATXAX(ATXTAX,0)),U,15)
 .. S ATXGLF=$G(^DIC(ATXFL,0,"GL"))
 .. S ATXGLF=ATXGLF_"""B"")"
 .. S ATXTAXM=$O(@ATXGLF@(ATXTAXM,0))
 . I 'ATXLOOK D
 .. S ATXTAXM=$P(ATXTAXM,"-")
 .. S ATXTAXM=ATXTAXM_" "
 . Q:$G(ATXTAXM)=""
 . S ATXFDA(ATXTXM,ATXIENS,.01)=ATXTAXM
 . I ATXTXM=9002226.02101 S ATXFDA(ATXTXM,ATXIENS,.02)=ATXTAXM
 . D UPDATE^DIE("","ATXFDA","ATXIENS","ATXERR(1)")
 S ^ATXTMP($J,ATXI+1)=$C(31)_$G(ATXERR)
 Q
 ;
CLEANTAX(TAXF,TAX) ;EP -- remove existing entries from 21 multiple before adding
 N ATXDA,TAXI,ATXAA
 S TAXI=TAXF_TAX_",21)"
 S DIK=TAXF_TAX_",21,",DA(1)=TAX
 S ATXDA=0 F  S ATXDA=$O(@TAXI@(ATXDA)) Q:'ATXDA  D
 . S DA=ATXDA
 . D ^DIK
 S ATXAA=TAXF_TAX_",21)"
 K @ATXAA@("AA")
 K DIK,DA
 Q
 ;
LABTAX(ATXRET,ATXSTR) ;EP -- return the lab taxonomy
 S X="MERR^BGPGTXU",@^%ZOSF("TRAP") ; m error trap
 N P,ATXRFL,ATXI,ATXERR,ATXIEN,ATXTAXE,ATXTAX,ATXDA,ATXNONC,ATXXRF
 N ATXGL,ATXGRF,ATXP
 S P="|"
 K ^ATXTMP($J)
 S ATXRET="^ATXTMP("_$J_")"
 S ATXI=0
 S ATXERR=""
 S ^ATXTMP($J,ATXI)="T00080LABTAXONOMY"_$C(30)
 F ATXP=3:1 S ATXTAXE=$P(ATXSTR,P,ATXP) Q:$G(ATXTAXE)=""  D
 . Q:$G(ATXTAXE)=""
 . ;S ATXTAXE=$P(ATXSTR,P)
 . S ATXTAX=$O(^ATXLAB("B",ATXTAXE,0))
 . Q:'$G(ATXTAX)
 . S ATXNONC=$P($G(^ATXLAB(ATXTAX,0)),U,13)
 . S ATXXRF=$P($G(^ATXLAB(ATXTAX,0)),U,14)
 . S ATXFL=60
 . S ATXGL=$G(^DIC(ATXFL,0,"GL"))
 . S ATXDA=0 F  S ATXDA=$O(^ATXLAB(ATXTAX,21,ATXDA)) Q:'ATXDA  D
 .. N ATXL,ATXH
 .. S ATXI=ATXI+1
 .. S ATXL=$P($G(^ATXLAB(ATXTAX,21,ATXDA,0)),U)
 .. S ATXH=$P($G(^ATXLAB(ATXTAX,21,ATXDA,0)),U,2)
 .. I (ATXL=ATXH)!($G(ATXH)="") D  Q
 ... I $G(ATXXRF)="" D  Q
 .... S ATXGRF=ATXGL_""""_ATXL_""""_")"
 .... S ^ATXTMP($J,ATXI)=$P($G(@ATXGRF@(0)),U)_$C(30)
 ... S ^ATXTMP($J,ATXI)=ATXL_$C(30)
 .. S ATXGRF=ATXGL_""""_ATXXRF_""")"
 .. N ATXIEN
 .. S ATXIEN=$O(@ATXGRF@(ATXL),-1)
 .. F  S ATXIEN=$O(@ATXGRF@(ATXIEN)) Q:ATXIEN>ATXH  D
 ... S ATXI=ATXI+1
 ... S ^ATXTMP($J,ATXI)=ATXIEN_$C(30)
 S ^ATXTMP($J,ATXI+1)=$C(31)_$G(ATXERR)
 Q
 ;
LTAX(ATXRET) ;EP -- generic lab taxonomy table
 S X="MERR^BGPGTXU",@^%ZOSF("TRAP") ; m error trap
 N P,ATXRFL,ATXI,ATXERR,ATXIEN,ATXTAXE,ATXTAX,ATXDA,ATXNONC,ATXXRF
 N ATXGL,ATXGRF,ATXP,ATXPKG,ATXTDA,ATXPKGI
 S P="|"
 K ^ATXTMP($J)
 S ATXRET="^ATXTMP("_$J_")"
 S ATXI=0
 S ATXERR=""
 S ATXPKG=$P(ATXSTR,P,2)
 S ATXPKGI=$O(^DIC(9.4,"C",ATXPKG,0))
 S ^ATXTMP($J,ATXI)="T00080LABTAXONOMY"_$C(30)
 S ATXTAX=0 F  S ATXTAX=$O(^ATXLAB("APKG",ATXPKGI,ATXTAX)) Q:'ATXTAX  D
 . ;S ATXTAX=0 F  S ATXTAX=$O(^ATXLAB(ATXTAX)) Q:'ATXTAX  D
 . ;S ATXTAX=$P($G(^ATXLAB(ATXP,0)),U)
 . ;Q:$E($P($G(^ATXLAB(ATXTAX,0)),U),1,2)'="DM"
 . ;S ATXTAXE=$P(ATXSTR,P)
 . ;S ATXTAX=$O(^ATXLAB("B",ATXTAXE,0))
 . ;Q:'$G(ATXTAX)
 . S ATXXRF=$P($G(^ATXLAB(ATXTAX,0)),U,8)
 . I $G(ATXXRF)="" S ATXXRF="B"
 . S ATXFL=$P($G(^ATXLAB(ATXTAX,0)),U,9)
 . S ATXGL=$G(^DIC(ATXFL,0,"GL"))
 . S ATXDA=0 F  S ATXDA=$O(^ATXLAB(ATXTAX,21,ATXDA)) Q:'ATXDA  D
 .. N ATXL,ATXH
 .. S ATXI=ATXI+1
 .. S ATXL=$P($G(^ATXLAB(ATXTAX,21,ATXDA,0)),U)
 .. S ATXGRF=ATXGL_""""_ATXL_""""_")"
 .. N ATXIEN
 .. S ATXI=ATXI+1
 .. S ^ATXTMP($J,ATXI)=$P($G(@ATXGRF@(0)),U)_$C(30)
 S ^ATXTMP($J,ATXI+1)=$C(31)_$G(ATXERR)
 Q
 ;
MEDBLD ;EP -- setup ATXSTR for medication taxonomy
 S X="MERR^BGPGTXU",@^%ZOSF("TRAP") ; m error trap
 N ATXTDA,ATXI,ATXPKG,ATXPKGI
 S ATXI=1
 S ATXPKG=$P(ATXSTR,P,2)
 S ATXPKGI=$O(^DIC(9.4,"C",ATXPKG,0))
 S ATXTDA=0 F  S ATXTDA=$O(^ATXAX(ATXTDA)) Q:'ATXTDA  D
 . ;S ATXTDA=0 F  S ATXTDA=$O(^ATXAX(ATXTDA)) Q:'ATXTDA  D
 . ;Q:$E($P($G(^ATXAX(ATXTDA,0)),U),1,2)'="DM"
 . Q:$P($G(^ATXAX(ATXTDA,0)),U,15)'=50
 . S ATXI=ATXI+1
 . S $P(ATXSTR,P,ATXI)=$P($G(^ATXAX(ATXTDA,0)),U)
 Q
 ;
