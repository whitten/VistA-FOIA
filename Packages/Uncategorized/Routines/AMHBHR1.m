AMHBHR1 ; IHS/CMI/LAB - behavioral health display for GUI ;
 ;;4.0;IHS BEHAVIORAL HEALTH;;MAY 14, 2010
 ;
 ;
 ;
TEST ;
 D TPRES(.RETVAL,"01/01/1900","12/31/2005","A","")
 Q
TPRES(AMHARRAY,AMHBD,AMHED,AMHOA,AMHTHER) ;EP - AMHBH RPT TP NEED RESOLVEDED
TPRES1 S JOB=$J,AMHGUI=1,XWBWRAP=1
 S ZTIO="",ZTQUEUED=1
 S AMHARRAY="^XTMP(""AMHRPT"","_$J_")"
 I $G(AMHBD)="" S ^XTMP("AMHRPT",JOB,.5)=2,^XTMP("AMHRPT",JOB,1)="Invalid beginning date passed" D KILL Q
 D DT^DILF("X",AMHBD,.AMHBD)
 I $G(AMHBD)=-1 S ^XTMP("AMHRPT",JOB,.5)=2,^XTMP("AMHRPT",JOB,1)="Invalid beginning date passed" D KILL Q
 I $G(AMHED)="" S ^XTMP("AMHRPT",JOB,.5)=2,^XTMP("AMHRPT",JOB,1)="Invalid ending date passed" D KILL Q
 D DT^DILF("X",AMHED,.AMHED)
 I $G(AMHED)=-1 S ^XTMP("AMHRPT",JOB,.5)=2,^XTMP("AMHRPT",JOB,1)="Invalid ending date passed" D KILL Q
 I $G(AMHOA)="" S ^XTMP("AMHRPT",JOB,.5)=2,^XTMP("AMHRPT",JOB,1)="All or One variable not passed" D KILL Q
 I "OA"'[AMHOA S ^XTMP("AMHRPT",JOB,.5)=2,^XTMP("AMHRPT",JOB,1)="All or One variable not passed as O or A" D KILL Q
 S AMHOA=AMHOA
 I AMHOA="O",$G(AMHTHER)="" S ^XTMP("AMHRPT",JOB,.5)=2,^XTMP("AMHRPT",JOB,1)="One provider parameter set, provider not defined" D KILL Q
 I AMHOA="O",'$D(^VA(200,AMHTHER,0)) S ^XTMP("AMHRPT",JOB,.5)=2,^XTMP("AMHRPT",JOB,1)="Provider IEN passed is not valid" D KILL Q
 S AMHTHER=$G(AMHTHER)
 K ^XTMP("AMHRPT",JOB)
 S ^XTMP("AMHRPTRUN",JOB)=""
 D ^XBKSET
 D PROC^AMHLETR
 ;S ZTRTN="TSK^AMHBHR1",ZTIO="",ZTDESC="AMH TP RES REPORT",ZTSAVE("AMH*")="",ZTSAVE("JOB")="",ZTDTH=$H D ^%ZTLOAD
 ;F I=1:1:120 Q:$G(^XTMP("AMHRPTRUN",$J))="DONE"  H 1
 D TSK
 D KILL
 Q
 ;
TSK ;
 D ^XBKSET
 S ^XTMP("AMHRPTRUN",JOB)="START"
 D GUIR^XBLM("PRINT^AMHLETR","^XTMP(""AMHRPT"",JOB)")
 S ^XTMP("AMHRPT",JOB,.5)=$O(^XTMP("AMHRPT",JOB,""),-1)+1
 S ^XTMP("AMHRPTRUN",JOB)="DONE"
 Q
 ;
KILL ;
 K AMHOA,AMHBT,AMHTOT
 K AMHCTR,AMHGUI,AMHSF,DIC,JOB,X,Y,ZTDESC,ZTDTH,ZTIO,ZTRTN,ZTSAVE
 D XIT^AMHLETR
 Q
