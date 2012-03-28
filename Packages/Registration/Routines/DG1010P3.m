DG1010P3 ;ALB/REW - PRINT 1010 CONT'D PART III ; 8/18/00 9:47am
 ;;5.3;Registration;**343**;Aug 13, 1993
 ;;1
PARTIII ;
 K DGP,DGD,DESTXT,DGADDR,DGD1,DGDCTY,DGDSL,DGDST,DGDSTR,DGDSUM,DGDZIP,DGQ,DG1010P2
 F I="VET",0,.13,.22,.25,.3,.311,.32,.321,.322,.36,.52,.53 S DGP(I)=$G(^DPT(DFN,I))
 S DGDMAX=1
 S X=$$POINT^DG1010P0(DGP(0),5,11,3),DGNOTMAR=$S(((X="M")!(X="S")):0,1:1) ;0=>MARRIED,SEPARATED
 S DGSPC=1,DGBLANK=0
 W !!,?50,"PART III - APPLICANT/SPOUSE DATA",!,$C(13),DGLUND
 S DGNA=DGNOTMAR
 ;$$DISP(N,P,NA,BL) RETURNS THE Pth PIECE OF NODE N USUALLY N=DGP(Z) WHERE Z IS NODE SUBSCRIPT NULL=UNANWERED OR NOT APPLICABLE BASED ON NA.  BL=1 WILL CAUSE ""(NOTHING) PRINTED
 W !,"1.   APPLICANT'S EMPLOYMENT STATUS: ",$$ESTATUS(DGP(.311)),!,$C(13),DGLUND
 ;X IS EMP STATUS NUMBER ON FORM
 S DGVUNEMP=$S(("3^7"[+X):1,1:0)
 W !,"2.   SPOUSE'S EMPLOYMENT STATUS:  ",$$ESTATUS(DGP(.25),DGNA),!,$C(13),DGLUND
 S DGSUNEMP=$S(("3^7"[+X):1,1:0)
 W !,?66,"| ",!?14,"3. APPLICANT INFORMATION",?66,"| ",?87,"4. SPOUSE'S INFORMATION",?131,$C(13),DGLUND
 W !,"3A.  OCCUPATION: ",$$DISP^DG1010P0(DGP(0),7),?66,"| ","4A.  OCCUPATION: ",$$DISP^DG1010P0(DGP(.25),14,DGNA),!,DGL2
 W !,"3B.  EMPLOYER (Name, Street Address, City, State, Zip)",?66,"| "
 W "4B.  EMPLOYER (Name, Street Address, City, State, Zip)"
NAME W !?5,$$DISP^DG1010P0(DGP(.311),1,DGVUNEMP) S (DGBLANK,DGBLANK1)=DGUNK
 S DGNA=$S(DGSUNEMP:1,1:DGNA)
 W ?66,"| ",?73,$$DISP^DG1010P0(DGP(.25),1,DGNA) S DGBLANK2=DGUNK
SETADD ;
 S DGI=.311,DGNA=0
 I (DGNOTMAR)!(DGSUNEMP) F DGDPC=3:1:8 S DGD(.25,DGDPC)=""
 I DGVUNEMP F DGDPC=3:1:8 S DGD(.311,DGDPC)=""
 S DGPCDIFF=0 ;OFFSET FROM APPLICANT PIECE
 F DGI=.311,.25 D
 .Q:(DGI=.25)&(DGNOTMAR!DGSUNEMP)
 .Q:(DGI=.311)&(DGVUNEMP)
 .I DGI=.25 S DGPCDIFF=-1,DGNA=DGNOTMAR,DGBLANK=DGBLANK2
 .;ADDR=3-5,6-8=CITY,STATE (NOTE:ZIP+4 IS ON .22 NODE & NOT 8TH PC)
 .F DGDPC=3:1:8 D
 ..S:(DGDPC<6) DGD(DGI,DGDPC)=$$DISP^DG1010P0(DGP(DGI),(DGDPC+DGPCDIFF),0,1)
 ..S:(DGDPC=6) DGD(DGI,DGDPC)=$$DISP^DG1010P0(DGP(DGI),(DGDPC+DGPCDIFF),0,1)_$S(('DGUNK):", ",1:"")
 ..S:(DGDPC=7) X=$P(DGP(DGI),U,7+DGPCDIFF),DGD(DGI,DGDPC)=$$UNK^DG1010P0($P($G(^DIC(5,+X,0)),U,2),0,1)_$S(('DGUNK):", ",1:"")
 ..I (DGDPC=4)!(DGDPC=5) S:(DGDMAX<DGDPC)&($L(DGD(DGI,DGDPC))) DGDMAX=DGDPC
 ..S:(DGDPC=8) DGD(DGI,DGDPC)=$$DISP^DG1010P0(DGP(.22),($S(DGI=.25:6,1:5)),0,1)
 S DGBLANK=0
STREET W !?5,DGD(.311,3),?66,"| ",?73,DGD(.25,3)
 W:(DGDMAX>3) !?5,DGD(.311,4),?66,"|",?73,DGD(.25,4)
 W:(DGDMAX>4) !?5,DGD(.311,5),?66,"|",?73,DGD(.25,5)
CTSTZP W !?5,DGD(.311,6),DGD(.311,7),DGD(.311,8),?66,"| ",?73,DGD(.25,6),DGD(.25,7),DGD(.25,8),!,DGL2
 W !,"3C.  WORK TELEPHONE NUMBER: ",$$DISP^DG1010P0(DGP(.311),9,0,DGBLANK1),?66,"| ","4C.  WORK TELEPHONE NUMBER: ",$$DISP^DG1010P0(DGP(.25),8,DGNA,DGBLANK2),?131,$C(13),DGLUND
 K DGVUNEMP,DGSUNEMP
 G PARTIV^DG1010P4
ESTATUS(N,NA) ;
 ; Returns the external value of the Employment Status for either the
 ; Spouse or the Patient, unless NA=1.
 ;   INPUT: N    -- A node [either the .311 or the .25]
 ;          NA   -- If =1 returns 'NOT APPLICABLE'
 ;       [N is REQUIRED]
 ;   OUTPUT[Returned & SET] --  X
 I $G(NA) S X="NOT APPLICABLE" G QE
 S X=+($P(N,U,15))
 I 'X S X="NOT ANSWERED" G QE
 S X=$P($P(^DD(2,.31115,0),U,3),";",X)
 I 'X S X="7.  UNKNOWN" G QE ;RESULT IF INPUT X>6
 S X=+X_".  "_$P(X,":",2)
QE ;
 Q X
