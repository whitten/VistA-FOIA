RAHLRU ;HISC/SWM - utilities for HL7 messaging ;1/28/00  11:03
 ;;5.0;Radiology/Nuclear Medicine;**10,25**;Jan 28, 2000
OBX11 ; set OBX-11, = 12th piece of string where piece 1 is "OBX"
 N RARPTIEN,Y
 S RARPTIEN=+$G(RARPT)
 S Y=$P($G(^RARPT(RARPTIEN,0)),U,5)
 S $P(HLA("HLS",RAN),HLFS,12)=$S(Y="R":"P",Y="V":"F",1:"I")
 I $D(^RARPT(RARPTIEN,"ERR")) D  Q
 .S $P(HLA("HLS",RAN),HLFS,12)="C"
 Q
 ;
ESCAPE(STR) ;
 Q:STR="" STR
 N BUF,ESC,CH,I1,I2
 ;--- Find all occurences of encoding characters and
 ;    save their positions to a local array
 F I1=1:1:4  D
 . S CH=$E(HLECH,I1),I2=1
 . F  S I2=$F(STR,CH,I2)  Q:'I2  S BUF(I2-1)=I1
 Q:$D(BUF)<10 STR
 ;--- Replace HL7 separator chars with encoding chars
 S (BUF,I2)="",ESC=$E(HLECH,3)  S:ESC="" ESC="\"
 F  S I1=I2,I2=$O(BUF(I2))  Q:I2=""  D
 . S BUF=BUF_$E(STR,I1+1,I2-1)_ESC_$E("SRET",BUF(I2))_ESC
 Q BUF_$E(STR,I1+1,$L(STR))
 ;
OBXPRC ;Compile 'OBX' Segment for Procedure
 S HLA("HLS",RAN)="OBX"_HLFS_HLFS_"CE"_HLFS_"P"_$E(HLECH)_"PROCEDURE"_$E(HLECH)_"L"_HLFS_HLFS_$P(RACN0,"^",2)
 S X=$S($D(^RAMIS(71,+$P(RACN0,"^",2),0)):$P(^(0),"^"),1:""),HLA("HLS",RAN)=HLA("HLS",RAN)_$E(HLECH)_X_$E(HLECH)_"L" D OBX11
 ; Replace above with following when Imaging can cope with ESC chars
 ; S X=$S($D(^RAMIS(71,+$P(RACN0,"^",2),0)):$P(^(0),"^"),1:""),HLA("HLS",RAN)=HLA("HLS",RAN)_$E(HLECH)_$$ESCAPE(X)_$E(HLECH)_"L" D OBX11
 Q
OBXMOD ; Compile 'OBX' segments for both types of modifiers
 ; Procedure modifiers
 N X3
 D MODS^RAUTL2 S HLA("HLS",RAN)="OBX"_HLFS_HLFS_"TX"_HLFS_"M"_$E(HLECH)_"MODIFIERS"_$E(HLECH)_"L"_HLFS_HLFS_Y D OBX11
 Q:Y(1)="None"
 ; CPT Modifiers
 F RAI=1:1 S X0=$P(Y(1),", ",RAI),X1=$P(Y(2),", ",RAI) Q:X0=""  D
 . S RAN=RAN+1
 . S X3=$$BASICMOD^RACPTMSC(X1,DT)
 . S HLA("HLS",RAN)="OBX"_HLFS_HLFS_"CE"_HLFS_"C4"_$E(HLECH)_"CPT MODIFIERS"_$E(HLECH)_"C4"_HLFS_HLFS_X0_$E(HLECH)_$P(X3,"^",3)_$E(HLECH)_"C4"
 . ; Replace above with following when Imaging can cope with ESC chars
 . ;S HLA("HLS",RAN)="OBX"_HLFS_HLFS_"CE"_HLFS_"C4"_$E(HLECH)_"CPT MODIFIERS"_$E(HLECH)_"C4"_HLFS_HLFS_X0_$E(HLECH)_$$ESCAPE($P(X3,"^",3))_$E(HLECH)_"C4"
 . I $P(X3,"^",4)]"" S HLA("HLS",RAN)=HLA("HLS",RAN)_$E(HLECH)_$P(X3,"^",4)_$E(HLECH)_$P(X3,"^",3)_$E(HLECH)_"C4"
 . ; Replace above with following when Imaging can cope with ESC chars
 . ;I $P(X3,"^",4)]"" S HLA("HLS",RAN)=HLA("HLS",RAN)_$E(HLECH)_$P(X3,"^",4)_$E(HLECH)_$$ESCAPE($P(X3,"^",3))_$E(HLECH)_"C4"
 . D OBX11
 . Q
 Q
 ;
OBXTCM ; Compile 'OBX' segment for latest TECH COMMENT
 ;
 ; Only Released version of Imaging 2.5 able to handle Tech Comments
 Q:'($$PATCH^XPDUTL("MAG*2.5*1")!(+$$VERSION^XPDUTL("MAG")>2.5))
 ;
 N X4,X3
 S X4=$$GETTCOM^RAUTL11(RADFN,RADTI,RACNI)
 Q:X4=""
 S RAN=RAN+1
 S HLA("HLS",RAN)="OBX"_HLFS_HLFS_"TX"_HLFS_"TCM"_$E(HLECH)_"TECH COMMENT"_$E(HLECH)_"L"_HLFS_HLFS
 D OBX11
 I $L(X4)+$L(HLA("HLS",RAN))'>245 D  Q
 .S $P(HLA("HLS",RAN),HLFS,6)=X4
 ;
 ; If Tech Comment is v. long it will need to be
 ; split into two parts. Do not split words if possible....
 ;
 S X3=$E(X4,1,245-$L(HLA("HLS",RAN)))
 I $L(X3," ")>1 S X3=$P(X3," ",1,$L(X3," ")-1)
 S X4=$P(X4,X3,2)
 S $P(HLA("HLS",RAN),HLFS,6)=X3
 S HLA("HLS",RAN,1)=X4_HLFS_$P(HLA("HLS",RAN),HLFS,7,12)
 S HLA("HLS",RAN)=$P(HLA("HLS",RAN),HLFS,1,6)
 Q
