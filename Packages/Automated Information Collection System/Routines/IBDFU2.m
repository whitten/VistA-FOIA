IBDFU2 ;ALB/CJM - ENCOUNTER FORM - BUILD FORM(copying blocks) ; 08-JAN-1993
 ;;3.0;AUTOMATED INFO COLLECTION SYS;;APR 24, 1997
 ;
COPYBLK(OLDBLOCK,IBFORM,FROMFILE,TOFILE,ROW,COL,TKORDER,NAME,RECMPILE) ;copys OLDBLOCK in FROMFILE to IBFORM in TOFILE
 ;makes the new block part of IBFORM if defined
 ;places block at (ROW,COL) if defined
 ;sets TOOL KIT ORDER TKORDER if defined and >0
 ;sets the block name to NAME if defined
 ;returns the ien of the new copy
 ;RECMPILE means don't copy compiled block
 ;
 Q:(FROMFILE'=357.1)&(FROMFILE'=358.1) ""
 Q:(TOFILE'=357.1)&(TOFILE'=358.1) ""
 N NODE,LIST,FLD,LINE,TEXT,NEWBLOCK,FROM,TO,SUB,I
 S NEWBLOCK=""
 S NODE=$G(^IBE(FROMFILE,OLDBLOCK,0)) Q:NODE="" ""
 S $P(NODE,"^",2)=$G(IBFORM)
 S:$G(NAME)="" NAME=$P(NODE,"^")
 S RECMPILE=+$G(RECMPILE)
 ;there must be a name
 Q:NAME="" ""
 S $P(NODE,"^")=NAME
 I $D(ROW),(ROW=+ROW) S $P(NODE,"^",4)=ROW
 I $D(COL),(COL=+COL) S $P(NODE,"^",5)=COL
 S:$D(TKORDER) $P(NODE,"^",14)=$S(TKORDER:TKORDER,1:"")
 K DIC,DO,DD,DINUM S DIC="^IBE("_TOFILE_",",X=NAME,DIC(0)=""
 D FILE^DICN K DIC,DIE,DA
 S NEWBLOCK=$S(+Y<0:"",1:+Y)
 Q:'NEWBLOCK ""
 S ^IBE(TOFILE,NEWBLOCK,0)=NODE
 S NODE=0 F  S NODE=$O(^IBE(FROMFILE,OLDBLOCK,NODE)) Q:'NODE  S ^IBE(TOFILE,NEWBLOCK,NODE)=$G(^IBE(FROMFILE,OLDBLOCK,NODE))
 K DIK,DA S DIK="^IBE("_TOFILE_",",DA=NEWBLOCK
 D IX1^DIK K DIK,DA
 ;I ('RECMPILE),TOFILE=357.1,FROMFILE=357.1,$D(^IBE(357.1,OLDBLOCK,"V")),$D(^IBE(357.1,OLDBLOCK,"S")),$D(^IBE(357.1,OLDBLOCK,"B")),$D(^IBE(357.1,OLDBLOCK,"H")) D
 ;.F SUB="S","V","B","H" S I=0 S ^IBE(357.1,NEWBLOCK,SUB,0)=$G(^IBE(357.1,OLDBLOCK,SUB,0)) F  S I=$O(^IBE(357.1,OLDBLOCK,SUB,I)) Q:'I  S ^IBE(357.1,NEWBLOCK,SUB,I,0)=$G(^IBE(357.1,OLDBLOCK,SUB,I,0))
 ;before any new block component is created, make sure there is no garbage around with dangling pointer pointing to new block
 D DLTCNTNT^IBDFU3(NEWBLOCK,TOFILE)
 ;
 ;now copy the old block's contents into the newblock
 S (LIST,LINE,TEXT)=""
 ;
 ;copy selection lists
 S FROM=$S(FROMFILE[358:358.2,1:357.2),TO=$S(TOFILE[358:358.2,1:357.2)
 F  S LIST=$O(^IBE(FROM,"C",OLDBLOCK,LIST)) Q:'LIST  I $$COPYLIST(LIST,OLDBLOCK,NEWBLOCK,FROM,TO)
 ;
 ;copy data fields
 S FROM=$S(FROMFILE[358:358.5,1:357.5),TO=$S(TOFILE[358:358.5,1:357.5)
 S FLD=0 F  S FLD=$O(^IBE(FROM,"C",OLDBLOCK,FLD)) Q:'FLD  D COPYFLD^IBDFU2A(FLD,OLDBLOCK,NEWBLOCK,FROM,TO)
 ;
 ;copy multiple choice fields
 S FROM=$S(FROMFILE[358:358.93,1:357.93),TO=$S(TOFILE[358:358.93,1:357.93)
 S FLD=0 F  S FLD=$O(^IBE(FROM,"C",OLDBLOCK,FLD)) Q:'FLD  D COPYMFLD^IBDFU2A(FLD,OLDBLOCK,NEWBLOCK,FROM,TO)
 ;
 ;copy hand print fields
 S FROM=$S(FROMFILE[358:358.94,1:359.94),TO=$S(TOFILE[358:358.94,1:359.94)
 S FLD=0 F  S FLD=$O(^IBE(FROM,"C",OLDBLOCK,FLD)) Q:'FLD  D COPYHFLD^IBDFU2A(FLD,OLDBLOCK,NEWBLOCK,FROM,TO)
 ;
 ;copy lines
 S FROM=$S(FROMFILE[358:358.7,1:357.7),TO=$S(TOFILE[358:358.7,1:357.7)
 F  S LINE=$O(^IBE(FROM,"C",OLDBLOCK,LINE)) Q:'LINE  D COPYLINE^IBDFU2A(LINE,OLDBLOCK,NEWBLOCK,FROM,TO)
 ;
 ;copy text areas
 S FROM=$S(FROMFILE[358:358.8,1:357.8),TO=$S(TOFILE[358:358.8,1:357.8)
 F  S TEXT=$O(^IBE(FROM,"C",OLDBLOCK,TEXT)) Q:'TEXT  D COPYTEXT^IBDFU2A(TEXT,OLDBLOCK,NEWBLOCK,FROM,TO)
 Q NEWBLOCK
 ;
COPYLIST(LIST,OLDBLOCK,NEWBLOCK,FROMFILE,TOFILE) ;
 ;returns the new list copied from LIST
 Q:'$G(LIST)!('$G(OLDBLOCK))!('$G(NEWBLOCK))!('$G(FROMFILE))!('$G(TOFILE)) 0
 Q:(FROMFILE'=357.2)&(FROMFILE'=358.2) 0
 Q:(TOFILE'=357.2)&(TOFILE'=358.2) 0
 N NODE,NAME,NEWLIST,GRP,SLCTN,COL,TO,FROM,TOPI,FROMPI,DYNAMIC
 S NEWLIST=""
 S NODE=$G(^IBE(FROMFILE,LIST,0)) Q:NODE="" 0
 S DYNAMIC=$P(NODE,"^",14)
 ;make sure the list really belongs to the block being copied - if not re-index it
 I $P(NODE,"^",2)='OLDBLOCK K DA S DA=LIST,DIK="^IBE("_FROMFILE_"," D IX^DIK K DIK Q 0
 S NAME=$P(NODE,"^",1),$P(NODE,"^",2)=NEWBLOCK
 S FROMPI=$P(NODE,"^",11)
 S TOPI=$$GETPI^IBDFU2B(FROMPI,$S(FROMFILE[358:358.6,1:357.6),$S(TOFILE[358:358.6,1:357.6)),$P(NODE,"^",11)=TOPI
 Q:NAME="" 0
 K DIC,DD,DINUM,DO S DIC="^IBE("_TOFILE_",",X=NAME,DIC(0)=""
 D FILE^DICN K DIC,DIE,DA
 S NEWLIST=$S(+Y<0:"",1:+Y)
 Q:'NEWLIST 0
 D DLISTCNT^IBDFU3(NEWLIST,TOFILE) ;clean up any dangling pointers that may be now pointing to this new, supposedly empty list
 ;
 ;now copy
 S ^IBE(TOFILE,NEWLIST,0)=NODE
 ;
 ;copy the column multiple
 S NODE=$G(^IBE(FROMFILE,LIST,1,0))
 I NODE'=""  S $P(NODE,"^",2)=TOFILE_"1I",^IBE(TOFILE,NEWLIST,1,0)=NODE S COL=0 F  S COL=$O(^IBE(FROMFILE,LIST,1,COL)) Q:'COL  S NODE=$G(^IBE(FROMFILE,LIST,1,COL,0)) S:NODE'="" ^IBE(TOFILE,NEWLIST,1,COL,0)=NODE
 ;
 ;now copy the subcolumn multiple
 S NODE=$G(^IBE(FROMFILE,LIST,2,0)) I NODE'=""  S $P(NODE,"^",2)=TOFILE_"2I",^IBE(TOFILE,NEWLIST,2,0)=NODE S COL=0 F  S COL=$O(^IBE(FROMFILE,LIST,2,COL)) Q:'COL  S NODE=$G(^IBE(FROMFILE,LIST,2,COL,0)) I NODE'="" D
 .S:$P(NODE,"^",6) $P(NODE,"^",6)=$$GETMA^IBDFU2B($P(NODE,"^",6),$S(FROMFILE[358:358.91,1:357.91),$S(TOFILE[358:358.91,1:357.91))
 .S:$P(NODE,"^",9) $P(NODE,"^",9)=$$GETQLFR^IBDFU2B($P(NODE,"^",9),$S(FROMFILE[358:358.98,1:357.98),$S(TOFILE[358:358.98,1:357.98))
 .S ^IBE(TOFILE,NEWLIST,2,COL,0)=NODE
 ;
 K DIK,DA S DIK="^IBE("_TOFILE_",",DA=NEWLIST
 D IX1^DIK K DIK,DA
 S FROM=$S(FROMFILE[358:358.4,1:357.4),TO=$S(TOFILE[358:358.4,1:357.4)
 ;
 ;don't want to copy groups and selections if the selections are not exportable or the list is dynamic
 I FROM'=TO,FROMPI,'$P($G(^IBE($S(FROM[358:358.6,1:357.6),FROMPI,2)),"^",18) Q NEWLIST
 I 'DYNAMIC S GRP="" F  S GRP=$O(^IBE(FROM,"D",LIST,GRP)) Q:'GRP  D COPYGRP^IBDFU2A(GRP,LIST,NEWLIST,NEWBLOCK,FROM,TO)
 Q NEWLIST
