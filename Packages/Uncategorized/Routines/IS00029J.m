IS00029J ;Compiled from script 'Generated: HL IHS IZV04 V03VXR OUT-O' on JUN 21, 2010
 ;Part 11
 ;Copyright 2010 SAIC
EN S @INV@("ZRA2.2")=X K DXS,D0
 ;SET ZRA2.3 = INSGX\^INTHL7FT(1,3)\\5\"99IHS"
 S D0=INDA S X="99IHS"
 S X1="^INTHL7FT(1,3)" X:$L($G(@X1)) $G(@X1) S X=$E(X,1,5)
 S @INV@("ZRA2.3")=X K DXS,D0
 D:'INVS MC^INHS
 K LINE S LINE="",CP=0 S L1="ZRA" S:$TR(L1,$G(SUBDELIM))="" L1="" D SETPIECE^INHU(.LINE,DELIM,1,L1,.CP) S L1=$G(@INV@("ZRA1.1"))
 S D0=INDA S X="^" S L1=L1_X
 S L1=L1_$G(@INV@("ZRA1.2"))
 S D0=INDA S X="^" S L1=L1_X
 S L1=L1_$G(@INV@("ZRA1.3")) S:$TR(L1,$G(SUBDELIM))="" L1="" D SETPIECE^INHU(.LINE,DELIM,2,L1,.CP) S L1=$G(@INV@("ZRA2.1"))
 S D0=INDA S X="^" S L1=L1_X
 S L1=L1_$G(@INV@("ZRA2.2"))
 S D0=INDA S X="^" S L1=L1_X
 S L1=L1_$G(@INV@("ZRA2.3")) S:$TR(L1,$G(SUBDELIM))="" L1="" D SETPIECE^INHU(.LINE,DELIM,3,L1,.CP)
 S LCT=LCT+1,^UTILITY("INH",$J,LCT)=LINE I $D(LINE)>9 M ^UTILITY("INH",$J,LCT)=LINE
 Q
L1 S INDA=INDA0 K INDA0
 D:'INVS MC^INHS
 ;Entering END section.
 I $G(INSTERR) Q $S($G(INREQERR)>INSTERR:INREQERR,1:INSTERR)
 S UIF=$$NEWO^INHD(INDEST,"^UTILITY(""INH"",$J)",+$P($G(^INRHT(INTT,0)),U,12),INTT,MESSID,$G(INQUE),$G(INORDUZ),$G(INORDIV),.INUIF6,.INUIF7,$G(INA("INMIDGEN")))
 I UIF<0 D ERROR^INHS("UIF creation failed",2) Q 2
 Q 0
