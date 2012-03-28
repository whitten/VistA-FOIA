RADD4 ;HISC/GJC-Radiology Utility Routine ;11/25/97  12:40
 ;;5.0;Radiology/Nuclear Medicine;;Mar 16, 1998
 ;
DCHK() ; Check if drug if DRUG is active AND a Radiopharmaceutical.
 ; 'RASTAT=1' if active AND RADG condition met
 ; 'RASTAT=0' if inactive OR RADG condition not met
 ; VERSION 5.0 called from ^DD(70.21,.01,12.1) & DCHK^RADD1
 ; 'Y'    is the IEN for the Drug file
 ; 'RADT' is the cutoff date for drugs in the drug file
 ; 'RADG':$S(RADG="R":Radiopharm,"P":non-Radioharm,1:non-Radiopharm)
 N RACLASS,RADRUG,RASTAT S:RADG']"" RADG="P"
 S RADRUG(2)=$P($G(^PSDRUG(Y,0)),"^",2)
 S RACLASS="^DX200^DX201^DX202^"
 S RASTAT=$$DCHK1()  ; is it active '1' yes, '0' no.
 I RASTAT D  ; is active check class
 . S:RADG="R"&(RACLASS'[("^"_RADRUG(2)_"^")) RASTAT=0
 . S:RADG="P"&(RACLASS[("^"_RADRUG(2)_"^")) RASTAT=0
 . Q
 Q RASTAT
 ;
DCHK1() ; Check if drug if DRUG is an active pharmaceutical
 ; '1' if active AND Pharm, '0' if inactive
 ; VERSION 5.0 called from DCHK above
 ; 'Y'    is the IEN for the Drug file
 ; 'RADT'  is the cutoff date for drugs in the drug file
 ; VERSION 5.0
 N RAINACT
 S RAINACT=+$G(^PSDRUG(Y,"I"))
 Q:'RAINACT 1 ; not inactive
 I RAINACT,(RAINACT'>RADT) Q 0 ; not active
 Q 1 ; active
 ;
VALADM() ;edit validation
 ;Used to validate/screen radiopharm dosage administrator,
 ;   radiopharm prescribing phys, person who measured radiopharm dose,
 ;----------------------------------------------------------------------
 ; RAD0  : IEN of entry in question for NUC MED EXAM DATA (70.2) file
 ; Y     : Pointer to the New Person file
 ; RADT  : Xam Date; if not passed, calculate exam date from file 70.2
 ; RAUTH : 1 - only staff/resid, must be auth'zd to write med orders
 ;       : 0 - staff/resid & tech's
 ;----------------------------------------------------------------------
 ; Output: '1' authorized to write med orders, else '0'
 ;----------------------------------------------------------------------
 N RAPS S RAPS=$G(^VA(200,Y,"PS"))
 ; $P(RAPS,"^")   - authorized to write med orders '1': Yes
 ; $P(RAPS,"^",4) - person CAN'T write med orders after this date(if any)
 S:$G(RADT)="" RADT=$P($G(^RADPTN(RAD0,0)),"^",2)
 I 'RAUTH,($D(^VA(200,"ARC","R",Y))!$D(^VA(200,"ARC","S",Y))!$D(^VA(200,"ARC","T",Y))) Q 1
 I RAUTH,($D(^VA(200,"ARC","R",Y))!$D(^VA(200,"ARC","S",Y))),(+$P(RAPS,"^")),($S('$P(RAPS,"^",4):1,$P(RAPS,"^",4)'<RADT:1,1:0)) Q 1
 Q 0
 ;
VOL() ; Validate the format of the value input for volume.
 ; RAX must be a number followed by a space then text -or-
 ; a number followed by text
 ; Input Variable : 'RAX'- user's input
 ; Output Variable: null if 'RAX' erroneous, formatted version of 'RAX'
 Q:(RAX'?0.5N0.1"."0.2N1" "1.30A)&(RAX'?0.5N0.1"."0.2N1.30A) ""
 N RAX1,RAY S RAX1=+RAX,RAY=$P(RAX,RAX1,2) Q:RAX1'>0 ""
 S RAY=$S($F(RAY," ")>0:$E(RAY,$F(RAY," "),9999),1:RAY)
 S RAY=$S($F(RAY,".")>0:$E(RAY,$F(RAY,"."),9999),1:RAY)
 S RAY=$$STRIP^XLFSTR(RAY,"0")
 S RAY=$$LOW^XLFSTR($E(RAY,1))
 I RAY'="c",(RAY'="m") Q ""
 Q RAX1_" "_RAY
