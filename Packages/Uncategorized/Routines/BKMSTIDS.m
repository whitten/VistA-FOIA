BKMSTIDS ;PRXM/HC/ALA-STI Incidences ; 31 Jul 2007  12:50 PM
 ;;2.1;HIV MANAGEMENT SYSTEM;;Feb 07, 2011
 ;
EN(BKMDFN,RPSDT,RPEDT,BKTYPE,BKARRAY,BKSTIY) ; PEP
 ; Input parameters
 ;   BKMDFN  - Patient internal entry number
 ;   RPSDT   - Report start date in FileMan date format
 ;   RPEDT   - Report end date in FileMan date format
 ;   BKTYPE  - Type of STI data, defaults to 'Key'
 ;   BKARRAY - Array to return raw data
 ;   BKSTIY  - Array to return data by totals
 ;
 NEW DIR,Y,X,DTOUT,DUOUT,DFN,SEX,SSN,PTNAME,AGE,AUPNDAYS,AUPNDOB
 NEW AUPNPAT,AUPNDOD,AUPNSEX,SIEN,STINM,STI,NIN,DDATA,RSTI,NSTI,RSTINM
 NEW RSTNM,RSIEN,NSIEN,NRSCR,HVDFL,BKTYPE,NCT,NSCR,RFL,SCREEN,LR,NTSCR
 NEW TOT,BKIN,BKN,HSCR,POP,INC,SDAT,STYP,BKBDT,BKEDT,CT,BKMARRAY,BSN
 NEW TOTIN,INCD,INCS,TOTNS,TOTRF,TOTSD,RC,TVALN,QFL,SSCREEN,BKSTY,BN
 ;
 S BKTYPE=$G(BKTYPE,"KEY"),HVDFL=0,AUPNPAT=BKMDFN
 K BKARRAY,BKSTIN,BKSTIY S QFL=0
 S BKBDT=$$FMADD^XLFDT(RPSDT,-60),BKEDT=$$FMADD^XLFDT(RPSDT,300)
 D EN^BKMSTI(AUPNPAT,BKBDT,BKEDT,BKTYPE,.BKARRAY,.HVDFL)
 I '$D(BKARRAY) S BKSTIY(0)=0 Q
 I $D(BKARRAY) D
 . NEW STI
 . S STI=""
 . F  S STI=$O(BKARRAY(STI)) Q:STI=""  D  Q:QFL
 .. I $G(BKARRAY(STI,"DEN"))'=0 S QFL=1 Q
 I 'QFL K BKARRAY S BKSTIY(0)=0 Q
 D INC
 ;  clean up extras values not needed
 S INC=0
 F  S INC=$O(BKSTIN(INC)) Q:INC=""  D
 . S STYP=""
 . F  S STYP=$O(BKSTIN(INC,STYP)) Q:STYP=""  D
 .. S SDAT="",CT=0
 .. F  S SDAT=$O(BKSTIN(INC,STYP,SDAT)) Q:SDAT=""  D
 ... S CT=CT+1
 ... I CT>1 K BKSTIN(INC,STYP,SDAT)
 ;
 S INC=0
 F  S INC=$O(BKSTIN(INC)) Q:INC=""  D
 . K REC,NBREC,NREC
 . S TOTIN=0
 . S TYP="" F  S TYP=$O(BKSTIN(INC,TYP)) Q:TYP=""  D
 .. S DAT=""
 .. F  S DAT=$O(BKSTIN(INC,TYP,DAT)) Q:DAT=""  D
 ... S BKN=$O(^BKM(90454,"D",TYP,""))
 ... S BKSTIY(INC,TYP,0)=1_"^^^^"_$$FMTE^XLFDT(DAT,"2Z")_" "_BKSTIN(INC,TYP,DAT),TOTIN=TOTIN+1
 ... S BN=0
 ... F  S BN=$O(^BKM(90454,BKN,10,BN)) Q:'BN  D
 .... S BSN=$P(^BKM(90454,BKN,10,BN,0),"^",1)
 .... S BKSTY=$P(^BKM(90454,BSN,0),"^",3),HVDFL=0
 .... I $D(BKSTIY(INC,BKSTY))>0 Q
 .... I $D(BKSTIN(INC,BKSTY))>0 Q
 .... I BKSTY="HIV" D
 ..... NEW HKDATE,HEDATE
 ..... S HKDATE="",HEDATE=DAT
 ..... S HVDFL=$$HIVS^BKMRMDR(.BKMDFN,.HKDATE,.HEDATE)
 ..... I +HVDFL=1 Q
 ..... I +HVDFL=0 S BKSTIY(INC,BKSTY)=$G(BKARRAY(TYP,"NUM",BKSTY,DAT))
 .... I +HVDFL=1 Q
 .... S BKSTIY(INC,BKSTY)=1
 .... I $G(BKARRAY(TYP,"NUM",BKSTY,DAT))'="" D
 ..... S $P(BKSTIY(INC,BKSTY),"^",2)=1
 ..... S $P(BKSTIY(INC,BKSTY),"^",4)=$P(BKARRAY(TYP,"NUM",BKSTY,DAT),"^",2)
 .... I $G(BKARRAY(TYP,"REF",BKSTY,DAT))'="" D
 ..... S $P(BKSTIY(INC,BKSTY),"^",2)=1
 ..... S $P(BKSTIY(INC,BKSTY),"^",3)=1
 ..... S $P(BKSTIY(INC,BKSTY),"^",4)=$P(BKARRAY(TYP,"REF",BKSTY,DAT),"^",1)
 . S BKSTIY(INC,0)=TOTIN
 ;
 S TOTIN=0,TOTNS=0,TOTSD=0,TOTRF=0
 S INC=0
 F  S INC=$O(BKSTIY(INC)) Q:INC=""  D
 . S TOTIN=TOTIN+$P(BKSTIY(INC,0),U,1)
 . S TYP=0 F  S TYP=$O(BKSTIY(INC,TYP)) Q:TYP=""  D
 .. I $G(BKSTIY(INC,TYP,0))'="" Q
 .. S TOTNS=TOTNS+$P(BKSTIY(INC,TYP),U,1)
 .. S TOTSD=TOTSD+$P(BKSTIY(INC,TYP),U,2)
 .. S TOTRF=TOTRF+$P(BKSTIY(INC,TYP),U,3)
 ;
 S BKSTIY(0)=TOTIN_U_TOTNS_U_TOTSD_U_TOTRF
 K BKSTIN
 Q
 ;
INC ;EP - Determine multiple incidences
 NEW TYP,DAT,INC,PDAT,NXDT,NTYP,DTDIF
 K MBKARAY,BKSTIN
 S TYP=""
 F  S TYP=$O(BKARRAY(TYP)) Q:TYP=""  D
 . S DAT=""
 . F  S DAT=$O(BKARRAY(TYP,"DEN",DAT)) Q:DAT=""  D
 .. S MBKARAY(DAT,TYP)=BKARRAY(TYP,"DEN",DAT)
 S INC=0,DAT="",PDAT=""
 F  S DAT=$O(MBKARAY(DAT)) Q:DAT=""  D
 . S INC=INC+1 D SDT(DAT) K MBKARAY(DAT)
 . S DTDIF=$$FMADD^XLFDT(DAT,60)
 . S NXDT=DAT F  S NXDT=$O(MBKARAY(NXDT)) Q:NXDT=""  D
 .. I NXDT<DTDIF D
 ... D SDT(NXDT)
 ... K MBKARAY(NXDT)
 Q
 ;
SDT(VDT) ;EP - Same date, multiple types
 S TYP=""
 F  S TYP=$O(MBKARAY(VDT,TYP)) Q:TYP=""  D
 . S BKSTIN(INC,TYP,VDT)=MBKARAY(VDT,TYP)
 Q
