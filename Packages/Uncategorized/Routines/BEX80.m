BEX80 ; cmi/anch/maw - BEX IHS Audiocare Supplements ; [ 03/02/2010  11:04 AM ]
 ;;1.0;BEX TELEPHONE REFILL SYSTEM;**1,2,4**;DEC 01, 2009
PATRX ;EP - PRINT ALL TRANSACTINS FOR ONE PATIENT
 ;CALLED FROM OPTION 'BEX PATIENT TRANSACTIONS'
 N VEX
 S DIC="^DPT(",DIC(0)="AEMQ",L=0
 S DIC("S")="I $D(^VEXHRX0(19080.1,""B"",+Y))"
 D ^DIC Q:Y=-1
 S VEX=+Y
 S DIC="^VEXHRX0(19080.1,"
 ;S BY="@INTERNAL(#.01)=VAR(""VEX"");S1,@+DATE(DATE/TIME),PRESCRIPTION NUMBER"
 S BY="@INTERNAL(#.01);S1,@+DATE(DATE/TIME),PRESCRIPTION NUMBER"
 S (FR,TO)=VEX
 S DHD="[BEX PATIENT TRANSACTIONS]"
 S FLDS="1;C1;N,!2,3,4,8"
 D EN1^DIP
 Q
 ;
DRG(VEXDRX)        ;EP - return the drug name
 N BEXXDA,VEXDIEN
 I '$G(BEXDRX) Q ""
 S BEXXDA=$O(^PSRX("B",BEXDRX,0))
 I '$G(BEXXDA) Q ""
 S BEXDIEN=$P($G(^PSRX(BEXXDA,0)),U,6)
 I '$G(BEXDIEN) Q ""
 Q $P($G(^PSDRUG(BEXDIEN,0)),U)
 ;
