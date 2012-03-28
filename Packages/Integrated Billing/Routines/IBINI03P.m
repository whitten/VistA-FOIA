IBINI03P	; ; 21-MAR-1994
	;;Version 2.0 ; INTEGRATED BILLING ;; 21-MAR-94
	Q:'DIFQ(354.1)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^DIC(354.1,0,"GL")
	;;=^IBA(354.1,
	;;^DIC("B","BILLING EXEMPTIONS",354.1)
	;;=
	;;^DIC(354.1,"%D",0)
	;;=^^21^21^2940214^^^^
	;;^DIC(354.1,"%D",1,0)
	;;=DO NOT EDIT THIS FILE.
	;;^DIC(354.1,"%D",2,0)
	;;= 
	;;^DIC(354.1,"%D",3,0)
	;;=Under normal operation it is not necessary to edit the fields in this
	;;^DIC(354.1,"%D",4,0)
	;;=file directly.  The option Manually Change Copay Exemption (Hardships) can
	;;^DIC(354.1,"%D",5,0)
	;;=be used to update and correct entries by creating a new exemption.
	;;^DIC(354.1,"%D",6,0)
	;;=If many patient records have problems the option Print/Verify Patient
	;;^DIC(354.1,"%D",7,0)
	;;=Exemption Status can be used to correct the entries.
	;;^DIC(354.1,"%D",8,0)
	;;= 
	;;^DIC(354.1,"%D",9,0)
	;;=This file will hold the time sensitive data that is used to determine
	;;^DIC(354.1,"%D",10,0)
	;;=if a patient can be exluded from any type of billing for a period of
	;;^DIC(354.1,"%D",11,0)
	;;=time.  The initial use of this file is for storing the time sensitive
	;;^DIC(354.1,"%D",12,0)
	;;=data related to whether or not a patient is exempt from pharmacy copay
	;;^DIC(354.1,"%D",13,0)
	;;=requirement due to income below the pension level.
	;;^DIC(354.1,"%D",14,0)
	;;= 
	;;^DIC(354.1,"%D",15,0)
	;;=The file then maintains the audit trail of changes to exemptions and
	;;^DIC(354.1,"%D",16,0)
	;;=historical data about exemptions.  The data in this file should be
	;;^DIC(354.1,"%D",17,0)
	;;=retained for approximately the current calendar year and the past
	;;^DIC(354.1,"%D",18,0)
	;;=calendar year.  The ability to purge data in this file will be added
	;;^DIC(354.1,"%D",19,0)
	;;=at a future time.
	;;^DIC(354.1,"%D",20,0)
	;;= 
	;;^DIC(354.1,"%D",21,0)
	;;=Per VHA Directive 10-93-142, this file definition should not be modified.
	;;^DD(354.1,0)
	;;=FIELD^^.15^15
	;;^DD(354.1,0,"DDA")
	;;=N
	;;^DD(354.1,0,"DT")
	;;=2930204
	;;^DD(354.1,0,"ID",.02)
	;;=S %I=Y,Y=$S('$D(^(0)):"",$D(^IBA(354,+$P(^(0),U,2),0))#2:$P(^(0),U,1),1:""),C=$P(^DD(354,.01,0),U,2) D Y^DIQ:Y]"" W "   ",Y,@("$E("_DIC_"%I,0),0)") S Y=%I K %I
	;;^DD(354.1,0,"ID",.04)
	;;=W "   ",@("$P($P($C(59)_$S($D(^DD(354.1,.04,0)):$P(^(0),U,3),1:0)_$E("_DIC_"Y,0),0),$C(59)_$P(^(0),U,4)_"":"",2),$C(59),1)")
	;;^DD(354.1,0,"ID",.1)
	;;=W "   ",@("$P($P($C(59)_$S($D(^DD(354.1,.1,0)):$P(^(0),U,3),1:0)_$E("_DIC_"Y,0),0),$C(59)_$P(^(0),U,10)_"":"",2),$C(59),1)")
	;;^DD(354.1,0,"IX","AA",354.1,.1)
	;;=
	;;^DD(354.1,0,"IX","ACAN",354.1,.14)
	;;=
	;;^DD(354.1,0,"IX","ACAN1",354.1,.02)
	;;=
	;;^DD(354.1,0,"IX","ACY",354.1,.01)
	;;=
	;;^DD(354.1,0,"IX","ACY1",354.1,.02)
	;;=
	;;^DD(354.1,0,"IX","ACY2",354.1,.1)
	;;=
	;;^DD(354.1,0,"IX","ACY3",354.1,.03)
	;;=
	;;^DD(354.1,0,"IX","AIVDT",354.1,.01)
	;;=
	;;^DD(354.1,0,"IX","AIVDT1",354.1,.1)
	;;=
	;;^DD(354.1,0,"IX","AIVDT2",354.1,.02)
	;;=
	;;^DD(354.1,0,"IX","AIVDT3",354.1,.03)
	;;=
	;;^DD(354.1,0,"IX","ALERT",354.1,.09)
	;;=
	;;^DD(354.1,0,"IX","AP",354.1,.02)
	;;=
	;;^DD(354.1,0,"IX","APIDT",354.1,.02)
	;;=
	;;^DD(354.1,0,"IX","APIDT1",354.1,.01)
	;;=
	;;^DD(354.1,0,"IX","APIDT2",354.1,.03)
	;;=
	;;^DD(354.1,0,"IX","APRIOR",354.1,.15)
	;;=
	;;^DD(354.1,0,"IX","AR",354.1,.05)
	;;=
	;;^DD(354.1,0,"IX","AS",354.1,.04)
	;;=
	;;^DD(354.1,0,"IX","B",354.1,.01)
	;;=
	;;^DD(354.1,0,"IX","C",354.1,.02)
	;;=
	;;^DD(354.1,0,"NM","BILLING EXEMPTIONS")
	;;=
	;;^DD(354.1,.01,0)
	;;=EFFECTIVE DATE^RDI^^0;1^S %DT="EX" D ^%DT S X=Y K:Y<1 X
	;;^DD(354.1,.01,.1)
	;;=
	;;^DD(354.1,.01,1,0)
	;;=^.1^^-1
	;;^DD(354.1,.01,1,1,0)
	;;=354.1^B
	;;^DD(354.1,.01,1,1,1)
	;;=S ^IBA(354.1,"B",$E(X,1,30),DA)=""
