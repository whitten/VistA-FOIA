BGPDP221 ; IHS/CMI/LAB - NO DESCRIPTION PROVIDED ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
 ;
POV ;EP
 I $G(BGPAREAA),'$G(BGPSUMR) G AREAPOV
 K ^TMP($J,"PHN")
 S BGPX=0 F  S BGPX=$O(^BGPD(BGPRPT,180,BGPX)) Q:BGPX'=+BGPX  D
 .S BGPY=^BGPD(BGPRPT,180,BGPX,0)
 .S C=$P(BGPY,U),HV=$P(BGPY,U,3),V=$P(BGPY,U,2)
 .I V S ^TMP($J,"PHN","ALLV","CY",9999999-V,C)=""
 .I HV S ^TMP($J,"PHN","HOME","CY",9999999-HV,C)=""
 S BGPX=0 F  S BGPX=$O(^BGPD(BGPRPT,480,BGPX)) Q:BGPX'=+BGPX  D
 .S BGPY=^BGPD(BGPRPT,480,BGPX,0)
 .S C=$P(BGPY,U),HV=$P(BGPY,U,3),V=$P(BGPY,U,2)
 .I V S ^TMP($J,"PHN","ALLV","PR",9999999-V,C)=""
 .I HV S ^TMP($J,"PHN","HOME","PR",9999999-HV,C)=""
 S BGPX=0 F  S BGPX=$O(^BGPD(BGPRPT,880,BGPX)) Q:BGPX'=+BGPX  D
 .S BGPY=^BGPD(BGPRPT,880,BGPX,0)
 .S C=$P(BGPY,U),HV=$P(BGPY,U,3),V=$P(BGPY,U,2)
 .I V S ^TMP($J,"PHN","ALLV","BL",9999999-V,C)=""
 .I HV S ^TMP($J,"PHN","HOME","BL",9999999-HV,C)=""
 Q
AREAPOV ;
 K ^TMP($J,"PHN")
 S BGPRPT=0 F  S BGPRPT=$O(BGPSUL(BGPRPT)) Q:BGPRPT'=+BGPRPT  D
 .S BGPX=0 F  S BGPX=$O(^BGPD(BGPRPT,180,BGPX)) Q:BGPX'=+BGPX  D
 ..S BGPY=^BGPD(BGPRPT,180,BGPX,0)
 ..S C=$P(BGPY,U),HV=$P(BGPY,U,3),V=$P(BGPY,U,2)
 ..I V S ^TMP($J,"PHN","ALLVT","CY",C)=$G(^TMP($J,"PHN","ALLVT","CY",C))+V
 ..I HV S ^TMP($J,"PHN","HOMET","CY",C)=$G(^TMP($J,"PHN","HOMET","CY",C))+HV
 .S BGPX=0 F  S BGPX=$O(^BGPD(BGPRPT,480,BGPX)) Q:BGPX'=+BGPX  D
 ..S BGPY=^BGPD(BGPRPT,480,BGPX,0)
 ..S C=$P(BGPY,U),HV=$P(BGPY,U,3),V=$P(BGPY,U,2)
 ..I V S ^TMP($J,"PHN","ALLVT","PR",C)=$G(^TMP($J,"PHN","ALLVT","PR",C))+V
 ..I HV S ^TMP($J,"PHN","HOMET","PR",C)=$G(^TMP($J,"PHN","HOMET","PR",C))+HV
 .S BGPX=0 F  S BGPX=$O(^BGPD(BGPRPT,880,BGPX)) Q:BGPX'=+BGPX  D
 ..S BGPY=^BGPD(BGPRPT,880,BGPX,0)
 ..S C=$P(BGPY,U),HV=$P(BGPY,U,3),V=$P(BGPY,U,2)
 ..I V S ^TMP($J,"PHN","ALLVT","BL",C)=$G(^TMP($J,"PHN","ALLVT","BL",C))+V
 ..I HV S ^TMP($J,"PHN","HOMET","BL",C)=$G(^TMP($J,"PHN","HOMET","BL",C))+HV
 S X="" F  S X=$O(^TMP($J,"PHN","ALLVT","CY",X)) Q:X=""  S Y=^TMP($J,"PHN","ALLVT","CY",X),^TMP($J,"PHN","ALLV","CY",9999999-Y,X)=""
 S X="" F  S X=$O(^TMP($J,"PHN","ALLVT","PR",X)) Q:X=""  S Y=^TMP($J,"PHN","ALLVT","PR",X),^TMP($J,"PHN","ALLV","PR",9999999-Y,X)=""
 S X="" F  S X=$O(^TMP($J,"PHN","ALLVT","BL",X)) Q:X=""  S Y=^TMP($J,"PHN","ALLVT","BL",X),^TMP($J,"PHN","ALLV","BL",9999999-Y,X)=""
 S X="" F  S X=$O(^TMP($J,"PHN","HOMET","CY",X)) Q:X=""  S Y=^TMP($J,"PHN","HOMET","CY",X),^TMP($J,"PHN","HOME","CY",9999999-Y,X)=""
 S X="" F  S X=$O(^TMP($J,"PHN","HOMET","PR",X)) Q:X=""  S Y=^TMP($J,"PHN","HOMET","PR",X),^TMP($J,"PHN","HOME","PR",9999999-Y,X)=""
 S X="" F  S X=$O(^TMP($J,"PHN","HOMET","BL",X)) Q:X=""  S Y=^TMP($J,"PHN","HOMET","BL",X),^TMP($J,"PHN","HOME","BL",9999999-Y,X)=""
 Q
