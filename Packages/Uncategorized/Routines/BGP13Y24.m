BGP13Y24 ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"1101,64455-0772-55 ",.01)
 ;;64455-0772-55
 ;;9002226.02101,"1101,64455-0772-55 ",.02)
 ;;64455-0772-55
 ;;9002226.02101,"1101,64455-0791-47 ",.01)
 ;;64455-0791-47
 ;;9002226.02101,"1101,64455-0791-47 ",.02)
 ;;64455-0791-47
 ;;9002226.02101,"1101,64455-0792-47 ",.01)
 ;;64455-0792-47
 ;;9002226.02101,"1101,64455-0792-47 ",.02)
 ;;64455-0792-47
 ;;9002226.02101,"1101,64455-0795-30 ",.01)
 ;;64455-0795-30
 ;;9002226.02101,"1101,64455-0795-30 ",.02)
 ;;64455-0795-30
 ;;9002226.02101,"1101,64455-0795-42 ",.01)
 ;;64455-0795-42
 ;;9002226.02101,"1101,64455-0795-42 ",.02)
 ;;64455-0795-42
 ;;9002226.02101,"1101,64455-0795-49 ",.01)
 ;;64455-0795-49
 ;;9002226.02101,"1101,64455-0795-49 ",.02)
 ;;64455-0795-49
 ;;9002226.02101,"1101,64455-0796-30 ",.01)
 ;;64455-0796-30
 ;;9002226.02101,"1101,64455-0796-30 ",.02)
 ;;64455-0796-30
 ;;9002226.02101,"1101,64455-0796-42 ",.01)
 ;;64455-0796-42
 ;;9002226.02101,"1101,64455-0796-42 ",.02)
 ;;64455-0796-42
 ;;9002226.02101,"1101,64455-0796-49 ",.01)
 ;;64455-0796-49
 ;;9002226.02101,"1101,64455-0796-49 ",.02)
 ;;64455-0796-49
 ;;9002226.02101,"1101,64455-0796-50 ",.01)
 ;;64455-0796-50
 ;;9002226.02101,"1101,64455-0796-50 ",.02)
 ;;64455-0796-50
 ;;9002226.02101,"1101,64455-0797-30 ",.01)
 ;;64455-0797-30
 ;;9002226.02101,"1101,64455-0797-30 ",.02)
 ;;64455-0797-30
 ;;9002226.02101,"1101,64455-0797-42 ",.01)
 ;;64455-0797-42
 ;;9002226.02101,"1101,64455-0797-42 ",.02)
 ;;64455-0797-42
 ;;9002226.02101,"1101,64455-0797-49 ",.01)
 ;;64455-0797-49
 ;;9002226.02101,"1101,64455-0797-49 ",.02)
 ;;64455-0797-49
 ;;9002226.02101,"1101,64455-0798-30 ",.01)
 ;;64455-0798-30
 ;;9002226.02101,"1101,64455-0798-30 ",.02)
 ;;64455-0798-30
 ;;9002226.02101,"1101,64455-0798-42 ",.01)
 ;;64455-0798-42
 ;;9002226.02101,"1101,64455-0798-42 ",.02)
 ;;64455-0798-42
 ;;9002226.02101,"1101,64455-0798-49 ",.01)
 ;;64455-0798-49
 ;;9002226.02101,"1101,64455-0798-49 ",.02)
 ;;64455-0798-49
 ;;9002226.02101,"1101,64455-0799-42 ",.01)
 ;;64455-0799-42
 ;;9002226.02101,"1101,64455-0799-42 ",.02)
 ;;64455-0799-42
 ;;9002226.02101,"1101,64679-0421-01 ",.01)
 ;;64679-0421-01
 ;;9002226.02101,"1101,64679-0421-01 ",.02)
 ;;64679-0421-01
 ;;9002226.02101,"1101,64679-0422-01 ",.01)
 ;;64679-0422-01
 ;;9002226.02101,"1101,64679-0422-01 ",.02)
 ;;64679-0422-01
 ;;9002226.02101,"1101,64679-0422-02 ",.01)
 ;;64679-0422-02
 ;;9002226.02101,"1101,64679-0422-02 ",.02)
 ;;64679-0422-02
 ;;9002226.02101,"1101,64679-0423-01 ",.01)
 ;;64679-0423-01
 ;;9002226.02101,"1101,64679-0423-01 ",.02)
 ;;64679-0423-01
 ;;9002226.02101,"1101,64679-0423-02 ",.01)
 ;;64679-0423-02
 ;;9002226.02101,"1101,64679-0423-02 ",.02)
 ;;64679-0423-02
 ;;9002226.02101,"1101,65162-0006-09 ",.01)
 ;;65162-0006-09
 ;;9002226.02101,"1101,65162-0006-09 ",.02)
 ;;65162-0006-09
 ;;9002226.02101,"1101,65162-0006-50 ",.01)
 ;;65162-0006-50
 ;;9002226.02101,"1101,65162-0006-50 ",.02)
 ;;65162-0006-50
 ;;9002226.02101,"1101,65162-0007-09 ",.01)
 ;;65162-0007-09
 ;;9002226.02101,"1101,65162-0007-09 ",.02)
 ;;65162-0007-09
 ;;9002226.02101,"1101,65162-0007-50 ",.01)
 ;;65162-0007-50
 ;;9002226.02101,"1101,65162-0007-50 ",.02)
 ;;65162-0007-50
 ;;9002226.02101,"1101,65162-0008-09 ",.01)
 ;;65162-0008-09
 ;;9002226.02101,"1101,65162-0008-09 ",.02)
 ;;65162-0008-09
 ;;9002226.02101,"1101,65162-0008-50 ",.01)
 ;;65162-0008-50
 ;;9002226.02101,"1101,65162-0008-50 ",.02)
 ;;65162-0008-50
 ;;9002226.02101,"1101,65162-0588-10 ",.01)
 ;;65162-0588-10
 ;;9002226.02101,"1101,65162-0588-10 ",.02)
 ;;65162-0588-10
 ;;9002226.02101,"1101,65162-0589-10 ",.01)
 ;;65162-0589-10
 ;;9002226.02101,"1101,65162-0589-10 ",.02)
 ;;65162-0589-10
 ;;9002226.02101,"1101,65243-0028-09 ",.01)
 ;;65243-0028-09
 ;;9002226.02101,"1101,65243-0028-09 ",.02)
 ;;65243-0028-09
 ;;9002226.02101,"1101,65243-0054-09 ",.01)
 ;;65243-0054-09
 ;;9002226.02101,"1101,65243-0054-09 ",.02)
 ;;65243-0054-09
 ;;9002226.02101,"1101,65243-0111-09 ",.01)
 ;;65243-0111-09
 ;;9002226.02101,"1101,65243-0111-09 ",.02)
 ;;65243-0111-09
 ;;9002226.02101,"1101,65243-0260-09 ",.01)
 ;;65243-0260-09
 ;;9002226.02101,"1101,65243-0260-09 ",.02)
 ;;65243-0260-09
 ;;9002226.02101,"1101,65243-0311-09 ",.01)
 ;;65243-0311-09
 ;;9002226.02101,"1101,65243-0311-09 ",.02)
 ;;65243-0311-09
 ;;9002226.02101,"1101,65243-0312-09 ",.01)
 ;;65243-0312-09
 ;;9002226.02101,"1101,65243-0312-09 ",.02)
 ;;65243-0312-09
 ;;9002226.02101,"1101,65243-0312-18 ",.01)
 ;;65243-0312-18
 ;;9002226.02101,"1101,65243-0312-18 ",.02)
 ;;65243-0312-18
 ;;9002226.02101,"1101,65243-0317-09 ",.01)
 ;;65243-0317-09
 ;;9002226.02101,"1101,65243-0317-09 ",.02)
 ;;65243-0317-09
 ;;9002226.02101,"1101,65243-0318-09 ",.01)
 ;;65243-0318-09
 ;;9002226.02101,"1101,65243-0318-09 ",.02)
 ;;65243-0318-09
 ;;9002226.02101,"1101,65597-0110-10 ",.01)
 ;;65597-0110-10
 ;;9002226.02101,"1101,65597-0110-10 ",.02)
 ;;65597-0110-10
 ;;9002226.02101,"1101,65597-0110-30 ",.01)
 ;;65597-0110-30
 ;;9002226.02101,"1101,65597-0110-30 ",.02)
 ;;65597-0110-30
 ;;9002226.02101,"1101,65597-0110-90 ",.01)
 ;;65597-0110-90
 ;;9002226.02101,"1101,65597-0110-90 ",.02)
 ;;65597-0110-90
 ;;9002226.02101,"1101,65597-0111-10 ",.01)
 ;;65597-0111-10
 ;;9002226.02101,"1101,65597-0111-10 ",.02)
 ;;65597-0111-10
 ;;9002226.02101,"1101,65597-0111-30 ",.01)
 ;;65597-0111-30
 ;;9002226.02101,"1101,65597-0111-30 ",.02)
 ;;65597-0111-30
 ;;9002226.02101,"1101,65597-0111-90 ",.01)
 ;;65597-0111-90
 ;;9002226.02101,"1101,65597-0111-90 ",.02)
 ;;65597-0111-90
 ;;9002226.02101,"1101,65597-0112-10 ",.01)
 ;;65597-0112-10
 ;;9002226.02101,"1101,65597-0112-10 ",.02)
 ;;65597-0112-10
 ;;9002226.02101,"1101,65597-0112-30 ",.01)
 ;;65597-0112-30
 ;;9002226.02101,"1101,65597-0112-30 ",.02)
 ;;65597-0112-30
 ;;9002226.02101,"1101,65597-0112-90 ",.01)
 ;;65597-0112-90
 ;;9002226.02101,"1101,65597-0112-90 ",.02)
 ;;65597-0112-90
 ;;9002226.02101,"1101,65597-0113-10 ",.01)
 ;;65597-0113-10
 ;;9002226.02101,"1101,65597-0113-10 ",.02)
 ;;65597-0113-10
 ;;9002226.02101,"1101,65597-0113-30 ",.01)
 ;;65597-0113-30
 ;;9002226.02101,"1101,65597-0113-30 ",.02)
 ;;65597-0113-30
 ;;9002226.02101,"1101,65597-0113-90 ",.01)
 ;;65597-0113-90
 ;;9002226.02101,"1101,65597-0113-90 ",.02)
 ;;65597-0113-90
 ;;9002226.02101,"1101,65597-0114-30 ",.01)
 ;;65597-0114-30
 ;;9002226.02101,"1101,65597-0114-30 ",.02)
 ;;65597-0114-30
 ;;9002226.02101,"1101,65597-0114-90 ",.01)
 ;;65597-0114-90
 ;;9002226.02101,"1101,65597-0114-90 ",.02)
 ;;65597-0114-90
 ;;9002226.02101,"1101,65597-0115-30 ",.01)
 ;;65597-0115-30
 ;;9002226.02101,"1101,65597-0115-30 ",.02)
 ;;65597-0115-30
 ;;9002226.02101,"1101,65597-0115-90 ",.01)
 ;;65597-0115-90
 ;;9002226.02101,"1101,65597-0115-90 ",.02)
 ;;65597-0115-90
 ;;9002226.02101,"1101,65597-0116-30 ",.01)
 ;;65597-0116-30
 ;;9002226.02101,"1101,65597-0116-30 ",.02)
 ;;65597-0116-30
 ;;9002226.02101,"1101,65597-0116-90 ",.01)
 ;;65597-0116-90
 ;;9002226.02101,"1101,65597-0116-90 ",.02)
 ;;65597-0116-90
 ;;9002226.02101,"1101,65597-0117-30 ",.01)
 ;;65597-0117-30
 ;;9002226.02101,"1101,65597-0117-30 ",.02)
 ;;65597-0117-30
 ;;9002226.02101,"1101,65597-0117-90 ",.01)
 ;;65597-0117-90
 ;;9002226.02101,"1101,65597-0117-90 ",.02)
 ;;65597-0117-90
 ;;9002226.02101,"1101,65597-0118-30 ",.01)
 ;;65597-0118-30
 ;;9002226.02101,"1101,65597-0118-30 ",.02)
 ;;65597-0118-30
 ;;9002226.02101,"1101,65597-0118-90 ",.01)
 ;;65597-0118-90
 ;;9002226.02101,"1101,65597-0118-90 ",.02)
 ;;65597-0118-90
 ;;9002226.02101,"1101,65726-0226-15 ",.01)
 ;;65726-0226-15
 ;;9002226.02101,"1101,65726-0226-15 ",.02)
 ;;65726-0226-15
 ;;9002226.02101,"1101,65726-0226-25 ",.01)
 ;;65726-0226-25
 ;;9002226.02101,"1101,65726-0226-25 ",.02)
 ;;65726-0226-25
 ;;9002226.02101,"1101,65726-0227-15 ",.01)
 ;;65726-0227-15
 ;;9002226.02101,"1101,65726-0227-15 ",.02)
 ;;65726-0227-15
 ;;9002226.02101,"1101,65726-0227-25 ",.01)
 ;;65726-0227-25
 ;;9002226.02101,"1101,65726-0227-25 ",.02)
 ;;65726-0227-25
 ;;9002226.02101,"1101,65726-0235-10 ",.01)
 ;;65726-0235-10
 ;;9002226.02101,"1101,65726-0235-10 ",.02)
 ;;65726-0235-10
 ;;9002226.02101,"1101,65726-0235-25 ",.01)
 ;;65726-0235-25
 ;;9002226.02101,"1101,65726-0235-25 ",.02)
 ;;65726-0235-25
 ;;9002226.02101,"1101,65726-0236-10 ",.01)
 ;;65726-0236-10
 ;;9002226.02101,"1101,65726-0236-10 ",.02)
 ;;65726-0236-10
 ;;9002226.02101,"1101,65726-0236-25 ",.01)
 ;;65726-0236-25
 ;;9002226.02101,"1101,65726-0236-25 ",.02)
 ;;65726-0236-25
 ;;9002226.02101,"1101,65862-0101-05 ",.01)
 ;;65862-0101-05
 ;;9002226.02101,"1101,65862-0101-05 ",.02)
 ;;65862-0101-05
 ;;9002226.02101,"1101,65862-0101-90 ",.01)
 ;;65862-0101-90
 ;;9002226.02101,"1101,65862-0101-90 ",.02)
 ;;65862-0101-90
 ;;9002226.02101,"1101,65862-0101-99 ",.01)
 ;;65862-0101-99
 ;;9002226.02101,"1101,65862-0101-99 ",.02)
 ;;65862-0101-99
 ;;9002226.02101,"1101,65862-0102-05 ",.01)
 ;;65862-0102-05
 ;;9002226.02101,"1101,65862-0102-05 ",.02)
 ;;65862-0102-05
 ;;9002226.02101,"1101,65862-0102-90 ",.01)
 ;;65862-0102-90
 ;;9002226.02101,"1101,65862-0102-90 ",.02)
 ;;65862-0102-90
 ;;9002226.02101,"1101,65862-0102-99 ",.01)
 ;;65862-0102-99
 ;;9002226.02101,"1101,65862-0102-99 ",.02)
 ;;65862-0102-99
 ;;9002226.02101,"1101,65862-0103-05 ",.01)
 ;;65862-0103-05
 ;;9002226.02101,"1101,65862-0103-05 ",.02)
 ;;65862-0103-05
 ;;9002226.02101,"1101,65862-0103-90 ",.01)
 ;;65862-0103-90
 ;;9002226.02101,"1101,65862-0103-90 ",.02)
 ;;65862-0103-90
 ;;9002226.02101,"1101,65862-0103-99 ",.01)
 ;;65862-0103-99
 ;;9002226.02101,"1101,65862-0103-99 ",.02)
 ;;65862-0103-99
 ;;9002226.02101,"1101,66105-0528-01 ",.01)
 ;;66105-0528-01
 ;;9002226.02101,"1101,66105-0528-01 ",.02)
 ;;66105-0528-01
 ;;9002226.02101,"1101,66105-0528-03 ",.01)
 ;;66105-0528-03
 ;;9002226.02101,"1101,66105-0528-03 ",.02)
 ;;66105-0528-03
 ;;9002226.02101,"1101,66105-0528-06 ",.01)
 ;;66105-0528-06
 ;;9002226.02101,"1101,66105-0528-06 ",.02)
 ;;66105-0528-06
 ;;9002226.02101,"1101,66105-0528-09 ",.01)
 ;;66105-0528-09
 ;;9002226.02101,"1101,66105-0528-09 ",.02)
 ;;66105-0528-09
 ;;9002226.02101,"1101,66105-0528-10 ",.01)
 ;;66105-0528-10
 ;;9002226.02101,"1101,66105-0528-10 ",.02)
 ;;66105-0528-10
 ;;9002226.02101,"1101,66105-0529-01 ",.01)
 ;;66105-0529-01
 ;;9002226.02101,"1101,66105-0529-01 ",.02)
 ;;66105-0529-01
 ;;9002226.02101,"1101,66105-0529-03 ",.01)
 ;;66105-0529-03
 ;;9002226.02101,"1101,66105-0529-03 ",.02)
 ;;66105-0529-03
 ;;9002226.02101,"1101,66105-0529-06 ",.01)
 ;;66105-0529-06
 ;;9002226.02101,"1101,66105-0529-06 ",.02)
 ;;66105-0529-06
 ;;9002226.02101,"1101,66105-0529-09 ",.01)
 ;;66105-0529-09
 ;;9002226.02101,"1101,66105-0529-09 ",.02)
 ;;66105-0529-09
 ;;9002226.02101,"1101,66105-0529-10 ",.01)
 ;;66105-0529-10
 ;;9002226.02101,"1101,66105-0529-10 ",.02)
 ;;66105-0529-10
 ;;9002226.02101,"1101,66116-0278-30 ",.01)
 ;;66116-0278-30
 ;;9002226.02101,"1101,66116-0278-30 ",.02)
 ;;66116-0278-30
 ;;9002226.02101,"1101,66116-0432-30 ",.01)
 ;;66116-0432-30
 ;;9002226.02101,"1101,66116-0432-30 ",.02)
 ;;66116-0432-30
 ;;9002226.02101,"1101,66116-0433-30 ",.01)
 ;;66116-0433-30
 ;;9002226.02101,"1101,66116-0433-30 ",.02)
 ;;66116-0433-30
 ;;9002226.02101,"1101,66116-0468-30 ",.01)
 ;;66116-0468-30
 ;;9002226.02101,"1101,66116-0468-30 ",.02)
 ;;66116-0468-30
 ;;9002226.02101,"1101,66116-0492-30 ",.01)
 ;;66116-0492-30
 ;;9002226.02101,"1101,66116-0492-30 ",.02)
 ;;66116-0492-30
 ;;9002226.02101,"1101,66336-0255-30 ",.01)
 ;;66336-0255-30
 ;;9002226.02101,"1101,66336-0255-30 ",.02)
 ;;66336-0255-30
 ;;9002226.02101,"1101,66336-0296-30 ",.01)
 ;;66336-0296-30
 ;;9002226.02101,"1101,66336-0296-30 ",.02)
 ;;66336-0296-30
 ;;9002226.02101,"1101,66336-0300-30 ",.01)
 ;;66336-0300-30
 ;;9002226.02101,"1101,66336-0300-30 ",.02)
 ;;66336-0300-30
 ;;9002226.02101,"1101,66336-0424-30 ",.01)
 ;;66336-0424-30
 ;;9002226.02101,"1101,66336-0424-30 ",.02)
 ;;66336-0424-30
 ;;9002226.02101,"1101,66336-0669-14 ",.01)
 ;;66336-0669-14
 ;;9002226.02101,"1101,66336-0669-14 ",.02)
 ;;66336-0669-14
 ;;9002226.02101,"1101,66336-0683-30 ",.01)
 ;;66336-0683-30
 ;;9002226.02101,"1101,66336-0683-30 ",.02)
 ;;66336-0683-30
 ;;9002226.02101,"1101,66336-0781-30 ",.01)
 ;;66336-0781-30
 ;;9002226.02101,"1101,66336-0781-30 ",.02)
 ;;66336-0781-30
 ;;9002226.02101,"1101,66336-0809-30 ",.01)
 ;;66336-0809-30
 ;;9002226.02101,"1101,66336-0809-30 ",.02)
 ;;66336-0809-30
 ;;9002226.02101,"1101,66336-0809-90 ",.01)
 ;;66336-0809-90
 ;;9002226.02101,"1101,66336-0809-90 ",.02)
 ;;66336-0809-90
