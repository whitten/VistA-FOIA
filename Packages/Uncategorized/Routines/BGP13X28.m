BGP13X28 ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"1100,51079-0981-57 ",.01)
 ;;51079-0981-57
 ;;9002226.02101,"1100,51079-0981-57 ",.02)
 ;;51079-0981-57
 ;;9002226.02101,"1100,51079-0981-60 ",.01)
 ;;51079-0981-60
 ;;9002226.02101,"1100,51079-0981-60 ",.02)
 ;;51079-0981-60
 ;;9002226.02101,"1100,51079-0982-01 ",.01)
 ;;51079-0982-01
 ;;9002226.02101,"1100,51079-0982-01 ",.02)
 ;;51079-0982-01
 ;;9002226.02101,"1100,51079-0982-17 ",.01)
 ;;51079-0982-17
 ;;9002226.02101,"1100,51079-0982-17 ",.02)
 ;;51079-0982-17
 ;;9002226.02101,"1100,51079-0982-19 ",.01)
 ;;51079-0982-19
 ;;9002226.02101,"1100,51079-0982-19 ",.02)
 ;;51079-0982-19
 ;;9002226.02101,"1100,51079-0982-20 ",.01)
 ;;51079-0982-20
 ;;9002226.02101,"1100,51079-0982-20 ",.02)
 ;;51079-0982-20
 ;;9002226.02101,"1100,51079-0982-30 ",.01)
 ;;51079-0982-30
 ;;9002226.02101,"1100,51079-0982-30 ",.02)
 ;;51079-0982-30
 ;;9002226.02101,"1100,51079-0982-40 ",.01)
 ;;51079-0982-40
 ;;9002226.02101,"1100,51079-0982-40 ",.02)
 ;;51079-0982-40
 ;;9002226.02101,"1100,51079-0982-56 ",.01)
 ;;51079-0982-56
 ;;9002226.02101,"1100,51079-0982-56 ",.02)
 ;;51079-0982-56
 ;;9002226.02101,"1100,51079-0982-57 ",.01)
 ;;51079-0982-57
 ;;9002226.02101,"1100,51079-0982-57 ",.02)
 ;;51079-0982-57
 ;;9002226.02101,"1100,51079-0982-60 ",.01)
 ;;51079-0982-60
 ;;9002226.02101,"1100,51079-0982-60 ",.02)
 ;;51079-0982-60
 ;;9002226.02101,"1100,51079-0983-01 ",.01)
 ;;51079-0983-01
 ;;9002226.02101,"1100,51079-0983-01 ",.02)
 ;;51079-0983-01
 ;;9002226.02101,"1100,51079-0983-17 ",.01)
 ;;51079-0983-17
 ;;9002226.02101,"1100,51079-0983-17 ",.02)
 ;;51079-0983-17
 ;;9002226.02101,"1100,51079-0983-19 ",.01)
 ;;51079-0983-19
 ;;9002226.02101,"1100,51079-0983-19 ",.02)
 ;;51079-0983-19
 ;;9002226.02101,"1100,51079-0983-20 ",.01)
 ;;51079-0983-20
 ;;9002226.02101,"1100,51079-0983-20 ",.02)
 ;;51079-0983-20
 ;;9002226.02101,"1100,51079-0983-30 ",.01)
 ;;51079-0983-30
 ;;9002226.02101,"1100,51079-0983-30 ",.02)
 ;;51079-0983-30
 ;;9002226.02101,"1100,51079-0983-40 ",.01)
 ;;51079-0983-40
 ;;9002226.02101,"1100,51079-0983-40 ",.02)
 ;;51079-0983-40
 ;;9002226.02101,"1100,51079-0983-56 ",.01)
 ;;51079-0983-56
 ;;9002226.02101,"1100,51079-0983-56 ",.02)
 ;;51079-0983-56
 ;;9002226.02101,"1100,51079-0983-57 ",.01)
 ;;51079-0983-57
 ;;9002226.02101,"1100,51079-0983-57 ",.02)
 ;;51079-0983-57
 ;;9002226.02101,"1100,51079-0983-60 ",.01)
 ;;51079-0983-60
 ;;9002226.02101,"1100,51079-0983-60 ",.02)
 ;;51079-0983-60
 ;;9002226.02101,"1100,51079-0984-01 ",.01)
 ;;51079-0984-01
 ;;9002226.02101,"1100,51079-0984-01 ",.02)
 ;;51079-0984-01
 ;;9002226.02101,"1100,51079-0984-20 ",.01)
 ;;51079-0984-20
 ;;9002226.02101,"1100,51079-0984-20 ",.02)
 ;;51079-0984-20
 ;;9002226.02101,"1100,51079-0984-40 ",.01)
 ;;51079-0984-40
 ;;9002226.02101,"1100,51079-0984-40 ",.02)
 ;;51079-0984-40
 ;;9002226.02101,"1100,51079-0984-50 ",.01)
 ;;51079-0984-50
 ;;9002226.02101,"1100,51079-0984-50 ",.02)
 ;;51079-0984-50
 ;;9002226.02101,"1100,51655-0278-25 ",.01)
 ;;51655-0278-25
 ;;9002226.02101,"1100,51655-0278-25 ",.02)
 ;;51655-0278-25
 ;;9002226.02101,"1100,51655-0279-24 ",.01)
 ;;51655-0279-24
 ;;9002226.02101,"1100,51655-0279-24 ",.02)
 ;;51655-0279-24
 ;;9002226.02101,"1100,51655-0286-24 ",.01)
 ;;51655-0286-24
 ;;9002226.02101,"1100,51655-0286-24 ",.02)
 ;;51655-0286-24
 ;;9002226.02101,"1100,51655-0287-24 ",.01)
 ;;51655-0287-24
 ;;9002226.02101,"1100,51655-0287-24 ",.02)
 ;;51655-0287-24
 ;;9002226.02101,"1100,51655-0311-25 ",.01)
 ;;51655-0311-25
 ;;9002226.02101,"1100,51655-0311-25 ",.02)
 ;;51655-0311-25
 ;;9002226.02101,"1100,51655-0975-24 ",.01)
 ;;51655-0975-24
 ;;9002226.02101,"1100,51655-0975-24 ",.02)
 ;;51655-0975-24
 ;;9002226.02101,"1100,51672-4037-01 ",.01)
 ;;51672-4037-01
 ;;9002226.02101,"1100,51672-4037-01 ",.02)
 ;;51672-4037-01
 ;;9002226.02101,"1100,51672-4037-03 ",.01)
 ;;51672-4037-03
 ;;9002226.02101,"1100,51672-4037-03 ",.02)
 ;;51672-4037-03
 ;;9002226.02101,"1100,51672-4038-01 ",.01)
 ;;51672-4038-01
 ;;9002226.02101,"1100,51672-4038-01 ",.02)
 ;;51672-4038-01
 ;;9002226.02101,"1100,51672-4038-03 ",.01)
 ;;51672-4038-03
 ;;9002226.02101,"1100,51672-4038-03 ",.02)
 ;;51672-4038-03
 ;;9002226.02101,"1100,51672-4039-01 ",.01)
 ;;51672-4039-01
 ;;9002226.02101,"1100,51672-4039-01 ",.02)
 ;;51672-4039-01
 ;;9002226.02101,"1100,51672-4039-03 ",.01)
 ;;51672-4039-03
 ;;9002226.02101,"1100,51672-4039-03 ",.02)
 ;;51672-4039-03
 ;;9002226.02101,"1100,51672-4040-01 ",.01)
 ;;51672-4040-01
 ;;9002226.02101,"1100,51672-4040-01 ",.02)
 ;;51672-4040-01
 ;;9002226.02101,"1100,51672-4040-03 ",.01)
 ;;51672-4040-03
 ;;9002226.02101,"1100,51672-4040-03 ",.02)
 ;;51672-4040-03
 ;;9002226.02101,"1100,51672-4045-01 ",.01)
 ;;51672-4045-01
 ;;9002226.02101,"1100,51672-4045-01 ",.02)
 ;;51672-4045-01
 ;;9002226.02101,"1100,51672-4046-01 ",.01)
 ;;51672-4046-01
 ;;9002226.02101,"1100,51672-4046-01 ",.02)
 ;;51672-4046-01
 ;;9002226.02101,"1100,52152-0238-08 ",.01)
 ;;52152-0238-08
 ;;9002226.02101,"1100,52152-0238-08 ",.02)
 ;;52152-0238-08
 ;;9002226.02101,"1100,52152-0238-30 ",.01)
 ;;52152-0238-30
 ;;9002226.02101,"1100,52152-0238-30 ",.02)
 ;;52152-0238-30
 ;;9002226.02101,"1100,52152-0239-08 ",.01)
 ;;52152-0239-08
 ;;9002226.02101,"1100,52152-0239-08 ",.02)
 ;;52152-0239-08
 ;;9002226.02101,"1100,52152-0239-30 ",.01)
 ;;52152-0239-30
 ;;9002226.02101,"1100,52152-0239-30 ",.02)
 ;;52152-0239-30
 ;;9002226.02101,"1100,52152-0240-08 ",.01)
 ;;52152-0240-08
 ;;9002226.02101,"1100,52152-0240-08 ",.02)
 ;;52152-0240-08
 ;;9002226.02101,"1100,52152-0240-30 ",.01)
 ;;52152-0240-30
 ;;9002226.02101,"1100,52152-0240-30 ",.02)
 ;;52152-0240-30
 ;;9002226.02101,"1100,52544-0668-01 ",.01)
 ;;52544-0668-01
 ;;9002226.02101,"1100,52544-0668-01 ",.02)
 ;;52544-0668-01
 ;;9002226.02101,"1100,52544-0668-05 ",.01)
 ;;52544-0668-05
 ;;9002226.02101,"1100,52544-0668-05 ",.02)
 ;;52544-0668-05
 ;;9002226.02101,"1100,52544-0669-01 ",.01)
 ;;52544-0669-01
 ;;9002226.02101,"1100,52544-0669-01 ",.02)
 ;;52544-0669-01
 ;;9002226.02101,"1100,52544-0669-05 ",.01)
 ;;52544-0669-05
 ;;9002226.02101,"1100,52544-0669-05 ",.02)
 ;;52544-0669-05
 ;;9002226.02101,"1100,52544-0670-01 ",.01)
 ;;52544-0670-01
 ;;9002226.02101,"1100,52544-0670-01 ",.02)
 ;;52544-0670-01
 ;;9002226.02101,"1100,52544-0670-05 ",.01)
 ;;52544-0670-05
 ;;9002226.02101,"1100,52544-0670-05 ",.02)
 ;;52544-0670-05
 ;;9002226.02101,"1100,52544-0671-01 ",.01)
 ;;52544-0671-01
 ;;9002226.02101,"1100,52544-0671-01 ",.02)
 ;;52544-0671-01
 ;;9002226.02101,"1100,52959-0137-15 ",.01)
 ;;52959-0137-15
 ;;9002226.02101,"1100,52959-0137-15 ",.02)
 ;;52959-0137-15
 ;;9002226.02101,"1100,52959-0180-30 ",.01)
 ;;52959-0180-30
 ;;9002226.02101,"1100,52959-0180-30 ",.02)
 ;;52959-0180-30
 ;;9002226.02101,"1100,52959-0180-60 ",.01)
 ;;52959-0180-60
 ;;9002226.02101,"1100,52959-0180-60 ",.02)
 ;;52959-0180-60
 ;;9002226.02101,"1100,52959-0498-00 ",.01)
 ;;52959-0498-00
 ;;9002226.02101,"1100,52959-0498-00 ",.02)
 ;;52959-0498-00
 ;;9002226.02101,"1100,52959-0728-15 ",.01)
 ;;52959-0728-15
 ;;9002226.02101,"1100,52959-0728-15 ",.02)
 ;;52959-0728-15
 ;;9002226.02101,"1100,52959-0728-20 ",.01)
 ;;52959-0728-20
 ;;9002226.02101,"1100,52959-0728-20 ",.02)
 ;;52959-0728-20
 ;;9002226.02101,"1100,52959-0728-30 ",.01)
 ;;52959-0728-30
 ;;9002226.02101,"1100,52959-0728-30 ",.02)
 ;;52959-0728-30
 ;;9002226.02101,"1100,52959-0729-30 ",.01)
 ;;52959-0729-30
 ;;9002226.02101,"1100,52959-0729-30 ",.02)
 ;;52959-0729-30
 ;;9002226.02101,"1100,52959-0729-60 ",.01)
 ;;52959-0729-60
 ;;9002226.02101,"1100,52959-0729-60 ",.02)
 ;;52959-0729-60
 ;;9002226.02101,"1100,52959-0729-90 ",.01)
 ;;52959-0729-90
 ;;9002226.02101,"1100,52959-0729-90 ",.02)
 ;;52959-0729-90
 ;;9002226.02101,"1100,52959-0753-00 ",.01)
 ;;52959-0753-00
 ;;9002226.02101,"1100,52959-0753-00 ",.02)
 ;;52959-0753-00
 ;;9002226.02101,"1100,52959-0753-30 ",.01)
 ;;52959-0753-30
 ;;9002226.02101,"1100,52959-0753-30 ",.02)
 ;;52959-0753-30
 ;;9002226.02101,"1100,52959-0756-30 ",.01)
 ;;52959-0756-30
 ;;9002226.02101,"1100,52959-0756-30 ",.02)
 ;;52959-0756-30
 ;;9002226.02101,"1100,52959-0831-30 ",.01)
 ;;52959-0831-30
 ;;9002226.02101,"1100,52959-0831-30 ",.02)
 ;;52959-0831-30
 ;;9002226.02101,"1100,52959-0835-30 ",.01)
 ;;52959-0835-30
 ;;9002226.02101,"1100,52959-0835-30 ",.02)
 ;;52959-0835-30
 ;;9002226.02101,"1100,52959-0835-60 ",.01)
 ;;52959-0835-60
 ;;9002226.02101,"1100,52959-0835-60 ",.02)
 ;;52959-0835-60
 ;;9002226.02101,"1100,52959-0841-30 ",.01)
 ;;52959-0841-30
 ;;9002226.02101,"1100,52959-0841-30 ",.02)
 ;;52959-0841-30
 ;;9002226.02101,"1100,52959-0841-60 ",.01)
 ;;52959-0841-60
 ;;9002226.02101,"1100,52959-0841-60 ",.02)
 ;;52959-0841-60
 ;;9002226.02101,"1100,52959-0854-20 ",.01)
 ;;52959-0854-20
 ;;9002226.02101,"1100,52959-0854-20 ",.02)
 ;;52959-0854-20
 ;;9002226.02101,"1100,52959-0854-30 ",.01)
 ;;52959-0854-30
 ;;9002226.02101,"1100,52959-0854-30 ",.02)
 ;;52959-0854-30
 ;;9002226.02101,"1100,52959-0907-30 ",.01)
 ;;52959-0907-30
 ;;9002226.02101,"1100,52959-0907-30 ",.02)
 ;;52959-0907-30
 ;;9002226.02101,"1100,52959-0942-30 ",.01)
 ;;52959-0942-30
 ;;9002226.02101,"1100,52959-0942-30 ",.02)
 ;;52959-0942-30
 ;;9002226.02101,"1100,52959-0973-30 ",.01)
 ;;52959-0973-30
 ;;9002226.02101,"1100,52959-0973-30 ",.02)
 ;;52959-0973-30
 ;;9002226.02101,"1100,52959-0975-30 ",.01)
 ;;52959-0975-30
 ;;9002226.02101,"1100,52959-0975-30 ",.02)
 ;;52959-0975-30
 ;;9002226.02101,"1100,52959-0997-30 ",.01)
 ;;52959-0997-30
 ;;9002226.02101,"1100,52959-0997-30 ",.02)
 ;;52959-0997-30
 ;;9002226.02101,"1100,53002-0431-00 ",.01)
 ;;53002-0431-00
 ;;9002226.02101,"1100,53002-0431-00 ",.02)
 ;;53002-0431-00
 ;;9002226.02101,"1100,53002-0431-30 ",.01)
 ;;53002-0431-30
 ;;9002226.02101,"1100,53002-0431-30 ",.02)
 ;;53002-0431-30
 ;;9002226.02101,"1100,53002-0431-60 ",.01)
 ;;53002-0431-60
 ;;9002226.02101,"1100,53002-0431-60 ",.02)
 ;;53002-0431-60
 ;;9002226.02101,"1100,53002-1086-00 ",.01)
 ;;53002-1086-00
 ;;9002226.02101,"1100,53002-1086-00 ",.02)
 ;;53002-1086-00
 ;;9002226.02101,"1100,53002-1086-03 ",.01)
 ;;53002-1086-03
 ;;9002226.02101,"1100,53002-1086-03 ",.02)
 ;;53002-1086-03
 ;;9002226.02101,"1100,53002-1086-06 ",.01)
 ;;53002-1086-06
 ;;9002226.02101,"1100,53002-1086-06 ",.02)
 ;;53002-1086-06
 ;;9002226.02101,"1100,53002-1123-00 ",.01)
 ;;53002-1123-00
 ;;9002226.02101,"1100,53002-1123-00 ",.02)
 ;;53002-1123-00
 ;;9002226.02101,"1100,53002-1123-03 ",.01)
 ;;53002-1123-03
 ;;9002226.02101,"1100,53002-1123-03 ",.02)
 ;;53002-1123-03
 ;;9002226.02101,"1100,53002-1123-06 ",.01)
 ;;53002-1123-06
 ;;9002226.02101,"1100,53002-1123-06 ",.02)
 ;;53002-1123-06
 ;;9002226.02101,"1100,53002-1178-00 ",.01)
 ;;53002-1178-00
 ;;9002226.02101,"1100,53002-1178-00 ",.02)
 ;;53002-1178-00
 ;;9002226.02101,"1100,53002-1178-03 ",.01)
 ;;53002-1178-03
 ;;9002226.02101,"1100,53002-1178-03 ",.02)
 ;;53002-1178-03
 ;;9002226.02101,"1100,53002-1178-06 ",.01)
 ;;53002-1178-06
 ;;9002226.02101,"1100,53002-1178-06 ",.02)
 ;;53002-1178-06
 ;;9002226.02101,"1100,53002-1225-00 ",.01)
 ;;53002-1225-00
 ;;9002226.02101,"1100,53002-1225-00 ",.02)
 ;;53002-1225-00
 ;;9002226.02101,"1100,53002-1225-03 ",.01)
 ;;53002-1225-03
 ;;9002226.02101,"1100,53002-1225-03 ",.02)
 ;;53002-1225-03
 ;;9002226.02101,"1100,53002-1225-06 ",.01)
 ;;53002-1225-06
 ;;9002226.02101,"1100,53002-1225-06 ",.02)
 ;;53002-1225-06
 ;;9002226.02101,"1100,53002-1463-00 ",.01)
 ;;53002-1463-00
 ;;9002226.02101,"1100,53002-1463-00 ",.02)
 ;;53002-1463-00
 ;;9002226.02101,"1100,53002-1463-03 ",.01)
 ;;53002-1463-03
 ;;9002226.02101,"1100,53002-1463-03 ",.02)
 ;;53002-1463-03
 ;;9002226.02101,"1100,54348-0099-30 ",.01)
 ;;54348-0099-30
 ;;9002226.02101,"1100,54348-0099-30 ",.02)
 ;;54348-0099-30
 ;;9002226.02101,"1100,54348-0100-30 ",.01)
 ;;54348-0100-30
 ;;9002226.02101,"1100,54348-0100-30 ",.02)
 ;;54348-0100-30
 ;;9002226.02101,"1100,54458-0956-10 ",.01)
 ;;54458-0956-10
 ;;9002226.02101,"1100,54458-0956-10 ",.02)
 ;;54458-0956-10
 ;;9002226.02101,"1100,54458-0957-10 ",.01)
 ;;54458-0957-10
 ;;9002226.02101,"1100,54458-0957-10 ",.02)
 ;;54458-0957-10
 ;;9002226.02101,"1100,54458-0958-10 ",.01)
 ;;54458-0958-10
 ;;9002226.02101,"1100,54458-0958-10 ",.02)
 ;;54458-0958-10
 ;;9002226.02101,"1100,54458-0959-10 ",.01)
 ;;54458-0959-10
 ;;9002226.02101,"1100,54458-0959-10 ",.02)
 ;;54458-0959-10
 ;;9002226.02101,"1100,54458-0991-05 ",.01)
 ;;54458-0991-05
 ;;9002226.02101,"1100,54458-0991-05 ",.02)
 ;;54458-0991-05
