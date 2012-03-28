BGP13T29 ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"992,13668-0009-74 ",.01)
 ;;13668-0009-74
 ;;9002226.02101,"992,13668-0009-74 ",.02)
 ;;13668-0009-74
 ;;9002226.02101,"992,13668-0010-01 ",.01)
 ;;13668-0010-01
 ;;9002226.02101,"992,13668-0010-01 ",.02)
 ;;13668-0010-01
 ;;9002226.02101,"992,13668-0010-05 ",.01)
 ;;13668-0010-05
 ;;9002226.02101,"992,13668-0010-05 ",.02)
 ;;13668-0010-05
 ;;9002226.02101,"992,13668-0010-06 ",.01)
 ;;13668-0010-06
 ;;9002226.02101,"992,13668-0010-06 ",.02)
 ;;13668-0010-06
 ;;9002226.02101,"992,13668-0010-30 ",.01)
 ;;13668-0010-30
 ;;9002226.02101,"992,13668-0010-30 ",.02)
 ;;13668-0010-30
 ;;9002226.02101,"992,13668-0011-01 ",.01)
 ;;13668-0011-01
 ;;9002226.02101,"992,13668-0011-01 ",.02)
 ;;13668-0011-01
 ;;9002226.02101,"992,13668-0011-05 ",.01)
 ;;13668-0011-05
 ;;9002226.02101,"992,13668-0011-05 ",.02)
 ;;13668-0011-05
 ;;9002226.02101,"992,13668-0011-08 ",.01)
 ;;13668-0011-08
 ;;9002226.02101,"992,13668-0011-08 ",.02)
 ;;13668-0011-08
 ;;9002226.02101,"992,13668-0011-30 ",.01)
 ;;13668-0011-30
 ;;9002226.02101,"992,13668-0011-30 ",.02)
 ;;13668-0011-30
 ;;9002226.02101,"992,16241-0759-01 ",.01)
 ;;16241-0759-01
 ;;9002226.02101,"992,16241-0759-01 ",.02)
 ;;16241-0759-01
 ;;9002226.02101,"992,16252-0533-30 ",.01)
 ;;16252-0533-30
 ;;9002226.02101,"992,16252-0533-30 ",.02)
 ;;16252-0533-30
 ;;9002226.02101,"992,16252-0533-50 ",.01)
 ;;16252-0533-50
 ;;9002226.02101,"992,16252-0533-50 ",.02)
 ;;16252-0533-50
 ;;9002226.02101,"992,16252-0534-30 ",.01)
 ;;16252-0534-30
 ;;9002226.02101,"992,16252-0534-30 ",.02)
 ;;16252-0534-30
 ;;9002226.02101,"992,16252-0534-50 ",.01)
 ;;16252-0534-50
 ;;9002226.02101,"992,16252-0534-50 ",.02)
 ;;16252-0534-50
 ;;9002226.02101,"992,16252-0534-90 ",.01)
 ;;16252-0534-90
 ;;9002226.02101,"992,16252-0534-90 ",.02)
 ;;16252-0534-90
 ;;9002226.02101,"992,16252-0535-30 ",.01)
 ;;16252-0535-30
 ;;9002226.02101,"992,16252-0535-30 ",.02)
 ;;16252-0535-30
 ;;9002226.02101,"992,16252-0535-50 ",.01)
 ;;16252-0535-50
 ;;9002226.02101,"992,16252-0535-50 ",.02)
 ;;16252-0535-50
 ;;9002226.02101,"992,16252-0535-90 ",.01)
 ;;16252-0535-90
 ;;9002226.02101,"992,16252-0535-90 ",.02)
 ;;16252-0535-90
 ;;9002226.02101,"992,16590-0011-30 ",.01)
 ;;16590-0011-30
 ;;9002226.02101,"992,16590-0011-30 ",.02)
 ;;16590-0011-30
 ;;9002226.02101,"992,16590-0011-56 ",.01)
 ;;16590-0011-56
 ;;9002226.02101,"992,16590-0011-56 ",.02)
 ;;16590-0011-56
 ;;9002226.02101,"992,16590-0011-60 ",.01)
 ;;16590-0011-60
 ;;9002226.02101,"992,16590-0011-60 ",.02)
 ;;16590-0011-60
 ;;9002226.02101,"992,16590-0011-72 ",.01)
 ;;16590-0011-72
 ;;9002226.02101,"992,16590-0011-72 ",.02)
 ;;16590-0011-72
 ;;9002226.02101,"992,16590-0011-90 ",.01)
 ;;16590-0011-90
 ;;9002226.02101,"992,16590-0011-90 ",.02)
 ;;16590-0011-90
 ;;9002226.02101,"992,16590-0012-15 ",.01)
 ;;16590-0012-15
 ;;9002226.02101,"992,16590-0012-15 ",.02)
 ;;16590-0012-15
 ;;9002226.02101,"992,16590-0012-30 ",.01)
 ;;16590-0012-30
 ;;9002226.02101,"992,16590-0012-30 ",.02)
 ;;16590-0012-30
 ;;9002226.02101,"992,16590-0012-60 ",.01)
 ;;16590-0012-60
 ;;9002226.02101,"992,16590-0012-60 ",.02)
 ;;16590-0012-60
 ;;9002226.02101,"992,16590-0012-90 ",.01)
 ;;16590-0012-90
 ;;9002226.02101,"992,16590-0012-90 ",.02)
 ;;16590-0012-90
 ;;9002226.02101,"992,16590-0013-30 ",.01)
 ;;16590-0013-30
 ;;9002226.02101,"992,16590-0013-30 ",.02)
 ;;16590-0013-30
 ;;9002226.02101,"992,16590-0013-60 ",.01)
 ;;16590-0013-60
 ;;9002226.02101,"992,16590-0013-60 ",.02)
 ;;16590-0013-60
 ;;9002226.02101,"992,16590-0013-90 ",.01)
 ;;16590-0013-90
 ;;9002226.02101,"992,16590-0013-90 ",.02)
 ;;16590-0013-90
 ;;9002226.02101,"992,16590-0036-30 ",.01)
 ;;16590-0036-30
 ;;9002226.02101,"992,16590-0036-30 ",.02)
 ;;16590-0036-30
 ;;9002226.02101,"992,16590-0036-60 ",.01)
 ;;16590-0036-60
 ;;9002226.02101,"992,16590-0036-60 ",.02)
 ;;16590-0036-60
 ;;9002226.02101,"992,16590-0037-30 ",.01)
 ;;16590-0037-30
 ;;9002226.02101,"992,16590-0037-30 ",.02)
 ;;16590-0037-30
 ;;9002226.02101,"992,16590-0037-60 ",.01)
 ;;16590-0037-60
 ;;9002226.02101,"992,16590-0037-60 ",.02)
 ;;16590-0037-60
 ;;9002226.02101,"992,16590-0038-30 ",.01)
 ;;16590-0038-30
 ;;9002226.02101,"992,16590-0038-30 ",.02)
 ;;16590-0038-30
 ;;9002226.02101,"992,16590-0038-60 ",.01)
 ;;16590-0038-60
 ;;9002226.02101,"992,16590-0038-60 ",.02)
 ;;16590-0038-60
 ;;9002226.02101,"992,16590-0055-30 ",.01)
 ;;16590-0055-30
 ;;9002226.02101,"992,16590-0055-30 ",.02)
 ;;16590-0055-30
 ;;9002226.02101,"992,16590-0055-60 ",.01)
 ;;16590-0055-60
 ;;9002226.02101,"992,16590-0055-60 ",.02)
 ;;16590-0055-60
 ;;9002226.02101,"992,16590-0055-90 ",.01)
 ;;16590-0055-90
 ;;9002226.02101,"992,16590-0055-90 ",.02)
 ;;16590-0055-90
 ;;9002226.02101,"992,16590-0056-30 ",.01)
 ;;16590-0056-30
 ;;9002226.02101,"992,16590-0056-30 ",.02)
 ;;16590-0056-30
 ;;9002226.02101,"992,16590-0056-60 ",.01)
 ;;16590-0056-60
 ;;9002226.02101,"992,16590-0056-60 ",.02)
 ;;16590-0056-60
 ;;9002226.02101,"992,16590-0056-90 ",.01)
 ;;16590-0056-90
 ;;9002226.02101,"992,16590-0056-90 ",.02)
 ;;16590-0056-90
 ;;9002226.02101,"992,16590-0066-30 ",.01)
 ;;16590-0066-30
 ;;9002226.02101,"992,16590-0066-30 ",.02)
 ;;16590-0066-30
 ;;9002226.02101,"992,16590-0066-60 ",.01)
 ;;16590-0066-60
 ;;9002226.02101,"992,16590-0066-60 ",.02)
 ;;16590-0066-60
 ;;9002226.02101,"992,16590-0067-30 ",.01)
 ;;16590-0067-30
 ;;9002226.02101,"992,16590-0067-30 ",.02)
 ;;16590-0067-30
 ;;9002226.02101,"992,16590-0067-60 ",.01)
 ;;16590-0067-60
 ;;9002226.02101,"992,16590-0067-60 ",.02)
 ;;16590-0067-60
 ;;9002226.02101,"992,16590-0081-30 ",.01)
 ;;16590-0081-30
 ;;9002226.02101,"992,16590-0081-30 ",.02)
 ;;16590-0081-30
 ;;9002226.02101,"992,16590-0081-60 ",.01)
 ;;16590-0081-60
 ;;9002226.02101,"992,16590-0081-60 ",.02)
 ;;16590-0081-60
 ;;9002226.02101,"992,16590-0081-72 ",.01)
 ;;16590-0081-72
 ;;9002226.02101,"992,16590-0081-72 ",.02)
 ;;16590-0081-72
 ;;9002226.02101,"992,16590-0081-90 ",.01)
 ;;16590-0081-90
 ;;9002226.02101,"992,16590-0081-90 ",.02)
 ;;16590-0081-90
 ;;9002226.02101,"992,16590-0083-30 ",.01)
 ;;16590-0083-30
 ;;9002226.02101,"992,16590-0083-30 ",.02)
 ;;16590-0083-30
 ;;9002226.02101,"992,16590-0083-60 ",.01)
 ;;16590-0083-60
 ;;9002226.02101,"992,16590-0083-60 ",.02)
 ;;16590-0083-60
 ;;9002226.02101,"992,16590-0084-30 ",.01)
 ;;16590-0084-30
 ;;9002226.02101,"992,16590-0084-30 ",.02)
 ;;16590-0084-30
 ;;9002226.02101,"992,16590-0084-60 ",.01)
 ;;16590-0084-60
 ;;9002226.02101,"992,16590-0084-60 ",.02)
 ;;16590-0084-60
 ;;9002226.02101,"992,16590-0085-30 ",.01)
 ;;16590-0085-30
 ;;9002226.02101,"992,16590-0085-30 ",.02)
 ;;16590-0085-30
 ;;9002226.02101,"992,16590-0085-60 ",.01)
 ;;16590-0085-60
 ;;9002226.02101,"992,16590-0085-60 ",.02)
 ;;16590-0085-60
 ;;9002226.02101,"992,16590-0086-30 ",.01)
 ;;16590-0086-30
 ;;9002226.02101,"992,16590-0086-30 ",.02)
 ;;16590-0086-30
 ;;9002226.02101,"992,16590-0086-60 ",.01)
 ;;16590-0086-60
 ;;9002226.02101,"992,16590-0086-60 ",.02)
 ;;16590-0086-60
 ;;9002226.02101,"992,16590-0087-30 ",.01)
 ;;16590-0087-30
 ;;9002226.02101,"992,16590-0087-30 ",.02)
 ;;16590-0087-30
 ;;9002226.02101,"992,16590-0087-60 ",.01)
 ;;16590-0087-60
 ;;9002226.02101,"992,16590-0087-60 ",.02)
 ;;16590-0087-60
 ;;9002226.02101,"992,16590-0099-30 ",.01)
 ;;16590-0099-30
 ;;9002226.02101,"992,16590-0099-30 ",.02)
 ;;16590-0099-30
 ;;9002226.02101,"992,16590-0099-60 ",.01)
 ;;16590-0099-60
 ;;9002226.02101,"992,16590-0099-60 ",.02)
 ;;16590-0099-60
 ;;9002226.02101,"992,16590-0100-30 ",.01)
 ;;16590-0100-30
 ;;9002226.02101,"992,16590-0100-30 ",.02)
 ;;16590-0100-30
 ;;9002226.02101,"992,16590-0100-60 ",.01)
 ;;16590-0100-60
 ;;9002226.02101,"992,16590-0100-60 ",.02)
 ;;16590-0100-60
 ;;9002226.02101,"992,16590-0100-90 ",.01)
 ;;16590-0100-90
 ;;9002226.02101,"992,16590-0100-90 ",.02)
 ;;16590-0100-90
 ;;9002226.02101,"992,16590-0139-30 ",.01)
 ;;16590-0139-30
 ;;9002226.02101,"992,16590-0139-30 ",.02)
 ;;16590-0139-30
 ;;9002226.02101,"992,16590-0139-60 ",.01)
 ;;16590-0139-60
 ;;9002226.02101,"992,16590-0139-60 ",.02)
 ;;16590-0139-60
 ;;9002226.02101,"992,16590-0153-30 ",.01)
 ;;16590-0153-30
 ;;9002226.02101,"992,16590-0153-30 ",.02)
 ;;16590-0153-30
 ;;9002226.02101,"992,16590-0153-60 ",.01)
 ;;16590-0153-60
 ;;9002226.02101,"992,16590-0153-60 ",.02)
 ;;16590-0153-60
 ;;9002226.02101,"992,16590-0154-30 ",.01)
 ;;16590-0154-30
 ;;9002226.02101,"992,16590-0154-30 ",.02)
 ;;16590-0154-30
 ;;9002226.02101,"992,16590-0154-60 ",.01)
 ;;16590-0154-60
 ;;9002226.02101,"992,16590-0154-60 ",.02)
 ;;16590-0154-60
 ;;9002226.02101,"992,16590-0155-30 ",.01)
 ;;16590-0155-30
 ;;9002226.02101,"992,16590-0155-30 ",.02)
 ;;16590-0155-30
 ;;9002226.02101,"992,16590-0155-60 ",.01)
 ;;16590-0155-60
 ;;9002226.02101,"992,16590-0155-60 ",.02)
 ;;16590-0155-60
 ;;9002226.02101,"992,16590-0166-30 ",.01)
 ;;16590-0166-30
 ;;9002226.02101,"992,16590-0166-30 ",.02)
 ;;16590-0166-30
 ;;9002226.02101,"992,16590-0166-60 ",.01)
 ;;16590-0166-60
 ;;9002226.02101,"992,16590-0166-60 ",.02)
 ;;16590-0166-60
 ;;9002226.02101,"992,16590-0166-90 ",.01)
 ;;16590-0166-90
 ;;9002226.02101,"992,16590-0166-90 ",.02)
 ;;16590-0166-90
 ;;9002226.02101,"992,16590-0171-30 ",.01)
 ;;16590-0171-30
 ;;9002226.02101,"992,16590-0171-30 ",.02)
 ;;16590-0171-30
 ;;9002226.02101,"992,16590-0171-60 ",.01)
 ;;16590-0171-60
 ;;9002226.02101,"992,16590-0171-60 ",.02)
 ;;16590-0171-60
 ;;9002226.02101,"992,16590-0171-90 ",.01)
 ;;16590-0171-90
 ;;9002226.02101,"992,16590-0171-90 ",.02)
 ;;16590-0171-90
 ;;9002226.02101,"992,16590-0181-30 ",.01)
 ;;16590-0181-30
 ;;9002226.02101,"992,16590-0181-30 ",.02)
 ;;16590-0181-30
 ;;9002226.02101,"992,16590-0181-60 ",.01)
 ;;16590-0181-60
 ;;9002226.02101,"992,16590-0181-60 ",.02)
 ;;16590-0181-60
 ;;9002226.02101,"992,16590-0181-90 ",.01)
 ;;16590-0181-90
 ;;9002226.02101,"992,16590-0181-90 ",.02)
 ;;16590-0181-90
 ;;9002226.02101,"992,16590-0231-30 ",.01)
 ;;16590-0231-30
 ;;9002226.02101,"992,16590-0231-30 ",.02)
 ;;16590-0231-30
 ;;9002226.02101,"992,16590-0231-60 ",.01)
 ;;16590-0231-60
 ;;9002226.02101,"992,16590-0231-60 ",.02)
 ;;16590-0231-60
 ;;9002226.02101,"992,16590-0231-90 ",.01)
 ;;16590-0231-90
 ;;9002226.02101,"992,16590-0231-90 ",.02)
 ;;16590-0231-90
 ;;9002226.02101,"992,16590-0232-30 ",.01)
 ;;16590-0232-30
 ;;9002226.02101,"992,16590-0232-30 ",.02)
 ;;16590-0232-30
 ;;9002226.02101,"992,16590-0232-45 ",.01)
 ;;16590-0232-45
 ;;9002226.02101,"992,16590-0232-45 ",.02)
 ;;16590-0232-45
 ;;9002226.02101,"992,16590-0232-60 ",.01)
 ;;16590-0232-60
 ;;9002226.02101,"992,16590-0232-60 ",.02)
 ;;16590-0232-60
 ;;9002226.02101,"992,16590-0232-90 ",.01)
 ;;16590-0232-90
 ;;9002226.02101,"992,16590-0232-90 ",.02)
 ;;16590-0232-90
 ;;9002226.02101,"992,16590-0246-30 ",.01)
 ;;16590-0246-30
 ;;9002226.02101,"992,16590-0246-30 ",.02)
 ;;16590-0246-30
 ;;9002226.02101,"992,16590-0246-60 ",.01)
 ;;16590-0246-60
 ;;9002226.02101,"992,16590-0246-60 ",.02)
 ;;16590-0246-60
 ;;9002226.02101,"992,16590-0246-90 ",.01)
 ;;16590-0246-90
 ;;9002226.02101,"992,16590-0246-90 ",.02)
 ;;16590-0246-90
 ;;9002226.02101,"992,16590-0249-30 ",.01)
 ;;16590-0249-30
 ;;9002226.02101,"992,16590-0249-30 ",.02)
 ;;16590-0249-30
 ;;9002226.02101,"992,16590-0249-60 ",.01)
 ;;16590-0249-60
 ;;9002226.02101,"992,16590-0249-60 ",.02)
 ;;16590-0249-60
 ;;9002226.02101,"992,16590-0249-90 ",.01)
 ;;16590-0249-90
 ;;9002226.02101,"992,16590-0249-90 ",.02)
 ;;16590-0249-90
 ;;9002226.02101,"992,16590-0250-30 ",.01)
 ;;16590-0250-30
 ;;9002226.02101,"992,16590-0250-30 ",.02)
 ;;16590-0250-30
 ;;9002226.02101,"992,16590-0250-60 ",.01)
 ;;16590-0250-60
 ;;9002226.02101,"992,16590-0250-60 ",.02)
 ;;16590-0250-60
 ;;9002226.02101,"992,16590-0250-90 ",.01)
 ;;16590-0250-90
 ;;9002226.02101,"992,16590-0250-90 ",.02)
 ;;16590-0250-90
 ;;9002226.02101,"992,16590-0251-30 ",.01)
 ;;16590-0251-30
 ;;9002226.02101,"992,16590-0251-30 ",.02)
 ;;16590-0251-30
 ;;9002226.02101,"992,16590-0251-60 ",.01)
 ;;16590-0251-60
 ;;9002226.02101,"992,16590-0251-60 ",.02)
 ;;16590-0251-60
 ;;9002226.02101,"992,16590-0251-90 ",.01)
 ;;16590-0251-90
 ;;9002226.02101,"992,16590-0251-90 ",.02)
 ;;16590-0251-90
 ;;9002226.02101,"992,16590-0322-30 ",.01)
 ;;16590-0322-30
 ;;9002226.02101,"992,16590-0322-30 ",.02)
 ;;16590-0322-30
 ;;9002226.02101,"992,16590-0322-56 ",.01)
 ;;16590-0322-56
 ;;9002226.02101,"992,16590-0322-56 ",.02)
 ;;16590-0322-56
 ;;9002226.02101,"992,16590-0322-60 ",.01)
 ;;16590-0322-60
