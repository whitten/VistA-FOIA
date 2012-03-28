BGP12VF ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"534,00247-0667-08 ",.01)
 ;;00247-0667-08
 ;;9002226.02101,"534,00247-0667-08 ",.02)
 ;;00247-0667-08
 ;;9002226.02101,"534,00247-0674-41 ",.01)
 ;;00247-0674-41
 ;;9002226.02101,"534,00247-0674-41 ",.02)
 ;;00247-0674-41
 ;;9002226.02101,"534,00247-0703-07 ",.01)
 ;;00247-0703-07
 ;;9002226.02101,"534,00247-0703-07 ",.02)
 ;;00247-0703-07
 ;;9002226.02101,"534,00247-0870-20 ",.01)
 ;;00247-0870-20
 ;;9002226.02101,"534,00247-0870-20 ",.02)
 ;;00247-0870-20
 ;;9002226.02101,"534,00247-0871-20 ",.01)
 ;;00247-0871-20
 ;;9002226.02101,"534,00247-0871-20 ",.02)
 ;;00247-0871-20
 ;;9002226.02101,"534,00247-0873-02 ",.01)
 ;;00247-0873-02
 ;;9002226.02101,"534,00247-0873-02 ",.02)
 ;;00247-0873-02
 ;;9002226.02101,"534,00247-0873-52 ",.01)
 ;;00247-0873-52
 ;;9002226.02101,"534,00247-0873-52 ",.02)
 ;;00247-0873-52
 ;;9002226.02101,"534,00247-0873-60 ",.01)
 ;;00247-0873-60
 ;;9002226.02101,"534,00247-0873-60 ",.02)
 ;;00247-0873-60
 ;;9002226.02101,"534,00247-0912-17 ",.01)
 ;;00247-0912-17
 ;;9002226.02101,"534,00247-0912-17 ",.02)
 ;;00247-0912-17
 ;;9002226.02101,"534,00247-1174-17 ",.01)
 ;;00247-1174-17
 ;;9002226.02101,"534,00247-1174-17 ",.02)
 ;;00247-1174-17
 ;;9002226.02101,"534,00247-1364-17 ",.01)
 ;;00247-1364-17
 ;;9002226.02101,"534,00247-1364-17 ",.02)
 ;;00247-1364-17
 ;;9002226.02101,"534,00247-1575-13 ",.01)
 ;;00247-1575-13
 ;;9002226.02101,"534,00247-1575-13 ",.02)
 ;;00247-1575-13
 ;;9002226.02101,"534,00247-1576-12 ",.01)
 ;;00247-1576-12
 ;;9002226.02101,"534,00247-1576-12 ",.02)
 ;;00247-1576-12
 ;;9002226.02101,"534,00247-1576-13 ",.01)
 ;;00247-1576-13
 ;;9002226.02101,"534,00247-1576-13 ",.02)
 ;;00247-1576-13
 ;;9002226.02101,"534,00247-1577-13 ",.01)
 ;;00247-1577-13
 ;;9002226.02101,"534,00247-1577-13 ",.02)
 ;;00247-1577-13
 ;;9002226.02101,"534,00247-1696-93 ",.01)
 ;;00247-1696-93
 ;;9002226.02101,"534,00247-1696-93 ",.02)
 ;;00247-1696-93
 ;;9002226.02101,"534,00247-1973-60 ",.01)
 ;;00247-1973-60
 ;;9002226.02101,"534,00247-1973-60 ",.02)
 ;;00247-1973-60
 ;;9002226.02101,"534,00247-1983-60 ",.01)
 ;;00247-1983-60
 ;;9002226.02101,"534,00247-1983-60 ",.02)
 ;;00247-1983-60
 ;;9002226.02101,"534,00247-2072-60 ",.01)
 ;;00247-2072-60
 ;;9002226.02101,"534,00247-2072-60 ",.02)
 ;;00247-2072-60
 ;;9002226.02101,"534,00247-2215-60 ",.01)
 ;;00247-2215-60
 ;;9002226.02101,"534,00247-2215-60 ",.02)
 ;;00247-2215-60
 ;;9002226.02101,"534,00247-2224-72 ",.01)
 ;;00247-2224-72
 ;;9002226.02101,"534,00247-2224-72 ",.02)
 ;;00247-2224-72
 ;;9002226.02101,"534,00247-2225-72 ",.01)
 ;;00247-2225-72
 ;;9002226.02101,"534,00247-2225-72 ",.02)
 ;;00247-2225-72
 ;;9002226.02101,"534,00247-2226-72 ",.01)
 ;;00247-2226-72
 ;;9002226.02101,"534,00247-2226-72 ",.02)
 ;;00247-2226-72
 ;;9002226.02101,"534,00247-2227-01 ",.01)
 ;;00247-2227-01
 ;;9002226.02101,"534,00247-2227-01 ",.02)
 ;;00247-2227-01
 ;;9002226.02101,"534,00247-2297-58 ",.01)
 ;;00247-2297-58
 ;;9002226.02101,"534,00247-2297-58 ",.02)
 ;;00247-2297-58
 ;;9002226.02101,"534,00378-6990-52 ",.01)
 ;;00378-6990-52
 ;;9002226.02101,"534,00378-6990-52 ",.02)
 ;;00378-6990-52
 ;;9002226.02101,"534,00378-6990-58 ",.01)
 ;;00378-6990-58
 ;;9002226.02101,"534,00378-6990-58 ",.02)
 ;;00378-6990-58
 ;;9002226.02101,"534,00378-6990-91 ",.01)
 ;;00378-6990-91
 ;;9002226.02101,"534,00378-6990-91 ",.02)
 ;;00378-6990-91
 ;;9002226.02101,"534,00378-6990-93 ",.01)
 ;;00378-6990-93
 ;;9002226.02101,"534,00378-6990-93 ",.02)
 ;;00378-6990-93
 ;;9002226.02101,"534,00378-6991-52 ",.01)
 ;;00378-6991-52
 ;;9002226.02101,"534,00378-6991-52 ",.02)
 ;;00378-6991-52
 ;;9002226.02101,"534,00378-6992-52 ",.01)
 ;;00378-6992-52
 ;;9002226.02101,"534,00378-6992-52 ",.02)
 ;;00378-6992-52
 ;;9002226.02101,"534,00378-6993-93 ",.01)
 ;;00378-6993-93
 ;;9002226.02101,"534,00378-6993-93 ",.02)
 ;;00378-6993-93
 ;;9002226.02101,"534,00456-0670-99 ",.01)
 ;;00456-0670-99
 ;;9002226.02101,"534,00456-0670-99 ",.02)
 ;;00456-0670-99
 ;;9002226.02101,"534,00456-0672-99 ",.01)
 ;;00456-0672-99
 ;;9002226.02101,"534,00456-0672-99 ",.02)
 ;;00456-0672-99
 ;;9002226.02101,"534,00472-0750-21 ",.01)
 ;;00472-0750-21
 ;;9002226.02101,"534,00472-0750-21 ",.02)
 ;;00472-0750-21
 ;;9002226.02101,"534,00472-0750-60 ",.01)
 ;;00472-0750-60
 ;;9002226.02101,"534,00472-0750-60 ",.02)
 ;;00472-0750-60
 ;;9002226.02101,"534,00472-0752-21 ",.01)
 ;;00472-0752-21
 ;;9002226.02101,"534,00472-0752-21 ",.02)
 ;;00472-0752-21
 ;;9002226.02101,"534,00472-0752-60 ",.01)
 ;;00472-0752-60
 ;;9002226.02101,"534,00472-0752-60 ",.02)
 ;;00472-0752-60
 ;;9002226.02101,"534,00472-0831-23 ",.01)
 ;;00472-0831-23
 ;;9002226.02101,"534,00472-0831-23 ",.02)
 ;;00472-0831-23
 ;;9002226.02101,"534,00472-0831-30 ",.01)
 ;;00472-0831-30
 ;;9002226.02101,"534,00472-0831-30 ",.02)
 ;;00472-0831-30
 ;;9002226.02101,"534,00472-0831-60 ",.01)
 ;;00472-0831-60
 ;;9002226.02101,"534,00472-0831-60 ",.02)
 ;;00472-0831-60
 ;;9002226.02101,"534,00472-0832-20 ",.01)
 ;;00472-0832-20
 ;;9002226.02101,"534,00472-0832-20 ",.02)
 ;;00472-0832-20
 ;;9002226.02101,"534,00472-0832-30 ",.01)
 ;;00472-0832-30
 ;;9002226.02101,"534,00472-0832-30 ",.02)
 ;;00472-0832-30
