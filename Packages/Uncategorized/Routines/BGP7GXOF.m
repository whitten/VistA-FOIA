BGP7GXOF ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON SEP 25, 2006 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"634,00904-2455-10 ",.01)
 ;;00904-2455-10
 ;;9002226.02101,"634,00904-2455-10 ",.02)
 ;;00904-2455-10
 ;;9002226.02101,"634,00904-5519-24 ",.01)
 ;;00904-5519-24
 ;;9002226.02101,"634,00904-5519-24 ",.02)
 ;;00904-5519-24
 ;;9002226.02101,"634,00904-5654-40 ",.01)
 ;;00904-5654-40
 ;;9002226.02101,"634,00904-5654-40 ",.02)
 ;;00904-5654-40
 ;;9002226.02101,"634,00904-5654-60 ",.01)
 ;;00904-5654-60
 ;;9002226.02101,"634,00904-5654-60 ",.02)
 ;;00904-5654-60
 ;;9002226.02101,"634,00904-7721-40 ",.01)
 ;;00904-7721-40
 ;;9002226.02101,"634,00904-7721-40 ",.02)
 ;;00904-7721-40
 ;;9002226.02101,"634,00904-7721-60 ",.01)
 ;;00904-7721-60
 ;;9002226.02101,"634,00904-7721-60 ",.02)
 ;;00904-7721-60
 ;;9002226.02101,"634,00904-7722-40 ",.01)
 ;;00904-7722-40
 ;;9002226.02101,"634,00904-7722-40 ",.02)
 ;;00904-7722-40
 ;;9002226.02101,"634,00904-7722-60 ",.01)
 ;;00904-7722-60
 ;;9002226.02101,"634,00904-7722-60 ",.02)
 ;;00904-7722-60
 ;;9002226.02101,"634,00904-7865-60 ",.01)
 ;;00904-7865-60
 ;;9002226.02101,"634,00904-7865-60 ",.02)
 ;;00904-7865-60
 ;;9002226.02101,"634,00904-7865-80 ",.01)
 ;;00904-7865-80
 ;;9002226.02101,"634,00904-7865-80 ",.02)
 ;;00904-7865-80
 ;;9002226.02101,"634,00905-5658-31 ",.01)
 ;;00905-5658-31
 ;;9002226.02101,"634,00905-5658-31 ",.02)
 ;;00905-5658-31
 ;;9002226.02101,"634,00927-0516-34 ",.01)
 ;;00927-0516-34
 ;;9002226.02101,"634,00927-0516-34 ",.02)
 ;;00927-0516-34
 ;;9002226.02101,"634,10939-0457-33 ",.01)
 ;;10939-0457-33
 ;;9002226.02101,"634,10939-0457-33 ",.02)
 ;;10939-0457-33
 ;;9002226.02101,"634,10939-0466-33 ",.01)
 ;;10939-0466-33
 ;;9002226.02101,"634,10939-0466-33 ",.02)
 ;;10939-0466-33
 ;;9002226.02101,"634,10939-0467-33 ",.01)
 ;;10939-0467-33
 ;;9002226.02101,"634,10939-0467-33 ",.02)
 ;;10939-0467-33
 ;;9002226.02101,"634,10939-0787-22 ",.01)
 ;;10939-0787-22
 ;;9002226.02101,"634,10939-0787-22 ",.02)
 ;;10939-0787-22
 ;;9002226.02101,"634,11441-0030-14 ",.01)
 ;;11441-0030-14
 ;;9002226.02101,"634,11441-0030-14 ",.02)
 ;;11441-0030-14
 ;;9002226.02101,"634,11822-3058-00 ",.01)
 ;;11822-3058-00
 ;;9002226.02101,"634,11822-3058-00 ",.02)
 ;;11822-3058-00
 ;;9002226.02101,"634,11822-3172-90 ",.01)
 ;;11822-3172-90
 ;;9002226.02101,"634,11822-3172-90 ",.02)
 ;;11822-3172-90
 ;;9002226.02101,"634,11845-0201-01 ",.01)
 ;;11845-0201-01
 ;;9002226.02101,"634,11845-0201-01 ",.02)
 ;;11845-0201-01
 ;;9002226.02101,"634,11845-0202-01 ",.01)
 ;;11845-0202-01
 ;;9002226.02101,"634,11845-0202-01 ",.02)
 ;;11845-0202-01
 ;;9002226.02101,"634,11926-0615-16 ",.01)
 ;;11926-0615-16
 ;;9002226.02101,"634,11926-0615-16 ",.02)
 ;;11926-0615-16
 ;;9002226.02101,"634,11926-0615-32 ",.01)
 ;;11926-0615-32
 ;;9002226.02101,"634,11926-0615-32 ",.02)
 ;;11926-0615-32
 ;;9002226.02101,"634,12280-0204-00 ",.01)
 ;;12280-0204-00
 ;;9002226.02101,"634,12280-0204-00 ",.02)
 ;;12280-0204-00
 ;;9002226.02101,"634,12333-9536-01 ",.01)
 ;;12333-9536-01
 ;;9002226.02101,"634,12333-9536-01 ",.02)
 ;;12333-9536-01
 ;;9002226.02101,"634,15127-0258-16 ",.01)
 ;;15127-0258-16
 ;;9002226.02101,"634,15127-0258-16 ",.02)
 ;;15127-0258-16
 ;;9002226.02101,"634,15127-0391-16 ",.01)
 ;;15127-0391-16
 ;;9002226.02101,"634,15127-0391-16 ",.02)
 ;;15127-0391-16
 ;;9002226.02101,"634,16500-0102-00 ",.01)
 ;;16500-0102-00
 ;;9002226.02101,"634,16500-0102-00 ",.02)
 ;;16500-0102-00
 ;;9002226.02101,"634,16500-0102-20 ",.01)
 ;;16500-0102-20
 ;;9002226.02101,"634,16500-0102-20 ",.02)
 ;;16500-0102-20
 ;;9002226.02101,"634,17022-5509-02 ",.01)
 ;;17022-5509-02
 ;;9002226.02101,"634,17022-5509-02 ",.02)
 ;;17022-5509-02
 ;;9002226.02101,"634,17022-5509-04 ",.01)
 ;;17022-5509-04
 ;;9002226.02101,"634,17022-5509-04 ",.02)
 ;;17022-5509-04
 ;;9002226.02101,"634,17022-5530-02 ",.01)
 ;;17022-5530-02
 ;;9002226.02101,"634,17022-5530-02 ",.02)
 ;;17022-5530-02
 ;;9002226.02101,"634,17022-5530-04 ",.01)
 ;;17022-5530-04
 ;;9002226.02101,"634,17022-5530-04 ",.02)
 ;;17022-5530-04
 ;;9002226.02101,"634,17236-0200-01 ",.01)
 ;;17236-0200-01
 ;;9002226.02101,"634,17236-0200-01 ",.02)
 ;;17236-0200-01
 ;;9002226.02101,"634,17236-0200-05 ",.01)
 ;;17236-0200-05
 ;;9002226.02101,"634,17236-0200-05 ",.02)
 ;;17236-0200-05
 ;;9002226.02101,"634,17236-0202-01 ",.01)
 ;;17236-0202-01
 ;;9002226.02101,"634,17236-0202-01 ",.02)
 ;;17236-0202-01
 ;;9002226.02101,"634,17236-0202-05 ",.01)
 ;;17236-0202-05
 ;;9002226.02101,"634,17236-0202-05 ",.02)
 ;;17236-0202-05
 ;;9002226.02101,"634,17236-0345-01 ",.01)
 ;;17236-0345-01
 ;;9002226.02101,"634,17236-0345-01 ",.02)
 ;;17236-0345-01
 ;;9002226.02101,"634,17236-0345-05 ",.01)
 ;;17236-0345-05
 ;;9002226.02101,"634,17236-0345-05 ",.02)
 ;;17236-0345-05
 ;;9002226.02101,"634,17236-0346-01 ",.01)
 ;;17236-0346-01
 ;;9002226.02101,"634,17236-0346-01 ",.02)
 ;;17236-0346-01
 ;;9002226.02101,"634,17236-0346-05 ",.01)
 ;;17236-0346-05
 ;;9002226.02101,"634,17236-0346-05 ",.02)
 ;;17236-0346-05
 ;;9002226.02101,"634,17236-0715-10 ",.01)
 ;;17236-0715-10
 ;;9002226.02101,"634,17236-0715-10 ",.02)
 ;;17236-0715-10
 ;;9002226.02101,"634,17314-2836-03 ",.01)
 ;;17314-2836-03
 ;;9002226.02101,"634,17314-2836-03 ",.02)
 ;;17314-2836-03
 ;;9002226.02101,"634,17314-4608-03 ",.01)
 ;;17314-4608-03
 ;;9002226.02101,"634,17314-4608-03 ",.02)
 ;;17314-4608-03
 ;;9002226.02101,"634,17314-4609-03 ",.01)
 ;;17314-4609-03
 ;;9002226.02101,"634,17314-4609-03 ",.02)
 ;;17314-4609-03
 ;;9002226.02101,"634,17314-4717-03 ",.01)
 ;;17314-4717-03
 ;;9002226.02101,"634,17314-4717-03 ",.02)
 ;;17314-4717-03
 ;;9002226.02101,"634,19458-9103-01 ",.01)
 ;;19458-9103-01
 ;;9002226.02101,"634,19458-9103-01 ",.02)
 ;;19458-9103-01
 ;;9002226.02101,"634,19458-9163-02 ",.01)
 ;;19458-9163-02
 ;;9002226.02101,"634,19458-9163-02 ",.02)
 ;;19458-9163-02
 ;;9002226.02101,"634,20254-0208-01 ",.01)
 ;;20254-0208-01
 ;;9002226.02101,"634,20254-0208-01 ",.02)
 ;;20254-0208-01
 ;;9002226.02101,"634,20254-0208-06 ",.01)
 ;;20254-0208-06
 ;;9002226.02101,"634,20254-0208-06 ",.02)
 ;;20254-0208-06
 ;;9002226.02101,"634,20254-0208-10 ",.01)
 ;;20254-0208-10
 ;;9002226.02101,"634,20254-0208-10 ",.02)
 ;;20254-0208-10
 ;;9002226.02101,"634,23513-0601-24 ",.01)
 ;;23513-0601-24
 ;;9002226.02101,"634,23513-0601-24 ",.02)
 ;;23513-0601-24
 ;;9002226.02101,"634,23513-0601-48 ",.01)
 ;;23513-0601-48
 ;;9002226.02101,"634,23513-0601-48 ",.02)
 ;;23513-0601-48
 ;;9002226.02101,"634,24385-0406-73 ",.01)
 ;;24385-0406-73
 ;;9002226.02101,"634,24385-0406-73 ",.02)
 ;;24385-0406-73
 ;;9002226.02101,"634,25077-1564-00 ",.01)
 ;;25077-1564-00
 ;;9002226.02101,"634,25077-1564-00 ",.02)
 ;;25077-1564-00
 ;;9002226.02101,"634,25332-0030-10 ",.01)
 ;;25332-0030-10
 ;;9002226.02101,"634,25332-0030-10 ",.02)
 ;;25332-0030-10
 ;;9002226.02101,"634,33261-0375-16 ",.01)
 ;;33261-0375-16
 ;;9002226.02101,"634,33261-0375-16 ",.02)
 ;;33261-0375-16
 ;;9002226.02101,"634,34575-0553-64 ",.01)
 ;;34575-0553-64
 ;;9002226.02101,"634,34575-0553-64 ",.02)
 ;;34575-0553-64
 ;;9002226.02101,"634,35515-0953-30 ",.01)
 ;;35515-0953-30
 ;;9002226.02101,"634,35515-0953-30 ",.02)
 ;;35515-0953-30
 ;;9002226.02101,"634,37205-0194-62 ",.01)
 ;;37205-0194-62
 ;;9002226.02101,"634,37205-0194-62 ",.02)
 ;;37205-0194-62
 ;;9002226.02101,"634,41163-0024-73 ",.01)
 ;;41163-0024-73
 ;;9002226.02101,"634,41163-0024-73 ",.02)
 ;;41163-0024-73
 ;;9002226.02101,"634,41163-0256-74 ",.01)
 ;;41163-0256-74
 ;;9002226.02101,"634,41163-0256-74 ",.02)
 ;;41163-0256-74
 ;;9002226.02101,"634,41163-0265-45 ",.01)
 ;;41163-0265-45
 ;;9002226.02101,"634,41163-0265-45 ",.02)
 ;;41163-0265-45
 ;;9002226.02101,"634,41163-0431-62 ",.01)
 ;;41163-0431-62
 ;;9002226.02101,"634,41163-0431-62 ",.02)
 ;;41163-0431-62
 ;;9002226.02101,"634,41280-0220-68 ",.01)
 ;;41280-0220-68
 ;;9002226.02101,"634,41280-0220-68 ",.02)
 ;;41280-0220-68
 ;;9002226.02101,"634,43292-0557-19 ",.01)
 ;;43292-0557-19
 ;;9002226.02101,"634,43292-0557-19 ",.02)
 ;;43292-0557-19
 ;;9002226.02101,"634,43292-0557-65 ",.01)
 ;;43292-0557-65
 ;;9002226.02101,"634,43292-0557-65 ",.02)
 ;;43292-0557-65
 ;;9002226.02101,"634,43292-0557-78 ",.01)
 ;;43292-0557-78
 ;;9002226.02101,"634,43292-0557-78 ",.02)
 ;;43292-0557-78
 ;;9002226.02101,"634,44514-0557-25 ",.01)
 ;;44514-0557-25
 ;;9002226.02101,"634,44514-0557-25 ",.02)
 ;;44514-0557-25
 ;;9002226.02101,"634,44514-0558-25 ",.01)
 ;;44514-0558-25
 ;;9002226.02101,"634,44514-0558-25 ",.02)
 ;;44514-0558-25
 ;;9002226.02101,"634,46703-0091-05 ",.01)
 ;;46703-0091-05
 ;;9002226.02101,"634,46703-0091-05 ",.02)
 ;;46703-0091-05
 ;;9002226.02101,"634,46703-0103-05 ",.01)
 ;;46703-0103-05
 ;;9002226.02101,"634,46703-0103-05 ",.02)
 ;;46703-0103-05
 ;;9002226.02101,"634,48135-1685-50 ",.01)
 ;;48135-1685-50
 ;;9002226.02101,"634,48135-1685-50 ",.02)
 ;;48135-1685-50
 ;;9002226.02101,"634,49072-0717-10 ",.01)
 ;;49072-0717-10
 ;;9002226.02101,"634,49072-0717-10 ",.02)
 ;;49072-0717-10
 ;;9002226.02101,"634,49260-0502-33 ",.01)
 ;;49260-0502-33
 ;;9002226.02101,"634,49260-0502-33 ",.02)
 ;;49260-0502-33
 ;;9002226.02101,"634,49260-0502-92 ",.01)
 ;;49260-0502-92
 ;;9002226.02101,"634,49260-0502-92 ",.02)
 ;;49260-0502-92
 ;;9002226.02101,"634,49260-5023-03 ",.01)
 ;;49260-5023-03
 ;;9002226.02101,"634,49260-5023-03 ",.02)
 ;;49260-5023-03
 ;;9002226.02101,"634,49260-5029-02 ",.01)
 ;;49260-5029-02
 ;;9002226.02101,"634,49260-5029-02 ",.02)
 ;;49260-5029-02
 ;;9002226.02101,"634,49348-0074-03 ",.01)
 ;;49348-0074-03
 ;;9002226.02101,"634,49348-0074-03 ",.02)
 ;;49348-0074-03
 ;;9002226.02101,"634,49348-0074-06 ",.01)
 ;;49348-0074-06
 ;;9002226.02101,"634,49348-0074-06 ",.02)
 ;;49348-0074-06
 ;;9002226.02101,"634,49348-0471-04 ",.01)
 ;;49348-0471-04
 ;;9002226.02101,"634,49348-0471-04 ",.02)
 ;;49348-0471-04
 ;;9002226.02101,"634,49483-0021-01 ",.01)
 ;;49483-0021-01
 ;;9002226.02101,"634,49483-0021-01 ",.02)
 ;;49483-0021-01
 ;;9002226.02101,"634,49483-0021-10 ",.01)
 ;;49483-0021-10
 ;;9002226.02101,"634,49483-0021-10 ",.02)
 ;;49483-0021-10
 ;;9002226.02101,"634,49483-0022-01 ",.01)
 ;;49483-0022-01
 ;;9002226.02101,"634,49483-0022-01 ",.02)
 ;;49483-0022-01
 ;;9002226.02101,"634,49483-0022-10 ",.01)
 ;;49483-0022-10
 ;;9002226.02101,"634,49483-0022-10 ",.02)
 ;;49483-0022-10
 ;;9002226.02101,"634,49483-0023-01 ",.01)
 ;;49483-0023-01
 ;;9002226.02101,"634,49483-0023-01 ",.02)
 ;;49483-0023-01
 ;;9002226.02101,"634,49483-0023-10 ",.01)
 ;;49483-0023-10
 ;;9002226.02101,"634,49483-0023-10 ",.02)
 ;;49483-0023-10
 ;;9002226.02101,"634,49483-0038-01 ",.01)
 ;;49483-0038-01
 ;;9002226.02101,"634,49483-0038-01 ",.02)
 ;;49483-0038-01
 ;;9002226.02101,"634,49483-0038-10 ",.01)
 ;;49483-0038-10
 ;;9002226.02101,"634,49483-0038-10 ",.02)
 ;;49483-0038-10
 ;;9002226.02101,"634,49614-0132-62 ",.01)
 ;;49614-0132-62
 ;;9002226.02101,"634,49614-0132-62 ",.02)
 ;;49614-0132-62
 ;;9002226.02101,"634,49614-0143-56 ",.01)
 ;;49614-0143-56
 ;;9002226.02101,"634,49614-0143-56 ",.02)
 ;;49614-0143-56
 ;;9002226.02101,"634,49614-0146-62 ",.01)
 ;;49614-0146-62
 ;;9002226.02101,"634,49614-0146-62 ",.02)
 ;;49614-0146-62
 ;;9002226.02101,"634,49727-0417-02 ",.01)
 ;;49727-0417-02
 ;;9002226.02101,"634,49727-0417-02 ",.02)
 ;;49727-0417-02
 ;;9002226.02101,"634,49727-0417-04 ",.01)
 ;;49727-0417-04
 ;;9002226.02101,"634,49727-0417-04 ",.02)
 ;;49727-0417-04
 ;;9002226.02101,"634,49727-0418-02 ",.01)
 ;;49727-0418-02
 ;;9002226.02101,"634,49727-0418-02 ",.02)
 ;;49727-0418-02
 ;;9002226.02101,"634,49727-0418-04 ",.01)
 ;;49727-0418-04
 ;;9002226.02101,"634,49727-0418-04 ",.02)
 ;;49727-0418-04
 ;;9002226.02101,"634,49727-0750-10 ",.01)
 ;;49727-0750-10
 ;;9002226.02101,"634,49727-0750-10 ",.02)
 ;;49727-0750-10
 ;;9002226.02101,"634,49727-0752-10 ",.01)
 ;;49727-0752-10
 ;;9002226.02101,"634,49727-0752-10 ",.02)
 ;;49727-0752-10
 ;;9002226.02101,"634,49727-0762-10 ",.01)
 ;;49727-0762-10
 ;;9002226.02101,"634,49727-0762-10 ",.02)
 ;;49727-0762-10
 ;;9002226.02101,"634,49999-0034-01 ",.01)
 ;;49999-0034-01
 ;;9002226.02101,"634,49999-0034-01 ",.02)
 ;;49999-0034-01
 ;;9002226.02101,"634,49999-0034-07 ",.01)
 ;;49999-0034-07
 ;;9002226.02101,"634,49999-0034-07 ",.02)
 ;;49999-0034-07
 ;;9002226.02101,"634,49999-0034-12 ",.01)
 ;;49999-0034-12
 ;;9002226.02101,"634,49999-0034-12 ",.02)
 ;;49999-0034-12
 ;;9002226.02101,"634,49999-0034-15 ",.01)
 ;;49999-0034-15
 ;;9002226.02101,"634,49999-0034-15 ",.02)
 ;;49999-0034-15
 ;;9002226.02101,"634,49999-0034-20 ",.01)
 ;;49999-0034-20
 ;;9002226.02101,"634,49999-0034-20 ",.02)
 ;;49999-0034-20
 ;;9002226.02101,"634,49999-0034-21 ",.01)
 ;;49999-0034-21
 ;;9002226.02101,"634,49999-0034-21 ",.02)
 ;;49999-0034-21
 ;;9002226.02101,"634,49999-0034-30 ",.01)
 ;;49999-0034-30
 ;;9002226.02101,"634,49999-0034-30 ",.02)
 ;;49999-0034-30
 ;;9002226.02101,"634,49999-0048-30 ",.01)
 ;;49999-0048-30
 ;;9002226.02101,"634,49999-0048-30 ",.02)
 ;;49999-0048-30
 ;;9002226.02101,"634,49999-0065-12 ",.01)
 ;;49999-0065-12
 ;;9002226.02101,"634,49999-0065-12 ",.02)
 ;;49999-0065-12
 ;;9002226.02101,"634,50428-1557-56 ",.01)
 ;;50428-1557-56
 ;;9002226.02101,"634,50428-1557-56 ",.02)
 ;;50428-1557-56
 ;;9002226.02101,"634,50428-2096-50 ",.01)
 ;;50428-2096-50
 ;;9002226.02101,"634,50428-2096-50 ",.02)
 ;;50428-2096-50
 ;;9002226.02101,"634,50428-2325-67 ",.01)
 ;;50428-2325-67
 ;;9002226.02101,"634,50428-2325-67 ",.02)
 ;;50428-2325-67
