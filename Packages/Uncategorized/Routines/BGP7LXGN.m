BGP7LXGN ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON AUG 28, 2006 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"624,58016-0821-10 ",.02)
 ;;58016-0821-10
 ;;9002226.02101,"624,58016-0821-12 ",.01)
 ;;58016-0821-12
 ;;9002226.02101,"624,58016-0821-12 ",.02)
 ;;58016-0821-12
 ;;9002226.02101,"624,58016-0821-14 ",.01)
 ;;58016-0821-14
 ;;9002226.02101,"624,58016-0821-14 ",.02)
 ;;58016-0821-14
 ;;9002226.02101,"624,58016-0821-15 ",.01)
 ;;58016-0821-15
 ;;9002226.02101,"624,58016-0821-15 ",.02)
 ;;58016-0821-15
 ;;9002226.02101,"624,58016-0821-20 ",.01)
 ;;58016-0821-20
 ;;9002226.02101,"624,58016-0821-20 ",.02)
 ;;58016-0821-20
 ;;9002226.02101,"624,58016-0821-21 ",.01)
 ;;58016-0821-21
 ;;9002226.02101,"624,58016-0821-21 ",.02)
 ;;58016-0821-21
 ;;9002226.02101,"624,58016-0821-24 ",.01)
 ;;58016-0821-24
 ;;9002226.02101,"624,58016-0821-24 ",.02)
 ;;58016-0821-24
 ;;9002226.02101,"624,58016-0821-25 ",.01)
 ;;58016-0821-25
 ;;9002226.02101,"624,58016-0821-25 ",.02)
 ;;58016-0821-25
 ;;9002226.02101,"624,58016-0821-28 ",.01)
 ;;58016-0821-28
 ;;9002226.02101,"624,58016-0821-28 ",.02)
 ;;58016-0821-28
 ;;9002226.02101,"624,58016-0821-30 ",.01)
 ;;58016-0821-30
 ;;9002226.02101,"624,58016-0821-30 ",.02)
 ;;58016-0821-30
 ;;9002226.02101,"624,58016-0821-40 ",.01)
 ;;58016-0821-40
 ;;9002226.02101,"624,58016-0821-40 ",.02)
 ;;58016-0821-40
 ;;9002226.02101,"624,58016-0821-50 ",.01)
 ;;58016-0821-50
 ;;9002226.02101,"624,58016-0821-50 ",.02)
 ;;58016-0821-50
 ;;9002226.02101,"624,58016-0821-60 ",.01)
 ;;58016-0821-60
 ;;9002226.02101,"624,58016-0821-60 ",.02)
 ;;58016-0821-60
 ;;9002226.02101,"624,58016-0822-00 ",.01)
 ;;58016-0822-00
 ;;9002226.02101,"624,58016-0822-00 ",.02)
 ;;58016-0822-00
 ;;9002226.02101,"624,58016-0822-12 ",.01)
 ;;58016-0822-12
 ;;9002226.02101,"624,58016-0822-12 ",.02)
 ;;58016-0822-12
 ;;9002226.02101,"624,58016-0822-15 ",.01)
 ;;58016-0822-15
 ;;9002226.02101,"624,58016-0822-15 ",.02)
 ;;58016-0822-15
 ;;9002226.02101,"624,58016-0822-20 ",.01)
 ;;58016-0822-20
 ;;9002226.02101,"624,58016-0822-20 ",.02)
 ;;58016-0822-20
 ;;9002226.02101,"624,58016-0822-30 ",.01)
 ;;58016-0822-30
 ;;9002226.02101,"624,58016-0822-30 ",.02)
 ;;58016-0822-30
 ;;9002226.02101,"624,58016-0996-00 ",.01)
 ;;58016-0996-00
 ;;9002226.02101,"624,58016-0996-00 ",.02)
 ;;58016-0996-00
 ;;9002226.02101,"624,58016-0996-30 ",.01)
 ;;58016-0996-30
 ;;9002226.02101,"624,58016-0996-30 ",.02)
 ;;58016-0996-30
 ;;9002226.02101,"624,58016-0996-60 ",.01)
 ;;58016-0996-60
 ;;9002226.02101,"624,58016-0996-60 ",.02)
 ;;58016-0996-60
 ;;9002226.02101,"624,58016-0996-90 ",.01)
 ;;58016-0996-90
 ;;9002226.02101,"624,58016-0996-90 ",.02)
 ;;58016-0996-90
 ;;9002226.02101,"624,58521-0162-10 ",.01)
 ;;58521-0162-10
 ;;9002226.02101,"624,58521-0162-10 ",.02)
 ;;58521-0162-10
 ;;9002226.02101,"624,58521-0754-10 ",.01)
 ;;58521-0754-10
 ;;9002226.02101,"624,58521-0754-10 ",.02)
 ;;58521-0754-10
 ;;9002226.02101,"624,58729-0453-01 ",.01)
 ;;58729-0453-01
 ;;9002226.02101,"624,58729-0453-01 ",.02)
 ;;58729-0453-01
 ;;9002226.02101,"624,58729-0453-37 ",.01)
 ;;58729-0453-37
 ;;9002226.02101,"624,58729-0453-37 ",.02)
 ;;58729-0453-37
 ;;9002226.02101,"624,58729-0512-43 ",.01)
 ;;58729-0512-43
 ;;9002226.02101,"624,58729-0512-43 ",.02)
 ;;58729-0512-43
 ;;9002226.02101,"624,58864-0734-20 ",.01)
 ;;58864-0734-20
 ;;9002226.02101,"624,58864-0734-20 ",.02)
 ;;58864-0734-20
 ;;9002226.02101,"624,58864-0797-30 ",.01)
 ;;58864-0797-30
 ;;9002226.02101,"624,58864-0797-30 ",.02)
 ;;58864-0797-30
 ;;9002226.02101,"624,59075-0650-20 ",.01)
 ;;59075-0650-20
 ;;9002226.02101,"624,59075-0650-20 ",.02)
 ;;59075-0650-20
 ;;9002226.02101,"624,59075-0651-20 ",.01)
 ;;59075-0651-20
 ;;9002226.02101,"624,59075-0651-20 ",.02)
 ;;59075-0651-20
 ;;9002226.02101,"624,59075-0652-20 ",.01)
 ;;59075-0652-20
 ;;9002226.02101,"624,59075-0652-20 ",.02)
 ;;59075-0652-20
 ;;9002226.02101,"624,59075-0653-20 ",.01)
 ;;59075-0653-20
 ;;9002226.02101,"624,59075-0653-20 ",.02)
 ;;59075-0653-20
 ;;9002226.02101,"624,59075-0654-20 ",.01)
 ;;59075-0654-20
 ;;9002226.02101,"624,59075-0654-20 ",.02)
 ;;59075-0654-20
 ;;9002226.02101,"624,59075-0655-20 ",.01)
 ;;59075-0655-20
 ;;9002226.02101,"624,59075-0655-20 ",.02)
 ;;59075-0655-20
 ;;9002226.02101,"624,60346-0042-12 ",.01)
 ;;60346-0042-12
 ;;9002226.02101,"624,60346-0042-12 ",.02)
 ;;60346-0042-12
 ;;9002226.02101,"624,60346-0042-15 ",.01)
 ;;60346-0042-15
 ;;9002226.02101,"624,60346-0042-15 ",.02)
 ;;60346-0042-15
 ;;9002226.02101,"624,60346-0042-20 ",.01)
 ;;60346-0042-20
 ;;9002226.02101,"624,60346-0042-20 ",.02)
 ;;60346-0042-20
 ;;9002226.02101,"624,60346-0042-30 ",.01)
 ;;60346-0042-30
 ;;9002226.02101,"624,60346-0042-30 ",.02)
 ;;60346-0042-30
 ;;9002226.02101,"624,60346-0042-40 ",.01)
 ;;60346-0042-40
 ;;9002226.02101,"624,60346-0042-40 ",.02)
 ;;60346-0042-40
 ;;9002226.02101,"624,60346-0042-44 ",.01)
 ;;60346-0042-44
 ;;9002226.02101,"624,60346-0042-44 ",.02)
 ;;60346-0042-44
 ;;9002226.02101,"624,60346-0042-90 ",.01)
 ;;60346-0042-90
 ;;9002226.02101,"624,60346-0042-90 ",.02)
 ;;60346-0042-90
 ;;9002226.02101,"624,60346-0264-04 ",.01)
 ;;60346-0264-04
 ;;9002226.02101,"624,60346-0264-04 ",.02)
 ;;60346-0264-04
 ;;9002226.02101,"624,60346-0264-88 ",.01)
 ;;60346-0264-88
 ;;9002226.02101,"624,60346-0264-88 ",.02)
 ;;60346-0264-88
 ;;9002226.02101,"624,60346-0486-30 ",.01)
 ;;60346-0486-30
 ;;9002226.02101,"624,60346-0486-30 ",.02)
 ;;60346-0486-30
 ;;9002226.02101,"624,60346-0486-90 ",.01)
 ;;60346-0486-90
 ;;9002226.02101,"624,60346-0486-90 ",.02)
 ;;60346-0486-90
 ;;9002226.02101,"624,60346-0639-30 ",.01)
 ;;60346-0639-30
 ;;9002226.02101,"624,60346-0639-30 ",.02)
 ;;60346-0639-30
 ;;9002226.02101,"624,60346-0857-08 ",.01)
 ;;60346-0857-08
 ;;9002226.02101,"624,60346-0857-08 ",.02)
 ;;60346-0857-08
 ;;9002226.02101,"624,60346-0857-14 ",.01)
 ;;60346-0857-14
 ;;9002226.02101,"624,60346-0857-14 ",.02)
 ;;60346-0857-14
 ;;9002226.02101,"624,60809-0510-55 ",.01)
 ;;60809-0510-55
 ;;9002226.02101,"624,60809-0510-55 ",.02)
 ;;60809-0510-55
 ;;9002226.02101,"624,60809-0510-72 ",.01)
 ;;60809-0510-72
 ;;9002226.02101,"624,60809-0510-72 ",.02)
 ;;60809-0510-72
 ;;9002226.02101,"624,60809-0511-55 ",.01)
 ;;60809-0511-55
 ;;9002226.02101,"624,60809-0511-55 ",.02)
 ;;60809-0511-55
 ;;9002226.02101,"624,60809-0511-72 ",.01)
 ;;60809-0511-72
 ;;9002226.02101,"624,60809-0511-72 ",.02)
 ;;60809-0511-72
 ;;9002226.02101,"624,61392-0382-32 ",.01)
 ;;61392-0382-32
 ;;9002226.02101,"624,61392-0382-32 ",.02)
 ;;61392-0382-32
 ;;9002226.02101,"624,61392-0382-45 ",.01)
 ;;61392-0382-45
 ;;9002226.02101,"624,61392-0382-45 ",.02)
 ;;61392-0382-45
 ;;9002226.02101,"624,61392-0382-51 ",.01)
 ;;61392-0382-51
 ;;9002226.02101,"624,61392-0382-51 ",.02)
 ;;61392-0382-51
 ;;9002226.02101,"624,61392-0382-54 ",.01)
 ;;61392-0382-54
 ;;9002226.02101,"624,61392-0382-54 ",.02)
 ;;61392-0382-54
 ;;9002226.02101,"624,61392-0382-56 ",.01)
 ;;61392-0382-56
 ;;9002226.02101,"624,61392-0382-56 ",.02)
 ;;61392-0382-56
 ;;9002226.02101,"624,61392-0382-91 ",.01)
 ;;61392-0382-91
 ;;9002226.02101,"624,61392-0382-91 ",.02)
 ;;61392-0382-91
 ;;9002226.02101,"624,61392-0391-45 ",.01)
 ;;61392-0391-45
 ;;9002226.02101,"624,61392-0391-45 ",.02)
 ;;61392-0391-45
 ;;9002226.02101,"624,61392-0391-54 ",.01)
 ;;61392-0391-54
 ;;9002226.02101,"624,61392-0391-54 ",.02)
 ;;61392-0391-54
 ;;9002226.02101,"624,61392-0391-56 ",.01)
 ;;61392-0391-56
 ;;9002226.02101,"624,61392-0391-56 ",.02)
 ;;61392-0391-56
 ;;9002226.02101,"624,61392-0391-91 ",.01)
 ;;61392-0391-91
 ;;9002226.02101,"624,61392-0391-91 ",.02)
 ;;61392-0391-91
 ;;9002226.02101,"624,61392-0392-32 ",.01)
 ;;61392-0392-32
 ;;9002226.02101,"624,61392-0392-32 ",.02)
 ;;61392-0392-32
 ;;9002226.02101,"624,61392-0392-45 ",.01)
 ;;61392-0392-45
 ;;9002226.02101,"624,61392-0392-45 ",.02)
 ;;61392-0392-45
 ;;9002226.02101,"624,61392-0392-51 ",.01)
 ;;61392-0392-51
 ;;9002226.02101,"624,61392-0392-51 ",.02)
 ;;61392-0392-51
 ;;9002226.02101,"624,61392-0392-54 ",.01)
 ;;61392-0392-54
 ;;9002226.02101,"624,61392-0392-54 ",.02)
 ;;61392-0392-54
 ;;9002226.02101,"624,61392-0392-90 ",.01)
 ;;61392-0392-90
 ;;9002226.02101,"624,61392-0392-90 ",.02)
 ;;61392-0392-90
 ;;9002226.02101,"624,61392-0392-91 ",.01)
 ;;61392-0392-91
 ;;9002226.02101,"624,61392-0392-91 ",.02)
 ;;61392-0392-91
 ;;9002226.02101,"624,61392-0721-30 ",.01)
 ;;61392-0721-30
 ;;9002226.02101,"624,61392-0721-30 ",.02)
 ;;61392-0721-30
 ;;9002226.02101,"624,61392-0721-31 ",.01)
 ;;61392-0721-31
 ;;9002226.02101,"624,61392-0721-31 ",.02)
 ;;61392-0721-31
 ;;9002226.02101,"624,61392-0721-32 ",.01)
 ;;61392-0721-32
 ;;9002226.02101,"624,61392-0721-32 ",.02)
 ;;61392-0721-32
 ;;9002226.02101,"624,61392-0721-39 ",.01)
 ;;61392-0721-39
 ;;9002226.02101,"624,61392-0721-39 ",.02)
 ;;61392-0721-39
 ;;9002226.02101,"624,61392-0721-45 ",.01)
 ;;61392-0721-45
 ;;9002226.02101,"624,61392-0721-45 ",.02)
 ;;61392-0721-45
 ;;9002226.02101,"624,61392-0721-51 ",.01)
 ;;61392-0721-51
 ;;9002226.02101,"624,61392-0721-51 ",.02)
 ;;61392-0721-51
 ;;9002226.02101,"624,61392-0721-54 ",.01)
 ;;61392-0721-54
 ;;9002226.02101,"624,61392-0721-54 ",.02)
 ;;61392-0721-54
 ;;9002226.02101,"624,61392-0721-60 ",.01)
 ;;61392-0721-60
 ;;9002226.02101,"624,61392-0721-60 ",.02)
 ;;61392-0721-60
 ;;9002226.02101,"624,61392-0721-90 ",.01)
 ;;61392-0721-90
 ;;9002226.02101,"624,61392-0721-90 ",.02)
 ;;61392-0721-90
 ;;9002226.02101,"624,61392-0721-91 ",.01)
 ;;61392-0721-91
 ;;9002226.02101,"624,61392-0721-91 ",.02)
 ;;61392-0721-91
 ;;9002226.02101,"624,62584-0896-01 ",.01)
 ;;62584-0896-01
 ;;9002226.02101,"624,62584-0896-01 ",.02)
 ;;62584-0896-01
 ;;9002226.02101,"624,63874-0214-01 ",.01)
 ;;63874-0214-01
 ;;9002226.02101,"624,63874-0214-01 ",.02)
 ;;63874-0214-01
 ;;9002226.02101,"624,63874-0214-30 ",.01)
 ;;63874-0214-30
 ;;9002226.02101,"624,63874-0214-30 ",.02)
 ;;63874-0214-30
 ;;9002226.02101,"624,63874-0226-01 ",.01)
 ;;63874-0226-01
 ;;9002226.02101,"624,63874-0226-01 ",.02)
 ;;63874-0226-01
 ;;9002226.02101,"624,63874-0226-02 ",.01)
 ;;63874-0226-02
 ;;9002226.02101,"624,63874-0226-02 ",.02)
 ;;63874-0226-02
 ;;9002226.02101,"624,63874-0226-04 ",.01)
 ;;63874-0226-04
 ;;9002226.02101,"624,63874-0226-04 ",.02)
 ;;63874-0226-04
 ;;9002226.02101,"624,63874-0226-10 ",.01)
 ;;63874-0226-10
 ;;9002226.02101,"624,63874-0226-10 ",.02)
 ;;63874-0226-10
 ;;9002226.02101,"624,63874-0226-12 ",.01)
 ;;63874-0226-12
 ;;9002226.02101,"624,63874-0226-12 ",.02)
 ;;63874-0226-12
 ;;9002226.02101,"624,63874-0226-15 ",.01)
 ;;63874-0226-15
 ;;9002226.02101,"624,63874-0226-15 ",.02)
 ;;63874-0226-15
 ;;9002226.02101,"624,63874-0226-20 ",.01)
 ;;63874-0226-20
 ;;9002226.02101,"624,63874-0226-20 ",.02)
 ;;63874-0226-20
 ;;9002226.02101,"624,63874-0226-28 ",.01)
 ;;63874-0226-28
 ;;9002226.02101,"624,63874-0226-28 ",.02)
 ;;63874-0226-28
 ;;9002226.02101,"624,63874-0226-30 ",.01)
 ;;63874-0226-30
 ;;9002226.02101,"624,63874-0226-30 ",.02)
 ;;63874-0226-30
 ;;9002226.02101,"624,63874-0226-60 ",.01)
 ;;63874-0226-60
 ;;9002226.02101,"624,63874-0226-60 ",.02)
 ;;63874-0226-60
 ;;9002226.02101,"624,63874-0226-90 ",.01)
 ;;63874-0226-90
 ;;9002226.02101,"624,63874-0226-90 ",.02)
 ;;63874-0226-90
 ;;9002226.02101,"624,63874-0227-01 ",.01)
 ;;63874-0227-01
 ;;9002226.02101,"624,63874-0227-01 ",.02)
 ;;63874-0227-01
 ;;9002226.02101,"624,63874-0227-02 ",.01)
 ;;63874-0227-02
 ;;9002226.02101,"624,63874-0227-02 ",.02)
 ;;63874-0227-02
 ;;9002226.02101,"624,63874-0227-04 ",.01)
 ;;63874-0227-04
 ;;9002226.02101,"624,63874-0227-04 ",.02)
 ;;63874-0227-04
 ;;9002226.02101,"624,63874-0227-05 ",.01)
 ;;63874-0227-05
 ;;9002226.02101,"624,63874-0227-05 ",.02)
 ;;63874-0227-05
 ;;9002226.02101,"624,63874-0227-06 ",.01)
 ;;63874-0227-06
 ;;9002226.02101,"624,63874-0227-06 ",.02)
 ;;63874-0227-06
 ;;9002226.02101,"624,63874-0227-08 ",.01)
 ;;63874-0227-08
 ;;9002226.02101,"624,63874-0227-08 ",.02)
 ;;63874-0227-08
 ;;9002226.02101,"624,63874-0227-09 ",.01)
 ;;63874-0227-09
 ;;9002226.02101,"624,63874-0227-09 ",.02)
 ;;63874-0227-09
 ;;9002226.02101,"624,63874-0227-10 ",.01)
 ;;63874-0227-10
 ;;9002226.02101,"624,63874-0227-10 ",.02)
 ;;63874-0227-10
 ;;9002226.02101,"624,63874-0227-12 ",.01)
 ;;63874-0227-12
 ;;9002226.02101,"624,63874-0227-12 ",.02)
 ;;63874-0227-12
 ;;9002226.02101,"624,63874-0227-14 ",.01)
 ;;63874-0227-14
 ;;9002226.02101,"624,63874-0227-14 ",.02)
 ;;63874-0227-14
 ;;9002226.02101,"624,63874-0227-15 ",.01)
 ;;63874-0227-15
 ;;9002226.02101,"624,63874-0227-15 ",.02)
 ;;63874-0227-15
 ;;9002226.02101,"624,63874-0227-16 ",.01)
 ;;63874-0227-16
 ;;9002226.02101,"624,63874-0227-16 ",.02)
 ;;63874-0227-16
 ;;9002226.02101,"624,63874-0227-18 ",.01)
 ;;63874-0227-18
 ;;9002226.02101,"624,63874-0227-18 ",.02)
 ;;63874-0227-18
 ;;9002226.02101,"624,63874-0227-20 ",.01)
 ;;63874-0227-20
 ;;9002226.02101,"624,63874-0227-20 ",.02)
 ;;63874-0227-20
 ;;9002226.02101,"624,63874-0227-21 ",.01)
 ;;63874-0227-21
 ;;9002226.02101,"624,63874-0227-21 ",.02)
 ;;63874-0227-21
 ;;9002226.02101,"624,63874-0227-24 ",.01)
 ;;63874-0227-24
 ;;9002226.02101,"624,63874-0227-24 ",.02)
 ;;63874-0227-24
 ;;9002226.02101,"624,63874-0227-28 ",.01)
 ;;63874-0227-28
 ;;9002226.02101,"624,63874-0227-28 ",.02)
 ;;63874-0227-28
 ;;9002226.02101,"624,63874-0227-30 ",.01)
 ;;63874-0227-30
 ;;9002226.02101,"624,63874-0227-30 ",.02)
 ;;63874-0227-30
 ;;9002226.02101,"624,63874-0227-36 ",.01)
 ;;63874-0227-36
