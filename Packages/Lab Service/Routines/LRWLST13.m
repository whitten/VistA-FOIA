LRWLST13 ; IHS/DIR/FJE - ACCESSION SETUP 1/7/87 12:12 PM ;
 ;;5.2;LR;**1013**;JUL 15, 2002
 ;
 ;;5.2;LAB SERVICE;;Sep 27, 1994
 S LRZO="^LRO(69.1,1,1,",LRZ1="69.11P",LRZ3=LRPR,LRZB=LRPR D Z^LRWU S:'$D(^LRO(69.1,1,1,LRPR,0)) ^(0)=LRPR
 S LRZO="^LRO(69.1,1,1,"_LRPR_",1,",LRZ1="69.12P",X=LRLLOC D A^LRWU S (LRZ3,LRILOC)=X1,LRZB=LRLLOC D Z^LRWU S:'$D(^LRO(69.1,1,1,LRPR,1,LRILOC,0)) ^(0)=LRLLOC
 S LRZO="^LRO(69.1,1,1,"_LRPR_",1,"_LRILOC_",1,",LRZ1="69.13",X=LRRB D A^LRWU S (LRZ3,LRIRB)=X1,LRZB=LRRB D Z^LRWU S:'$D(^LRO(69.1,1,1,LRPR,1,LRILOC,1,LRIRB,0)) ^(0)=LRRB
 S LRZO="^LRO(69.1,1,1,"_LRPR_",1,"_LRILOC_",1,"_LRIRB_",1,",LRZ1="69.14P",LRZ3=LRDFN,LRZB=LRDFN D Z^LRWU S:'$D(^LRO(69.1,1,1,LRPR,1,LRILOC,1,LRIRB,1,LRDFN,0)) ^(0)=LRDFN
 S LRZO="^LRO(69.1,1,1,"_LRPR_",1,"_LRILOC_",1,"_LRIRB_",1,"_LRDFN_",1,",LRZ1="69.15",LRZ3=LRSN,LRZB=LRSN D Z^LRWU S:'$D(^LRO(69.1,1,1,LRPR,1,LRILOC,1,LRIRB,1,LRDFN,1,LRSN,0)) ^(0)=LRTJ_"^"_LRAD
 S LRZO="^LRO(69.1,1,1,"_LRPR_",1,"_LRILOC_",1,"_LRIRB_",1,"_LRDFN_",1,"_LRSN_",1,",LRZ1="69.16P",LRZ3=LRAA,LRZB=LRAA D Z^LRWU S:'$D(^LRO(69.1,1,1,LRPR,1,LRILOC,1,LRIRB,1,LRDFN,1,LRSN,1,LRAA,0)) ^(0)=LRAA
 S LRZO="^LRO(69.1,1,1,"_LRPR_",1,"_LRILOC_",1,"_LRIRB_",1,"_LRDFN_",1,"_LRSN_",1,"_LRAA_",1,",LRZ1="69.17",LRZ3=LRAN,LRZB=LRAN D Z^LRWU S:'$D(^LRO(69.1,1,1,LRPR,1,LRILOC,1,LRIRB,1,LRDFN,1,LRSN,1,LRAA,1,LRAN,0)) ^(0)=LRAN
