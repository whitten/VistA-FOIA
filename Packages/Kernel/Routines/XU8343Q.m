XU8343Q ;BPOIFO/DW-UPDATE NEW-STYLE XREF FOR XU*8*343;9:19 AM  22 Jun 2004
 ;;8.0;KERNEL;**343**; Jul 10, 1995;
 ;
NEWXR ;Update the new-style "B" cross-reference of file #200
 D BMES^XPDUTL("Updating the B cross-reference...")
 ;
 N XUXR,XURES,XUOUT
 S XUXR("FILE")=200
 S XUXR("NAME")="B"
 S XUXR("TYPE")="R"
 S XUXR("USE")="LS"
 S XUXR("EXECUTION")="F"
 S XUXR("ACTIVITY")="IR"
 S XUXR("SHORT DESCR")="Regular ""B"" index on .01 field with transform, length 35."
 S XUXR("VAL",1)=.01
 S XUXR("VAL",1,"SUBSCRIPT")=1
 S XUXR("VAL",1,"LENGTH")=35
 S XUXR("VAL",1,"COLLATION")="F"
 S XUXR("VAL",1,"XFORM FOR LOOKUP")="N XLFNAME S XLFNAME=X S X=$$FORMAT^XLFNAME7(.XLFNAME,3,35,,0)"
 D CREIXN^DDMOD(.XUXR,"SW",.XURES,"XUOUT")
 Q
