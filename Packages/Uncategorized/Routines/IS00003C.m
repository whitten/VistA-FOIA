IS00003C ;Compiled from script 'Generated: X1 IHS 835 IN-I' on DEC 03, 2002
 ;Part 4
 ;Copyright 2002 SAIC
EN S:DO @("@INV@(""NM14"")")=$$PIECE^INHU(.LINE,DELIM,5)
 S:DO @("@INV@(""NM15"")")=$$PIECE^INHU(.LINE,DELIM,6)
 S:DO @("@INV@(""NM16"")")=$$PIECE^INHU(.LINE,DELIM,7)
 S:DO @("@INV@(""NM17"")")=$$PIECE^INHU(.LINE,DELIM,8)
 S:DO @("@INV@(""NM18"")")=$$PIECE^INHU(.LINE,DELIM,9)
 S:DO @("@INV@(""NM19"")")=$$PIECE^INHU(.LINE,DELIM,10)
 Q:MATCH
 D:'INVS MC^INHS
 D GET^INHOU(UIF,0) S LINE=$G(LINE),DO=0
 I 'MATCH,LINE?1"N"1"M"1"1".ANPC S DO=1,MATCH=1
 E  S LCT=LCT-CNT,DO=0
 S:DO @("@INV@(""NM11"")")=$$PIECE^INHU(.LINE,DELIM,2)
 S:DO @("@INV@(""NM12"")")=$$PIECE^INHU(.LINE,DELIM,3)
 S:DO @("@INV@(""NM13"")")=$$PIECE^INHU(.LINE,DELIM,4)
 S:DO @("@INV@(""NM14"")")=$$PIECE^INHU(.LINE,DELIM,5)
 S:DO @("@INV@(""NM15"")")=$$PIECE^INHU(.LINE,DELIM,6)
 S:DO @("@INV@(""NM16"")")=$$PIECE^INHU(.LINE,DELIM,7)
 S:DO @("@INV@(""NM17"")")=$$PIECE^INHU(.LINE,DELIM,8)
 S:DO @("@INV@(""NM18"")")=$$PIECE^INHU(.LINE,DELIM,9)
 S:DO @("@INV@(""NM19"")")=$$PIECE^INHU(.LINE,DELIM,10)
 Q:MATCH
 D:'INVS MC^INHS
 D GET^INHOU(UIF,0) S LINE=$G(LINE),DO=0
 I 'MATCH,LINE?1"N"1"M"1"1".ANPC S DO=1,MATCH=1
 E  S LCT=LCT-CNT,DO=0
 S:DO @("@INV@(""NM11"")")=$$PIECE^INHU(.LINE,DELIM,2)
 S:DO @("@INV@(""NM12"")")=$$PIECE^INHU(.LINE,DELIM,3)
 S:DO @("@INV@(""NM13"")")=$$PIECE^INHU(.LINE,DELIM,4)
 S:DO @("@INV@(""NM14"")")=$$PIECE^INHU(.LINE,DELIM,5)
 S:DO @("@INV@(""NM15"")")=$$PIECE^INHU(.LINE,DELIM,6)
 S:DO @("@INV@(""NM16"")")=$$PIECE^INHU(.LINE,DELIM,7)
 S:DO @("@INV@(""NM17"")")=$$PIECE^INHU(.LINE,DELIM,8)
 S:DO @("@INV@(""NM18"")")=$$PIECE^INHU(.LINE,DELIM,9)
 S:DO @("@INV@(""NM19"")")=$$PIECE^INHU(.LINE,DELIM,10)
 Q:MATCH
 D:'INVS MC^INHS
 D GET^INHOU(UIF,0) S LINE=$G(LINE),DO=0
 I 'MATCH,LINE?1"N"1"M"1"1".ANPC S DO=1,MATCH=1
 E  S LCT=LCT-CNT,DO=0
 S:DO @("@INV@(""NM11"")")=$$PIECE^INHU(.LINE,DELIM,2)
 S:DO @("@INV@(""NM12"")")=$$PIECE^INHU(.LINE,DELIM,3)
 S:DO @("@INV@(""NM13"")")=$$PIECE^INHU(.LINE,DELIM,4)
 S:DO @("@INV@(""NM14"")")=$$PIECE^INHU(.LINE,DELIM,5)
 S:DO @("@INV@(""NM15"")")=$$PIECE^INHU(.LINE,DELIM,6)
 S:DO @("@INV@(""NM16"")")=$$PIECE^INHU(.LINE,DELIM,7)
 S:DO @("@INV@(""NM17"")")=$$PIECE^INHU(.LINE,DELIM,8)
 S:DO @("@INV@(""NM18"")")=$$PIECE^INHU(.LINE,DELIM,9)
 S:DO @("@INV@(""NM19"")")=$$PIECE^INHU(.LINE,DELIM,10)
 Q:MATCH
 D:'INVS MC^INHS
 D GET^INHOU(UIF,0) S LINE=$G(LINE),DO=0
 I 'MATCH,LINE?1"N"1"M"1"1".ANPC S DO=1,MATCH=1
 E  S LCT=LCT-CNT,DO=0
 S:DO @("@INV@(""NM11"")")=$$PIECE^INHU(.LINE,DELIM,2)
 S:DO @("@INV@(""NM12"")")=$$PIECE^INHU(.LINE,DELIM,3)
 S:DO @("@INV@(""NM13"")")=$$PIECE^INHU(.LINE,DELIM,4)
 S:DO @("@INV@(""NM14"")")=$$PIECE^INHU(.LINE,DELIM,5)
 S:DO @("@INV@(""NM15"")")=$$PIECE^INHU(.LINE,DELIM,6)
 S:DO @("@INV@(""NM16"")")=$$PIECE^INHU(.LINE,DELIM,7)
 S:DO @("@INV@(""NM17"")")=$$PIECE^INHU(.LINE,DELIM,8)
 S:DO @("@INV@(""NM18"")")=$$PIECE^INHU(.LINE,DELIM,9)
 S:DO @("@INV@(""NM19"")")=$$PIECE^INHU(.LINE,DELIM,10)
 Q:MATCH
 D:'INVS MC^INHS
 D GET^INHOU(UIF,0) S LINE=$G(LINE),DO=0
 I 'MATCH,LINE?1"N"1"M"1"1".ANPC S DO=1,MATCH=1
 E  S LCT=LCT-CNT,DO=0
 S:DO @("@INV@(""NM11"")")=$$PIECE^INHU(.LINE,DELIM,2)
 S:DO @("@INV@(""NM12"")")=$$PIECE^INHU(.LINE,DELIM,3)
 S:DO @("@INV@(""NM13"")")=$$PIECE^INHU(.LINE,DELIM,4)
 S:DO @("@INV@(""NM14"")")=$$PIECE^INHU(.LINE,DELIM,5)
 S:DO @("@INV@(""NM15"")")=$$PIECE^INHU(.LINE,DELIM,6)
 S:DO @("@INV@(""NM16"")")=$$PIECE^INHU(.LINE,DELIM,7)
 S:DO @("@INV@(""NM17"")")=$$PIECE^INHU(.LINE,DELIM,8)
 S:DO @("@INV@(""NM18"")")=$$PIECE^INHU(.LINE,DELIM,9)
 S:DO @("@INV@(""NM19"")")=$$PIECE^INHU(.LINE,DELIM,10)
 Q:MATCH
 D:'INVS MC^INHS
 D GET^INHOU(UIF,0) S LINE=$G(LINE),DO=0
 I 'MATCH,LINE?1"M"1"I"1"A".ANPC S DO=1,MATCH=1
 E  S LCT=LCT-CNT,DO=0
 S:DO @("@INV@(""MIA1"")")=$$PIECE^INHU(.LINE,DELIM,2)
 S:DO @("@INV@(""MIA2"")")=$$PIECE^INHU(.LINE,DELIM,3)
 S:DO @("@INV@(""MIA3"")")=$$PIECE^INHU(.LINE,DELIM,4)
 S:DO @("@INV@(""MIA4"")")=$$PIECE^INHU(.LINE,DELIM,5)
 S:DO @("@INV@(""MIA5"")")=$$PIECE^INHU(.LINE,DELIM,6)
 S:DO @("@INV@(""MIA6"")")=$$PIECE^INHU(.LINE,DELIM,7)
 S:DO @("@INV@(""MIA7"")")=$$PIECE^INHU(.LINE,DELIM,8)
 S:DO @("@INV@(""MIA8"")")=$$PIECE^INHU(.LINE,DELIM,9)
 S:DO @("@INV@(""MIA9"")")=$$PIECE^INHU(.LINE,DELIM,10)
 S:DO @("@INV@(""MIA10"")")=$$PIECE^INHU(.LINE,DELIM,11)
 S:DO @("@INV@(""MIA11"")")=$$PIECE^INHU(.LINE,DELIM,12)
9 G EN^IS00003D
