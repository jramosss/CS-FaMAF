.text
.org 0x0000

ADD X26, XZR, XZR
ADD X25, XZR, X15
ADD XZR, XZR, XZR
ADD XZR, XZR, XZR

subs:
  STUR X25, [X26,#0]                         
  ADD X26,X26,X8
  SUBS X25, X25, X5
  B.EQ adds            
  ADD XZR, XZR, XZR
  ADD XZR, XZR, XZR
  ADD XZR, XZR, XZR
  CBZ XZR, subs
  ADD XZR, XZR, XZR
  ADD XZR, XZR, XZR
  ADD XZR, XZR, XZR

        
adds: 
	CBZ XZR, adds	

        
       
 
