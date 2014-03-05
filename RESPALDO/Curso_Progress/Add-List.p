FUNCTION Add-List Char
 (Lst as char,   /* Mdo => 1 = Prefijo con repeticion */
  Txt as char,   /*        2 = Sufijo  con repeticion */
  Sep as char,   /*        3 = Prefijo sin repeticion */
  Mdo as int):   /*        4 = Sufijo  sin repeticion */ 

 Def var PreF as log.
 Def var Step  as int.

 Assign PreF = (Mdo = 1 or Mdo = 3).

 Do Step = 1 to 5:
  Case Step:
   when 1 then if Mdo le 2                 then Step = 3.
   when 2 then if Txt = ""                 then Step = 3.
   when 3 then if Lookup(Txt,Lst,Sep) ne 0 then Step = 5.
   when 4 then if Lst = ""                 then Sep = "".
   when 5 then Case PreF:
                when yes then Lst = Txt + Sep + Lst.
                when no  then Lst = Lst + Sep + Txt.
               end.
  end.
 end.
 
 Return Lst.
End.
