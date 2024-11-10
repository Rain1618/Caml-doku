type ('r, 'c, 's) position = Position

type r1
type r2
type r3
type r4

type c1
type c2
type c3
type c4

type s1
type s2
type s3
type s4

type a = (r1, c1, s1) position
type b = (r2, c1, s2) position
type c = (r3, c1, s1) position
type d = (r4, c1, s2) position

type e = (r1, c2, s3) position
type f = (r2, c2, s4) position
type g = (r3, c2, s3) position
type h = (r4, c2, s4) position

type i = (r1, c3, s1) position
type j = (r2, c3, s2) position
type k = (r3, c3, s1) position
type l = (r4, c3, s2) position

type m = (r1, c4, s3) position
type n = (r2, c4, s4) position
type o = (r3, c4, s3) position
type p = (r4, c4, s4) position

type 'fields row constraint 'fields = < a : 'a; b : 'b; c : 'c; d : 'd >
type 'fields column constraint 'fields = < a : 'a; b : 'b; c : 'c; d : 'd >
type 'fields square constraint 'fields = < a : 'a; b : 'b; c : 'c; d : 'd >

type ('position, 'row, 'column, 'square) symbol =
  | A : (('r, 'c, 's) position, 
          < a : 'r; .. > row,
          < a : 'c; .. > column,
          < a : 's; .. > square) 
          symbol
  | B : (('r, 'c, 's) position, 
        < b : 'r; .. > row,
        < b : 'c; .. > column,
        < b : 's; .. > square) 
        symbol
  | C : (('r, 'c, 's) position, 
        < c : 'r; .. > row,
        < c : 'c; .. > column,
        < c : 's; .. > square) 
        symbol
  | D : (('r, 'c, 's) position, 
        < d : 'r; .. > row,
        < d : 'c; .. > column,
        < d : 's; .. > square) 
        symbol

(*  a b c d 
    e f g h
    i j k l 
    m n o p
*)


(*
type grid = 
  Grid :
    (a, 'row1, 'column1, 'square1) symbol * 
    (b, 'row1, 'column2, 'square1) symbol * 
    (c, 'row1, 'column3, 'square2) symbol * 
    (d, 'row1, 'column4, 'square2) symbol *

    (e, 'row2, 'column1, 'square1) symbol * 
    (f, 'row2, 'column2, 'square1) symbol * 
    (g, 'row2, 'column3, 'square2) symbol * 
    (h, 'row2, 'column4, 'square2) symbol *

    (i, 'row3, 'column1, 'square3) symbol * 
    (j, 'row3, 'column2, 'square3) symbol * 
    (k, 'row3, 'column3, 'square4) symbol * 
    (l, 'row3, 'column4, 'square4) symbol *

    (m, 'row4, 'column1, 'square3) symbol * 
    (n, 'row4, 'column2, 'square3) symbol * 
    (o, 'row4, 'column3, 'square4) symbol * 
    (p, 'row4, 'column4, 'square4) symbol 
    
    -> solution (* error here *)


let test x = match x with
  | Grid (* error here *)
    (A, _, C, _,
    _, D, _, B,
    _, A, D, _,
    D, _, B, _) -> .
  | _ -> ()
*)


