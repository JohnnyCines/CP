module BTree

open Cp

// (1) Datatype definition -----------------------------------------------------
type BTree<'a> =  Empty | Node of 'a * (BTree<'a> * BTree<'a>)

let inLTree x = either (konst Empty) Node x

let outLTree x = match x with
        |Empty -> Left ()
        |Node (a,(t1,t2)) -> Right(a,(t1,t2))

// (2) Ana + cata + hylo -------------------------------------------------------

let baseBTree f g = id -|- (f >< (g >< g))

let recBTree f = baseBTree id g         // that is:  id -|- (f >< f)

let rec cataBTree a = g << (recBTree (cataBTree g)) << outBTree

let rec anaBTree f = inBTree << (recBTree (anaBTree g) ) << g

let hyloBTree a c = cataBTree h << anaBTree g

// (3) Map ---------------------------------------------------------------------

//instance Functor BTree
//         where fmap f = cataBTree ( inBTree . baseBTree f id )
let fmap f = cataBTree ( inBTree << baseBTree f id )

let fmap f = anaBTree ( baseBTree f id << outBTree )

// (4) Examples ----------------------------------------------------------------

// (4.1) Inversion (mirror) ----------------------------------------------------

let invBTree x = cataBTree (inBTree << (id -|- id >< swap)) x

// (4.2) Counting --------------------------------------------------------------

let countBTree x = cataBTree (either (konst 0) (succ << (uncurry (+)) << p2)) x

// (4.3) Serialization ---------------------------------------------------------

let inordt x = cataBTree inord x                 // in-order traversal

// where

let inord x = 
        let join(x,(l,r)) = l @ [x] @ r
        in either nil join x

let preordt x = cataBTree preord x                  // pre-order traversal

let preord x =  
        let  f(x,(l,r)) = x :: l @ r
        in either nil f x

let postordt x = 
        let f(x,(l,r))=l @ r @ [x]
        in cataBTree (either nil f) x               // post-order traversal

// (4.4) Quicksort -------------------------------------------------------------

let qSort x = hyloBTree inord qsep x               // the same as (cataBTree inord) . (anaBTree qsep)

let qsep x = match x with
        |[] -> Left ()
        |(h :: t) -> let (s,l) = part (<h) t in Right (h,(s,l))

//let part x = ...
        
// (4.5) Traces ----------------------------------------------------------------

let traces x = cataBTree (either (konst [[]]) tunion) x

let tunion(a,(l,r)) x = union (map ((:) a)) l) (map ((:) a)) r) 

// (4.6) Towers of Hanoi -------------------------------------------------------
let hanoi x = hyloBTree present strategy x

let present x = inord x  //same as in qSort

let strategy x = match x with
            |(d,0) -> Left ()
            |(d,n+1) -> Right ((n,d),((not d,n),(not d,n)))

// -------------------------- end of library ----------------------------------