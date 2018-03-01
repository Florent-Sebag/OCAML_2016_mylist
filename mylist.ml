type 'a  my_list =
  | Item of ('a * 'a my_list)
  | Empty

let cons elem list = Item (elem, list)

let rec fold_left fnct elem = function
  | Empty -> elem
  | Item (head, sl) -> fold_left fnct (fnct elem head) sl

let length list =
  let f acc z = acc + 1
  in fold_left f 0 list

let hd = function
  | Empty           -> raise (Failure "hd")
  | Item (head, li) -> head

let tl = function
  | Empty           -> raise (Failure "tl")
  | Item (head, li) -> li

let rec nth list = function
  | n when n > length list - 1 -> raise (Failure "nth")
  | n when n == 0 -> hd list
  | n when n > 0 -> nth (tl list) (n - 1)
  | _ -> raise (Invalid_argument "List.nth")

let rev list =
    let algo result elem = (Item(elem, result))
  in fold_left algo Empty list

let append l1 l2 =
    let algo result elem = (Item(elem, result))
  in fold_left algo l2 (rev l1)

let rev_append l1 l2 =
    let algo result elem = (Item(elem, result))
  in fold_left algo l2 l1

let flatten list =
    let algo result elem = (append result elem)
  in fold_left algo Empty list

let iter fct list =
    let algo result elem = fct elem
  in fold_left algo () list

let map fct list =
    let algo result elem = Item ((fct elem), result)
  in rev (fold_left algo Empty list)

let for_all fct list =
    let algo result elem = (fct elem) && result
  in fold_left algo true list

let exists fct list =
    let algo result elem = (fct elem) || result
  in fold_left algo false list

let mem a list =
    let algo result elem = result || (elem = a)
  in fold_left algo false list

let memq a list =
    let algo result elem = result || (elem == a)
  in fold_left algo false list

let filter fct list =
    let algo result elem =
      if fct elem == true
      then Item(elem, result)
      else result
  in fold_left algo Empty (rev list)

let split list =
    let algo (r1, r2) (e1, e2) = (Item(e1, r1), Item(e2, r2))
  in fold_left algo (Empty, Empty) (rev list)

let combine l1 l2 =
  if (length l2) != (length l1) then raise (Invalid_argument "") else
  let rec algo result list = function
    | Empty -> result
    | Item (head, sl) -> algo (Item (((hd list), head), result)) (tl list) sl
  in algo Empty (rev l1) (rev l2)
