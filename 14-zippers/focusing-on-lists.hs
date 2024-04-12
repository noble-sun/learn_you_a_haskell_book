-- This is an alias for a Zipper of list
-- The first list is the sub-list of current focus, and the second is the breadcrumbs.
type ListZipper a = ([a], [a])

-- This function takes a ListZipper and returns a new ListZipper
goForward :: ListZipper a -> ListZipper a
-- Uses list pattern matching to take the head of the first list, that is the focus list.
-- It will return a tuple where the first element is the tails of the list, and the second
-- element is the head of the first list plus the second list
goForward (x:xs, bs) = (xs, x:bs)

-- This function is basically the inverse of the function above. It will take the 
-- head of the breadcrumb list and put it as the head of the focus list
goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

-- For example: let xs = [1,2,3,4]
-- goForward (xs,[]) -------------> ([2,3,4],[1])
-- goForward ([2,3,4],[1]) -------> ([3,4],[2,1])
-- goBack ([3,4],[2,1]) ----------> ([2,3,4],[1])




