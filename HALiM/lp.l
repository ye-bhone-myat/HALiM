;;;; Athor : Ye Bhone Myat
;;;; Problem : Yes

;; singleton-p - determines if list passed in is a singleton or not
( defun singleton-p ( lst )
	( cond
		( ( null lst ) nil )
		( ( null ( cdr lst ) ) t )
		( t ( ) )
	)
)

;; rac - the opposite of car - evaluates to the last member of the list passed in
( defun rac ( lst )
	( cond
		( ( singleton-p lst ) ( car lst ) )
		( t ( rac ( cdr lst ) ) )
	)
)

;; rdc - the opposite of cdr - returns the list of all the members of the original list
;; except for the last element
( defun rdc ( lst )
	( cond
		( ( singleton-p lst ) ( ) )
		( t ( cons ( car lst ) ( rdc ( cdr lst ) ) ) )
	)
)

;; snoc - the opposite of cons - adds given element to end of given list
( defun snoc ( elem lst )
	( cond
		( ( null lst ) ( list elem ) )
		( t
			( cons ( car lst ) ( snoc elem ( cdr lst ) ) )
		)
	)
)

;; palindrome-p - determines if given list is a palindrome
(defun palindrome-p (lst)
	(cond
		( ( null lst ) t )
		( ( singleton-p lst ) t )
		( ( equal ( car lst ) ( rac lst ))
			( palindrome-p ( cdr ( rdc lst ) ) ) )
	)
)

;; select - returns the element of a given list from a given position
( defun select ( index lst ) 
	( cond 
		( ( < index 0 ) nil )
		( ( > index ( - ( length lst ) 1 ) ) nil )
		( ( = index 0 ) ( car lst ) )
		( t 
			( select ( - index 1 ) ( cdr lst ) ) 
		)
	)
)

;; pick - randomly picks an element from a given list
( defun pick ( lst &aux n picked )
        ( cond
                ( ( null lst ) ( ) )
                ( t
		  ( setf n ( random ( length lst ) ) )
		  ( setf picked ( select n lst ) )
		    picked
		  )
        )
)

;; sum - returns the value that is the sum of all the values of elements in a given list
( defun sum ( lst )
	( cond 
		( ( null lst ) 0 )
		( t 
			( + ( car lst ) ( sum ( cdr lst ) ) )
		)
	)

)

;; product - returns the value that is the product of all the values of elements in a given list
( defun product ( lst )
	( cond 
		( ( null lst ) 1 )
		( t
			( * ( car lst ) ( product ( cdr lst ) ) )
		)
	)
)

;; iota - given number n, returns a list of ascending numbers from 1 to n
( defun iota ( n )
	( cond
		( ( < n 1 ) ( ) )
		( t
			( snoc n ( iota ( - n 1 ) ) )
		)
	)
)

;; duplicate - given number n and parameter elem, returns a list containing n instances of elem
( defun duplicate ( n elem )
	( cond 
		( ( < n 1 ) ( ) )
		( t
			( cons elem ( duplicate ( - n 1 ) elem ) )
		)
	)
)

;; factorial - returns the factorial of a given number
( defun factorial ( n )
	( cond
		( ( < n 0 ) ( ) )
		( t 
			( product ( iota n ) )
		)
	)
)

;; power - given numbers n and exp, returns the value that is the result of raising n to exp-th exponent
( defun power ( n exp )
	( cond
		( ( < exp 0 )
			( / 1 ( product ( duplicate ( abs exp ) n ) ) ) 
		)
		( t
			( product ( duplicate exp n ) )
		)
	)
)

;; filter-in - given parameters pred and lst, returns a sublist of lst containing elements which are true with respect to pred
( defun filter-in ( pred lst )
	( cond
		( ( null lst ) ( ) )
		( ( funcall pred ( car lst ) )  
			( cons ( car lst ) ( filter-in pred ( cdr lst ) ) ) 
		)
		( t
			( filter-in pred ( cdr lst ) )
		)
	)
)

;; filter-out - given parameters pred and lst, returns a sublist of lst containing elements which are not true with respect to pred
( defun filter-out ( pred lst )
	( cond
		( ( null lst ) ( ) )
		( ( funcall pred ( car lst ) )
			( filter-out pred ( cdr lst ) )
		 )
		( t
			( cons ( car lst ) ( filter-out pred ( cdr lst ) ) ) 
		)
	)
)

;; take-from - given an object o and a list, returns a list with all occurrences of o removed
( defun take-from ( o lst )
	( cond
		( ( null lst ) ( ) ) 
		( ( equal o ( car lst ) )
			( take-from o ( cdr lst )  )
		)
		( t
			( cons ( car lst ) ( take-from  o ( cdr lst ) ) )
		)
	)
)

;; random-permutation - given a list, return a random permutation of given list
( defun random-permutation ( lst &aux elem remainder ) 
  ( cond
    ( ( null lst ) ( ) )
    ( t
      ( setf elem ( select ( random ( length lst ) ) lst ) )
      ( setf remainder ( kick elem lst ) )
      ( cons elem ( random-permutation remainder ) )
      )
    )
  )

;; kick - returns a list with one occurrance of elem removed from lst
( defun kick ( elem lst )
    ( cond
      ( ( null lst ) 
	  	( ) )
      ( ( equal elem ( car lst ) )
	  	( cdr lst ) )
      ( t
		( cons ( car lst ) ( kick elem ( cdr lst ) ) ) 
	  )
    )
)

;; split - given a list and index, returns a list containing
;;			two lists:
;; 			- the first from the start of the original list
;; 			to one less than the given index,
;;			- and the second from the given index to the end
;; 			of the list.
( defun split ( lst ind )
	( list ( split-frnt lst ind ) ( split-back lst ind ) )
)

( defun split-frnt ( lst ind )
	( cond
		( ( = ind 0 )
			()
		)
		( t
			( cons ( car lst ) ( split-frnt ( cdr lst ) ( - ind 1 ) ) )
		)
	)
)

( defun split-back ( lst ind )
	( cond 
		( ( = ind 0 )
			lst
		)
		( t
			( split-back ( cdr lst ) ( - ind 1 ) )
		)
	)
)

( defun unique ( l &aux 1st )
	( if ( null l )
		()
		( let ()
			( setf 1st ( car l ) )
			( cond 
				( ( position 1st ( cdr l ) )
					( unique ( cdr l ) )
				)
				( t
					( cons 1st ( unique ( cdr l ) ) )
				)
			)
		)
	)
)

( defun remove-nth ( index lst &aux split-lst )
	( setf split-lst ( split lst index ) )
	( append ( car split-lst ) ( cdadr split-lst ) )
)
