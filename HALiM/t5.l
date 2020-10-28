;; HALiM
;; Task 5: The Players
;; Ye Bhone Myat

;; Load previous task

( load "t4.l" )

;; ==========

;; Declare constants
( setf *n-carpets* 12 )
;; ==========

( defun dummy-human ()
	( setf *dummy-human* ( make-instance 'human :name "Le Chad" :carpets '( ( r . 12 ) ( g . 12 ) ) ) )
)
( defun dummy-randa ()
	( setf *dummy-randa* ( make-instance 'randa :carpets '( ( r . 12 ) ( g . 12 ) ) ) )
)
( defun dummy-halim ()
	( setf *dummy-halim* ( make-instance 'halim :carpets '( ( r . 12 ) ( g . 12 ) ) ) )
)

;; === Player class ===
( defclass player ()
	(
		;; alist pairlists of the form ( color . count )
		( carpets
			:accessor player-carpets
			:initarg :carpets
		)
		;; win/loss stats
		( stats
			:accessor player-stats
			:initform 0
		)
		( coins
			:accessor player-coins
			:initform 30
		)
		( name
			:accessor player-name
			:initarg :name
		)
	)
)
;; ==========

;; === Human class ===
;; extends Player class
( defclass human ( player ) 
	(
		( name
			:initform "human"
		)
	)
)
;; ==========

;; === Computer class ===
;; Parent class of HALiM and Randa classes
;; ----------
( defclass computer ( player ) () )
;; ==========

;; === HALiM class ===
;; extends Player class
;; -----
;; The AI player
;; -----
( defclass halim ( computer )
	(
		( name
			:initform "HALiM"
		)
		;; tracks the current phase of the game
		;; 0 - early, 1 - mid, 2 - late
		( game-phase
			:accessor halim-game-phase
			:initform 0
		)
		;; tracks the financial status of halim
		;; 2 - good; 1 - neutral ; 0 - poor/critical
		( finances
			:accessor halim-finances
			:initform 2
		)
	)
)
;; ==========

;; === Randa class ===
;; extends Player class
;; -----
;; The Random player
;; -----
( defclass randa ( computer ) 
	(
		( name
			:initform "Randa"
		)
	)
)
;; ==========

;; method display-player
;; ==========
;; Takes a player to be described textually
;; -----
;; Textually describes a player's name, balance, and inventory
;; -----
( defmethod display-player ( ( p player ) )
	( format t
;;		"~:(~A~)~%==========~%Dirhams: ~A~%~:(~A~) carpets: ~A~%~:(~A~) carpets: ~A~%"
		"==========~%~:(~A~)~%==========~%Dirhams: ~A~%~{~#[No carpets left~:;~A~]~%~:}"
		( player-name p )
		( player-coins p )
		( mapcar ( lambda ( x )
				( format nil "~:(~A~) carpets: ~A" 
					( cdr ( assoc ( car x ) *colors* ) )
					( cdr x )
				)
			)
			( available-carpets p )
		)
	)
)
;; ==========

;; function subtract-carpet
;; ==========
;; Takes player p and a color
;; -----
;; Subtracts 1 carpet of the given color
;; from the given player's inventory
;; -----
( defun subtract-carpet ( ( p player ) color )
	( setf ( cdr ( assoc color ( player-carpets p ) ) )
		( - ( cdr ( assoc color ( player-carpets p ) ) ) 1 )
	)
)
;; ==========

;; function pay-up
;; ==========
;; Takes a payer, a payee, and an amount to be paid
;; ----------
;; Subtracts amount from the payer and
;; adds the same amount to the payee
( defun pay-up ( ( payer player ) ( payee player ) amount )
	( setf ( player-coins payer ) ( - ( player-coins payer ) amount ) )
	( setf ( player-coins payee ) ( + ( player-coins payee ) amount ) )
)

;; generic method handle-placement
;; ==========
( defgeneric handle-placement ( p ) )
;; ==========

;; method handle-placement [human]
;; ==========
;; Takes a human player
;; ----------
;; Prompts a human player for the
;; placement information of a carpet.
;; Also handles typographic errors and
;; illegal moves
;; After the human has entered the
;; placement information, the method
;; performs the necessary placement
;; of the carpet and also subtracts
;; a carpet of the given color from
;; the human's inventory
;; -----
( defmethod handle-placement ( ( p human ) &aux placement color direction orient target ) 
	( format t "Choose where to place a carpet~%" )
	( format t "To place a RED carpet to the WEST of Assam and oriented NORTH, enter R W N~%" )
	( setf placement ( list ( read ) ( read ) ( read ) ) )
	( loop while ( not ( placeable-p p ( nth 0 placement ) ( nth 1 placement ) ( nth 2 placement ) ) )
		do ( format t "Choose where to place a carpet~%" )
		do ( format t "To place a RED carpet to the WEST of Assam and oriented NORTH, enter R W N~%" )
		do ( setf placement ( list ( read ) ( read ) ( read ) ) )
	)
	( setf color ( nth 0 placement ) )
	( setf direction ( nth 1 placement ) )
	( setf orient ( nth 2 placement ) )
	( setf target ( cdr ( get-neighbor-pairlis ( assam-tile *assam* ) direction ) ) )
	( carpet-place target orient color )
	( subtract-carpet p color )
)
;; ==========


;; method handle-placement [randa]
;; ==========
;; Takes a randa player
;; ----------
;; The method randomly selects a tile
;; to place a carpet on, and randomly
;; selects the direction it is to be 
;; placed in.
;; Afterwards, the method randomly 
;; picks a carpet from randa's inventory
;; and places it with the selected orientation
;; ----- 
( defmethod handle-placement ( ( p randa ) &aux turnables candidate-tiles directions direction orient color target colors )
	( setf directions ( carpet-placeables ( assam-tile *assam* ) ) )
	( setf candidate-tiles ( filter-out
		( lambda (x)
			( null ( carpet-orientables x ) )
		)
		( mapcar #'cdr directions )
	) )
	( setf direction ( nth ( random ( length candidate-tiles ) ) candidate-tiles ) )
	( setf target ( cdr ( rassoc direction directions ) ) )
	( setf turnables ( carpet-orientables target ) )
	( setf orient ( nth ( random ( length turnables ) ) turnables ) )
	( setf colors ( mapcar #'car ( available-carpets p ) ) )
	( setf color ( nth ( random ( length colors ) ) colors ) )
	( carpet-place target ( car orient ) color )
	( subtract-carpet p color )
)
;; ==========


;; method handle-placement [halim]
;; ==========
;; Takes a halim player
;; ----------
;; Allows HALiM to select a tile upon which
;; it believes a carpet should be placed,
;; and then allows it to select the carpet's
;; orientation and color.
;; The method then places the carpet
;; as per HALiM's specifications,
;; and subtracts a carpet of the same
;; color from its inventory
;; -----
( defmethod handle-placement ( ( p halim ) &aux candidate-tiles turnables directions direction orient color target colors )
	( setf directions ( carpet-placeables ( assam-tile *assam* ) ) )
	( setf candidate-tiles ( filter-out
		( lambda (x)
			( null ( carpet-orientables x ) )
		)
		( mapcar #'cdr directions )
	) )
	( setf direction ( nth ( placement-select p candidate-tiles ) candidate-tiles ) )
	( if *verbose* ( format t "Placing carpet ~:(~A~)~%" ( cdr ( assoc ( car ( rassoc direction directions ) ) *directions* ) ) ) )
	( setf target ( cdr ( rassoc direction directions ) ) )
	( setf orient ( orient-color-select p target ) )
	( setf color ( cadr orient ) )
	( setf orient ( car orient ) )
	( carpet-place target orient color )
	( subtract-carpet p color )
)
;; ==========

;; function available-carpets
;; ==========
;; Takes a player whose carpets are to
;; be counted
;; -----
;; Returns a list of available carpets
;; and their colors in the player's
;; inventory
;; -----
( defun available-carpets ( ( p player ) )
	( filter-out ( lambda ( x ) 
			( = 0 ( cdr x ) )
		)
		( player-carpets p )
	)
)
;; ==========

;; generic method handle-turn
;; ==========
( defgeneric handle-turn ( p ) )
;; ==========

;; mathod handle-turn [human]
;; ==========
;; Takes a human player
;; -----
;; Prompts the human player to pick a direction
;; from a list of directions Assam can legally
;; turn towards.
;; Afterwards, sets Assam's heading to the 
;; heading picked by the player
;; -----
( defmethod handle-turn ( ( p human ) &aux direction valid-directions )
	( setf valid-directions ( remove-opposite ( car ( assam-heading *assam* ) ) ( mapcar #'car *directions* ) ) )
	( format t "Choose the direction Assam should turn to.~%" )
	( format t "Assam can turn (~{~A~#[~;, or ~:;, ~]~}).~%" valid-directions )
	( setf direction ( read ) )
	( loop while ( not ( position direction valid-directions ) )
		do ( format t "!~A is not a valid direction. Try one of ~A!~%~%" direction valid-directions )
		do ( format t "Choose the direction Assam should turn to.~%" )
		do ( format t "Assam can turn (~{~A~#[~;, or ~:;, ~]~}).~%"
				( remove-opposite ( car ( assam-heading *assam* ) ) ( mapcar #'car *directions* ) )
			)
		do ( setf direction ( read ) )
	)
	( assam-face direction *assam* )
)
;; ==========

;; method handle-turn [randa]
;; ==========
;; Takes a randa player
;; -----
;; Randomly picks a direction for Assam to
;; turn to, from a list of legal directions,
;; and turns Assam to said direction.
;; -----
( defmethod handle-turn ( ( p randa ) &aux turnables direction )
	( setf turnables ( remove-opposite ( car ( assam-heading *assam* ) ) ( mapcar #'car *directions* ) ) )
	( setf direction ( nth ( random ( length turnables ) ) turnables ) )
	( assam-face direction *assam* )
)
;; ==========

;; method handle-turn [halim]
;; -----
;; Takes a halim player
;; -----
;; Allows HALiM to select a direction
;; that it thinks Assam should turn to,
;; from a list of legal directions.
;; The method then turns Assam to said
;; direction.
;; -----
( defmethod handle-turn ( ( p halim ) &aux turnables direction )
	( setf turnables ( remove-opposite ( assam-heading *assam* ) *directions* ) )
;;	( setf direction ( car ( nth ( path-heuristics p turnables ) turnables ) ) )
	( setf direction ( path-select p turnables ) )
	( if *verbose* ( format t "Turning ~A~%" ( cdr ( assoc direction *directions* ) ) ) )
	( assam-face direction *assam* )
)
;; ==========
