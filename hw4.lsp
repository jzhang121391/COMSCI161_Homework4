; CS 161 Spring 2013: HW4 skeleton

; FUNCTION: ANSWER
; PURPOSE:  Search through the passed-in episodic memory (list of frame atoms)
;           for a frame that can be UNIFRAME'd (homework #3) with the query
;           frame, by binding variables expressed in the query.
; OUTPUT:   If a frame that can be unified is found, substitute the corresponding
;           binding into the question frame, and return the result.
;           As a special case, if the predicate of the query is "ACT" (which corresponds
;           roughly to an English query "what did... do..."), then ANSWER should look
;           for frames in the episodic memory whose slots unify with the slots of
;           the query, with the exception of the "IS" slot in the query, which
;           can be ignored. It should return the result of unifying the found
;           frame with the query (leaving out the "IS" slot). For example, see
;           the Test-Case-2 in the homework PDF.
;           If no frame can be found, then return NIL.
; INPUTS:   QCON: query frame
;           EP-STMEM: list of frame atoms to search for answer in

(defun ANSWER-ACT (QCON EP-STMEM)
	(cond
		((null EP-STMEM) nil)
		((null (UNIFRAME QCON (rest (eval (first EP-STMEM))) '(T))) (ANSWER-ACT QCON (rest EP-STMEM)))
		((not (null (UNIFRAME QCON (rest (eval (first EP-STMEM))) '(T)))) (SUBST-FETA (eval (first EP-STMEM)) (UNIFRAME QCON (rest (eval (first EP-STMEM))) '(T))))
	)
)

(defun ANSWER (QCON EP-STMEM)
	(cond
		((null EP-STMEM) nil)
		((equal (first QCON) 'ACT) (ANSWER-ACT (rest (RM-SLOT 'IS QCON)) EP-STMEM))
		((null (UNIFRAME QCON (eval (first EP-STMEM)) '(T))) (ANSWER QCON (rest EP-STMEM)))
		((not (null (UNIFRAME QCON (eval (first EP-STMEM)) '(T)))) (SUBST-FETA (eval (first EP-STMEM)) (UNIFRAME QCON (eval (first EP-STMEM)) '(T))))
	)
)

; -----------------------------------------------------------------------------

; FUNCTION: C-GEN
; PURPOSE:  Converts a frame into an English sentence, using a list of "sentence
;           patterns" (ENG-PATS) and decision-trees to help make some more
;           natural sounding phrasings.
; OUTPUT:   List of atoms, readible as an English sentence. (Essentially, this
;           is the inverse of the "C-ANALYZE" operation from HW#2)
; INPUTS:   C-ANS: a frame to convert into a sentence
;           ENG-PATS: a list of English sentence patterns, of the form
;           ( (pred (element)* ) )
;           where element is either
;           -- An atom (interpreted as a slot name), in which case C-GEN should
;              insert the result of recursing on the FILLER of that slot
;           -- A list of the form (PHR (atom)+) (the literal symbol PHR followed by a list of
;              atoms, interpreted as words to append to the sentence)
;           -- A list of the form (DECIDE PRED), in which case the utility function 
;              DECIDE-PHR will be dispatched to use the entry for PRED in D-TREES to 
;              decide which words to add (see specs for DECIDE-PHR)
;           D-TREES: a list of decision trees, whose format is described in
;                    the documentation for DECIDE-PHR

(defun C-GEN-HELPER (PAT C-ANS ENG-PATS D-TREES)
	(cond
		((null PAT) nil)
		((not (null (PATH-SL (list (first PAT)) C-ANS)))
			(append (C-GEN (PATH-SL (list (first PAT)) C-ANS) ENG-PATS D-TREES) (C-GEN-HELPER (rest PAT) C-ANS ENG-PATS D-TREES)))
		((and (listp (first PAT)) (equal (first (first PAT)) 'DECIDE)) 
			(append (C-GEN (DECIDE-PHR (second (first PAT)) C-ANS D-TREES) ENG-PATS D-TREES) (C-GEN-HELPER (rest PAT) C-ANS ENG-PATS D-TREES)))
		((and (listp (first PAT)) (equal (first (first PAT)) 'PHR)) 
			(append (second (first PAT)) (C-GEN-HELPER (rest PAT) C-ANS ENG-PATS D-TREES)))
		(t (C-GEN-HELPER (rest PAT) C-ANS ENG-PATS D-TREES))
	)
)

(defun C-GEN (C-ANS ENG-PATS D-TREES)
	(cond
		((null (GET-TREE (first C-ANS) ENG-PATS)) C-ANS)
		(t (C-GEN-HELPER (second (GET-TREE (first C-ANS) ENG-PATS)) C-ANS ENG-PATS D-TREES))
	)
)

; -----------------------------------------------------------------------------

; FUNCTION: DECIDE-PHR
; PURPOSE:  Locate in the list of D-TREES an entry indexed by the given predicate
;           DECIDE-PHR then traverses the decision tree, checking attributes
;           of the TOP-LEVEL SLOTS of the frame in the order of traversal. Leaf
;           nodes always are a list of the form (PHR (word)+), that is a list
;           starting with the literal element "PHR" and followed by a list of
;           atoms listing a phrase.
; OUTPUT:   A phrase (list of words), or NIL if not found
; INPUTS:   PRED: predicate to choose which tree out of our list of d-trees
;           FRAME: frame to check top-level slots of
;           D-TREES: list of decision trees, indexed by predicate; format is
;           ( (pred-1 (node (val-1-1 (leaf or node…))
;                … … 
;                       (val-1-n (leaf or node…)))
;             (pred-2 (node (val-2-1 (leaf or node...)) … )
;           For example, the D-TREES list:
;           ( (HUMAN
;               (NOBILITY
;                  (ROYAL (GENDER
;                            (FEMALE (PHR (QUEEN)))
;                            (MALE (PHR (KING)))
;                         )
;                  )
;                  (NOBLE (GENDER
;                            (MALE (PHR (LORD)))
;                            (FEMALE (PHR (LADY)))
;                         )
;                  )
;                  (COMMON (PHR (PEASANT)))
;               )
;             )
;           )
;           has a single tree, which is defined for frames whose predicate is HUMAN.
;           It first checks the TOP-LEVEL slot "NOBILITY" -- if it is "royal," then it
;           checks the TOP-LEVEL slot "GENDER" and outputs either (QUEEN) or (KING)
;           accordingly. (Similarly for "NOBLE"; but if NOBILITY is "COMMON" then
;           it doesn't check gender, it just outputs (PEASANT)).

(defun GET-TREE (PRED D-TREES)
	(cond
		((null D-TREES) nil)
		((equal PRED (first (first D-TREES))) (first D-TREES))
		(t (GET-TREE PRED (rest D-TREES)))
	)
)

(defun IS-IN (SLOT FRAME)
	(cond
		((null FRAME) nil)
		((equal SLOT (first (first FRAME))) (second (first FRAME)))
		(t (IS-IN SLOT (rest FRAME)))
	)
)

(defun DECIDE-PHR-HELPER (D-TREE FRAME)
	(cond
		((equal (first D-TREE) 'PHR) (second D-TREE))
		((or 
			(null D-TREE) 
			(null (IS-IN (first D-TREE) (rest FRAME)))
			(null (IS-IN (first (IS-IN (first D-TREE) (rest FRAME))) (rest D-TREE)))
		) nil)
		(t (DECIDE-PHR-HELPER (IS-IN (first (IS-IN (first D-TREE) (rest FRAME))) (rest D-TREE)) FRAME))
	)
)

(defun DECIDE-PHR (PRED FRAME D-TREES)
	(DECIDE-PHR-HELPER (second (GET-TREE PRED D-TREES)) FRAME)
)

(load "hw-1-solution.lsp")
(load "hw-3-solution.lsp")