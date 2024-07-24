(defun c:TC
  ( / 
    *error* oldecho
    bname ename
    plist ptr ptl ptu ptd
    xlist glist llist 
    gripr gripl gripu gripd
    lablr labll lablu labld
  )

  ; error function
  (defun *error* ( msg )

    (if oldecho  (setvar "CMDECHO" oldecho))

    (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nError: " msg))
    )
    (princ)
  ); end defun *error*
  
  (while (OR (= bname nil) (= bname ""))
    (setq bname (vl-string-trim " " (strcase (getstring "\nEnter New Type: "))))
  )

  (print bname)
  
  ; end program if new block name already exists
  (if (not (tblsearch "BLOCK" bname))

    (progn

      (setq oldecho (getvar "CMDECHO"))  
      (setvar "CMDECHO" 0)

      (setvar "ERRNO" 0)
      (while  (AND  (not ename) (/= (getvar "ERRNO") 52))
              (if   (AND (setq ename (car (entsel "\nSelect Reference Block: "))) (/= (cdr (assoc 0 (entget ename))) "INSERT"))
                    (setq ename nil)
              )
      )

      ; end program if reference block contains attributes
      (if (/= (vla-get-hasattributes (vlax-ename->vla-object ename)) :vlax-true)

          (progn
    
            (command "-BEDIT" (vla-get-effectivename (vlax-ename->vla-object ename)))

            ; set text to new value (in block editor)
            (foreach a (mapcar 'cadr (ssnamex (ssget "_X" '((0 . "TEXT")))))
              (entmod (subst (cons 1 bname) (assoc 1 (entget a)) (entget a)))
            )

            ; make a list (plist) of centerpoints of all text objects
            (foreach a (mapcar 'cadr (ssnamex (ssget "_X" '((0 . "TEXT")))))
              (setq plist (cons (polar 
                                  (cdr (assoc 10 (entget a)))
                                  (angle      (cdr (assoc 10 (entget a)))
                                              (list
                                                (+ (car (cdr (assoc 10 (entget a)))) (car (cadr (textbox (entget a)))) )
                                                (+ (cadr (cdr (assoc 10 (entget a)))) (cadr (cadr (textbox (entget a)))) )
                                                (+ (caddr (cdr (assoc 10 (entget a)))) (caddr (cadr (textbox (entget a)))) )
                                              )
                                  )
                                  (/ 
                                    (distance (cdr (assoc 10 (entget a)))
                                              (list
                                                (+ (car (cdr (assoc 10 (entget a)))) (car (cadr (textbox (entget a)))) )
                                                (+ (cadr (cdr (assoc 10 (entget a)))) (cadr (cadr (textbox (entget a)))) )
                                                (+ (caddr (cdr (assoc 10 (entget a)))) (caddr (cadr (textbox (entget a)))) )
                                              )
                                    )
                                  2.0
                                  )
                                )
                                plist
                          ); end cons
              ); end setq
            ); end foreach

            (setq ptr (car (vl-sort plist (function (lambda (x1 x2) (> (car x1) (car x2))))))
                  ptr (list (car ptr) 0.0 (caddr ptr))
                  ptl (car (vl-sort plist (function (lambda (x1 x2) (< (car x1) (car x2))))))
                  ptl (list (car ptl) 0.0 (caddr ptl))
                  ptu (car (vl-sort plist (function (lambda (y1 y2) (> (cadr y1) (cadr y2))))))
                  ptu (list 0.0 (cadr ptu) (caddr ptu))
                  ptd (car (vl-sort plist (function (lambda (y1 y2) (< (cadr y1) (cadr y2))))))
                  ptd (list 0.0 (cadr ptd) (caddr ptd))
            )
            ; create list of all dynamic property entities
            (foreach a (mapcar 'cadr (ssnamex (ssget "_X")))
              (if (not (cdr (assoc 0 (entget a))))
                  (setq xlist (cons (cdr (assoc -1 (entget a))) xlist))
              )
            )
            ; create list of all dynamic properties that are grips
            (foreach g xlist
              (if (= (vla-get-objectname (vlax-ename->vla-object g)) "AcDbBlockXYGripEntity")
                  (setq glist (cons g glist))
              )
            )
            ; set grip properties to left and right
            (setq gripr (car (vl-sort glist (function (lambda (g1 g2) (> (getpropertyvalue g1 "Position/X") (getpropertyvalue g2 "Position/X"))))))
                  gripl (car (vl-sort glist (function (lambda (g1 g2) (< (getpropertyvalue g1 "Position/X") (getpropertyvalue g2 "Position/X"))))))
                  gripu (car (vl-sort glist (function (lambda (g1 g2) (> (getpropertyvalue g1 "Position/Y") (getpropertyvalue g2 "Position/Y"))))))
                  gripd (car (vl-sort glist (function (lambda (g1 g2) (< (getpropertyvalue g1 "Position/Y") (getpropertyvalue g2 "Position/Y"))))))
            )
            ; move grip positions
            (vla-put-position (vlax-ename->vla-object gripr) (vlax-3d-point ptr))
            (vla-put-position (vlax-ename->vla-object gripl) (vlax-3d-point ptl))
            (vla-put-position (vlax-ename->vla-object gripu) (vlax-3d-point ptu))
            (vla-put-position (vlax-ename->vla-object gripd) (vlax-3d-point ptd))

            ; create list of all dynamic properties that are labels
            (foreach l xlist
              (if (= (vla-get-objectname (vlax-ename->vla-object l)) "AcDbBlockPointParameterEntity")
                  (setq llist (cons l llist))
              )
            )
            ; set label properties to left and right
            (setq lablr (car (vl-sort llist (function (lambda (l1 l2) (> (getpropertyvalue l1 "LabelPoint/X") (getpropertyvalue l2 "LabelPoint/X"))))))
                  labll (car (vl-sort llist (function (lambda (l1 l2) (< (getpropertyvalue l1 "LabelPoint/X") (getpropertyvalue l2 "LabelPoint/X"))))))
                  lablu (car (vl-sort llist (function (lambda (l1 l2) (> (getpropertyvalue l1 "LabelPoint/Y") (getpropertyvalue l2 "LabelPoint/Y"))))))
                  labld (car (vl-sort llist (function (lambda (l1 l2) (< (getpropertyvalue l1 "LabelPoint/Y") (getpropertyvalue l2 "LabelPoint/Y"))))))
            )
            ; move label position (right)
            (setpropertyvalue lablr "BasePoint/X" (car ptr))
            (setpropertyvalue lablr "BasePoint/Y" 0.0)
            (setpropertyvalue lablr "LabelPoint/X" (car (polar ptr (angle ptr '(0 0 0)) -4.0)))
            (setpropertyvalue lablr "LabelPoint/Y" 0.0)
            ; move label position (left)
            (setpropertyvalue labll "BasePoint/X" (car ptl))
            (setpropertyvalue labll "BasePoint/Y" 0.0)
            (setpropertyvalue labll "LabelPoint/X" (car (polar ptl (angle ptl '(0 0 0)) -4.0)))
            (setpropertyvalue labll "LabelPoint/Y" 0.0)
            ; move label position (up)
            (setpropertyvalue lablu "BasePoint/X" 0.0)
            (setpropertyvalue lablu "BasePoint/Y" (cadr ptu))
            (setpropertyvalue lablu "LabelPoint/X" 0.0)
            (setpropertyvalue lablu "LabelPoint/Y" (cadr (polar ptu (angle ptu '(0 0 0)) -4.0)))
            ; move label position (down)
            (setpropertyvalue labld "BasePoint/X" 0.0)
            (setpropertyvalue labld "BasePoint/Y" (cadr ptd))
            (setpropertyvalue labld "LabelPoint/X" 0.0)
            (setpropertyvalue labld "LabelPoint/Y" (cadr (polar ptd (angle ptd '(0 0 0)) -4.0)))
            
            (command "BSAVEAS" bname "BCLOSE")     
            (entmake  (list (cons 0 "INSERT")
                            (cons 8 (if (not (tblsearch "LAYER" "CDL-A-LTG-FIXTURES"))
                                        (progn
                                          (entmake  (list (cons 0 "LAYER")
                                                          (cons 100 "AcDbSymbolTableRecord")
                                                          (cons 100 "AcDbLayerTableRecord")
                                                          (cons 2 "CDL-A-LTG-FIXTURES")
                                                          (cons 70 0)
                                                          (cons 62 7)
                                                          (cons 6 "Continuous")
                                                          (cons 370 -3)
                                                    ); end list
                                          ); end entmake
                                          "CDL-A-LTG-FIXTURES"
                                        ); end progn
                                        "CDL-A-LTG-FIXTURES"
                                    ); end if
                            ); end cons
                            (cons 2 bname)
                            (cons 10 (getpoint "\nSelect Insertion Point: "))
                      ); end list
            ); end entmake
              
            (if oldecho (setvar "CMDECHO" oldecho))
        
          ); end progn
        
          (alert "\nReference block cannot contain attributes!")
        
      ); end if

    ); end progn

    (alert "\nBlockname must be unique!")

  ); end if

  (princ)

); end defun