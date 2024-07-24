(defun c:TS
  ( / 
   *error* oldecho
   ; tscale ts
   ename elist ecent sset bname blist
   plin pset cset
   oscale
   tmulti
   n
   visi brot trot hrot arot flip look
  )
  
; error function
(defun *error* ( msg )
    
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
  (if sset     (command-s "undo" ""))
  (if oldecho  (setvar "CMDECHO" oldecho))

  (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
  )
  (princ)
); end defun *error*
  
(vl-load-com)

(if (not tscale)
    (setq tscale "1-4\""))
  
(initget "1-32\" 1-16\" 3-32\" 1-8\" 3-16\" 1-4\" 3-8\" 1-2\" 3-4\" 1.0\" 1.5\" 3.0\"")
(setq tscale  (getkword (strcat "\nCurrent Scale " "<" tscale "> " "New Scale: " "[1-32\"/1-16\"/3-32\"/1-8\"/3-16\"/1-4\"/3-8\"/1-2\"/3-4\"/1.0\"/1.5\"/3.0\"] : "))
      ts      (cond
                ( (= tscale "1-32\"") 8.0          )    ;   8
                ( (= tscale "1-16\"") 4.0          )    ;   4
                ( (= tscale "3-32\"") (/ 8.0 3.0)  )    ; ~ 2.666
                ( (= tscale "1-8\"" ) 2.0          )    ;   2
                ( (= tscale "3-16\"") (/ 4.0 3.0)  )    ; ~ 1.333
                ( (= tscale "1-4\"" ) 1.0          )    ;   1
                ( (= tscale "3-8\"" ) (/ 2.0 3.0)  )    ; ~ 0.666
                ( (= tscale "1-2\"" ) 0.5          )    ;   0.5
                ( (= tscale "3-4\"" ) (/ 1.0 3.0)  )    ; ~ 0.333
                ( (= tscale "1.0\"" ) 0.25         )    ;   0.25
                ( (= tscale "1.5\"" ) (/ 1.0 6.0)  )    ; ~ 0.166
                ( (= tscale "3.0\"" ) (/ 1.0 12.0) )    ; ~ 0.083
                ( t nil )
              ); end cond
); end setq

(setq sset (ssget '((0 . "INSERT,MTEXT,TEXT,LEADER,DIMENSION") (410 . "Model"))))

(if sset
  (progn
    
    ; start undo marks
    (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))

    (setq oldecho (getvar "CMDECHO"))
    (setvar "CMDECHO" 0)

    (setq n 0)
    (repeat (sslength sset)
          
      (setq ename (ssname sset n)
            ecent (cdr (assoc 10 (entget ename)))
            blist (if (= (cdr (assoc 0 (entget (ssname sset n)))) "INSERT")
                      (if (member (vla-get-effectivename (vlax-ename->vla-object (ssname sset n))) blist)
                          blist
                          (cons (vla-get-effectivename (vlax-ename->vla-object (ssname sset n))) blist)
                      )
                      blist
                  ); end if
      ); end setq

      (cond ( (OR (= (cdr (assoc 0 (entget ename))) "MTEXT") (= (cdr (assoc 0 (entget ename))) "TEXT"))
              (setq oscale  (/ (cdr (assoc 40 (entget ename)))  (cond
                                                                  ( (= (cdr (assoc 7 (entget ename))) "CDL3")
                                                                    3.0
                                                                  )
                                                                  ( (OR (= (cdr (assoc 7 (entget ename))) "CDL4.5") (= (cdr (assoc 7 (entget ename))) "CDL4.5-BOLD"))
                                                                    4.5
                                                                  )
                                                                  ( T 
                                                                    (alert  (strcat
                                                                              "\nTurboScale™ is unable to find a reference scale for this "
                                                                              (strcat (cdr (assoc 0 (entget ename))))
                                                                              " entity and will exit.
                                                                              \nUse CDL Text Styles to avoid errors!"
                                                                            )
                                                                    )
                                                                    (exit)
                                                                  )
                                                                ); end cond
                            ); end /
              ); end setq

            ); end TEXT condition

            ( (= (cdr (assoc 0 (entget ename))) "LEADER")
              (setq oscale (/ (vla-get-arrowheadsize (vlax-ename->vla-object ename)) 4))
              (setq ecent (cdr (assoc 10 (reverse (entget ename)))))
              (vla-put-arrowheadsize (vlax-ename->vla-object ename) (* (* (/ 1.0 oscale) ts) (vla-get-arrowheadsize (vlax-ename->vla-object ename))))
            ); end LEADER condition
              
            ( (= (cdr (assoc 0 (entget ename))) "DIMENSION")
              (setq oscale (/ (vla-get-textheight (vlax-ename->vla-object ename)) 3))
              (vla-put-textheight (vlax-ename->vla-object ename) (* (* (/ 1.0 oscale) ts) (vla-get-textheight (vlax-ename->vla-object ename))))
              (vla-put-arrowheadsize (vlax-ename->vla-object ename) (* (* (/ 1.0 oscale) ts) (vla-get-arrowheadsize (vlax-ename->vla-object ename))))
            ); end DIMENSION condition
            
            ( t nil )
            
      ); end cond

      (if (not (OR (= (cdr (assoc 0 (entget ename))) "INSERT") (= (cdr (assoc 0 (entget ename))) "DIMENSION")))
          (progn
            (setq tmulti (* (/ 1.0 oscale) ts))
            (vla-scaleentity (vlax-ename->vla-object ename) (vlax-3d-point ecent) tmulti)
          )
      ); end if

      (setq n (1+ n))

    ); end repeat

    (foreach b blist

      (if (not (>= (cdr (assoc 70 (entget (tblobjname "BLOCK" b)))) 4))
          (progn
            (setq bname (tblobjname "BLOCK" b))
            (if (not (cdr (assoc 4 (entget bname))))
                (setq oscale "1-4\"")
                (setq oscale (cdr (assoc 4 (entget bname))))
            )
            (setq oscale  (cond
                            ( (= oscale "1-32\"") 8.0          )    ;   8
                            ( (= oscale "1-16\"") 4.0          )    ;   4
                            ( (= oscale "3-32\"") (/ 8.0 3.0)  )    ; ~ 2.666
                            ( (= oscale "1-8\"" ) 2.0          )    ;   2
                            ( (= oscale "3-16\"") (/ 4.0 3.0)  )    ; ~ 1.333
                            ( (= oscale "1-4\"" ) 1.0          )    ;   1
                            ( (= oscale "3-8\"" ) (/ 2.0 3.0)  )    ; ~ 0.666
                            ( (= oscale "1-2\"" ) 0.5          )    ;   0.5
                            ( (= oscale "3-4\"" ) (/ 1.0 3.0)  )    ; ~ 0.333
                            ( (= oscale "1.0\"" ) 0.25         )    ;   0.25
                            ( (= oscale "1.5\"" ) (/ 1.0 6.0)  )    ; ~ 0.166
                            ( (= oscale "3.0\"" ) (/ 1.0 12.0) )    ; ~ 0.083
                            ( t nil )
                          ); end cond
            ); end setq

            (setq tmulti (* (/ 1.0 oscale) ts))

            (cond
              ( (AND (vl-string-search "ft" (cdr (assoc 2 (entget bname)))) (vl-string-search "in" (cdr (assoc 2 (entget bname)))))

                (command "-BEDIT" b)

                (setq cset  (ssget "_X" '((0 . "CIRCLE")))
                      plin  (ssget "_X" '((0 . "LWPOLYLINE")))
                      n     0
                ); end setq

                (cond
                  ( (AND (if plin (= (sslength plin) 1)) (OR cset))
                    (setq plin  (ssname (ssget "_X" '((0 . "LWPOLYLINE"))) 0)
                          pset  (reverse 
                                    (if (= (vla-get-closed (vlax-ename->vla-object plin)) :vlax-true)
                                        (append
                                          (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget plin)))
                                          (list (cdr (assoc 10 (entget plin))))
                                        )
                                        (mapcar 'cdr (vl-remove-if '(lambda (x) (/= 10 (car x))) (entget plin)))
                                    ); end if
                                ); end reverse
                    ); end setq
                    (repeat (sslength cset)
                      (vla-scaleentity
                        (vlax-ename->vla-object (ssname cset n))
                        (vlax-3d-point (nth (fix (/ (1+ n) 2)) pset))
                        tmulti
                      ); end vla-scaleentity
                      (setq n (1+ n))
                    ); end repeat
                  )
                  ( (not plin)
                    (repeat (sslength cset)
                      (vla-scaleentity
                        (vlax-ename->vla-object (ssname cset n))
                        (vlax-3d-point (cdr (assoc 10 (entget (ssname cset n)))))
                        tmulti
                      ); end vla-scaleentity
                      (setq n (1+ n))
                    ); end repeat
                  )
                  ( (AND (if plin (> (sslength plin) 1)) (OR cset))
                    (repeat (sslength plin)
                      (vla-scaleentity
                        (vlax-ename->vla-object (ssname cset n))
                        (vlax-3d-point  (polar
                                          (cdr (assoc 10 (entget (ssname cset n))))
                                          (angle (cdr (assoc 10 (entget (ssname cset n)))) (cdr (assoc 10 (entget (ssname cset (1+ n))))))
                                          (* oscale -3.0)
                                        ); end polar
                        ); end vlax-3d-point
                        tmulti
                      ); end vla-scaleentity
                      (vla-scaleentity
                        (vlax-ename->vla-object (ssname cset (1+ n)))
                        (vlax-3d-point  (polar
                                          (cdr (assoc 10 (entget (ssname cset (1+ n)))))
                                          (angle (cdr (assoc 10 (entget (ssname cset (1+ n))))) (cdr (assoc 10 (entget (ssname cset n)))))
                                          (* oscale -3.0)
                                        ); end polar
                        ); end vlax-3d-point
                        tmulti
                      ); end vla-scaleentity
                      (setq n (+ n 2))
                    ); end repeat
                  )
                  ( t nil )
                ); end cond

                (setq sset (ssget "_X" '((-4 . "<NOT") (0 . "LWPOLYLINE,CIRCLE") (-4 . "NOT>"))))
                (setq ecent 
                        (polar
                          (cdr (assoc 11 (entget (ssname (ssget "_X" '((0 . "TEXT") (7 . "CDL4.5-BOLD"))) 0))))
                          (angle
                            (cdr (assoc 11 (entget (ssname (ssget "_X" '((0 . "TEXT") (7 . "CDL4.5-BOLD"))) 0))))
                            (cdr (assoc 11 (entget (ssname (ssget "_X" '((0 . "TEXT") (7 . "CDL3"))) 0))))
                          )
                          (* oscale -5.0)
                        ); end polar
                ); end setq
                (setq n 0)
                (repeat (sslength sset)
                  (if (NOT 
                        (OR
                          (= (vla-get-objectname (vlax-ename->vla-object (ssname sset n))) "AcDbBlockFlipGripEntity")
                          (= (vla-get-objectname (vlax-ename->vla-object (ssname sset n))) "AcDbBlockFlipParameterEntity")
                          (if (vlax-property-available-p (vlax-ename->vla-object (ssname sset n)) "ParameterName")
                              (OR
                                (= (vlax-get-property (vlax-ename->vla-object (ssname sset n)) "ParameterName") "Point2")
                                (= (vlax-get-property (vlax-ename->vla-object (ssname sset n)) "ParameterName") "Point3")
                                (= (vlax-get-property (vlax-ename->vla-object (ssname sset n)) "ParameterName") "Point4")
                                (= (vlax-get-property (vlax-ename->vla-object (ssname sset n)) "ParameterName") "Point5")
                              ); end OR
                          ); end if
                          (if (vlax-property-available-p (vlax-ename->vla-object (ssname sset n)) "Name")
                              (OR
                                (= (vla-get-name (vlax-ename->vla-object (ssname sset n))) "Point2")
                                (= (vla-get-name (vlax-ename->vla-object (ssname sset n))) "Point3")
                                (= (vla-get-name (vlax-ename->vla-object (ssname sset n))) "Point4")
                                (= (vla-get-name (vlax-ename->vla-object (ssname sset n))) "Point5")
                              ); end OR
                          ); end if
                        ); end OR
                      ); end NOT
                      (vla-scaleentity (vlax-ename->vla-object (ssname sset n)) (vlax-3d-point ecent) tmulti)
                  ); end if
                  (setq n (1+ n))
                ); end repeat

                (command  "BSAVE" "BCLOSE")

              ); end condition for TurboTape block
                  
              ( (OR ( = b "W4") ( = b "W8") ( = b "W16") ( = b "TR2") ( = b "TR4") ( = b "VP4") ( = b "VP8"))
                  
                (command "-BEDIT" b)
                
                (setq sset (ssget "_X" '((-4 . "<NOT") (0 . "LINE,LWPOLYLINE") (-4 . "NOT>"))))

                (setq n 0)
                (repeat (sslength sset)
                  (if (NOT 
                        (OR
                          (= (vla-get-objectname (vlax-ename->vla-object (ssname sset n))) "AcDbBlockFlipGripEntity")
                          (= (vla-get-objectname (vlax-ename->vla-object (ssname sset n))) "AcDbBlockFlipParameterEntity")
                          (= (vla-get-objectname (vlax-ename->vla-object (ssname sset n))) "AcDbBlockRotationGripEntity")
                          (= (vla-get-objectname (vlax-ename->vla-object (ssname sset n))) "AcDbBlockRotationParameterEntity")
                        ); end OR
                      ); end NOT
                      (vla-scaleentity (vlax-ename->vla-object (ssname sset n)) (vlax-3d-point '(0 0 0)) tmulti)
                  ); end if
                  (setq n (1+ n))
                ); end repeat

                (command  "BSAVE" "BCLOSE")
                  
              ); end condition for linear blocks

              ( T 
                (command  "-BEDIT" b
                          "SCALE" "ALL" "" '(0 0 0) tmulti
                          "BSAVE"
                          "BCLOSE"
                )
              ); end condition for all other blocks
                
            ); end cond

            (vla-put-comments
                    (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) b)
                    (if (= tscale "1-4\"") "" tscale)
            )
              
          ); end progn
        ); end if

    ); end foreach

    (setq sset (ssget "_X" '((0 . "INSERT") (410 . "Model"))))

    (setq n 0)
    (repeat (sslength sset)
      (if (member (vla-get-effectivename (vlax-ename->vla-object (ssname sset n))) blist)
          (progn
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list (ssname sset n) "AcDbDynBlockPropertyVisibility1"))))
                (setq visi (getpropertyvalue (ssname sset n) "AcDbDynBlockPropertyVisibility1"))
            )
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list (ssname sset n) "AcDbDynBlockPropertyAngle1"))))
                (setq brot (getpropertyvalue (ssname sset n) "AcDbDynBlockPropertyAngle1"))
            )
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list (ssname sset n) "AcDbDynBlockPropertyTRIM ROTATE"))))
                (setq trot (getpropertyvalue (ssname sset n) "AcDbDynBlockPropertyTRIM ROTATE"))
            )
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list (ssname sset n) "AcDbDynBlockPropertyHOUSING ROTATE"))))
                (setq hrot (getpropertyvalue (ssname sset n) "AcDbDynBlockPropertyHOUSING ROTATE"))
            )
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list (ssname sset n) "AcDbDynBlockPropertyARROW ROTATE"))))
                (setq arot (getpropertyvalue (ssname sset n) "AcDbDynBlockPropertyARROW ROTATE"))
            )
            (if (not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list (ssname sset n) "AcDbDynBlockPropertyFlip state1"))))
                (setq flip (getpropertyvalue (ssname sset n) "AcDbDynBlockPropertyFlip state1"))
            )
                
            (if (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list (ssname sset n) "AcDbDynBlockPropertyLookup1")))
                (vla-resetblock (vlax-ename->vla-object (ssname sset n)))
            )

            (if visi (progn (setpropertyvalue (ssname sset n) "AcDbDynBlockPropertyVisibility1" visi)     (setq visi nil)))
            (if brot (progn (setpropertyvalue (ssname sset n) "AcDbDynBlockPropertyAngle1" brot)          (setq brot nil)))
            (if trot (progn (setpropertyvalue (ssname sset n) "AcDbDynBlockPropertyTRIM ROTATE" trot)     (setq trot nil)))
            (if hrot (progn (setpropertyvalue (ssname sset n) "AcDbDynBlockPropertyHOUSING ROTATE" hrot)  (setq hrot nil)))
            (if arot (progn (setpropertyvalue (ssname sset n) "AcDbDynBlockPropertyARROW ROTATE" arot)    (setq arot nil)))
            (if flip (progn (setpropertyvalue (ssname sset n) "AcDbDynBlockPropertyFlip state1" flip)     (setq flip nil)))
              
          ); end progn
      ); end if

      (setq n (1+ n))

    ); end repeat
      
    (foreach b blist
      (if (= (cdr (assoc 70 (entget (tblobjname "BLOCK" b)))) 2)
          (command "attsync" "name" b)
      )
    )

    (setvar "CMDECHO" oldecho)
    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))

  ); end progn
); end if

(princ)

); end defun