(in-package :asterblaster)

(def class* asteroid ()
  ((x 0 :type fixnum)
   (y 0 :type fixnum)
   (size :type fixnum)))

(def class* player ()
  ((name :type string)
   (x 0 :type fixnum)
   (y 0 :type fixnum)))

(def class* projectile ()
  ((x 0 :type fixnum)
   (y 0 :type fixnum)))

(defun update-game-state ()
  (block handler
    ;; remember about locking!
    (loop for msg = (recv *update-state-channel*)
       do (case (type-of msg)
            (quit-message 
             (return-from handler))
            (user-join-message 
             (format t "handling join~&"))))))
