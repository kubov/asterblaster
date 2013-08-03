(in-package :asterblaster)

(def class* pos-vector ()
  ((x 0 :type fixnum)
   (y 0 :type fixnum)))

(def class* asteroid ()
  ((position nil :type pos-vector)
   (speed 0 :type fixnum)
   (radius 20 :type fixnum)
   (direction nil :type pos-vector)
   (size :type fixnum)))

(def class* player ()
  ((name :type string)
   (speed 0 :type fixnum)
   (radius 20 :type fixnum)
   (direction nil :type pos-vector)
   (position nil :type pos-vector)
   (colided nil :type boolean)))

(def class* projectile ()
  ((x 0 :type fixnum)
   (y 0 :type fixnum)))

(def class* game-state ()
  ((players nil :type hash-table)
   (asteroids nil :type hash-table)
   (projectiles nil :type hash-table)))

(defun multiply-by-scalar (vect scalar)
  (with-slots (x y) vect
    (make-instance 'pos-vector
                   :x (* x scalar)
                   :y (* y scalar))))

(defun recalc-pos-vector (current-pos vect speed)
  (let* ((vect (multiply-by-scalar vect speed)))
    (with-slots (x y) current-pos
      (setf (x-of current-pos) (+ x (x-of vect)))
      (setf (y-of current-pos) (+ y (y-of vect))))))

(defun add-to-hash-table (hash key elem)
  (setf (gethash key hash) elem))

(defun recalc-player (player-id player)
  (with-slots (position speed direction) player
    (recalc-pos-vector position direction speed)))

(defun recalc-asteroid (asteroid-id asteroid)
  (recalc-player asteroid-id asteroid))

(defun recalc-players (players)
  (maphash 'recalc-player players))

(defun recalc-asteroids (asteroids)
  (maphash 'recalc-asteroid asteroids))

(defun colliding? (obj1 obj2)
  )

(defun check-colisions (state)
  )

(defun update-state (state)
  ())

(defun update-game-state ()
  (block handler
    ;; remember about locking!
    (loop for msg = (recv *update-state-channel*)
       do (case (type-of msg)
            (quit-message
             (return-from handler))
            (user-join-message
             (format t "handling join~&"))))))
