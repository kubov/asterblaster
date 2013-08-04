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
   (direction (make-instance 'pos-vector) :type pos-vector)
   (position (make-instance 'pos-vector) :type pos-vector)
   (colided (make-instance 'pos-vector) :type boolean)))

(def class* projectile ()
  ((x 0 :type fixnum)
   (y 0 :type fixnum)))

(def class* game-state ()
  ((players (make-hash-table) :type hash-table)
   (asteroids (make-hash-table) :type hash-table)
   (projectiles (make-hash-table) :type hash-table)))

(defparameter *global-game-state* (make-instance 'game-state))

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
  (declare (ignore player-id))
  (with-slots (position speed direction) player
    (recalc-pos-vector position direction speed)))

(defun recalc-asteroid (asteroid-id asteroid)
  (recalc-player asteroid-id asteroid))

(defun recalc-players (players)
  (maphash 'recalc-player players))

(defun recalc-asteroids (asteroids)
  (maphash 'recalc-asteroid asteroids))

(defun square (x)
  (* x x))

(defun diff (x y)
  (- x y))

(defun distance (x1 y1 x2 y2)
  (sqrt (+
         (square (diff x1 x2))
         (square (diff y1 y2)))))

(defun colliding? (obj1 obj2)
  (with-slots (pos1 r1) obj1
    (with-slots (pos2 r2) obj2
      (with-slots (x1 y1) pos1
        (with-slots (x2 y2) pos2
          (< (distance x1 y1 x2 y2) (+ r1 r2)))))))

(defun check-colisions (state)
  (with-slots (players asteroids) state
    (append
     (check-colisions-between players players)
     (check-colisions-between players asteroids))))

(defun check-collisions-between (hash1 hash2)
  (loop for key1 being the hash-key in hash1
        for value1 being the hash-value in hash1 do
       (loop for key2 being the hash-key in hash2
          for value2 being the hash-value in hash2
            when (colliding? value1 value2)
            collect (cons key1 key2))))

(defun update-state ()
  )

(defun handle-player-join (player)
  (with-slots (name id) player
    (with-slots (players) *global-game-state*
      (let ((p (make-instance 'player :name name)))
        (add-to-hash-table players id p))))
  t)

(defun update-game-state ()
  (block handler
    ;; remember about locking!
    (loop for msg = (recv *update-state-channel*)
       do (ecase (type-of msg)
            (stop-message
             (return-from handler))
            (user-join-message
             (format t "handling join~&")
             (handle-player-join msg))))))
