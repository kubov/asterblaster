(in-package :asterblaster)

(defparameter *canvas-w* 200)
(defparameter *canvas-h* 200)

(def class* pos-vector ()
  ((x 0 :type fixnum)
   (y 0 :type fixnum)))

(defun get-random-spot ()
  (make-instance 'pos-vector
                 :x (random *canvas-w*)
                 :y (random *canvas-h*)))

(defun find-free-spot ()
  ;stub
  (get-random-spot))

(defun get-stanard-spot ()
  (make-instance 'pos-vector))

(def class* asteroid ()
  ((position (get-random-spot) :type pos-vector)
   (speed 0 :type fixnum)
   (radius 20 :type fixnum)
   (direction (get-random-spot) :type pos-vector)
   (size :type fixnum)))

(def class* player ()
  ((name :type string)
   (speed 0 :type fixnum)
   (radius 20 :type fixnum)
   (direction (get-stanard-spot) :type pos-vector)
   (position (find-free-spot) :type pos-vector)))

(def class* projectile ()
  ((position (get-stanard-spot) :type pos-vector)
   (radius 2 :type fixnum)
   (speed 10 :type fixnum)
   (direction (get-stanard-spot) :type pos-vector)))

(def class* game-state ()
  ((players (make-hash-table) :type hash-table)
   (asteroids (make-hash-table) :type hash-table)
   (projectiles (make-hash-table) :type hash-table)))

(defparameter *global-game-state* (make-instance 'game-state))
(defparameter *game-state-lock* (bordeaux-threads:make-lock "game state lock"))

(defparameter *test-players* (make-hash-table))
(defparameter *test-state* (make-instance 'game-state))

(defun make-test-object (type)
  (make-instance type
                 :position (get-random-spot)
                 :direction (get-random-spot)
                 :speed (/ (random 11) 10)))

(defun init-test ()
  (with-slots (players projectiles asteroids) *test-state*
    (loop for i from 0 to 5 do
       (add-to-hash-table
        players
        i
        (make-test-object 'player)))
    (loop for i from 5 to 10 do
       (add-to-hash-table
        projectiles
        i
        (make-test-object 'projectile)))
    (loop for i from 10 to 15 do
       (add-to-hash-table
        asteroids
        i
        (make-test-object 'asteroid)))))

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

(defun recalc-projectiles (projectiles)
  (recalc-players projectiles))

(defun square (x)
  (* x x))

(defun diff (x y)
  (- x y))

(defun distance (x1 y1 x2 y2)
  (sqrt (+
         (square (diff x1 x2))
         (square (diff y1 y2)))))

(defun colliding? (obj1 obj2)
  (with-slots ((pos1 position) (r1 radius)) obj1
    (with-slots ((pos2 position) (r2 radius)) obj2
      (with-slots ((x1 x) (y1 y)) pos1
        (with-slots ((x2 x) (y2 y)) pos2
          (< (distance x1 y1 x2 y2) (+ r1 r2)))))))

(defun check-collisions-between (hash1 hash2)
  (loop for key1 being the hash-key in hash1
        for value1 being the hash-value in hash1 do
       (loop for key2 being the hash-key in hash2
          for value2 being the hash-value in hash2
            when (colliding? value1 value2)
            collect (cons key1 key2))))

(defun check-collisions (state)
  (with-slots (players asteroids) state
    (append
     (check-collisions-between players players)
     (check-collisions-between players asteroids))))

(defun update-state (state)
  (with-slots (players asteroids projectiles) *global-game-state*
    (recalc-asteroids asteroids)
    (recalc-players players)
    (recalc-projectiles projectiles)
    (let ((collisions (check-collisions state)))
      ;do something with colliding objects
      (identity collisions))))

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

(defun send-state-to-clients ()
  (let (json clients)
    (with-lock-held (*game-state-lock*)
      (setf json (encode-json-to-string
                  (make-instance 'server-message
                                 :msg-type "state"
                                 :data *global-game-state*))))
    (with-lock-held (*client-db-lock*)
      (setf clients (hash-table-values *connected-clients*)))
    (loop for client in clients
       do (write-to-client-text client json))))
