(in-package :asterblaster)

(defparameter *canvas-w* 600)
(defparameter *canvas-h* 600)

(def class* pos-vector ()
  ((x 0 :type fixnum)
   (y 0 :type fixnum)))

(defun get-random-spot ()
  (make-instance 'pos-vector
                 :x (random *canvas-w*)
                 :y (random *canvas-h*)))

(defun find-free-spot ()
  ;;stub
  (get-random-spot))

(defun get-standard-spot ()
  (make-instance 'pos-vector :x 0 :y 0))

(def class* asteroid ()
  ((position (get-random-spot) :type pos-vector)
   (speed 0 :type fixnum)
   (radius 60 :type fixnum)
   (direction (get-random-spot) :type pos-vector)
   (size :type fixnum)))

(def class* player ()
  ((name :type string)
   (speed 0 :type fixnum)
   (radius 60 :type fixnum)
   (accelerating? nil :type boolean)
   (direction (get-standard-spot) :type pos-vector)
   (position (find-free-spot) :type pos-vector)))

(def class* projectile ()
  ((position (get-standard-spot) :type pos-vector)
   (radius 2 :type fixnum)
   (speed 10 :type fixnum)
   (direction (get-standard-spot) :type pos-vector)))

(def class* game-state ()
  ((players (make-hash-table) :type hash-table)
   (asteroids (make-hash-table) :type hash-table)
   (projectiles (make-hash-table) :type hash-table)))

(defparameter *global-game-state* (make-instance 'game-state))
(defparameter *game-state-lock* (bordeaux-threads:make-lock "game state lock"))

(defparameter *test-players* (make-hash-table))
(defparameter *test-state* (make-instance 'game-state))


(defun make-random-object (type)
  (make-instance type
                 :position (get-random-spot)
                 :direction (make-instance 'pos-vector
                                           :x (/
                                               (- (random 20) 10) 2)
                                           :y (/
                                               (- (random 20) 10) 2))
                 :speed (/ (random 11) 4)))


(defun generate-initial-state ()
  (with-slots (asteroids) *global-game-state*
    (loop for i from 0 to 6 do
         (add-to-hash-table
          asteroids
          i
          (make-random-object 'asteroid)))))


(defun get-object (type id)
  (case type
    (player (gethash id (players-of *global-game-state*)))
    (asteroid (gethash id (asteroids-of *global-game-state*)))
    (projectiles (gethash id (projectiles-of *global-game-state*)))))

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
  (with-slots (position speed direction accelerating?) player
    (recalc-pos-vector position direction speed)
    (if accelerating?
        (incf speed 1)
        (setf speed (if (>= speed 0.5)
                        (- speed 0.5)
                        0)))))

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
  (unless (eq obj1 obj2)
    (with-slots ((pos1 position) (r1 radius)) obj1
      (with-slots ((pos2 position) (r2 radius)) obj2
        (with-slots ((x1 x) (y1 y)) pos1
          (with-slots ((x2 x) (y2 y)) pos2
            (< (distance x1 y1 x2 y2) (+ r1 r2))))))))

(defun transform-cords (position)
  (with-slots (x y) position
    (list
     (mod (+ (/ *canvas-w* 2) x) *canvas-h*)
     (mod (+ (/ *canvas-h* 2) y) *canvas-w*))))

(defun check-collisions-between (hash1 hash2)
  (loop for key1 being the hash-key in hash1
     for value1 being the hash-value in hash1
     append (loop for key2 being the hash-key in hash2
          for value2 being the hash-value in hash2
          when (colliding? value1 value2)
           collect (cons
                    (cons value1 (type-of value1))
                    (cons value2 (type-of value2))))))

(defun check-collisions (state)
  (with-slots (players asteroids) state
    (append
     (check-collisions-between players players)
     (check-collisions-between players asteroids))))

(defun update-state (state)
  (with-slots (players asteroids projectiles) state
    (recalc-asteroids asteroids)
    (recalc-players players)
    (recalc-projectiles projectiles)
    (let ((collisions (check-collisions state)))
                                        ;do something with colliding objects
      collisions)))

(defun handle-player-join (player)
  (with-slots (name id) player
    (with-slots (players) *global-game-state*
      (let ((p (make-instance 'player :name name)))
        (add-to-hash-table players id p))))
  t)

(defun handle-player-leave (msg)
  (with-slots (id) msg
    (with-slots (players) *global-game-state*
      (remhash id players))))

(defun handle-player-accelarate (msg)
  (with-slots (id status) msg
    (with-slots (accelerating?) (get-object'player id)
      (cond
        ((equal status "down") (setf accelerating? t))
        ((equal status "up") (setf accelerating? nil))))))

(defun handle-player-rotate (msg))

(defun update-game-state ()
  (block handler
    ;; remember about locking!
    (loop for msg = (recv *update-state-channel*)
       do (ecase (type-of msg)
            (stop-message
             (return-from handler))
            (user-join-message
             (format t "handling join~&")
             (handle-player-join msg))
            (user-leave-message         ; TODO: implement
             (handle-player-leave msg))
            (user-accelerate-message
             t)
            (user-rotate-message
             t)))))

(defun send-state-to-clients ()
  (loop do
       (sleep 1/4)
       (let (json clients)
         (with-lock-held (*game-state-lock*)
           (with-slots (players asteroids projectiles) *global-game-state*
             (setf json (encode-json-to-string
                         (make-server-message 'state-server-message
                                              :players players
                                              :asteroids asteroids
                                              :projectiles projectiles
                                              :collisions '())))))
         (with-lock-held (*client-db-lock*)
           (setf clients (hash-table-values *connected-clients*)))
         (loop for client in clients
            do (write-to-client-text client json)))))
