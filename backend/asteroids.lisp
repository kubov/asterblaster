(in-package :asterblaster)

(defparameter *canvas-w* 600)
(defparameter *canvas-h* 600)
(defparameter *acceleration* 1)
(defparameter *speed-damping-factor* -2/10)
(defparameter *max-speed* 10)

(defparameter *shoot-timeout* 36)

(defparameter *root-degree* 36)
(defparameter *pi* 3.14)

(def class* pos-vector (game-entity)
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

(def class* game-entity () ())

(defmethod print-object ((object game-entity) stream)
  (print-unreadable-object (object stream)
    (format stream "~s ~s" (type-of object)
            (loop for i in (get-slots object)
               collect (cons i (slot-value object i))))))

(defparameter *asteroid-id-seq* 0)

(def class* asteroid (game-entity)
  ((position (get-random-spot) :type pos-vector)
   (speed 0 :type fixnum)
   (id :type fixnum)
   (alive t :type boolean)
   (radius 30 :type fixnum)
   (direction (get-random-spot) :type pos-vector)))

(defun root-of-unity (k)
  (make-instance 'pos-vector 
                 :x (cos (/ (* 2 *pi* k) *root-degree*))
                 :y (sin (/ (* 2 *pi* k) *root-degree*))))

(defun cons-to-pos-vector (p)
  (make-instance 'pos-vector :x (car p) :y (cdr p)))

(def class* player (game-entity)
  ((name "unknown-player" :type string)
   (id :type fixnum)
   (speed 0 :type fixnum)
   (alive t :type boolean)
   (radius 30 :type fixnum)
   (k 0 :type fixnum)
   (rotating? nil :type boolean)
   (rotation-direction nil :type boolean)
   (accelerating? nil :type boolean)
   (direction (make-instance 'pos-vector :x 0 :y 0) :type pos-vector)
   (position (find-free-spot) :type pos-vector)
   (shooting? nil :type boolean)
   (shoot-timeout -1 :type fixnum)))

(defparameter *projectile-id-seq* 0)

(def class* projectile (game-entity)
  ((position (get-standard-spot) :type pos-vector)
   (radius 2 :type fixnum)
   (id :type fixnum)
   (alive t :type boolean)
   (speed 10 :type fixnum)
   (direction (get-standard-spot) :type pos-vector)))

(def class* game-state ()
  ((players (make-hash-table) :type hash-table)
   (asteroids (make-hash-table) :type hash-table)
   (projectiles (make-hash-table) :type hash-table)))

(defparameter *global-game-state* nil)
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
  (let ((state (make-instance 'game-state)))
    (with-slots (asteroids) state
      (loop for i from 0 to 6 do
           (let ((a (make-random-object 'asteroid))
                 (new-id (incf *asteroid-id-seq*)))
             (setf (id-of a) new-id)
             (add-to-hash-table asteroids new-id a))))
    state))


(defun get-object (type id)
  (case type
    (player (gethash id (players-of *global-game-state*)))
    (asteroid (gethash id (asteroids-of *global-game-state*)))
    (projectiles (gethash id (projectiles-of *global-game-state*)))))

(defun add-pos-vectors (vect1 vect2)
  (with-slots ((x1 x) (y1 y)) vect1
    (with-slots ((x2 x) (y2 y)) vect2
      (make-instance 'pos-vector
                     :x (+ x1 x2)
                     :y (+ y1 y2)))))

(defun multiply-by-scalar (vect scalar)
  (with-slots (x y) vect
    (make-instance 'pos-vector
                   :x (* x scalar)
                   :y (* y scalar))))

(defun vector-length (vect)
  (with-slots (x y) vect
    (sqrt (+ (* x x) (* y y)))))

(defun normalize-vector (vect &optional (scalar 1))
  (with-slots (x y) vect
    (let ((len (vector-length vect)))
      (if (zerop len)
          vect
          (multiply-by-scalar vect (/ scalar len))))))

(defun mod-vector (vect)
  (with-slots (x y) vect
    (setf x (truncate x)
          y (truncate y))
    (setf x (mod x *canvas-w*)
          y (mod y *canvas-h*))
    vect))

(defun recalc-pos-vector (current-pos vect speed)
  (let* ((vect (multiply-by-scalar vect speed)))
    (with-slots (x y) current-pos
      (setf (x-of current-pos) (mod (truncate (+ x (x-of vect))) *canvas-w*))
      (setf (y-of current-pos) (mod (truncate (+ y (y-of vect))) *canvas-h* )))))

(defun add-to-hash-table (hash key elem)
  (setf (gethash key hash) elem))

(defun recalc-player (player-id player)
  (declare (ignore player-id))
  (with-slots (rotation-direction k position speed 
                                  direction accelerating? rotating?) 
      player
    (when rotating?
      (setf k (mod
               (if rotation-direction
                   (1+ k) 
                   (1- k))
               *root-degree*)))
    (let* ((vdelta
            (if accelerating?
                (multiply-by-scalar (root-of-unity k) *acceleration*)
                (make-instance 'pos-vector :x 0 :y 0)))
           (new-dir (add-pos-vectors direction vdelta)))
      ;; change velocity according to acceleration
      (setf direction (normalize-vector new-dir
                                        (min (vector-length new-dir)
                                             *max-speed*)))
      ;; slow down because of resistance
      (setf direction (add-pos-vectors direction 
                                       (normalize-vector direction
                                                         *speed-damping-factor*)))
      (setf position (mod-vector (add-pos-vectors position direction))))))

(defun recalc-asteroid (asteroid-id asteroid)
  (declare (ignore asteroid-id))
  (with-slots (position speed direction) asteroid
    (recalc-pos-vector position direction speed)))

(defun recalc-projectile (projectile-id projectile)
  (declare (ignore projectile-id))
  (with-slots (position speed direction) projectile
    (recalc-pos-vector position direction speed)))


(defun recalc-players (players)
  (maphash 'recalc-player players))

(defun recalc-asteroids (asteroids)
  (maphash 'recalc-asteroid asteroids))

(defun recalc-projectiles (projectiles)
  (maphash 'recalc-asteroid projectiles))

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
           collect (list
                    (list key1 (type-of value1))
                    (list key2 (type-of value2))))))

(defun check-collisions (state)
  (with-slots (players asteroids projectiles) state
    (append
     (check-collisions-between players players)
     (check-collisions-between players asteroids)
     (check-collisions-between asteroids projectiles))))

(defun maybe-shoot-projectiles (players projectiles)
  (loop for player being the hash-value in players 
     when (shooting? player)
     do 
       (with-slots (position k shoot-timeout) player
         (cond
           ((zerop shoot-timeout) 
            (let ((new-id (incf *projectile-id-seq*)))
              (setf (gethash new-id projectiles)
                    (make-instance 'projectile
                                   :id new-id
                                   :position position
                                   :direction (root-of-unity k))
                    shoot-timeout *shoot-timeout*)))
           ((> shoot-timeout 0) (decf shoot-timeout))
           (t nil)))))

(defun update-state (state)
  (with-slots (players asteroids projectiles) state
    (recalc-asteroids asteroids)
    (recalc-players players)
    (maybe-shoot-projectiles players projectiles)
    (recalc-projectiles projectiles)
    (let ((collisions (check-collisions state)))
      (with-collisions state collisions))))

(defun objects-from-type (state type)
  (case type
    (player (players-of state))
    (asteroid (asteroids-of state))
    (projectile (projectiles-of state))))

(defun kill-object (type id)
  (with-slots (alive) (get-object type id)
    (setf alive nil))

(defun with-collisions (state col)
  (mapcar #'(lambda (p)
              (let ((first-id (first (first p)))
                    (first-type (second (first p)))
                    (second-id (first (second p)))
                    (second-type (second (second p))))
                (kill-object first-type first-id)
                (kill-object second-type second-id))) col))



(defun handle-player-join (player)
  (with-slots (name id) player
    (with-slots (players) *global-game-state*
      (let ((p (make-instance 'player 
                              :id id
                              :name name
                              :direction (make-instance 'pos-vector
                                                        :x 1
                                                        :y 0))))
        (with-lock-held (*game-state-lock*)
          (add-to-hash-table players id p)))))
  t)

(defun handle-player-leave (msg)
  (with-slots (id) msg
    (with-slots (players) *global-game-state*
      (with-lock-held (*game-state-lock*)
        (remhash id players)))))

(defun handle-player-accelarate (msg)
  (with-slots (id status) msg
    (with-slots (accelerating?) (get-object 'player id)
      (with-lock-held (*game-state-lock*)
        (cond
          ((equal status "down") (setf accelerating? t))
          ((equal status "up") (setf accelerating? nil)))))))

(defun handle-player-rotate (msg)
  (with-slots (id status direction) msg
    (with-lock-held (*game-state-lock*)
      (with-slots (rotating? rotation-direction)
          (get-object 'player id)
        (cond
          ((equal status "down") (setf rotating? t))
          ((equal status "up") (setf rotating? nil)))
        (setf rotation-direction (equal direction "right"))))))

(defun handle-player-shoot (msg)
  (with-slots (id status) msg
    (let ((is-shooting (equal status "down")))
      (with-lock-held (*game-state-lock*) 
        (with-slots (shooting? shoot-timeout) (get-object 'player id)
          (setf shooting? is-shooting
                shoot-timeout (if is-shooting 
                                   (max shoot-timeout 0)
                                   -1)))))))

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
             (handle-player-accelarate msg))
            (user-rotate-message
             (handle-player-rotate msg))
            (user-shot-message
             (handle-player-shoot msg))))))

(defun send-state-to-clients ()
  (loop do
       (sleep 1/30)
       (let (collisions json clients)
         (with-lock-held (*game-state-lock*)
           (setf collisions (update-state *global-game-state*))
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
