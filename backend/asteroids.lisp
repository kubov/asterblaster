(in-package :asterblaster)

(defparameter *canvas-w* 600)
(defparameter *canvas-h* 600)
(defparameter *acceleration* 1)
(defparameter *speed-damping-factor* -2/10)
(defparameter *max-speed* 10)

(defparameter *shoot-timeout* 36)
(defparameter *ufo-shoot-timeout* 36)

(defparameter *projectile-ttl* *shoot-timeout*)

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
   (alive? t :type boolean)
   (radius 30 :type fixnum)
   (direction (get-random-spot) :type pos-vector)))

(def class* ufo (game-entity)
  ((position (get-random-spot) :type pos-vector)
   (speed 0 :type fixnum)
   (alive? t :type boolean)
   (radius 10 :type fixnum)
   (shoot-timeout -1 :type fixnum)
   (toggle-timeout 300 :type fixnum)
   (direction (get-random-spot) :type pos-vector)))


(def class* player (game-entity)
  ((name "unknown-player" :type string)
   (id :type fixnum)
   (score 0 :type fixnum)
   (speed 0 :type fixnum)
   (alive? t :type boolean)
   (radius 10 :type fixnum)
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
   (owner 0 :type fixnum)
   (alive? t :type boolean)
   (speed 10 :type fixnum)
   (ttl *projectile-ttl* :type fixnum)
   (direction (get-standard-spot) :type pos-vector)))

(def class* game-state ()
  ((players (make-hash-table) :type hash-table)
   (asteroids (make-hash-table) :type hash-table)
   (projectiles (make-hash-table) :type hash-table)
   (ufo-projectiles (make-hash-table) :type hash-table)
   (ufo)))

(defparameter *global-game-state* nil)
(defparameter *game-state-lock* (bordeaux-threads:make-lock "game state lock"))

(defparameter *test-players* (make-hash-table))
(defparameter *test-state* (make-instance 'game-state))

(defun root-of-unity (k)
  (make-instance 'pos-vector 
                 :x (cos (/ (* 2 *pi* k) *root-degree*))
                 :y (sin (/ (* 2 *pi* k) *root-degree*))))

(defun cons-to-pos-vector (p)
  (make-instance 'pos-vector :x (car p) :y (cdr p)))


(defun make-random-object (type)
  (make-instance type
                 :position (get-random-spot)
                 :direction (make-instance 'pos-vector
                                           :x (/
                                               (- (random 20) 10) 2.0)
                                           :y (/
                                               (- (random 20) 10) 2.0))
                 :speed (+ 0.5 (random 0.5))))


(defun generate-initial-state ()
  (let ((state (make-instance 'game-state)))
    (with-slots (asteroids ufo) state
      (loop for i from 0 to 3 do
           (let ((a (make-random-object 'asteroid))
                 (new-id (incf *asteroid-id-seq*)))
             (setf (id-of a) new-id)
             (add-to-hash-table asteroids new-id a)))
      (setf ufo (make-instance 'ufo 
                               :position (get-random-spot)
                               :direction (make-instance 'pos-vector
                                                         :x (/
                                                             (- (random 20) 10) 2.0)
                                                         :y (/
                                                             (- (random 20) 10) 2.0))
                               :speed (+ 0.5 (random 0.5))
                               :alive? nil)))
    state))


(defun get-object (type id)
  (case type
    (player (gethash id (players-of *global-game-state*)))
    (asteroid (gethash id (asteroids-of *global-game-state*)))
    (projectile (gethash id (projectiles-of *global-game-state*)))))

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

(defun complex-multiply (vect1 vect2)
  (with-slots ((x1 x) (y1 y)) vect1
    (with-slots ((x2 x) (y2 y)) vect2
      (let ((c (* (complex x1 y1) (complex x2 y2))))
        (make-instance 'pos-vector
                       :x (realpart c)
                       :y (imagpart c))))))

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
      (setf direction (if (> (vector-length direction) (abs *speed-damping-factor*)) 
                          (add-pos-vectors direction 
                                           (normalize-vector direction
                                                             *speed-damping-factor*))
                          (make-instance 'pos-vector :x 0 :y 0)))
      (setf position (mod-vector (add-pos-vectors position direction))))))

(defun recalc-asteroid (asteroid-id asteroid)
  (declare (ignore asteroid-id))
  (with-slots (position direction) asteroid
    (setf position (mod-vector (add-pos-vectors position direction)))))

(defun recalc-projectile (projectile-id projectile)
  (declare (ignore projectile-id))
  (with-slots (alive? position speed direction ttl) projectile
    (recalc-pos-vector position direction speed)
    (decf ttl)
    (when (minusp ttl)
      (setf alive? nil))))


(defun recalc-players (players)
  (maphash 'recalc-player players))

(defun recalc-asteroids (asteroids)
  (maphash 'recalc-asteroid asteroids))

(defun recalc-projectiles (projectiles)
  (maphash 'recalc-projectile projectiles))

(defun square (x)
  (* x x))

(defun diff (x y)
  (- x y))

(defun distance (x1 y1 x2 y2)
  (sqrt (+
         (square (diff x1 x2))
         (square (diff y1 y2)))))

(defun colliding? (obj1 obj2)
  (unless (and (eq obj1 obj2) (alive? obj1) (alive? obj2))
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

(defun find-collisions-between (list1 list2)
  (loop for value1 in list1
     when (alive? value1)
     append (loop for value2 in list2
               when (and (alive? value2)  (colliding? value1 value2))
               collect (list value1
                             value2))))

(defun find-collisions (state)
  (with-slots (players asteroids projectiles ufo ufo-projectiles) state
    (let ((player-asteroid-collisions
           (find-collisions-between 
            (hash-table-values players) 
            (hash-table-values asteroids)))
          (asteroid-projectile-collissions 
           (find-collisions-between (hash-table-values asteroids) 
                                    (append (hash-table-values ufo-projectiles)
                                            (hash-table-values projectiles))))
          (player-ufo-projectile-collisions
           (find-collisions-between (hash-table-values players)
                                    (hash-table-values ufo-projectiles)))
          (ufo-player-projectile-collisions
           (find-collisions-between (list ufo)
                                    (hash-table-values projectiles))))
      (loop for col in player-asteroid-collisions
         do (destructuring-bind (player asteroid) col
              (setf (alive? player) nil)
              (break-asteroid state asteroid)))
      (loop for col in asteroid-projectile-collissions
         do (destructuring-bind (asteroid projectile) col
              (setf (alive? projectile) nil)
              (unless (minusp (owner-of projectile))
                ;; we don't score ufo
                (incf (score-of (get-object 'player (owner-of projectile)))))
              (break-asteroid state asteroid)))
      (loop for col in player-ufo-projectile-collisions
         do (destructuring-bind (player projectile) col
              (setf (alive? player) nil)
              (setf (alive? projectile) nil)))
      (loop for col in ufo-player-projectile-collisions
         do (destructuring-bind (ufo projectile) col
              (toggle-ufo ufo)
              (incf (score-of (get-object 'player (owner-of projectile))) 10))) 
      #+nil(nconc player-asteroid-collisions asteroid-projectile-collissions))))

(defun break-asteroid (state asteroid)
  (setf (alive? asteroid) nil)
  (make-two-smaller-asteroids state asteroid))

(defun make-two-smaller-asteroids (state old-asteroid)
  (with-slots (radius position speed direction) old-asteroid
    (when (<= radius 10)
      (return-from make-two-smaller-asteroids))
    (let* ((new-radius (- radius 10))
           (new-k (random *root-degree*))
           (other-k (random *root-degree*))
           (id1 (incf *asteroid-id-seq*)) 
           (id2 (incf *asteroid-id-seq*)))
      (setf (gethash id1 (asteroids-of state))
            (make-instance 'asteroid
                           :id id1
                           :position position
                           :radius new-radius
                           :speed speed
                           :direction (complex-multiply direction
                                                        (root-of-unity new-k)))
            (gethash id2 (asteroids-of state))
            (make-instance 'asteroid
                           :id id2
                           :position position
                           :radius new-radius
                           :speed speed
                           :direction (complex-multiply direction
                                                        (root-of-unity other-k)))))))


(defun maybe-shoot-projectiles (players projectiles)
  (loop for player being the hash-value in players 
     when (and (alive? player) (shooting? player))
     do 
       (with-slots (position k shoot-timeout id) player
         (cond
           ((zerop shoot-timeout) 
            (let ((new-id (incf *projectile-id-seq*)))
              (setf (gethash new-id projectiles)
                    (make-instance 'projectile
                                   :id new-id
                                   :owner id
                                   :position (make-instance 'pos-vector
                                                            :x (x-of position)
                                                            :y (y-of position))
                                   :direction (root-of-unity k))
                    shoot-timeout *shoot-timeout*)))
           ((> shoot-timeout 0) (decf shoot-timeout))
           (t nil)))))

(defun recalc-ufo (ufo ufo-projectiles)
  (with-slots (position direction shoot-timeout) ufo
    (setf position (mod-vector (add-pos-vectors position direction)))
    (unless (minusp shoot-timeout)
      (if (zerop shoot-timeout)
          (let ((new-id (incf *projectile-id-seq*)))
            (setf (gethash new-id ufo-projectiles)
                  (make-instance 'projectile
                                 :id new-id
                                 :owner -1 ;;ufo
                                 :position (make-instance 'pos-vector
                                                            :x (x-of position)
                                                            :y (y-of position))
                                 :direction (root-of-unity (random *root-degree*))))
            (setf shoot-timeout *ufo-shoot-timeout*))
          (decf shoot-timeout)))))

(defun toggle-ufo (ufo)
  (with-slots (alive? shoot-timeout toggle-timeout) ufo
    (if alive?
        (progn 
          (setf alive? nil
                toggle-timeout 300
                shoot-timeout -1))
        (progn 
          (setf alive? t
                toggle-timeout 150
                shoot-timeout *ufo-shoot-timeout*)))))

(defun update-state (state)
  (with-slots (players asteroids projectiles ufo-projectiles ufo) 
      state
    (recalc-asteroids asteroids)
    (recalc-players players)
    (maybe-shoot-projectiles players projectiles)
    (recalc-projectiles projectiles)
    (recalc-projectiles ufo-projectiles)
    (recalc-ufo ufo ufo-projectiles)
    (find-collisions state)))

(defun objects-from-type (state type)
  (case type
    (player (players-of state))
    (asteroid (asteroids-of state))
    (projectile (projectiles-of state))))

(defun kill-object (type id)
  (with-slots (alive?) (get-object type id)
    (setf alive? nil)))


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
           (when (or
                  (zerop (count-if #'alive? 
                                   (hash-table-values (players-of *global-game-state*))))
                  (zerop (count-if #'alive?
                                   (hash-table-values (asteroids-of *global-game-state*)))))
               ;; no alive players, reset game
             (let ((p (players-of *global-game-state*))) ;; save old players
               (setf *global-game-state* (generate-initial-state))
               (setf (players-of *global-game-state*) p)
               (loop for player being the hash-value in p
                  do (with-slots (alive? rotating? accelerating? direction) player
                         (setf alive? t
                               direction (make-instance 'pos-vector
                                                        :x 0.0
                                                        :y 0.0)
                               rotating? nil
                               accelerating? nil)
                         (send-to-client (get-client-by-id (id-of player))
                                         (make-server-message 'hello-reply-message
                                                              :id (id-of player)))))
               (sleep 2)))
           ;; toggle ufo?
           (if (zerop (toggle-timeout-of (ufo-of *global-game-state*)))
               (toggle-ufo (ufo-of *global-game-state*))
               (decf (toggle-timeout-of (ufo-of *global-game-state*))))
           
           (with-slots (players asteroids projectiles ufo-projectiles ufo) 
               *global-game-state*
             (setf collisions (update-state *global-game-state*))
             (setf json (encode-json-to-string
                         (make-server-message 'state-server-message
                                              :players players
                                              :asteroids asteroids
                                              :projectiles projectiles
                                              :ufo-projectiles ufo-projectiles
                                              :ufo ufo
                                              :collisions '())))))
         (with-lock-held (*client-db-lock*)
           (setf clients (hash-table-values *connected-clients*)))
         (loop for client in clients
               do (write-to-client-text client json)))))
