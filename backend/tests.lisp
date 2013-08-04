(in-package :asterblaster)

(defparameter *test-players* (make-hash-table))
(defparameter *test-state* (make-instance 'game-state))

(defun make-test-object (i type)
  (make-instance type
                 :id i
                 :position (get-random-spot)
                 :direction (make-instance 'pos-vector
                                           :x (/
                                               (- (random 20) 10) 2)
                                           :y (/
                                               (- (random 20) 10) 2))
                 :speed (/ (random 11) 4)))

(defun init-test ()
  (with-slots (players projectiles asteroids) *test-state*
    (loop for i from 0 to 3 do
         (add-to-hash-table
          players
          i
          (make-test-object i 'player)))
    (loop for i from 5 to 7 do
         (add-to-hash-table
          projectiles
          i
          (make-test-object i 'projectile)))
    (loop for i from 10 to 13 do
         (add-to-hash-table
          asteroids
          i
          (make-test-object i 'asteroid)))))

(defun multiply-by-scalar (vect scalar)
  (with-slots (x y) vect
    (make-instance 'pos-vector
                   :x (* x scalar)
                   :y (* y scalar))))


(defun draw-object (object size r g b)
  (let ((cord (transform-cords (position-of object))))
    (pal:draw-rectangle
     (pal:v (first cord) (second cord))
     size
     size
     r
     g
     b
     255
     :absolutep nil
     :smoothp nil)))


(defun draw-objects (objects size r g b)
  (loop for val being the hash-value in objects do
       (draw-object val size r g b)))

(defun test-draw ()
  (pal:with-pal (:width *canvas-w* :height *canvas-h* :title "test")
    (pal:event-loop ()
      (pal:clear-screen (pal:color 255 255 0))
      (draw-objects (asteroids-of *test-state*) 20 0 0 255)
      (draw-objects (players-of *test-state*) 10 0 255 0)
      (draw-objects (projectiles-of *test-state*) 5 0 0 0)
      (let ((col (update-state *test-state*)))
        (with-collisions *test-state* col))
      (sleep 1/10))))

(defun run()
  (init-test)
  (test-draw))
