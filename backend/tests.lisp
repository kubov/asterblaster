(in-package :asterblaster)

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


(defun draw-asteroid (nothing asteroid)
  (let ((cord (f (position-of asteroid))))
    (format t "~A ~A~%" (first cord) (second cord))
    (pal:draw-rectangle
     (pal:v 100 100)
     10
     10
     0
     0
     0
     255
     :absolutep nil
     :smoothp nil)))


(defun draw-asteroids (asteroids)
  (maphash 'draw-asteroid asteroids))

(defun test-draw ()
  (pal:with-pal (:width *canvas-w* :height *canvas-h* :title "test")
    (pal:event-loop ()
      (pal:clear-screen (pal:color 255 0 0))
      (draw-asteroids (asteroids-of *test-state*))
     (sleep 1))))
