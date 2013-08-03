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
