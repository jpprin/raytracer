(in-package :com.godel.lights)

(defstruct (light (:constructor point-light (position intensity)))
  position intensity)


