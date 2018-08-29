(ns gamma-driver.impl.bind
  (:require
   [gamma-driver.impl.variable :as gv]
   [gamma-driver.impl.resource :as gr]
   [gamma-driver.protocols     :as gp]))

(defn bind-dispatch-fn [element data]
  (if (= :variable (:tag element))
    (cond
      (= :attribute (:storage element)) :attribute
      (and
        (= :uniform (:storage element))
        (= :sampler2D (:type element))) :texture-uniform
      (= :uniform (:storage element)) :uniform)
    (cond
      (= :element-index (:tag element)) :element-index
      (= :variable-array (:tag element)) :variable-array)))


(defmulti bind*
          (fn [fns driver program element data]
            (bind-dispatch-fn element data)))


(defmethod bind* :attribute [fns driver program element input]
  (gp/bind-attribute
    driver
    program
    element
    (gp/array-buffer
      driver
      (let [input (if (map? input) input {:data input})
            data (:data input)]
        (assoc input
          :data (if (.-buffer data)
                  data
                  (js/Float32Array. (clj->js (flatten data))))
          :usage :static-draw
          :element element
          :count (if-let [c (:count input)]
                   c
                   (if (seqable? data)
                     (count data))))))))

(defmethod bind* :uniform [fns driver program element input]
  (gp/bind-uniform
    driver
    program
    element
    (let [input (if (map? input) input {:data input})]
      (assoc input
        :element element
        :data (clj->js (flatten [(:data input)]))))))

(defmethod bind* :element-index [fns driver program element input]
  (let [spec (let [input (if (map? input)
                           input
                           {:data input})]
               (assoc input
                 ;; Probably already flattened, but keeping it here for now
                 :data (js/Uint16Array. (clj->js (flatten (:data input))))
                 :usage :static-draw
                 :element element
                 :count (count (:data input))))]
    (gp/element-array-buffer driver spec)))


(defmethod bind* :texture-uniform [{:keys [texture] :as fns} driver program variable input]
  (gp/bind-texture-uniform
    driver
    program
    variable
    (texture
      driver
      ;; not sure if this is the right logic
      input)))


;; program should do useProgram; basic driver should cache the program

;; (.useProgram (:gl driver) (:program program))

; --- (it does - see gamma-driver.impl.resource/program)

(defn bind
  "
    Binds the given data to
    the given program

  "
  [fns driver program data]
  (let [p (gp/program driver program)]
    (doseq [[k v] data]
      (bind* fns driver p k v))
    p))


;; bind should return the program or the driver?

(comment
  (draw-arrays d (bind xx) {:mode :triangles}))



