(ns gamma.webgl.drivers.model
  (:require [gamma.webgl.shader :as shader]
            [gamma.webgl.platform.constants :as c]))























(comment
  (def a (atom {}))

  (def r (->Root a nil))

  (swap! a merge {:arraybuffers (->Arraybuffers r (atom {}))
                  :bindings (->GlobalBindings r (atom {}))
                  ;:texture-units (texture-units r)
                  :programs (->Programs r (atom {}))})



  )


(comment

  (extend-protocol
    IPrintWithWriter
    Atom
    (-pr-writer [a writer opts]
      (-write writer "#object [cljs.core.Atom ")
      (-write writer "]")))

  (atom nil)


  (require '[gamma.api :as g])
  (require '[gamma.webgl.platform.constants :as c])

  (def pos (g/attribute "posAttr" :vec2))

  (defn example-shader []
    (shader/compile
      {:id              :hello-triangle
       :vertex-shader   {(g/gl-position) (g/vec4 pos 0 1)}
       :fragment-shader {(g/gl-frag-color) (g/vec4 1 0 0 1)}}))

  ;; Helpers
  (defn get-context [id]
    (.getContext
      (.getElementById js/document id)
      "webgl"))

  (def s (example-shader))




  (defn default-layout [attribute]
    {:normalized? false
     :size        ({:float 1 :vec2 2 :vec3 3 :vec4 4}
                    (:type attribute))
     :type        (c/constants ::c/float)
     :offset      0
     :stride      0})


  (require 'gamma.webgl.api)

  (def ab (gamma.webgl.api/arraybuffer))
  (def x [[0 0] [0 1] [-1 1]])

  (def gl (get-context "gl-canvas"))
  (def model (root (atom {}) gl))

  (gamma.webgl.shader/install-shader (:gl model) s)



  (resolve-in model [:arraybuffers ab :object])
  (resolve-in model [:arraybuffers ab :data])
  (resolve-in model [:programs s :object])

  (conform
    (resolve-in model [:arraybuffers ab])
    {:data x})


  (conform model {:bindings {:program s}})

  (resolve model :arraybuffers)

  ;; isn't creating the ab


  (do
    (conform model {:arraybuffers {ab {:data x}}})
    (conform
      model
      {:programs
       {s {:attributes {pos {:arraybuffer ab
                             :layout      (default-layout pos)}}}}})
    (conform model {:bindings {:program s}})
    (.drawArrays
      gl (c/constants ::c/triangles) 0 3)

    )



  )