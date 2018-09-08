(ns gamma-driver.impl.resource
  (:require
    [gamma-driver.gl-enums :as gle]))

(defn shader [gl spec]
  (let [s (.createShader
                 gl
                 ({:vertex-shader gle/VERTEX_SHADER
                   :fragment-shader gle/FRAGMENT_SHADER}
                   (:tag spec)))]
    (if s
      (do
        (.shaderSource gl s (:glsl spec))
        (.compileShader gl s)
        (let [compiled (.getShaderParameter gl s gle/COMPILE_STATUS)]
          (if compiled
            (assoc spec (:tag spec) s)
            (throw (js/Error. (str "failed to compile " (name (:tag spec)) ":"
                                   (.getShaderInfoLog gl s)))))))
      (throw (js/Error. (str "Unable to create " (name (:tag spec))))))))


(defn program [gl spec]
  (if (:program spec)
    (do
      (.useProgram gl (:program spec))
      spec)
    (let [v (shader gl (assoc (:vertex-shader spec) :tag :vertex-shader))
          f (shader gl (assoc (:fragment-shader spec) :tag :fragment-shader))
          p (.createProgram gl)]
     (.attachShader gl p (:vertex-shader v))
     (.attachShader gl p (:fragment-shader f))
     (.linkProgram gl p)
     (if (.getProgramParameter gl p gle/LINK_STATUS)
       (do
         (.useProgram gl p)
         (assoc spec :program p
                    :vertex-shader v
                    :fragment-shader f))
       (throw
         (js/Error.
           (str "failed to link program: "
                (.getProgramInfoLog gl p))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defn array-buffer [gl spec]
  (let [buffer (or (:array-buffer spec) (.createBuffer gl))]
    (.bindBuffer gl gle/ARRAY_BUFFER buffer)
    (.bufferData
     gl
     gle/ARRAY_BUFFER
     (:data spec)
     (or ({:static-draw gle/STATIC_DRAW :dynamic-draw gle/DYNAMIC_DRAW} (:usage spec))
         gle/STATIC_DRAW))
    (assoc spec :array-buffer buffer)))




(defn element-array-buffer [gl spec]
  (let [buffer (or (:element-array-buffer spec) (.createBuffer gl))]
    (.bindBuffer gl gle/ELEMENT_ARRAY_BUFFER buffer)
    (.bufferData
      gl
      gle/ELEMENT_ARRAY_BUFFER
      (:data spec)
      (or ({:static-draw gle/STATIC_DRAW :dynamic-draw gle/DYNAMIC_DRAW} (:usage spec))
          gle/STATIC_DRAW))
    (assoc spec :element-array-buffer buffer)))


;; parts of creating texture
;; internal texture specification
;; input data specification: color format, type, image

(defn- texture-unpack [gl spec]
  (let [{:keys [flip-y]} spec]
    (if (not (nil? flip-y))
      (.pixelStorei gl gle/UNPACK_FLIP_Y_WEBGL flip-y))))


(def texture-wrap-constants
  {:repeat gle/REPEAT
   :clamp-to-edge gle/CLAMP_TO_EDGE
   :mirrored-repeat gle/MIRRORED_REPEAT})


(defn- texture-wrap [gl spec]
  (let [{:keys [s t]} spec]
    (if s
      (.texParameteri
        gl
        gle/TEXTURE_2D
        gle/TEXTURE_WRAP_S
        (texture-wrap-constants s)))
    (if t
      (.texParameteri
        gl
        gle/TEXTURE_2D
        gle/TEXTURE_WRAP_T
        (texture-wrap-constants t)))))


(def texture-filter-constants
  {:linear gle/LINEAR
   :nearest gle/NEAREST
   :nearest-mipmap-nearest gle/NEAREST_MIPMAP_NEAREST
   :linear-mipmap-nearest gle/LINEAR_MIPMAP_NEAREST
   :nearest-mipmap-linear gle/NEAREST_MIPMAP_LINEAR
   :linear-mipmap-linear gle/LINEAR_MIPMAP_LINEAR})

(defn- texture-filter [gl spec]
  (let [{:keys [min mag]} spec]
    (if min
      (.texParameteri
        gl
        gle/TEXTURE_2D
        gle/TEXTURE_MIN_FILTER
        (texture-filter-constants min)))
    (if mag
      (.texParameteri
        gl
        gle/TEXTURE_2D
        gle/TEXTURE_MAG_FILTER
        (texture-filter-constants mag)))))

(defn texture-data-type [d]
  ;; ImageData | HTMLImageElement | HTMLCanvasElement | HTMLVideoElement
  (if (or (instance? js/ImageData d)
          (instance? js/HTMLImageElement d)
          (instance? js/HTMLCanvasElement d)
          (instance? js/HTMLVideoElement d))
    :image
    (if (instance? js/Float32Array d)
      :pixels
      (throw (js/Error. (str "texture data type not supported: " (pr-str d)) ))))
  ;; arraybufferview
  )

(defn texture [gl {:keys [texture-id format internal-format type data texture width height unpack filter wrap] :as spec}]
  ;(println "tex>" texture-id)
  (if (and false texture)
    spec
    (let [
            tex (.createTexture gl)
         ]
       (texture-unpack gl unpack)
       (.activeTexture gl (+ gle/TEXTURE0 texture-id))
       (.bindTexture gl gle/TEXTURE_2D tex)
       (texture-wrap gl wrap)
       (texture-filter gl filter)
       (case (texture-data-type data)
         :image  (.texImage2D gl gle/TEXTURE_2D 0 internal-format format type data)
         :pixels (.texImage2D gl gle/TEXTURE_2D 0 internal-format width height 0 format type data 0))
       (.bindTexture gl gle/TEXTURE_2D nil)
       (assoc spec :tag :texture :texture tex))))

(def renderbuffer-formats
  {:depth-component16 gle/DEPTH_COMPONENT16
   :rgba4 gle/RGBA4
   :rgb5-a1 gle/RGB5_A1
   :rgb565 gle/RGB565
   :stencil-index8 gle/STENCIL_INDEX8})

(defn render-buffer [gl spec]
  (let [rb (.createRenderbuffer gl)
        {:keys [width height format]} spec]
    (.bindRenderbuffer gl gle/RENDERBUFFER rb)
    (.renderbufferStorage
      gl
      gle/RENDERBUFFER
      (renderbuffer-formats format)
      width
      height)
    (assoc spec :tag :render-buffer :render-buffer rb)))


(defn frame-buffer-attachment [gl fb [attachment attachment-point]]
  (case (:tag attachment)
    :texture
    (.framebufferTexture2D
      gl
      gle/FRAMEBUFFER
      attachment-point
      gle/TEXTURE_2D
      (:texture attachment)
      0)

    :render-buffer
    (.framebufferRenderbuffer
      gl
      gle/FRAMEBUFFER
      attachment-point
      gle/RENDERBUFFER
      (:render-buffer attachment))))

(defn frame-buffer [gl spec]
  (let [fb (.createFramebuffer gl)
        {:keys [color depth stencil depth-stencil]} spec]
    (.bindFramebuffer gl gle/FRAMEBUFFER fb)
    (dorun
      (map
       #(frame-buffer-attachment gl fb %)
       (filter
         first
         [[color gle/COLOR_ATTACHMENT0]
         [depth gle/DEPTH_ATTACHMENT]
         [stencil gle/STENCIL_ATTACHMENT]
         [depth-stencil gle/DEPTH_STENCIL_ATTACHMENT]])))

    ;(println (.checkFramebufferStatus gl gle/FRAMEBUFFER)
    (.bindFramebuffer gl gle/FRAMEBUFFER nil)
    (assoc spec :tag :frame-buffer :frame-buffer fb)))




(defn release [gl spec]
  (case (:tag spec)
    :program (.deleteProgram gl (:program spec))
    :array-buffer (.deleteBuffer gl (:array-buffer spec))
    :element-array-buffer (.deleteBuffer gl (:element-array-buffer spec))
    :frame-buffer (.deleteFramebuffer gl (:frame-buffer spec))
    :render-buffer (.deleteRenderbuffer gl (:render-buffer spec))
    :vertex-shader (.deleteShader gl (:vertex-shader spec))
    :fragment-shader (.deleteShader gl (:fragment-shader spec))
    :texture (.deleteTexture gl(:texture spec))))

