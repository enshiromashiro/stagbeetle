(ns stagbeetle.core
  (:gen-class)
  (:use [seesaw.core]
        [seesaw.graphics]
        [stagbeetle.graphics]))

(set! *warn-on-reflection* true)

(import (java.awt.image BufferedImage
                        Raster
                        WritableRaster)
        (java.awt Component
                  Graphics
                  Graphics2D))


(def +fps+ 30)
(def +width+ 320)
(def +height+ 240)

(def frm (frame :title "stagbeetle"))
(def cnv (canvas))
(def ^BufferedImage img
  (buffered-image +width+ +height+ BufferedImage/TYPE_INT_ARGB))

(defn paint [^Component c ^Graphics g]
  (try (.drawImage g img 0 0 nil)))


;;; drawing thread
(def +stop+ (ref true))
(defn stop [] (dosync (ref-set +stop+ true)))
(defmacro on-drawing-thread [& body]
  `(do (dosync (alter +stop+ (constantly false)))
       (.start (Thread.
                #(while (false? @+stop+)
                   ~@body
                   (repaint! cnv)
                   (Thread/sleep (/ 1000 +fps+)))))))

(defn init []
  (native!)
  (config! cnv :size [+width+ :by +height+])
  (config! cnv :paint paint)
  (config! frm :content cnv))

(defn show []
  (-> frm pack! show!))

(defn draw-wave []
  (draw-stripe! (.getRaster img) 20 0 [255 255 255 255] [90 170 255 255])
  (repaint! frm)
  (Thread/sleep 1000)
  (transform-transverse! (.getRaster img) (.getData img)
                         #(Math/sin %) 20 10 0)
  (repaint! frm)
  (Thread/sleep 1000)
  (let [^WritableRaster des (.getRaster img)
        ^Raster src (.getData img)
        ph (ref 0)]
    (on-drawing-thread
     (transform-longitudinal! des src #(Math/sin %) 3 5 @ph)
     (dosync (alter ph #(mod (+ % 0.1) 6.28))))))


(defn -main
  [& args]
  (init)
  (config! frm :on-close :exit)
  (draw-wave)
  (show))
