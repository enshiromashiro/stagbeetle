(ns stagbeetle.core
  (:gen-class)
  (:use [seesaw.core]
        [seesaw.graphics]
        [stagbeetle.graphics]
        [stagbeetle.image]))

(import (java.awt.image BufferedImage
                        Raster
                        WritableRaster)
        (java.awt Component
                  Graphics
                  Graphics2D))


(def +fps+ 30)
(def +width+ 640)
(def +height+ 480)

(def frm (frame :title "stagbeetle"
                :resizable? false))
(def cnv (canvas))
(def ^BufferedImage img
  (buffered-image (int (/ +width+ 2)) (int (/ +height+ 2))
                  BufferedImage/TYPE_INT_ARGB))

(defn paint [^Component c ^Graphics g]
  (try (.drawImage g img 0 0 +width+ +height+
                   0 0 (int (/ +width+ 2)) (int (/ +height+ 2)) nil)))


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
  (let [png (read-image "img01.png")
        r (.getRaster img)
        w (.getWidth ^Raster png)
        h (.getHeight ^Raster png)
        ofx (atom 0)
        ofy (atom 0)]
    (on-drawing-thread
     (tile! r png @ofx @ofy)
     (transform-transverse-alternate! r (.getData img)
                                      #(Math/sin %) 15 20 0)
     (reset! ofx (mod (inc @ofx) w))
     (reset! ofy (mod (inc @ofy) h)))))


(defn -main
  [& args]
  (init)
  (config! frm :on-close :exit)
  (draw-wave)
  (show))
