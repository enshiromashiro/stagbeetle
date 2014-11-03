(ns stagbeetle.graphics
  (:gen-class)
  (:use [seesaw.core]
        [seesaw.graphics]))

(set! *warn-on-reflection* true)

(import (java.awt.image BufferedImage
                        Raster
                        RenderedImage
                        WritableRaster)
        (java.awt Color
                  Component
                  Graphics
                  Graphics2D))

(defmacro draw-to-raster [^WritableRaster r & body]
  `(let [~'w (.getWidth ~r)
         ~'h (.getHeight ~r)]
     (dotimes [~'y ~'h]
       (dotimes [~'x ~'w]
         ~@body))))

(defn fill-raster [^WritableRaster ras c]
  (draw-to-raster ras
    (.setPixel ras x y (int-array c)))
  ras)

(defn draw-stripe! [^WritableRaster ras
                   i o c1 c2]
  (draw-to-raster ras
                  (if (even? (quot (+ o x) i))
                    (.setPixel ras x y (int-array c1))
                    (.setPixel ras x y (int-array c2))))
  ras)

(defn draw-checker! [^WritableRaster ras
                    sw sh ofx ofy c1 c2]
  (draw-to-raster ras
    (if (even? (quot (+ ofx x) sw))
      (if (even? (quot (+ ofy y) sh))
        (.setPixel ras x y (int-array c1))
        (.setPixel ras x y (int-array c2)))
      (if (even? (quot (+ ofy y) sh))
        (.setPixel ras x y (int-array c2))
        (.setPixel ras x y (int-array c1))))))

(defn draw-line!
  ([^WritableRaster dest ^Raster src dy sy]
     (let [w (.getWidth dest)
           c (int-array (.getNumBands (.getSampleModel dest)))]
       (dotimes [x w]
         (.getPixel src x (int sy) c)
         (.setPixel dest x (int dy) c))))
  ([^WritableRaster dest ^Raster src dy sy off]
     (let [w (.getWidth dest)
           c (int-array (.getNumBands (.getSampleModel dest)))]
       (dotimes [x w]
         (.getPixel src (int (mod (+ off x) w)) (int sy) c)
         (.setPixel dest x (int dy) c)))))

(defn transform-transverse!
  [^WritableRaster dest ^Raster src wfn amp frq phs]
  (let [h (.getHeight dest)
        rad (ref phs)
        step (/ 1 frq)
        pi2 (* Math/PI 2)]
    (dotimes [y h]
      (let [tx (int (* (wfn @rad) amp))]
        (draw-line! dest src y y tx)
        (dosync (alter rad #(mod (+ % step) pi2)))))))

(defn transform-longitudinal! [^WritableRaster dest
                               ^Raster src
                               wfn amp frq phs]
  (let [h (.getHeight dest)
        step (/ 1 frq)
        hamp (int (/ amp 2))
        pi2 (* Math/PI 2)
        dy (ref 0)
        inc-dy (fn [] (dosync (alter dy #(inc %))))
        add-dy (fn [x] (dosync (alter dy #(mod (+ x %) h))))]
    (loop [py 0
           rad phs]
      (when (and (< py h) (< @dy h))
        (let [v (int (+ hamp (* (wfn rad) amp)))]
          (draw-line! dest src @dy py)
          (inc-dy)
          (dotimes [i v]
            (when (< (+ i @dy) h)
              (draw-line! dest src (+ i @dy) py)))
          (add-dy v))
        (recur (inc py) (mod (+ rad step) pi2))))))

