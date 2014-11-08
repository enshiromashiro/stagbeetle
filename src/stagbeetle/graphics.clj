(ns stagbeetle.graphics
  (:gen-class)
  (:use [seesaw.core]
        [seesaw.graphics]))

(import (java.awt.image BufferedImage
                        Raster
                        RenderedImage
                        WritableRaster)
        (java.awt Color
                  Component
                  Graphics
                  Graphics2D))


;;;; to-raster utilities

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


;;;; drawing

(defn draw!
  [^WritableRaster dest ^Raster ras dx dy]
  (let [dw (.getWidth dest)
        dh (.getHeight dest)
        rw (.getWidth ras)
        rh (.getHeight ras)
        x-init (if (neg? dx) (Math/abs (int dx)) 0)
        y-init (if (neg? dy) (Math/abs (int dy)) 0)
        x-end (if (> (+ dx rw) dw) (- dw dx) rw)
        y-end (if (> (+ dy rh) dh) (- dh dy) rh)
        x (atom x-init)
        y (atom y-init)
        c (int-array (.getNumBands (.getSampleModel dest)))]
    (while (< @y y-end)
      (while (< @x x-end)
        (.getPixel ras (int @x) (int @y) c)
        (.setPixel dest (int (+ @x dx)) (int (+ @y dy)) c)
        (swap! x inc))
      (reset! x x-init)
      (swap! y inc))))

(defn tile!
  [^WritableRaster dest ^Raster ras ofx ofy]
  (let [dw (.getWidth dest)
        dh (.getHeight dest)
        rw (.getWidth ras)
        rh (.getHeight ras)]
    (doseq [y (range (- (Math/abs (int ofy)))
                     (int (+ (* 2 rh) (quot dh rh))) rh)]
      (doseq [x (range (- (Math/abs (int ofx)))
                       (int (+ (* 2 rw) (quot dw rw))) rw)]
        (draw! dest ras x y)))))


;;;; transforms

(defn translate!
  [^WritableRaster dest ^Raster src ofx ofy]
  (let [w (.getWidth dest)
        h (.getHeight dest)]
    (dotimes [y h]
      (draw-line! dest src (mod (+ y ofy) h) y ofx))))

(defn transform-transverse!
  [^WritableRaster dest ^Raster src wfn amp frq phs]
  (let [h (.getHeight dest)
        rad (atom phs)
        step (/ 1 frq)
        pi2 (* Math/PI 2)]
    (dotimes [y h]
      (let [tx (int (* (wfn @rad) amp))]
        (draw-line! dest src y y tx)
        (swap! rad #(mod (+ % step) pi2))))))

(defn transform-transverse-alternate!
  [^WritableRaster dest ^Raster src wfn amp frq phs]
  (let [h (.getHeight dest)
        rad (atom phs)
        step (/ 1 frq)
        pi2 (* Math/PI 2)]
    (dotimes [y h]
      (let [tx (int (* (wfn @rad) amp))]
        (draw-line! dest src y y (if (even? y) tx (- tx)))
        (swap! rad #(mod (+ % step) pi2))))))

(defn transform-longitudinal! [^WritableRaster dest
                               ^Raster src
                               wfn amp frq phs]
  (let [h (.getHeight dest)
        step (/ 1 frq)
        hamp (int (/ amp 2))
        pi2 (* Math/PI 2)
        dy (atom 0)
        inc-dy (fn []  (swap! dy #(inc %)))
        add-dy (fn [x] (swap! dy #(mod (+ x %) h)))]
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

