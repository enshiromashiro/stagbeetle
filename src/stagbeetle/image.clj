(ns stagbeetle.image
  (:require [clojure.java.io :as io])
  (:import (javax.imageio ImageIO)))


;;;; to-raster utilities

(defn read-image [path]
  (.getData (ImageIO/read (io/resource path))))
