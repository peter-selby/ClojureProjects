;;; Borrowed from
;;; http://stackoverflow.com/questions/2792451/improving-my-first-clojure-program?rq=1

(ns a-ui-app.core
  (:gen-class))

(import [java.awt    Color  Dimension  event.KeyListener])
(import [javax.swing JFrame JPanel                      ])

(def x (ref 0))
(def y (ref 0))

#_(def panel
  (proxy [JPanel KeyListener] []
    (getPreferredSize [] (Dimension. 100 100))
    (keyPressed [e]
      (let [keyCode (.getKeyCode e)]
        (if (== 37 keyCode) (dosync (alter x dec))
        (if (== 38 keyCode) (dosync (alter y dec))
        (if (== 39 keyCode) (dosync (alter x inc))
        (if (== 40 keyCode) (dosync (alter y inc))
                            (println keyCode)))))))
    (keyReleased [e])
    (keyTyped [e])))

(def panel
  (proxy [JPanel KeyListener] []
    (getPreferredSize [] (Dimension. 100 100))
    (keyPressed [e]
      (let [keyCode (.getKeyCode e)]
        (dosync
         (apply alter
           (case keyCode
             37 [x dec]
             38 [y dec]
             39 [x inc]
             40 [y inc])))
        ;(println keyCode)
        ))
    (keyReleased [e])
    (keyTyped [e])))

(def frame (JFrame. "Test"))

(defn drawRectangle [p]
  (doto (.getGraphics p)  
    (.setColor (java.awt.Color/WHITE))
    (.fillRect 0 0 100 100)
    (.setColor (java.awt.Color/BLUE))
    (.fillRect (* 10 (deref x)) (* 10 (deref y)) 10 10)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!")
  (doto panel
    (.setFocusable true)
    (.addKeyListener panel))

  (doto frame
    (.add panel)
    (.pack)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setVisible true))

  (loop []
    (drawRectangle panel)
    (Thread/sleep 10)
    (recur))
  )

