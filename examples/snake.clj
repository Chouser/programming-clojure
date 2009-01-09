; Inspired by the snakes that have gone before:
; Abhishek Reddy's snake: http://www.plt1.com/1070/even-smaller-snake/
; Mark Volkmann's snake: http://www.ociweb.com/mark/programming/ClojureSnake.html 
(ns examples.snake
  (:gen-class :extends java.applet.Applet :main true)
  (:import (java.awt Color Graphics2D Container)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener KeyEvent))
  (:use clojure.contrib.import-static
	[clojure.contrib.seq-utils :only (includes?)]))
(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)

; Game board and coordinates. points are [x,y] vectors 
(def width 75)
(def height 50)
(def point-size 10)
(def turn-millis 75)
(def win-length 5)

(defn add-points [& pts] 
  (vec (apply map + pts)))

(defn point-to-screen-rect [pt] 
  (map #(* point-size %) 
       [(pt 0) (pt 1) 1 1]))

(def dirs { VK_LEFT  [-1  0] 
            VK_RIGHT [ 1  0]
            VK_UP    [ 0 -1] 
	    VK_DOWN  [ 0  1]})

; apple
(defn create-apple [] 
  {:location [(rand-int width) (rand-int height)]
   :color (Color. 210 50 90)
   :type :apple}) 

; snake
(defn create-snake []
  {:body (list [1 1]) 
   :dir [1 0]
   :type :snake
   :color (Color. 15 160 70)})


(defn move [{:keys [body dir] :as snake} & grow]
  (assoc snake :body (cons (add-points (first body) dir) 
			   (if grow body (butlast body)))))

(defn turn [snake newdir] 
  (if newdir (assoc snake :dir newdir) snake))

(defn win? [{body :body}]
  (>= (count body) win-length))

(defn head-overlaps-body? [{[head & body] :body}]
  ; have proposed to SS that argument order be reversed:
  (includes? head body))

(def lose? head-overlaps-body?)

(defn collision? [{[snake-head] :body} {apple :location}]
   (= snake-head apple))

; game state updates
(defn fresh-game [msg]
  {:type :game
   :apple (create-apple)
   :snake (create-snake)
   :msg msg})

(defn next-frame [{:keys [snake apple] :as game}]
  (cond
    (lose? snake) (fresh-game "You lose!")
    (win? snake)  (fresh-game "You win!")
    (collision? snake apple)
      (assoc game
             :msg   nil
             :apple (create-apple)
             :snake (move snake :grow))
    :else (assoc game :msg nil :snake (move snake))))

(defn handle-keycode [{:keys [snake] :as game} keycode]
  (assoc game :snake (turn snake (dirs keycode))))

; drawing
(defn clear-screen [#^Graphics2D g]
  (.setColor g Color/WHITE)
  (.fillRect g 0 0 (* width point-size) (* height point-size)))

(defn fill-point [#^Graphics2D g pt color] 
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color) 
    (.fillRect g x y width height)))

(defmulti paint (fn [g object & _] (:type object)))

(defmethod paint :snake [g {:keys [body color]}]
  (doseq [point body]
    (fill-point g point color)))

(defmethod paint :apple [g {:keys [location color]}]
  (fill-point g location color))

(defmethod paint :game [g {:keys [apple snake]}]
  (paint g apple)
  (paint g snake))

; main function
(defn run-game [#^Container parent]
  (let [game (ref (fresh-game nil))
        panel (proxy [JPanel ActionListener KeyListener] []
                (paintComponent [g]
                  (clear-screen g)
                  (when-let [msg (:msg @game)]
                    (JOptionPane/showMessageDialog parent msg))
                  (paint g @game))
                (keyPressed [#^KeyEvent e]
                  (dosync (alter game handle-keycode (.getKeyCode e))))
                (keyReleased [e])
                (keyTyped [e])
                (actionPerformed [e]
                  (dosync (alter game next-frame))
                  (.repaint #^JPanel this)))]

    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))

    (doto parent
      (.add panel)
      (.setSize (* width point-size) (* height point-size))
      (.setVisible true))

    (.start (Timer. turn-millis panel))))

; run as an app
(defn -main []
  (run-game (JFrame. "Snake")))

; run as an applet
(defn -start [this]
  (run-game this))
