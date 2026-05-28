(ns unicode.map
  (:require [clojure.string :as str]))

(enable-console-print!)

(def canvas (.getElementById js/document "map"))
(def ctx (.getContext canvas "2d" #js {:alpha false}))
(def hud (.getElementById js/document "hud"))
(def font-file (.getElementById js/document "font-file"))

(def colors
  {:background "#121614"
   :plane-a "rgba(85, 109, 96, 0.16)"
   :plane-b "rgba(92, 83, 116, 0.16)"
   :plane-line "rgba(244, 241, 232, 0.18)"
   :grid "rgba(244, 241, 232, 0.08)"
   :text "#f4f1e8"
   :muted "#b8b6aa"
   :selected "#f3c05a"
   :default "#28352f"
   :control "#6d597a"
   :surrogate "#8b3340"
   :private "#5f6f52"
   :seed-block "#2f6d69"})

(def state
  (atom {:data nil
         :dpr 1
         :columns 256
         :max-codepoint 0x10FFFF
         :base-cell 18
         :zoom 0.05
         :offset-x 0
         :offset-y 0
         :dragging? false
         :drag-x 0
         :drag-y 0
         :selected 0x4E00
         :hover nil
         :font "system-ui"
         :plane-grid-columns 5
         :plane-gap-cells 24}))

(declare draw update-hud)

(defn clamp [value low high]
  (max low (min high value)))

(defn code-label [cp]
  (let [hex (.toUpperCase (.toString cp 16))
        width (if (<= cp 0xFFFF) 4 6)]
    (str "U+" (.padStart hex width "0"))))

(defn glyph-for [cp]
  (if (<= 0xD800 cp 0xDFFF)
    ""
    (try
      (js/String.fromCodePoint cp)
      (catch :default _ ""))))

(defn find-range [cp]
  (some (fn [range]
          (when (<= (:start range) cp (:end range))
            range))
        (get-in @state [:data :ranges])))

(defn find-block [cp]
  (some (fn [block]
          (when (<= (:start block) cp (:end block))
            block))
        (get-in @state [:data :blocks])))

(defn find-plane [cp]
  (get-in @state [:data :planes (js/Math.floor (/ cp 0x10000))]))

(defn plane-size []
  (* (:columns @state) (:base-cell @state)))

(defn plane-gap []
  (* (:plane-gap-cells @state) (:base-cell @state)))

(defn plane-count []
  (count (get-in @state [:data :planes])))

(defn plane-grid-rows []
  (js/Math.ceil (/ (plane-count) (:plane-grid-columns @state))))

(defn world-width []
  (+ (* (:plane-grid-columns @state) (plane-size))
     (* (dec (:plane-grid-columns @state)) (plane-gap))))

(defn world-height []
  (+ (* (plane-grid-rows) (plane-size))
     (* (dec (plane-grid-rows)) (plane-gap))))

(defn cell-size []
  (* (:base-cell @state) (:zoom @state)))

(defn plane-origin [plane-number]
  (let [grid-col (mod plane-number (:plane-grid-columns @state))
        grid-row (js/Math.floor (/ plane-number (:plane-grid-columns @state)))
        step (+ (plane-size) (plane-gap))]
    {:x (* grid-col step)
     :y (* grid-row step)}))

(defn codepoint->world [cp]
  (let [plane (js/Math.floor (/ cp 0x10000))
        intra (mod cp 0x10000)
        col (mod intra (:columns @state))
        row (js/Math.floor (/ intra (:columns @state)))
        origin (plane-origin plane)]
    {:x (+ (:x origin) (* col (:base-cell @state)))
     :y (+ (:y origin) (* row (:base-cell @state)))
     :plane plane
     :col col
     :row row}))

(defn codepoint->screen [cp]
  (let [{:keys [x y plane col row]} (codepoint->world cp)
        size (cell-size)]
    {:x (+ (:offset-x @state) (* x (:zoom @state)))
     :y (+ (:offset-y @state) (* y (:zoom @state)))
     :size size
     :plane plane
     :col col
     :row row}))

(defn screen->world [x y]
  {:x (/ (- x (:offset-x @state)) (:zoom @state))
   :y (/ (- y (:offset-y @state)) (:zoom @state))})

(defn screen->codepoint [x y]
  (let [{world-x :x world-y :y} (screen->world x y)
        step (+ (plane-size) (plane-gap))
        grid-col (js/Math.floor (/ world-x step))
        grid-row (js/Math.floor (/ world-y step))
        local-x (- world-x (* grid-col step))
        local-y (- world-y (* grid-row step))
        plane (+ (* grid-row (:plane-grid-columns @state)) grid-col)]
    (when (and (<= 0 plane (dec (plane-count)))
               (<= 0 local-x (plane-size))
               (<= 0 local-y (plane-size)))
      (let [col (js/Math.floor (/ local-x (:base-cell @state)))
            row (js/Math.floor (/ local-y (:base-cell @state)))
            cp (+ (* plane 0x10000) (* row (:columns @state)) col)]
        (when (<= cp (:max-codepoint @state))
          cp)))))

(defn resize []
  (let [dpr (or (.-devicePixelRatio js/window) 1)]
    (swap! state assoc :dpr dpr)
    (set! (.-width canvas) (js/Math.floor (* (.-innerWidth js/window) dpr)))
    (set! (.-height canvas) (js/Math.floor (* (.-innerHeight js/window) dpr)))
    (set! (.. canvas -style -width) (str (.-innerWidth js/window) "px"))
    (set! (.. canvas -style -height) (str (.-innerHeight js/window) "px"))
    (.setTransform ctx dpr 0 0 dpr 0 0)
    (draw)))

(defn fit []
  (let [padding 42
        next-zoom (clamp (min (/ (- (.-innerWidth js/window) padding) (world-width))
                              (/ (- (.-innerHeight js/window) padding) (world-height)))
                         0.01 4)]
    (swap! state assoc
           :zoom next-zoom
           :offset-x (/ (- (.-innerWidth js/window) (* (world-width) next-zoom)) 2)
           :offset-y (/ (- (.-innerHeight js/window) (* (world-height) next-zoom)) 2))
    (draw)))

(defn zoom-at [delta x y]
  (let [{world-x :x world-y :y} (screen->world x y)
        next-zoom (clamp (* (:zoom @state) delta) 0.01 4)]
    (swap! state assoc
           :zoom next-zoom
           :offset-x (- x (* world-x next-zoom))
           :offset-y (- y (* world-y next-zoom)))
    (draw)))

(defn jump-to [cp]
  (let [cp (clamp cp 0 (:max-codepoint @state))
        {:keys [x y]} (codepoint->world cp)
        size (:base-cell @state)]
    (swap! state assoc
           :selected cp
           :offset-x (- (/ (.-innerWidth js/window) 2) (* (+ x (/ size 2)) (:zoom @state)))
           :offset-y (- (/ (.-innerHeight js/window) 2) (* (+ y (/ size 2)) (:zoom @state))))
    (update-hud)
    (draw)))

(defn parse-codepoint [value]
  (let [trimmed (str/trim value)
        match (re-matches #"(?i)^(?:U\+|0x)?([0-9a-f]{1,6})$" trimmed)]
    (when match
      (let [cp (js/parseInt (second match) 16)]
        (when (and (js/Number.isFinite cp)
                   (<= cp (:max-codepoint @state)))
          cp)))))

(defn draw-background [width height]
  (set! (.-fillStyle ctx) (:background colors))
  (.fillRect ctx 0 0 width height))

(defn draw-plane-tiles [width height]
  (.save ctx)
  (set! (.-font ctx) "12px system-ui")
  (set! (.-textBaseline ctx) "top")
  (doseq [plane (get-in @state [:data :planes])]
    (let [origin (plane-origin (:number plane))
          x (+ (:offset-x @state) (* (:x origin) (:zoom @state)))
          y (+ (:offset-y @state) (* (:y origin) (:zoom @state)))
          side (* (plane-size) (:zoom @state))]
      (when (and (<= x width) (<= y height) (>= (+ x side) 0) (>= (+ y side) 0))
        (set! (.-fillStyle ctx) (if (even? (:number plane)) (:plane-a colors) (:plane-b colors)))
        (.fillRect ctx x y side side)
        (set! (.-strokeStyle ctx) (:plane-line colors))
        (set! (.-lineWidth ctx) 1)
        (.strokeRect ctx x y side side)
        (.save ctx)
        (.beginPath ctx)
        (.rect ctx x y side side)
        (.clip ctx)
        (when (> side 28)
          (set! (.-fillStyle ctx) (:muted colors))
          (.fillText ctx (str "Plane " (:number plane)) (+ x 6) (+ y 5)))
        (when (and (> side 180)
                   (not= (:name plane) (str "Plane " (:number plane))))
          (.fillText ctx (:name plane) (+ x 6) (+ y 21)))
        (.restore ctx))))
  (.restore ctx))

(defn draw-ranges [width height]
  (let [size (cell-size)]
    (doseq [range (get-in @state [:data :ranges])]
      (let [start (:start range)
            end (:end range)
            color-key (keyword (:kind range))]
        (loop [cp start]
          (when (<= cp end)
            (let [plane (js/Math.floor (/ cp 0x10000))
                  plane-end (min end (+ (* plane 0x10000) 0xFFFF))
                  first-intra (mod cp 0x10000)
                  last-intra (mod plane-end 0x10000)
                  first-row (js/Math.floor (/ first-intra (:columns @state)))
                  last-row (js/Math.floor (/ last-intra (:columns @state)))
                  origin (plane-origin plane)
                  sx (+ (:offset-x @state) (* (:x origin) (:zoom @state)))
                  sy (+ (:offset-y @state) (* (+ (:y origin) (* first-row (:base-cell @state))) (:zoom @state)))
                  h (* (inc (- last-row first-row)) size)]
              (when (and (<= sx width) (<= sy height)
                         (>= (+ sx (* (:columns @state) size)) 0)
                         (>= (+ sy h) 0))
                (set! (.-fillStyle ctx) (get colors color-key (:default colors)))
                (set! (.-globalAlpha ctx) (if (< size 2) 0.85 0.45))
                (.fillRect ctx sx sy (* (:columns @state) size) (max 1 h))
                (set! (.-globalAlpha ctx) 1))
              (recur (inc plane-end)))))))))

(defn visible-plane-range [plane-number width height]
  (let [origin (plane-origin plane-number)
        size (cell-size)
        sx (+ (:offset-x @state) (* (:x origin) (:zoom @state)))
        sy (+ (:offset-y @state) (* (:y origin) (:zoom @state)))
        min-col (clamp (dec (js/Math.floor (/ (- 0 sx) size))) 0 (dec (:columns @state)))
        max-col (clamp (inc (js/Math.ceil (/ (- width sx) size))) 0 (dec (:columns @state)))
        min-row (clamp (dec (js/Math.floor (/ (- 0 sy) size))) 0 255)
        max-row (clamp (inc (js/Math.ceil (/ (- height sy) size))) 0 255)]
    {:sx sx :sy sy :min-col min-col :max-col max-col :min-row min-row :max-row max-row}))

(defn draw-cells [width height]
  (let [size (cell-size)]
    (when (>= size 2)
      (.save ctx)
      (set! (.-lineWidth ctx) 1)
      (set! (.-strokeStyle ctx) (:grid colors))
      (set! (.-font ctx) (str (max 8 (js/Math.floor (* size 0.58))) "px " (:font @state)))
      (set! (.-textAlign ctx) "center")
      (set! (.-textBaseline ctx) "middle")
      (doseq [plane (range (plane-count))]
        (let [{:keys [sx sy min-col max-col min-row max-row]} (visible-plane-range plane width height)]
          (doseq [row (range min-row (inc max-row))
                  col (range min-col (inc max-col))]
            (let [cp (+ (* plane 0x10000) (* row (:columns @state)) col)
                  x (+ sx (* col size))
                  y (+ sy (* row size))
                  range (find-range cp)
                  block (find-block cp)]
              (set! (.-fillStyle ctx)
                    (cond
                      range (get colors (keyword (:kind range)) (:default colors))
                      block (:seed-block colors)
                      :else (:default colors)))
              (set! (.-globalAlpha ctx) (cond range 0.42 block 0.28 :else 0.2))
              (.fillRect ctx x y size size)
              (set! (.-globalAlpha ctx) 1)
              (when (>= size 7)
                (.strokeRect ctx x y size size))
              (when (>= size 13)
                (let [glyph (glyph-for cp)]
                  (when-not (str/blank? glyph)
                    (set! (.-fillStyle ctx) (:text colors))
                    (.fillText ctx glyph (+ x (/ size 2)) (+ y (/ size 2) 1)))))
              (when (>= size 42)
                (set! (.-fillStyle ctx) (:muted colors))
                (set! (.-font ctx) "9px ui-monospace, SFMono-Regular, Menlo, monospace")
                (.fillText ctx (code-label cp) (+ x (/ size 2)) (- (+ y size) 9))
                (set! (.-font ctx) (str (max 8 (js/Math.floor (* size 0.58))) "px " (:font @state))))))))
      (.restore ctx))))

(defn draw-selection []
  (let [{:keys [x y size]} (codepoint->screen (:selected @state))]
    (.save ctx)
    (set! (.-lineWidth ctx) (max 2 (min 5 (* size 0.1))))
    (set! (.-strokeStyle ctx) (:selected colors))
    (.strokeRect ctx x y size size)
    (.restore ctx)))

(defn draw []
  (when (:data @state)
    (let [width (.-innerWidth js/window)
          height (.-innerHeight js/window)]
      (draw-background width height)
      (draw-plane-tiles width height)
      (draw-ranges width height)
      (draw-cells width height)
      (draw-selection))))

(defn info-line [cp]
  (let [plane (find-plane cp)
        range (find-range cp)
        block (find-block cp)
        glyph (glyph-for cp)]
    (str (code-label cp)
         (when-not (str/blank? glyph) (str "  " glyph))
         "  Plane " (:number plane)
         (when block (str "  " (:name block)))
         (when range (str "  " (:label range))))))

(defn update-hud []
  (when (:data @state)
    (set! (.-textContent hud)
          (str "Unicode plane matrix"
               "\n" (info-line (or (:hover @state) (:selected @state)))))))

(defn load-font [file]
  (let [name (str "LoadedFont" (.now js/Date))
        url (.createObjectURL js/URL file)
        face (js/FontFace. name (str "url(" url ")"))]
    (-> (.load face)
        (.then
         (fn [loaded-face]
           (.add (.-fonts js/document) loaded-face)
           (swap! state assoc :font name)
           (update-hud)
           (draw))))))

(defn bind-events []
  (.addEventListener js/window "resize" resize)
  (.addEventListener
   canvas "pointerdown"
   (fn [event]
     (.setPointerCapture canvas (.-pointerId event))
     (swap! state assoc
            :dragging? true
            :drag-x (.-clientX event)
            :drag-y (.-clientY event))
     (.add (.-classList canvas) "dragging")))
  (.addEventListener
   canvas "pointermove"
   (fn [event]
     (if (:dragging? @state)
       (do
         (swap! state
                (fn [s]
                  (-> s
                      (update :offset-x + (- (.-clientX event) (:drag-x s)))
                      (update :offset-y + (- (.-clientY event) (:drag-y s)))
                      (assoc :drag-x (.-clientX event)
                             :drag-y (.-clientY event)))))
         (draw))
       (do
         (swap! state assoc :hover (screen->codepoint (.-clientX event) (.-clientY event)))
         (update-hud)))))
  (.addEventListener
   canvas "pointerup"
   (fn [event]
     (.releasePointerCapture canvas (.-pointerId event))
     (swap! state assoc :dragging? false)
     (.remove (.-classList canvas) "dragging")
     (when-let [cp (screen->codepoint (.-clientX event) (.-clientY event))]
       (swap! state assoc :selected cp)
       (update-hud)
       (draw))))
  (.addEventListener
   canvas "wheel"
   (fn [event]
     (.preventDefault event)
     (zoom-at (if (neg? (.-deltaY event)) 1.18 (/ 1 1.18))
              (.-clientX event)
              (.-clientY event)))
   #js {:passive false})
  (.addEventListener
   js/window "keydown"
   (fn [event]
     (case (.-key event)
       ("0" "Home") (do (.preventDefault event) (fit))
       "/" (do (.preventDefault event)
               (when-let [cp (parse-codepoint (js/prompt "Jump to code point" (code-label (:selected @state))))]
                 (jump-to cp)))
       "f" (do (.preventDefault event) (.click font-file))
       nil)))
  (.addEventListener
   font-file "change"
   (fn [_]
     (when-let [file (aget (.-files font-file) 0)]
       (-> (load-font file)
           (.catch (fn [err] (.error js/console err))))))))

(defn init []
  (bind-events)
  (-> (js/fetch "/data/map.json")
      (.then (fn [response] (.json response)))
      (.then
       (fn [data]
         (let [data (js->clj data :keywordize-keys true)]
           (swap! state assoc
                  :data data
                  :columns (:columns data)
                  :max-codepoint (:maxCodepoint data))
           (resize)
           (fit)
           (update-hud))))
      (.catch
       (fn [err]
         (set! (.-textContent hud) "Load failed")
         (.error js/console err)))))

(init)
