(require 'b)
(require 'cl-lib)
(require 'map)
(require 'dash)

(defconst aoc-games-played-input
  (-> "input.txt" get-buffer b-string-no-properties s-trim s-lines))

(length aoc-games-played-input)

(cl-defun get-game-id (game-name)
  (let ((re (rx "Game " (group (1+ num)))))
    (->> game-name (s-match re) cl-second string-to-number)))

(cl-defun subset->alist (colors)
  (cl-loop for color in colors
           for (num color) = (s-split (rx " ") color)
           collect (cons (read color) (string-to-number num))))

(cl-defun line->game (line)
  (pcase-let* ((`(,game ,subsets) (s-split (rx ": ") line))
               (subset-list (s-split (rx "; ") subsets))
               (subsets (--map (s-split (rx ", ") it) subset-list)))
    (cons
     (get-game-id game)
     (-map #'subset->alist subsets)))  )

(let ((line (car aoc-games-played-input)))
  (line->game line))

(defconst limits '((red . 12) (green . 13) (blue . 14)))

(cl-defun game-possible-p (game)
  (cl-loop for subset in (cdr game)
           unless (cl-loop for (color . limit) in limits
                           always (<= (map-elt subset color 0) limit))
           return nil
           finally return t))

(defconst line-impossible
  "Game 1: 12 blue, 15 red, 2 green; 17 red, 8 green, 5 blue; 8 red, 17 blue; 9 green, 1 blue, 4 red")
(defconst line-possible
  "Game 2: 6 red, 6 blue, 2 green; 1 blue, 1 red; 6 green, 1 red, 10 blue")

(list (game-possible-p (line->game line-possible))
      (game-possible-p (line->game line-impossible)))

(cl-loop for line in aoc-games-played-input
         for game = (line->game line)
         if (game-possible-p game)
         sum (car game))
