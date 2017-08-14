;;; brew.el --- Emacs homebrew tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Emacs homebrew tools

;;; Code:

(require 'brew-hop)

(defgroup brew nil
  "brew.el customizations"
  :version "1.0"
  :group 'brew)

(defcustom brew-boil-volume
  7.0
  "Gallons of liquid you intend to boil"
  :type 'number
  :group 'brew)

(defcustom brew-water-absorption-ratio
  0.2
  "Gallons of water absorbed per pound of grain in the mash.

This value is usually between 0.1 and 0.2 gallons per pound. In my
system it is very close to 0.2"
  :type 'number
  :group 'brew)

(defcustom brew-kettle-correction
  0.2
  "A height correction in inches applied when calculating gallons of
  liquid from inches of liquid in the brew kettle.

To find this value, poor 3 gallons of water into the kettle and
measure the height of the liquid from the bottom of the kettle. Next
find the calculated height of 3 gallons of liquid based on the volume
of the kettle, assuming it is a perfect cylinder. The difference
between these 2 values is the correction."
  :type 'number
  :group 'brew)

(defun brew-gravity-to-plato (&optional gravity)
  "Convert specific GRAVITY to degrees Plato"
  (interactive)
  (let* ((og (if gravity gravity
             (read-number "Specific gravity (1.040): ")))
         (plato (/ (* (- og 1) 1000) 4)))
    plato)
  )

(defun brew-adjust-gravity (&optional gravity)
  "Adjusts GRAVITY by temperature.

For every 10 degrees over 60 dF add 0.004 to gravity- and vice versa
for every 10 dF under 60."
  (interactive)
  (let* ((g (if gravity gravity
               (read-number "Gravity: ")))
         (df (read-number "Temperature (dF): "))
         (ga (+ (* (/ (- df 60) 10) 0.003) g)))
    (insert "Adjusted gravity: "
            (format "%.3f" ga)))
  )

(defun brew-starter-volume (&optional original-gravity)
  "Suggest a starter volume (liters) for a wort with ORIGINAL-GRAVITY.

The starter volume assumes a simple starter. If you are using a stir
plate, cut the volume in half, down to a 1 liter minimum"
  (interactive)
  (let* ((plato (brew-gravity-to-plato original-gravity))
         (cells (* plato 15))
         (packs (/ cells 200))
         (liters (if (< packs 1.0) 2.0
                   (* packs 2))))
    liters)
  )

(defun brew-starter-dme (&optional original-gravity)
  "Suggest a starter DME measure (grams) for a wort with
ORIGINAL-GRAVITY"
  (interactive)
  (let* ((vol (brew-starter-volume original-gravity))
         (grams (* vol 100)))
    grams)
 )

(defun brew-starter (&optional original-gravity)
  "Suggests yeast starter measurements, based on the planned
ORIGINAL-GRAVITY of wort."
  (interactive)
  (insert "--Yeast Starter--\n")
  (let* ((og (if original-gravity original-gravity
               (read-number "Original gravity of wort: ")))
         (sp (downcase (read-string "Stir plate (y/n): ")))
         (plato (brew-gravity-to-plato og))
         (vol (brew-starter-volume og))
         (dme (brew-starter-dme og)))
    (insert "Original gravity: "
            (format "%.3f" og)
            " ("
            (format "%.1f" plato)
            " degrees Plato)")
    (insert "\n")
    (insert "- Starter volume: "
            (if (string= sp "y")
                (format "%.1f" (/ vol 2))
              (format "%.1f" vol))
            " liters")
    (insert "\n")
    (insert "- Starter DME: "
            (if (string= sp "y")
                (format "%.1f" (/ dme 2))
              (format "%.1f" dme))
            " grams")
    (if (string= sp "y")
        (insert "\n(Assuming a stir plate)"))
    (insert "\n\n")
    (insert "Add "
            (if (string= sp "y")
                (format "%.1f" (/ dme 2))
              (format "%.1f" dme))
            " grams DME and a pinch of yeast nutrient"
            " to boiling vessel."
            "\n"
            "Fill vessel to "
            (if (string= sp "y")
                (format "%.1f" (/ vol 2))
              (format "%.1f" vol))
            " liters. Boil gently for 15 minutes.")
    (insert "\n")
    )
  )

(defun brew-kettle-gallons (&optional inches-wort)
  "Converts INCHES-WORT in the brew kettle to gallons. Where
INCHES-WORT is measured from the bottom of the brew kettle.

This measure is particular to my Volrath 15 gallon brew kettle- note
the 0.20 inch adjustment to the height measurement."
  (interactive)
  (let* ((inches (if inches-wort inches-wort
                   (read-number "Inches of wort: ")))
         (height (- inches brew-kettle-correction))
         (gallons (/ (* (* height 64) 3.14) 231)))
    (message "%s inches of wort in the kettle equals %s gallons"
             (format "%.2f" inches)
             (format "%.2f" gallons))
    gallons)
  )

(defun brew-kettle-inches (&optional gallons-wort)
  "Converts GALLONS-WORT in the brew kettle to a measure of the height
in inches of wort in the brew kettle.

This measure is particular to my Volrath 15 gallon brew kettle- note
the 0.20 inch adjustment to the final height measurement."
  (interactive)
  (let* ((gallons (if gallons-wort gallons-wort
                    (read-number "Gallons of wort: ")))
         (height (/ (* 231.0 gallons) (* 64.0 3.14)))
         (inches (+ height brew-kettle-correction)))
    (message "%s gallons of wort equals %s inches in the kettle"
             (format "%.2f" gallons)
             (format "%.2f" inches))
    inches)
  )

(defun brew-water-absorbed (&optional pounds-grain)
  "Calculates the amount of water absorbed by POUNDS-GRAIN in the mash

In most systems this is somewhere between 0.1 and 0.2 gallons. In my
system I find this value is closer to 0.2"
  (interactive)
  (let* ((pounds (if pounds-grain pounds-grain
               (read-number "Pounds of grain: ")))
         (gallons (* pounds brew-water-absorption-ratio)))
    (message "%s pounds grain absorbs %s gallons water"
             (format "%.2f" pounds)
             (format "%.2f" gallons))
    gallons)
  )

(defun brew-water-strike (&optional pounds-grain)
  "Calculates a suggested amount of strike water based on a mash of
  POUNDS-GRAIN.

Assumes 1.2 quarts of water per pound of grain."
  (let* ((pounds (if pounds-grain pounds-grain
                   (read-number "Pounds of grain: ")))
         (gallons (/ (* 1.2 pounds) 4.0)))
    (message "%s gallons strike water for %s pounds grain"
             (format "%.2f" gallons)
             (format "%.2f" pounds))
    gallons)
  )

(defun brew-water (&optional pounds-grain)
  "Suggests strike, pre-lauter add-in and sparge water amounts based
  on POUNDS-GRAIN.

Assumes a boil volume of 7 gallons and final volume of 5 gallons."
  (interactive)
  (insert "--Brew Water--\n")
  (let* ((pounds (if pounds-grain pounds-grain
                   (read-number "Pounds of grain: ")))
         (mashtmp (read-number "Mash temperature: "))
         (strike (brew-water-strike pounds))
         (absorbed (brew-water-absorbed pounds))
         (prelaut (- (/ brew-boil-volume 2.0) (- strike absorbed)))
         (sparge (/ brew-boil-volume 2.0)))
    (insert "Pounds of grain: "
            (format "%.2f" pounds))
    (insert "\n")
    (insert "Mash temperature: "
            (format "%d" mashtmp)
            " dF")
    (insert "\n")
    (insert "- Strike water ("
            (format "%d" (+ 10.0 mashtmp))
            " dF): "
            (format "%.2f" strike)
            " gallons")
    (insert "\n")
    (insert "- Pre-lauter (180 dF):   "
            (format "%.2f" prelaut)
            " gallons")
    (insert "\n")
    (insert "- Sparge water (180 dF): "
            (format "%.2f" sparge)
            " gallons")
    (insert "\n\n")
    (insert "Heat " (format "%.2f" strike)
            " gallons of water to " (format "%d" (+ 10.0 mashtmp))
            " dF. Pour into mash tun.")
    (insert "\n")
    (insert "Add grain, stir and cover. Mash for 60 to 90 minutes.")
    (insert "\n")
    (insert "Heat " (format "%.2f" prelaut)
            " gallons water to 180 dF."
            " Add to mash, stir, wait 20 minutes, drain.")
    (insert "\n")
    (insert "Heat " (format "%.2f" sparge)
            " gallons water to 180 dF."
            " Add to mash, stir, wait 20 minutes, drain.")
    (insert "\n")
    (insert "Boil kettle should contain "
            (format "%.2f" brew-boil-volume)
            " gallons liquor")
    (insert "\n")
    )
  )

(defun brew-day ()
  "Wraps calls to brew-starter and brew-water."
  (interactive)
  (brew-starter)
  (insert "\n")
  (brew-water)
  )

(defun brew-hops-aau-to-oz ()
  "Converts hops measured in AAU to an equivalent ounce measurement

AAU is 1 ounce of hops with an alpha acid percentage. For example, 5
AAU is 1 ounce of hops at 5% AA"
  (interactive)
  (let* ((aau (read-number "Hop AAU: "))
         (aa (read-number "Hop percent AA: "))
         (oz (/ aau aa)))
    (message "%s AAU is equivalent to %s ounces at %s percent AA"
             (format "%.2f" aau)
             (format "%.2f" oz)
             (format "%.2f" aa))
    oz)
  )

(provide 'brew)

;;; brew.el ends here
