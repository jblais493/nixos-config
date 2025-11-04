;;; doom-spacegrey-theme.el --- I'm sure you've heard of it -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: October 20, 2025
;; Author: joshuablais <https://github.com/jblais493>
;; Maintainer:
;; Source: http://kkga.github.io/spacegray/
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(require 'doom-themes)

(defgroup doom-spacegrey-theme nil
  "Options for the `doom-spacegrey' theme."
  :group 'doom-themes)

(defcustom doom-spacegrey-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-spacegrey-theme
  :type 'boolean)

(defcustom doom-spacegrey-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-spacegrey-theme
  :type 'boolean)

(defcustom doom-spacegrey-comment-bg doom-spacegrey-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-spacegrey-theme
  :type 'boolean)

(defcustom doom-spacegrey-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-spacegrey-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme lauds
    "The Lauds Light colorscheme"

  ;; name        default   256       16
  ((bg         '("#f0efeb" nil       nil            ))  ; aged vellum - perfect
   (bg-alt     '("#e0dcd4" nil       nil            ))  ; slightly darker panels
   (base0      '("#f5f4f2" "white"   "white"        ))
   (base1      '("#efeeed" "#e4e4e4" "white"        ))
   (base2      '("#e5e3e0" "#d0d0d0" "brightwhite"  ))
   (base3      '("#d8d6d3" "#c6c6c6" "brightwhite"  ))
   (base4      '("#b8b5b0" "#b2b2b2" "brightwhite"  ))
   (base5      '("#9a9791" "#979797" "brightblack"  ))
   (base6      '("#7d7a75" "#6b6b6b" "brightblack"  ))
   (base7      '("#5f5c58" "#525252" "brightblack"  ))
   (base8      '("#2d2a27" "#1e1e1e" "black"        ))
   (fg         '("#1a1d21" "#2d2d2d" "black"        ))  ; warm charcoal - perfect
   (fg-alt     '("#4A4D51" "#4e4e4e" "brightblack"  ))
   (grey       base4)
   
   ;; REFINED: Slightly more saturation for instant recognition
   (red        '("#8B6666" "#BF616A" "red"          ))  ; deeper dusty rose
   (orange     '("#7A6D5A" "#A2957C" "brightred"    ))  ; richer earth-clay
   (green      '("#5A6B5A" "#6e7d64" "green"        ))  ; stronger olive
   (blue       '("#5A6B7A" "#64717d" "brightblue"   ))  ; deeper slate-blue
   (yellow     '("#8B7E52" "#A2957C" "yellow"       ))  ; richer sand-gold
   (violet     '("#7d7470" "#7d7470" "brightmagenta"))  ; warmer dust-mauve
   (teal       '("#4D6B6B" "#6e7d75" "brightgreen"  ))  ; deeper sage-grey
   (dark-blue  '("#546070" "#546070" "blue"         ))  ; deep charcoal-slate
   (magenta    '("#756e75" "#756e75" "magenta"      ))  ; deeper grey-plum
   (cyan       '("#64757d" "#64757d" "brightcyan"   ))  ; deeper grey-water
   (dark-cyan  '("#546470" "#546470" "cyan"         ))  ; deep grey-storm

   ;; face categories -- required for all themes
   (highlight      orange)
   (vertical-bar   (doom-darken bg 0.25))
   (selection      base4)
   (builtin        cyan)
   (comments       base5) 
   (doc-comments   base5)
   (constants      teal)
   (functions      blue)
   (keywords       base6)
   (methods        blue)
   (operators      base7)
   (type           blue)
   (strings        green)
   (variables      fg-alt)
   (numbers        orange)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg-alt) "black" "black"))
   (-modeline-bright doom-spacegrey-brighter-modeline)
   (-modeline-pad
    (when doom-spacegrey-padded-modeline
      (if (integerp doom-spacegrey-padded-modeline) doom-spacegrey-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt (doom-blend violet base4 (if -modeline-bright 0.5 0.2)))
   (modeline-bg
    (if -modeline-bright
        (doom-darken base3 0.1)
      base1))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken base3 0.05)
      base1))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg 0.1)))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if doom-spacegrey-comment-bg (doom-lighten bg 0.05)))
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground fg)
   (css-selector             :foreground red)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-darken bg 0.1))
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground fg :weight 'ultra-bold)
   ((outline-2 &override) :foreground (doom-blend fg blue 0.35))
   ((outline-3 &override) :foreground (doom-blend fg blue 0.7))
   ((outline-4 &override) :foreground blue)
   ((outline-5 &override) :foreground (doom-blend magenta blue 0.2))
   ((outline-6 &override) :foreground (doom-blend magenta blue 0.4))
   ((outline-7 &override) :foreground (doom-blend magenta blue 0.6))
   ((outline-8 &override) :foreground fg)
   ;;;; org <built-in>
   (org-block            :background (doom-darken bg-alt 0.04))
   (org-block-begin-line :foreground base4 :slant 'italic :background (doom-darken bg 0.04))
   (org-ellipsis         :underline nil :background bg    :foreground red)
   ((org-quote &override) :background base1)
   (org-hide :foreground bg)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l))))

  ;;;; Base theme variable overrides-
  ;; ()
  )

;;; doom-spacegrey-theme.el ends here


