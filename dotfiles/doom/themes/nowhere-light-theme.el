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

(def-doom-theme nowhere-light
    "The Nowhere Light colorscheme"

  ;; name        default   256       16
  ((bg         '("#ebe9e6" nil       nil            ))  ; warm off-white, like aged paper
   (bg-alt     '("#e0deda" nil       nil            ))  ; slightly warmer panels
   (base0      '("#f5f4f2" "white"   "white"        ))  ; pure light
   (base1      '("#efeeed" "#e4e4e4" "white"        ))
   (base2      '("#e5e3e0" "#d0d0d0" "brightwhite"  ))
   (base3      '("#d8d6d3" "#c6c6c6" "brightwhite"  ))
   (base4      '("#b8b5b0" "#b2b2b2" "brightwhite"  ))
   (base5      '("#9a9791" "#979797" "brightblack"  ))
   (base6      '("#7d7a75" "#6b6b6b" "brightblack"  ))
   (base7      '("#5f5c58" "#525252" "brightblack"  ))
   (base8      '("#2d2a27" "#1e1e1e" "black"        ))
   (fg         '("#3d3a36" "#2d2d2d" "black"        ))  ; warm charcoal
   (fg-alt     '("#5a5754" "#4e4e4e" "brightblack"  ))

   (grey       base4)
   (red        '("#8a7575" "#8a7575" "red"          ))  ; muted dusty rose
   (orange     '("#9a8b70" "#9a8b70" "brightred"    ))  ; warm earth - like that link color
   (green      '("#758270" "#758270" "green"        ))  ; muted olive
   (blue       '("#707a85" "#707a85" "brightblue"   ))  ; cool grey-blue
   (yellow     '("#9a8f75" "#9a8f75" "yellow"       ))  ; warm sand - for highlights
   (violet     '("#857d7a" "#857d7a" "brightmagenta"))  ; barely lavender
   (teal       '("#758580" "#758580" "brightgreen"  ))  ; sage grey
   (dark-blue  '("#5a6570" "#5a6570" "blue"         ))
   (magenta    '("#7d757d" "#7d757d" "magenta"      ))
   (cyan       '("#707d85" "#707d85" "brightcyan"   ))
   (dark-cyan  '("#5f6a70" "#5f6a70" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      orange)
   (vertical-bar   (doom-darken bg 0.25))
   (selection      base4)
   (builtin        orange)
   (comments       (if doom-spacegrey-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-spacegrey-brighter-comments dark-cyan base5) 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       violet)
   (methods        blue)
   (operators      fg)
   (type           yellow)
   (strings        green)
   (variables      red)
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


