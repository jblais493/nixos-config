;;; compline-theme.el --- I'm sure you've heard of it -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: December 31, 2017 (#129)
;; Author: joshuablais <https://github.com/jblais493>
;; Maintainer:
;; Source: https://compline-theme.el/
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

(def-doom-theme compline
    "The compline colorscheme"

  ;; name        default   256       16
  ((bg         '("#1a1d21" nil       nil            ))  ; cool slate, the stone walls
   (bg-alt     '("#22262b" nil       nil            ))  ; slightly lighter panels
   (base0      '("#0f1114" "black"   "black"        ))
   (base1      '("#171a1e" "#1e1e1e" "brightblack"  ))
   (base2      '("#1f2228" "#2e2e2e" "brightblack"  ))
   (base3      '("#282c34" "#2F3237" "brightblack"  ))
   (base4      '("#3d424a" "#4f5b66" "brightblack"  ))
   (base5      '("#515761" "#65737E" "brightblack"  ))
   (base6      '("#676d77" "#6b6b6b" "brightblack"  ))
   (base7      '("#8b919a" "#979797" "brightblack"  ))
   (base8      '("#e0dcd4" "#dfdfdf" "white"        ))
   (fg         '("#e0dcd4" "#c0c5ce" "brightwhite"  ))  ; warm parchment, easier on eyes
   (fg-alt     '("#c0bdb8" "#a0a0a0" "white"        ))  ; muted secondary

   (grey       base4)
   (red        '("#c8beb8" "#BF616A" "red"          ))  ; warm ash-rose      L=78%
   (orange     '("#c8c0b0" "#D08770" "brightred"    ))  ; warm sand-stone    L=79%
   (green      '("#b4beb4" "#A3BE8C" "green"        ))  ; neutral sage       L=76%
   (blue       '("#b4bec8" "#8FA1B3" "brightblue"   ))  ; cool-lean steel    L=77%
   (yellow     '("#ccc4b0" "#ECBE7B" "yellow"       ))  ; warm wheat-gold    L=80%
   (violet     '("#c4beb8" "#b48ead" "brightmagenta"))  ; warm dust-lavender L=78%
   (teal       '("#b0c0b8" "#44b9b1" "brightgreen"  ))  ; cool-lean seafoam  L=77%
   (dark-blue  '("#9ca8b4" "#2257A0" "blue"         ))  ; deep grey-slate    L=68%
   (magenta    '("#c0b8bc" "#c678dd" "magenta"      ))  ; warm grey-mauve    L=76%
   (cyan       '("#b0bcc8" "#46D9FF" "brightcyan"   ))  ; cool-lean ice      L=77%
   (dark-cyan  '("#9ca8b0" "#5699AF" "cyan"         ))  ; deep grey-water    L=68%

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

;;; compline-theme.el ends here
