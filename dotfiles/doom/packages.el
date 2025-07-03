;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; (package! emmet-mode)
;; (package! calfw)
;; (package! calfw-org)
;; (package! web-beautify)
(package! alert)
(package! counsel)
(package! sqlite3)
(package! impatient-mode)
;; (package! ssh)
;; (package! multiple-cursors)
;; (package! org-super-agenda)
(package! org-fancy-priorities)
;; (package! theme-magic)
(package! centaur-tabs)
(package! prettier-js)
;; (package! company-tabnine)
;; (package! sublimity)
(package! treemacs-all-the-icons)
(package! peep-dired)
;; (package! dimmer)
;; (package! beacon)
(package! org-auto-tangle)
(package! aggressive-indent)
(package! powerthesaurus)
(package! elfeed-tube)
(package! elfeed-tube-mpv)
(package! define-word)
(package! svelte-mode)

;; Nix related
(package! agenix)

;; these are not working currently
;; TODO (package! prescient)
;; TODO (package! vertico-prescient)
(package! gptel)
(package! elysium :recipe (:host github :repo "lanceberge/elysium" :branch "master"))
(package! aider :recipe (:host github :repo "tninja/aider.el" :files ("*.el")))

;; Trying to get mu4e nano working
;; (package! svg-tag-mode)
;; (package! nano-theme)
;; (package! mu4e-dashboard
;;   :recipe (:host github
;;            :repo "rougier/mu4e-dashboard"
;;            :files ("*.el")
;;            :build (:not compile)
;;            :no-build t
;;            :pin nil
;;            :includes (mu4e)
;;            :pre-build nil))
;; (package! mu4e-thread-folding
;;   :recipe (:host github
;;            :repo "rougier/mu4e-thread-folding"
;;            :files ("*.el")
;;            :build (:not compile)
;;            :no-build t
;;            :pin nil))
;; (package! nano-modeline
;;   :recipe (:host github
;;            :repo "rougier/nano-modeline"))

;; Installing pgmacs for postgres databases
(package! pg
  :recipe (:host github :repo "emarsden/pg-el"))

(package! pgmacs
  :recipe (:host github :repo "emarsden/pgmacs"
           :files ("*.el" "*.texi" "dir"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"))))

(package! calibredb)

;; (package! jabber)
(package! elpher)
(package! emms)
;; Epub reader
(package! nov)
;; (package! minimap)
;; Rest Client
(package! restclient)
;; Flutter Development
;;(package! flutter)
;;(package! dart-mode)
;;(package! lsp-dart)
;; Go lang dev
(package! go-mode)
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

;; copilot setup
;;(package! copilot
;;  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))

;; org-roam
(unpin! org-roam)
(package! org-roam-ui)

;; (package! gitconfig-mode
;;   :recipe (:host github :repo "magit/git-modes"
;; 	   :files ("gitconfig-mode.el")))
;; (package! gitignore-mode
;;   :recipe (:host github :repo "magit/git-modes"
;; 	   :files ("gitignore-mode.el")))

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
