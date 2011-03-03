;;; Commentary:

;; wrap-region is a minor mode that wraps a region with
;; punctuations. For tagged markup modes, such as HTML and XML, it
;; wraps with tags.
;;
;; To use wrap-region, make sure that this file is in Emacs load-path:
;;   (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require wrap-region:
;;   (require 'wrap-region)
;;
;;
;; To start wrap-region:
;;   (wrap-region-mode t) or M-x wrap-region-mode
;;
;; If you only want wrap-region active in some mode, use a hook:
;;   (add-hook 'ruby-mode-hook 'wrap-region-mode)
;;
;; Or if you want to activate it in all buffers, use the global mode:
;;   (wrap-region-global-mode t)
;;
;;
;; To wrap a region, select that region and hit one of the punctuation
;; keys. In "tag-modes" (html-mode, sgml-mode or xml-mode), "<" is
;; replaced and wraps the region with a tag. To activate this behavior
;; in a mode other than default, you do:
;;   (add-to-list 'wrap-region-tag-active-modes 'some-tag-mode)
;;
;; `wrap-region-punctuations-table' contains a few default
;; punctuations that wraps. You can add you own like this:
;;   (wrap-region-add-punctuation "#" "#")
;;
;; Wrap Region stores a list (`wrap-region-except-modes') of modes in
;; which `wrap-region-mode' should not be activated in (note, only if
;; you use the global mode) because of conflicting key bindings.
;;
;; You can add new except modes like this:
;;   (add-to-list 'wrap-region-except-modes 'conflicting-mode)


