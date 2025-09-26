;;
;; mtg.el
;; ------

;; Author: Michael Dickens <contact@mdickens.me>
;; Created: 2025-09-26

;;; Commentary:

;; Display Magic: the Gathering cards in Emacs.


;;; Code:

(require 'url)
(require 'url-util)
(require 'json)


;; Customization options
(defgroup mtg nil
  "Image hover preview settings."
  :group 'multimedia)

(defcustom mtg/db-path "~/.emacs.d/mtg-cards"
  "Directory in which to store MTG card images and info. Default is ~/.emacs.d/mtg-cards."
  :type 'string
  :group 'mtg)

(defcustom mtg/preview-max-width 488
  "Maximum width for MTG card previews."
  :type 'integer
  :group 'mtg)

(defcustom mtg/preview-max-height 680
  "Maximum height for MTG card previews."
  :type 'integer
  :group 'mtg)

(defcustom mtg/default-format 'standard
  "Preferred MTG format for the purposes of determining card legality.
mtg.el will try to determine the format from org-mode properties, but
this value is used if no format is specified."
  :type 'symbol
  :options '(alchemy brawl commander explorer historic legacy modern pauper pionner standard vintage)
  :group 'mtg)

(defvar mtg/saved-max-mini-window-height max-mini-window-height
  "Saved value of max-mini-window-height used to reset after calling
#'mtg/show-card-at-point.")


(defvar mtg/card-info-alist nil)


(defun reset-max-mini-window-height ()
  "Restore the default minibuffer height. This function is used as a hook
on minibuffer-exit-hook, and it automatically removes itself after being
called."
  (setq max-mini-window-height mtg/saved-max-mini-window-height)
  (remove-hook 'minibuffer-exit-hook 'reset-max-mini-window-height))


(defun mtg/get-legal-card-path (card-name)
  "Get the file path for the MTG card CARD-NAME, assuming it's legal."
  (expand-file-name (concat card-name ".jpg") mtg/db-path))


(defun mtg/get-illegal-card-path (card-name)
  "Get the file path for the MTG card CARD-NAME, assuming it's legal."
  (expand-file-name (concat card-name "-illegal.jpg") mtg/db-path))


(defun mtg/get-card-info-path (card-name)
  "Get the file path for the MTG card CARD-NAME."
  (expand-file-name (concat card-name "-info-alist.el") mtg/db-path))


(defun mtg/get-format ()
  "Get the preferred Magic: the Gathering Format. If the current major mode is org-mode, look for a property called \"MTG_FORMAT\". If it exists, return its value. If it does not exist or if the major mode is not org-mode, instead return mtg/default-format."
  (or
   (if (string= "org-mode" major-mode)
       (org-entry-get (point) "MTG_FORMAT" t)
     nil)
   (symbol-name mtg/default-format)))


(defun mtg/get-card-path (card-name)
  "Get the file path for the MTG card CARD-NAME, depending on legality. If
the card is legal in the preferred format, return the path for the card
image. If it is illegal, return the path for a red-tinted version of the
image."
  (let* ((card-info (mtg/load-card-info card-name))
         (legalities (cdr (assoc "legalities" card-info)))
         (legal? (string= "legal" (cdr (assoc (mtg/get-format) legalities)))))
    (if legal?
        (mtg/get-legal-card-path card-name)
      (mtg/get-illegal-card-path card-name))))


(defun mtg/save-card-info (card-name card-info)
  (with-temp-buffer
    (print card-info (current-buffer))
    (write-file (mtg/get-card-info-path card-name))))


(defun mtg/load-card-info (card-name)
  (with-temp-buffer
    (insert-file-contents (mtg/get-card-info-path card-name))
    (read (current-buffer))))


(defun mtg/add-red-tint (image-path output-path &optional tint-strength)
  "Add a red tint to IMAGE-PATH and save to OUTPUT-PATH.
  TINT-STRENGTH defaults to 0.2 (20% tint)."
  (let ((tint-strength (or tint-strength 0.2)))
    ;; Note: Emacs doesn't have built-in image processing like PIL
    ;; This is a simplified version using ImageMagick if available
    (if (executable-find "convert")
        (let ((cmd (format "convert \"%s\" -fill \"rgba(255,0,0,%s)\" -colorize %d%% \"%s\""
                           image-path
                           tint-strength
                           (round (* tint-strength 100))
                           output-path)))
          (shell-command cmd))
      ;; Fallback: just copy the file if ImageMagick isn't available
      (copy-file image-path output-path t)
      (message "ImageMagick not found. Red tint not applied to %s" output-path))))


(defun mtg/parse-json-response (buffer)
  "Parse JSON response from BUFFER.
  Returns the parsed JSON object or nil on error."
  (when buffer
    (with-current-buffer buffer
      (goto-char (point-min))
      ;; Skip HTTP headers
      (when (re-search-forward "^\r?$" nil t)
        (condition-case err
            (let ((json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'string))
              (json-read))
          (error
           (message "Error parsing JSON: %s" (error-message-string err))
           nil))))))


(cl-defun mtg/search-card (card-name)
  "Search for a Magic card by CARD-NAME and download its image.
  Save the image to the configured root path. If the card is not
  legal in Historic format, add a red tint to indicate this."
  (interactive "sCard name:")

  (let* ((legal-path (mtg/get-legal-card-path card-name))
         (illegal-path (mtg/get-illegal-card-path card-name))
         (query-params (url-build-query-string `(("fuzzy" ,card-name))))
         (api-url (concat "https://api.scryfall.com/cards/named?" query-params))
         (timeout-seconds 3)
         (response-buffer nil)
         (card-info nil))

    ;; Ensure the root directory exists
    (unless (file-exists-p mtg/db-path)
      (make-directory mtg/db-path t))

    ;; If card already exists, return early
    (when (file-exists-p legal-path)
      (cl-return-from mtg/search-card))

    (message "Downloading %s from Scryfall..." card-name)
    (setq response-buffer (url-retrieve-synchronously api-url t t timeout-seconds))

    (when (not response-buffer)
      (error "Failed to retrieve card data for %s" card-name))

    (setq card-info (mtg/parse-json-response response-buffer))
    (kill-buffer response-buffer)

    (when (not card-info)
      (error "Failed to parse card data for %s" card-name))

    (mtg/save-card-info card-name card-info)

    (let* ((legalities (cdr (assoc "legalities" card-info)))
           (historic-legal (string= (cdr (assoc "historic" legalities)) "legal"))
           (image-uris (cdr (assoc "image_uris" card-info)))
           (img-url (cdr (assoc "normal" image-uris))))

      (when (not img-url)
        (error "No image URL found for %s" card-name))

      (when (not (url-copy-file img-url legal-path t))
        (error "Failed to download image from Scryfall: %s" card-name))

      (mtg/add-red-tint legal-path illegal-path)

      ;; Respect Scryfall rate limiting
      (sleep-for 0.05))))


(defun mtg/show-card-at-point (card-name &optional arg)
  "Display a link in the minibuffer as if it as a Magic: the Gathering card."
  (mtg/search-card card-name)
  (condition-case err
      (let* ((card-path (mtg/get-card-path card-name))
             (image (create-image card-path nil nil
                                  :max-width mtg/preview-max-width
                                  :max-height mtg/preview-max-height))
             (image-string (propertize " " 'display image)))
        (setq mtg/saved-max-mini-window-height max-mini-window-height)
        (add-hook 'minibuffer-exit-hook #'reset-max-mini-window-height)
        (setq max-mini-window-height 0.75)
        (message "%s" image-string))
    (error "Could not display image: %s" (error-message-string err))))


;; TODO: this requires having custom CSS for 'tooltip-image, which isn't part of
;; this module
(defun mtg/card-export (path desc format)
  "Determine how to render MTG cards when exporting org-mode to HTML."
  (when (eq format 'html)
    (let ((name (or desc path)))
      (when (not (file-exists-p (mtg/get-card-path name)))
        (mtg/search-card name))
      (format "<span class=\"image-tooltip\">%s<span class=\"tooltip-image\"><img src=\"%s\"></span></span>"
              name (mtg/get-card-path name)))))


;; Treat links starting with "mtg:" as MTG cards.
(org-link-set-parameters "mtg"
                         :follow #'mtg/show-card-at-point
                         :export #'mtg/card-export
                         :face 'org-link)

(provide 'mtg)
