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

(defconst mtg/online-only-formats '(alchemy brawl explorer historic)
  "A list of online-only formats. If #'mtg/get-format returns an
online-only format, then the displayed image will be the online-only
version of a card.")

(defcustom mtg/default-format 'standard
  "Preferred MTG format for the purposes of determining card legality.
mtg.el will try to determine the format from org-mode properties, but
this value is used if no format is specified."
  :type 'symbol
  :options '(alchemy brawl commander explorer historic legacy modern pauper pionner standard vintage)
  :group 'mtg)

(defcustom mtg/download-both-versions t
  "If non-nil, download both the paper version of a card and its online version, prefixed with \"A-\". For example, Scryfall has two separate cards \"Narfi, Betrayer King\" and \"A-Narfi, Betrayer King\" where the \"A-\" version is errata'd to change the cost from 3UB to 2UB.

If this variable is nil, only download and display the paper version. This halves the number of web requests needed, but it will sometimes cause the incorrect version of a card to be displayed."
  :type 'symbol
  :options '(nil t)
  :group 'mtg)

;; We could make it even more space-efficient by downloading the "small" sized
;; image instead of "normal" sized, but IMO the text is too blurry on small
;; images.
(defcustom mtg/space-efficient-mode nil
  "If this variable is nil, mtg.el creates four copies of every image. If this variable is non-nil, mtg.el will only create one copy of every image, but some functionality will not work properly. In particular:

1. Cards will display the same regardless of whether they're legal.
2. For cards with separate paper and online versions, only the paper version will be displayed.

If this variable is nil, only one version of a card will be downloaded even if 'mtg/download-both-versions non-nil. See also the documentation on 'mtg/download-both-versions."
  :type 'symbol
  :options '(nil t)
  :group 'mtg)

(defvar mtg/saved-max-mini-window-height max-mini-window-height
  "Saved value of max-mini-window-height used to reset after calling
#'mtg/show-card-at-point.")


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
  (downcase
   (or
    (if (string= "org-mode" major-mode)
        (org-entry-get (point) "MTG_FORMAT" t)
      nil)
    (symbol-name mtg/default-format))))


(defun mtg/legal? (card-name)
  (let* ((card-info (mtg/load-card-info card-name))
         (legalities (cdr (assoc "legalities" card-info)))
         (legal? (member (cdr (assoc (mtg/get-format) legalities))
                         '("legal" "restricted" nil))))
    legal?))


(defun mtg/get-card-path (card-name)
  "Get the file path for the MTG card CARD-NAME, depending on legality. If
the card is legal in the preferred format, return the path for the card
image. If it is illegal, return the path for a red-tinted version of the
image. If legality could not be determined, the card is assumed to be
legal."
  (let ((online-format? (member (intern (mtg/get-format)) mtg/online-only-formats))
        (paper-legal? nil)
        (alchemy-legal? nil)
        (alchemy-card-name
         (if (s-starts-with? "A-" card-name)
             card-name
           (concat "A-" card-name))))
    (when (and
           (not mtg/space-efficient-mode)
           online-format?)
      ;; Online-only cards are prefixed with "A-" and are sometimes different from
      ;; the paper version. A card is legal if either version of it is legal.
      (setq alchemy-legal? (mtg/legal? alchemy-card-name)))

    (setq paper-legal? (mtg/legal? card-name))

    ;; Determining legality is a bit complicated because sometimes a revised
    ;; card gets reverted such that the paper version is now legal online, and
    ;; the online version isn't legal anywhere. So you have to check both
    ;; legalities separately.
    (cond
     (mtg/space-efficient-mode
      (mtg/get-legal-card-path card-name))
     (paper-legal?
      (mtg/get-legal-card-path card-name))
     (alchemy-legal?
      (mtg/get-legal-card-path alchemy-card-name))
     (online-format?
      (mtg/get-illegal-card-path alchemy-card-name))
     (t
      (mtg/get-illegal-card-path card-name)))))


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


(cl-defun mtg/fetch-card-single-version (card-name)
  (let* ((legal-path (mtg/get-legal-card-path card-name))
         (illegal-path (mtg/get-illegal-card-path card-name))
         (query-params (url-build-query-string `(("fuzzy" ,card-name))))
         (api-url (concat "https://api.scryfall.com/cards/named?" query-params))
         (timeout-seconds 3)
         (response-buffer nil)
         (card-info nil)
         (image-uris nil)
         (image-url nil)
         )

    ;; Ensure the root directory exists
    (unless (file-exists-p mtg/db-path)
      (make-directory mtg/db-path t))

    ;; If card already exists, return early
    (when (file-exists-p legal-path)
      (cl-return-from mtg/fetch-card-single-version))

    (message "Downloading %s from Scryfall..." card-name)
    (setq response-buffer (url-retrieve-synchronously api-url t t timeout-seconds))

    (when (not response-buffer)
      (error "Failed to retrieve card info for %s" card-name))

    (setq card-info (mtg/parse-json-response response-buffer))
    (kill-buffer response-buffer)

    (when (not card-info)
      (error "Failed to parse card info for %s" card-name))

    (when (assoc "card_faces" card-info)
      ;; If multiple cards are returned, take the first one
      (setq card-info (car (cdr (assoc "card_faces" card-info)))))

    (mtg/save-card-info card-name card-info)

    (setq image-uris (cdr (assoc "image_uris" card-info)))
    (setq image-url (cdr (assoc "normal" image-uris)))

    (message "image URIs: %s" image-uris)
    (when (not image-url)
      (error "No image URL for card: %s" card-name))

    (when (not (url-copy-file image-url legal-path t))
      (error "Failed to download image from Scryfall: %s" card-name))

    (mtg/add-red-tint legal-path illegal-path)

    ;; Respect Scryfall rate limiting
    (sleep-for 0.05)))

(defun mtg/fetch-card (card-name)
  "Search for a Magic card by CARD-NAME and download its image and metadata
from Scryfall. Save the image to the configured path at mtg/db-path."
  (interactive "sCard name:")

  (mtg/fetch-card-single-version card-name)
  (when (and mtg/download-both-versions
             (not mtg/space-efficient-mode))
    (let ((card-A-name (concat "A-" card-name)))
      (condition-case err
          (mtg/fetch-card-single-version card-A-name)
        ;; If there was an error trying to fetch the A- version, simply use the
        ;; regular version. This usually happens because Scryfall does not have
        ;; an A- version.
        (error
         (dolist (func (list #'mtg/get-legal-card-path
                             #'mtg/get-illegal-card-path
                             #'mtg/get-card-info-path))
           (copy-file (funcall func card-name) (funcall func card-A-name) t)))))))

(defun mtg/show-card-at-point (card-name &optional arg)
  "Display a link in the minibuffer as if it as a Magic: the Gathering card."
  (mtg/fetch-card card-name)
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


(defun mtg/card-export (path desc format)
  "Determine how to render MTG cards when exporting org-mode to HTML."
  (cond
   ((eq format 'html)
    (let ((card-name (or desc path)))
      (mtg/fetch-card card-name)
      (format "<span class=\"mtg-tooltip\">%s<span class=\"mtg-tooltip-image\"><img src=\"%s\"></span></span>"
              card-name (mtg/get-card-path card-name))))

   ;; For any other export type, just return the path to the card image file.
   (t (mtg/get-card-path card-name))))


(cl-defun mtg/org-table-insert-computed-column (transform-function)
  "Add a new column to the right of point in an org-mode table.
For each row, apply TRANSFORM-FUNCTION to the cell contents at point's column
and insert the result in the new column.
TRANSFORM-FUNCTION should take a string and return a string."
  ;; 1. Check if point is on an org-mode table
  (unless (org-at-table-p)
    (message "Point is not on an org-mode table")
    (cl-return-from mtg/org-table-insert-computed-column))

  (let ((current-col (org-table-current-column)))
    (save-excursion
      (org-table-goto-column (1+ current-col))
      (org-table-insert-column)

      (goto-char (org-table-begin))
      (while (org-at-table-p)
        ;; Skip horizontal separator lines
        (unless (org-at-table-hline-p)
          (let ((cell-content (org-table-get-field current-col)))
            (org-table-goto-column (1+ current-col))
            (org-table-get-field (1+ current-col)
                                 (funcall transform-function
                                          (string-trim cell-content)))))
        (forward-line 1))))

  (org-table-align))


(defmacro mtg/prompt-for-property ()
  '(completing-read
    "Card property: "
    '("mana_cost" "cmc" "type_line" "oracle_text" "power" "toughness" "colors" "color_identity" "keywords" "name" "legalities" "set" "set_name" "set_type" "rarity" "digital" "released_at")))


(defun mtg/get-property (property)
  "Get property PROPERTY for the MTG card at point, or nil if the property
value could not be determined.

Examples of valid properties include: name; power; toughness; cmc; colors; rarity; keywords.

There is no simple way to sort by type. There is a \"type_line\" property, but e.g. it treats \"creature\" and \"legendary creature\" as two different things.
"
  (interactive (list (mtg/prompt-for-property)))
  (save-excursion
    (when (null (org-element-link-parser))
      ;; go to start of link
      (search-backward "[["))
    (let* ((card-name (org-element-property :path (org-element-link-parser)))
           (card-info
            (condition-case nil
                (mtg/load-card-info card-name)
              ;; gracefully fail by using an empty alist
              (error '()))))
      (cdr (assoc property card-info)))))


(defun mtg/table-insert-column (property)
  "Given an org-mode table with a column of MTG card links, create a new column to the right and fill each cell with property PROPERTY for each respective card.

For more on properties, see #'mtg/get-property.

If the point is not at an org-mode table, do nothing.

If a cell does not contain a link that can be parsed as a known MTG card, leave the respective cell empty."
  (interactive (list (mtg/prompt-for-property)))
  (mtg/org-table-insert-computed-column
   (lambda (card-link)
     (with-temp-buffer
       (insert card-link)
       (goto-char (point-min))
       (let ((v (mtg/get-property property)))
         (format
          "%s"
          (cond
           ((null v) "")

           ;; convert floats to ints if possible
           ((and (numberp v)
                 (= v (floor v)))
            (floor v))
           (t v))))))))


(defun -mtg/single-color-to-num (c)
  (cond
   ((string= "W" c) 1)
   ((string= "U" c) 2)
   ((string= "B" c) 3)
   ((string= "R" c) 4)
   ((string= "G" c) 5)))


(defun -mtg/color-to-num (color-list accum)
  "Convert a list of MTG colors to a number, such that an ordering of the numbers is equivalent to a particular color ordering.

The color ordering is defined as follows:

1. A card with N + 1 colors is always considered greater than a card
   with N colors.
2. Cards with multiple colors are sorted first by the first color,
   then by the second color, etc.
3. Colors are ordered as Colorless < White < Blue < Black < Red < Green.
"
  (if (null color-list)
      accum
    (let ((c (car color-list)))
      (-mtg/color-to-num
       (cdr color-list)
       (+ (* 6 accum)
          (-mtg/single-color-to-num c))))))


(defun mtg/table-sort-by-property (property arg)
  "If point is at an org-table and the current column contains MTG card links, sort the org-table at point according to property PROPERTY of those cards.

For more on properties, see #'mtg/get-property.

If a prefix arg is provided, sort in reverse order.
"
  (interactive (list (mtg/prompt-for-property)
                     current-prefix-arg))
  (org-table-sort-lines
   nil
   (if (equal '(4) arg) ?F ?f)

   ;; get key on which to sort
   (lambda (cell-content)
     (with-temp-buffer
       (insert cell-content)
       (goto-char (point-min))
       (let ((k (mtg/get-property property)))
         ;; convert property value to a sortable value
         (cond
          ((string= "rarity" property)
           (cond
            ((string= "common" k) 0)
            ((string= "uncommon" k) 1)
            ((string= "rare" k) 2)
            ((string= "mythic" k) 3)
            ((string= "bonus" k) 4)))
          ((string= "colors" property)
           (-mtg/color-to-num (sort k :key #'-mtg/single-color-to-num) 0))
          ((member property '("power" "toughness"))
           ;; convert power/toughness to a number. consider "X" to be larger
           ;; than any number
           (if (string-search "X" k)
               99999
             (string-to-number k)))
          (t k)))))

   ;; comparison function. assumes both values are the same type
   #'value<))


(with-eval-after-load 'org
  ;; Treat links starting with "mtg:" as MTG cards.
  (org-link-set-parameters "mtg"
                           :follow #'mtg/show-card-at-point
                           :export #'mtg/card-export
                           :face 'org-link))

(provide 'mtg)

;;; mtg.el ends here
