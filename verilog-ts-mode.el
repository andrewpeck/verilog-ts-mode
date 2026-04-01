;;; Treesitter-based completion for verilog-ts-mode ;;;

(defvar verilog-ts-keywords
  '(;; Design units
    "module" "endmodule" "interface" "endinterface" "package" "endpackage"
    "program" "endprogram" "class" "endclass" "primitive" "endprimitive"
    "checker" "endchecker" "config" "endconfig"
    ;; Port directions / net / data types
    "input" "output" "inout" "ref"
    "wire" "tri" "wand" "wor" "trior" "triand" "tri0" "tri1" "trireg" "uwire" "supply0" "supply1"
    "reg" "logic" "bit" "byte" "shortint" "int" "longint" "integer" "time"
    "real" "shortreal" "realtime"
    "signed" "unsigned" "void"
    ;; Qualifiers / storage
    "parameter" "localparam" "genvar" "specparam"
    "automatic" "static" "local" "protected" "virtual" "pure" "extern"
    "const" "var"
    ;; Type constructors
    "typedef" "enum" "struct" "union" "packed" "tagged"
    ;; Procedural blocks
    "always" "always_ff" "always_comb" "always_latch" "initial" "final"
    ;; Block delimiters
    "begin" "end" "fork" "join" "join_any" "join_none"
    "function" "endfunction" "task" "endtask" "return"
    ;; Generate
    "generate" "endgenerate"
    ;; Control flow
    "if" "else" "case" "casex" "casez" "endcase"
    "for" "foreach" "while" "do" "repeat" "forever" "break" "continue"
    "unique" "unique0" "priority" "default"
    ;; Assignments / drives
    "assign" "force" "release" "deassign"
    "posedge" "negedge" "edge"
    ;; OOP
    "this" "super" "new" "null" "extends" "implements"
    "import" "export" "with"
    ;; Assertions
    "assert" "assume" "cover" "restrict"
    "property" "endproperty" "sequence" "endsequence"
    "disable" "iff"
    ;; Clocking / timing
    "clocking" "endclocking" "covergroup" "endgroup" "coverpoint" "cross"
    ;; Misc
    "wait" "event" "specify" "endspecify" "table" "endtable"
    "macromodule" "connectmodule" "endconnectmodule" "bind")
  "Common Verilog/SystemVerilog keywords offered as completion candidates.")

(defvar-local verilog-ts--candidates-cache nil
  "Cached completion candidates for the current buffer.
Populated lazily on the first completion request and recomputed on every
buffer save via `verilog-ts--update-cache'.")

(define-derived-mode verilog-ts-mode verilog-mode "Verilog"
  "A mode for Verilog."
  (when (treesit-ready-p 'verilog)
    (treesit-parser-create 'verilog)
    (treesit-major-mode-setup))
  (add-hook 'after-save-hook #'verilog-ts--update-cache nil t))

(defun verilog-ts--typed-children (node types)
  "Return direct children of NODE whose node-type is a member of TYPES."
  (let (result)
    (dotimes (i (treesit-node-child-count node))
      (let ((child (treesit-node-child node i)))
        (when (member (treesit-node-type child) types)
          (push child result))))
    (nreverse result)))

(defun verilog-ts--id-children (node)
  "Return text of every direct simple/escaped identifier child of NODE."
  (mapcar (lambda (c) (treesit-node-text c t))
          (verilog-ts--typed-children node '("simple_identifier" "escaped_identifier"))))

(defun verilog-ts--query-nodes (root pattern)
  "Return all nodes matched by the simple type-only PATTERN in ROOT's subtree.
PATTERN must be a string of the form \"(node_type) @c\" — no structural constraints."
  (mapcar #'cdr (treesit-query-capture root pattern)))

(defun verilog-ts--update-cache ()
  "Recompute `verilog-ts--candidates-cache' for the current buffer.
Intended as an `after-save-hook' so that the next completion request
returns pre-computed results immediately."
  (setq verilog-ts--candidates-cache (verilog-ts--compute-candidates)))

(defun verilog-ts--collect-candidates ()
  "Return completion candidates for the current buffer, using a cache.
Computes and caches the candidates on the first call; subsequent calls
return the cached list until the buffer is saved."
  (or verilog-ts--candidates-cache
      (setq verilog-ts--candidates-cache (verilog-ts--compute-candidates))))

(defun verilog-ts--compute-candidates ()
  "Collect declared identifiers from the current buffer via tree-sitter.
Returns a list of propertized strings; each has a `verilog-ts-type' text property
holding the declaration keyword (port, wire, logic, reg, parameter, ...)."
  (when (treesit-parser-list nil 'verilog)
    (let ((root (treesit-buffer-root-node 'verilog))
          (seen (make-hash-table :test 'equal))
          candidates)
      (cl-flet ((add (name label)
                  (when (and name (not (string-empty-p name)) (not (gethash name seen)))
                    (puthash name t seen)
                    (push (propertize name 'verilog-ts-type label) candidates))))

        ;; ANSI port declarations
        ;; Port name is in a port_identifier child (which wraps simple_identifier).
        (dolist (decl (verilog-ts--query-nodes root "(ansi_port_declaration) @c"))
          (let ((port-id (car (verilog-ts--typed-children decl '("port_identifier")))))
            (add (when port-id (treesit-node-text port-id t)) "port")))

        ;; Non-ANSI port declarations (input x, y; / output z;)
        (dolist (decl (verilog-ts--query-nodes
                       root "(input_declaration) @c
                             (output_declaration) @c
                             (inout_declaration) @c"))
          (dolist (port-list (verilog-ts--typed-children
                              decl '("list_of_port_identifiers"
                                     "list_of_variable_identifiers")))
            (dolist (name (verilog-ts--id-children port-list))
              (add name "port"))))

        ;; Net declarations (wire, tri, wand, wor, ...)
        (dolist (net-decl (verilog-ts--query-nodes root "(net_declaration) @c"))
          (let* ((nt-node (car (verilog-ts--typed-children net-decl '("net_type"))))
                 (label   (if nt-node (treesit-node-text nt-node t) "wire")))
            (dolist (asgn-list (verilog-ts--typed-children
                                net-decl '("list_of_net_decl_assignments")))
              (dolist (asgn (verilog-ts--typed-children asgn-list '("net_decl_assignment")))
                (add (car (verilog-ts--id-children asgn)) label)))))

        ;; Data declarations (logic, reg, bit, int, longint, ...)
        (dolist (data-decl (verilog-ts--query-nodes root "(data_declaration) @c"))
          (let* ((type-node (treesit-search-subtree
                             data-decl
                             (lambda (n) (member (treesit-node-type n)
                                                 '("integer_vector_type" "integer_atom_type")))
                             nil nil 4))
                 (label (if type-node (treesit-node-text type-node t) "var")))
            (dolist (var-list (verilog-ts--typed-children
                               data-decl '("list_of_variable_decl_assignments")))
              (dolist (asgn (verilog-ts--typed-children var-list '("variable_decl_assignment")))
                (add (car (verilog-ts--id-children asgn)) label)))))

        ;; Parameter and localparam declarations
        ;; param_assignment has a parameter_identifier child (not a bare simple_identifier).
        ;; Also include parameter_port_declaration for #(...) module header params.
        (dolist (decl (verilog-ts--query-nodes
                       root "(parameter_declaration) @c
                             (local_parameter_declaration) @c
                             (parameter_port_declaration) @c"))
          (let ((label (if (equal (treesit-node-type decl) "local_parameter_declaration")
                           "localparam" "parameter")))
            (dolist (param-list (verilog-ts--typed-children decl '("list_of_param_assignments")))
              (dolist (asgn (verilog-ts--typed-children param-list '("param_assignment")))
                (let ((param-id (car (verilog-ts--typed-children asgn '("parameter_identifier")))))
                  (add (when param-id (treesit-node-text param-id t)) label))))))

        ;; Genvar declarations
        (dolist (decl (verilog-ts--query-nodes root "(genvar_declaration) @c"))
          (dolist (genvar-list (verilog-ts--typed-children decl '("list_of_genvar_identifiers")))
            (dolist (name (verilog-ts--id-children genvar-list))
              (add name "genvar"))))

        ;; Keywords
        (dolist (kw verilog-ts-keywords)
          (add kw "keyword")))

      (nreverse candidates))))

(defun verilog-ts-capf ()
  "Completion-at-point function for `verilog-ts-mode'.
Gathers declared signals, ports, and variables from the current buffer
using treesitter and presents them as annotated completion candidates."
  (let* ((end   (point))
         (start (save-excursion (skip-syntax-backward "w_") (point))))
    (when-let ((candidates (verilog-ts--collect-candidates)))
      (list start end candidates
            :annotation-function
            (lambda (cand)
              (when-let ((type (get-text-property 0 'verilog-ts-type cand)))
                (concat "  " type)))
            :company-kind
            (lambda (cand)
              (pcase (get-text-property 0 'verilog-ts-type cand)
                ("port"                        'interface)
                ((or "parameter" "localparam") 'constant)
                ("keyword"                     'keyword)
                (_                             'variable)))
            :exclusive 'no))))
